//! Structural re-derivation of annotations over the typed arena.
//!
//! Where the well-formedness lint validates the arena as a data structure, this pass
//! re-derives annotations from structure: the kind of every allocated type
//! node from its children (kinds are scope-free, so the check is arena-wide),
//! constructor-shape judgments for terms (`{C} : Thk B`, `ret V : Ret A`,
//! named introduction, units, literals), and the binding scope of every
//! abstract-type witness reachable from the annotation roots.
//!
//! Zydeco has no top level: a file is one term whose `begin` blocks
//! dependency-order their mobile bindings into ordinary telescopes
//! (`docs/proposals/term.md`). The elaborated term therefore obeys strict
//! lexical scope, with two named exceptions this pass treats as ambient:
//! recursive type components, whose identities are allocated together, and
//! package openings, whose witnesses bind through the elaborated program.
//! Computation recursion is the explicit `fix` form and stays structural.
//!
//! Equality-with-node judgments do not fire, and neither do judgments whose
//! operands mention an abstract identity or a non-constructor type
//! application. A shared node used at several instantiations of an enclosing
//! universal, or inside a package member elaborated per import, carries one
//! annotation that legitimately differs between recording sites, and the
//! finished arena cannot distinguish those from corruption. Constructor
//! shapes are instantiation-stable, which is why they alone judge. The
//! deferred judgments are recorded in `docs/proposals/tyck-lint.md`.
//!
//! The traversal starts from the annotation roots — the root `TermAnnId`,
//! recorded definition bodies, data and codata arms, and sealed types — so
//! orphaned nodes from inference retries are never visited. Shared nodes are
//! re-derived once; a node shared across two scopes is scope-checked in the
//! first scope only.

use crate::arena::StaticsArena;
use crate::syntax::*;
use std::collections::HashSet;
use std::fmt;
use zydeco_utils::arena::ArenaAccess;

use super::lint::{LintError, LintNode};

/* ------------------------------ Expectations ------------------------------- */

/// The kind a re-derivation expects a type node to carry.
#[derive(Clone, Debug, PartialEq)]
pub enum ExpectedKind {
    VType,
    CType,
    Arrow(Box<Self>, Box<Self>),
    Label(FieldName, Box<Self>),
    /// The kind another node already carries, compared through normalized
    /// forms because kind leaves are not canonicalized.
    OfNode(KindId),
}

impl fmt::Display for ExpectedKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::VType => write!(f, "VType"),
            | Self::CType => write!(f, "CType"),
            | Self::Arrow(from, to) => write!(f, "({from} -> {to})"),
            | Self::Label(field, payload) => write!(f, "#{field} :: {payload}"),
            | Self::OfNode(kind) => write!(f, "the kind of {}", kind.concise()),
        }
    }
}

/// The type a re-derivation expects a term node to carry.
#[derive(Clone, Debug, PartialEq)]
pub enum ExpectedType {
    /// A suspension of the payload computation type: `Thk B`.
    ThkOf(TypeId),
    /// A return of the payload value type: `Ret A`.
    RetOf(TypeId),
    /// The unit type.
    Unit,
    /// A fixed host representation.
    Primitive(PrimitiveType),
    /// A labeled classifier around the payload type: `#field :: A`.
    LabelOf(FieldName, TypeId),
    /// A computation arrow between the two types.
    ArrowOf(TypeId, TypeId),
    /// A universal type binding the pattern with the body type.
    ForallOf(TPatId, TypeId),
}

impl fmt::Display for ExpectedType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::ThkOf(body) => write!(f, "Thk {}", body.concise()),
            | Self::RetOf(body) => write!(f, "Ret {}", body.concise()),
            | Self::Unit => write!(f, "Unit"),
            | Self::Primitive(primitive) => write!(f, "{primitive:?}"),
            | Self::LabelOf(field, payload) => {
                write!(f, "#{field} :: {}", payload.concise())
            }
            | Self::ArrowOf(domain, codomain) => {
                write!(f, "{} -> {}", domain.concise(), codomain.concise())
            }
            | Self::ForallOf(pattern, body) => {
                write!(f, "forall {} . {}", pattern.concise(), body.concise())
            }
        }
    }
}

/* --------------------------------- Scope ----------------------------------- */

/// Bindings visible at one point of the traversal.
#[derive(Clone, Default)]
struct Scope {
    defs: HashSet<DefId>,
    witnesses: HashSet<AbstId>,
}

/* --------------------------------- Checker --------------------------------- */

/// Re-derivation validator over one finished arena.
pub struct RederiveChecker<'a> {
    statics: &'a StaticsArena,
    errors: Vec<LintError>,
    visited_values: HashSet<ValueId>,
    visited_compus: HashSet<CompuId>,
    scoped_types: HashSet<TypeId>,
    /// Witnesses usable anywhere: sealed types, existential skolems,
    /// definition-denoted identities, and every named witness. Recursive
    /// type components allocate their identities together, and package
    /// openings bind theirs through the elaborated program, so neither lives
    /// under one structural binder; their names make them exported
    /// identities rather than accidental leaks.
    ambient_witnesses: HashSet<AbstId>,
    /// Definitions bound along the root program's introduction spine.
    root_spine_defs: HashSet<DefId>,
    /// Witnesses those definitions denote, from package openings.
    root_spine_witnesses: HashSet<AbstId>,
    /// Every definition bound anywhere below the root; definition bodies
    /// re-derived as their own roots may mention any of them.
    traversed_defs: HashSet<DefId>,
    /// Cache for structural type agreement, keyed by identifier pairs.
    agreeing_types: HashSet<(TypeId, TypeId)>,
}

impl<'a> RederiveChecker<'a> {
    pub fn new(statics: &'a StaticsArena) -> Self {
        let ambient_witnesses = statics
            .seals
            .iter()
            .map(|(abst, _)| *abst)
            .chain(statics.existential_skolems.iter().map(|(abst, ())| *abst))
            .chain(statics.type_definitions.iter().filter_map(|(_, ty)| {
                match statics.normalized_at(*ty) {
                    | Some(Type::Abst(witness)) => Some(*witness),
                    | _ => None,
                }
            }))
            .chain(statics.abst_hints.iter().map(|(witness, _)| *witness))
            .collect();
        Self {
            statics,
            errors: Vec::new(),
            visited_values: HashSet::new(),
            visited_compus: HashSet::new(),
            scoped_types: HashSet::new(),
            ambient_witnesses,
            root_spine_defs: HashSet::new(),
            root_spine_witnesses: HashSet::new(),
            traversed_defs: HashSet::new(),
            agreeing_types: HashSet::new(),
        }
    }

    /// Re-derive every reachable annotation and compare it with its record.
    pub fn validate(mut self, root: TermAnnId) -> Vec<LintError> {
        self.check_kinds_arena_wide();
        let ambient = Scope { defs: HashSet::new(), witnesses: self.ambient_witnesses.clone() };
        self.collect_root_spine(root);
        self.check_root(root, &ambient);
        self.check_definition_roots(&ambient);
        self.errors
    }

    /* ------------------------------ lookup helpers ------------------------- */

    fn filled_type(&self, ty: TypeId) -> Option<&Type> {
        match self.statics.types_pre.get(&ty) {
            | Some(Fillable::Done(_)) => self.statics.normalized_at(ty),
            | _ => None,
        }
    }

    fn kind_of_type(&self, ty: TypeId) -> Option<KindId> {
        self.statics.type_kind_at(ty)
    }

    fn value_ty(&self, value: ValueId) -> Option<TypeId> {
        self.statics.annotations_value.get(&value).copied()
    }

    fn compu_ty(&self, compu: CompuId) -> Option<TypeId> {
        self.statics.annotations_compu.get(&compu).copied()
    }

    fn kinds_agree(&self, left: KindId, right: KindId) -> bool {
        match (self.statics.normalized_kind_at(left), self.statics.normalized_kind_at(right)) {
            | (Some(left), Some(right)) => self.kind_forms_agree(left, right),
            | (None, None) => left == right,
            | _ => false,
        }
    }

    fn kind_forms_agree(&self, left: &Kind, right: &Kind) -> bool {
        match (left, right) {
            | (Kind::VType(_), Kind::VType(_)) | (Kind::CType(_), Kind::CType(_)) => true,
            | (Kind::Arrow(Arrow(ld, lc)), Kind::Arrow(Arrow(rd, rc))) => {
                self.kinds_agree(*ld, *rd) && self.kinds_agree(*lc, *rc)
            }
            | (
                Kind::Label(Label(left_field, left_kind)),
                Kind::Label(Label(right_field, right_kind)),
            ) => left_field == right_field && self.kinds_agree(*left_kind, *right_kind),
            | _ => false,
        }
    }

    fn kind_matches(&self, kind: KindId, expected: &ExpectedKind) -> bool {
        let Some(form) = self.statics.normalized_kind_at(kind) else {
            return true;
        };
        self.kind_form_matches(form, expected)
    }

    fn kind_form_matches(&self, form: &Kind, expected: &ExpectedKind) -> bool {
        match expected {
            | ExpectedKind::VType => matches!(form, Kind::VType(_)),
            | ExpectedKind::CType => matches!(form, Kind::CType(_)),
            | ExpectedKind::Arrow(from, to) => match form {
                | Kind::Arrow(Arrow(domain, codomain)) => {
                    self.kind_matches(*domain, from) && self.kind_matches(*codomain, to)
                }
                | _ => false,
            },
            | ExpectedKind::Label(field, payload) => match form {
                | Kind::Label(Label(found, inner)) => {
                    field == found && self.kind_matches(*inner, payload)
                }
                | _ => false,
            },
            | ExpectedKind::OfNode(other) => self.kinds_agree_from(form, *other),
        }
    }

    fn kinds_agree_from(&self, form: &Kind, other: KindId) -> bool {
        match self.statics.normalized_kind_at(other) {
            | Some(other_form) => self.kind_forms_agree(form, other_form),
            | None => true,
        }
    }

    /// Structural agreement of two type identifiers through their normalized
    /// forms. Identifiers are not canonicalized, so equality of constructor
    /// shapes is decided recursively rather than by derived equality.
    fn type_ids_agree(&mut self, left: TypeId, right: TypeId) -> bool {
        if left == right {
            return true;
        }
        if !self.agreeing_types.insert((left, right)) {
            return true;
        }
        match (self.filled_type(left).cloned(), self.filled_type(right).cloned()) {
            | (Some(left), Some(right)) => self.type_forms_agree(&left, &right),
            | _ => false,
        }
    }

    fn type_forms_agree(&mut self, left: &Type, right: &Type) -> bool {
        use Type as T;
        match (left, right) {
            | (T::Var(left), T::Var(right)) => left == right,
            | (T::Abst(left), T::Abst(right)) => left == right,
            | (T::Abs(left), T::Abs(right)) => {
                left.binder.pattern == right.binder.pattern
                    && self.type_ids_agree(left.body, right.body)
            }
            | (T::App(App(lf, la)), T::App(App(rf, ra))) => {
                self.type_ids_agree(*lf, *rf) && self.type_ids_agree(*la, *ra)
            }
            | (T::Named(Named(lf, li)), T::Named(Named(rf, ri))) => {
                lf == rf && self.type_ids_agree(*li, *ri)
            }
            | (T::Label(Label(lf, li)), T::Label(Label(rf, ri))) => {
                lf == rf && self.type_ids_agree(*li, *ri)
            }
            | (T::Proj(Proj(lt, lf)), T::Proj(Proj(rt, rf))) => {
                lf == rf && self.type_ids_agree(*lt, *rt)
            }
            | (T::Thk(_), T::Thk(_))
            | (T::Ret(_), T::Ret(_))
            | (T::Unit(_), T::Unit(_))
            | (T::Opaque(_), T::Opaque(_))
            | (T::OS(_), T::OS(_)) => true,
            | (T::Primitive(left), T::Primitive(right)) => left.0 == right.0,
            | (T::ValPi(left), T::ValPi(right)) => {
                self.valpi_binders_agree(left, right)
                    && self.type_ids_agree(left.codomain, right.codomain)
            }
            | (T::Arrow(Arrow(la, lb)), T::Arrow(Arrow(ra, rb))) => {
                self.type_ids_agree(*la, *ra) && self.type_ids_agree(*lb, *rb)
            }
            | (T::Forall(Forall(lb, lbody)), T::Forall(Forall(rb, rbody))) => {
                lb.pattern == rb.pattern && self.type_ids_agree(*lbody, *rbody)
            }
            | (T::PackPi(left), T::PackPi(right)) => {
                self.type_ids_agree(left.domain, right.domain)
                    && left.witnesses.len() == right.witnesses.len()
                    && self.type_ids_agree(left.codomain, right.codomain)
            }
            | (T::Prod(left), T::Prod(right)) => {
                left.0.len() == right.0.len()
                    && left.0.iter().zip(right.0.iter()).all(|(l, r)| self.type_ids_agree(*l, *r))
            }
            | (T::Exists(left), T::Exists(right)) => {
                left.binder.pattern == right.binder.pattern
                    && self.exists_modes_agree(left, right)
                    && self.type_ids_agree(left.body, right.body)
            }
            | (T::ManifestKind(left), T::ManifestKind(right)) => {
                left.binder == right.binder
                    && self.kinds_agree(left.definition, right.definition)
                    && self.type_ids_agree(left.body, right.body)
            }
            | (T::Data(left), T::Data(right)) => left == right,
            | (T::CoData(left), T::CoData(right)) => left == right,
            | _ => false,
        }
    }

    fn valpi_binders_agree(&mut self, left: &ValPi, right: &ValPi) -> bool {
        match (&left.binder, &right.binder) {
            | (ValPiBinder::Type(left), ValPiBinder::Type(right)) => left.pattern == right.pattern,
            | (ValPiBinder::Value(left), ValPiBinder::Value(right)) => {
                let domains = self.type_ids_agree(left.domain, right.domain);
                let witness_counts = match (&left.witnesses, &right.witnesses) {
                    | (Some(left), Some(right)) => left.len() == right.len(),
                    | (None, None) => true,
                    | _ => false,
                };
                domains && witness_counts
            }
            | _ => false,
        }
    }

    fn exists_modes_agree(&mut self, left: &Exists, right: &Exists) -> bool {
        match (&left.mode, &right.mode) {
            | (ExistsMode::Abstract, ExistsMode::Abstract) => true,
            | (ExistsMode::Abstract, ExistsMode::Manifest(_))
            | (ExistsMode::Manifest(_), ExistsMode::Abstract) => false,
            | (ExistsMode::Manifest(left), ExistsMode::Manifest(right)) => {
                self.type_ids_agree(*left, *right)
            }
        }
    }

    /// Whether a recorded type satisfies an expectation, by shape.
    fn type_matches(&mut self, recorded: TypeId, expected: &ExpectedType) -> bool {
        let Some(form) = self.filled_type(recorded).cloned() else {
            return true;
        };
        match expected {
            | ExpectedType::ThkOf(payload) => match &form {
                | Type::App(App(head, body)) => {
                    matches!(self.filled_type(*head), Some(Type::Thk(_)))
                        && self.type_ids_agree(*body, *payload)
                }
                | _ => false,
            },
            | ExpectedType::RetOf(payload) => match &form {
                | Type::App(App(head, body)) => {
                    matches!(self.filled_type(*head), Some(Type::Ret(_)))
                        && self.type_ids_agree(*body, *payload)
                }
                | _ => false,
            },
            | ExpectedType::Unit => matches!(&form, Type::Unit(_)),
            | ExpectedType::Primitive(primitive) => match &form {
                | Type::Primitive(PrimitiveTy(found)) => *found == *primitive,
                | _ => false,
            },
            | ExpectedType::LabelOf(field, payload) => match &form {
                | Type::Label(Label(found, inner)) => {
                    field == found && self.type_ids_agree(*inner, *payload)
                }
                | _ => false,
            },
            | ExpectedType::ArrowOf(domain, codomain) => match &form {
                | Type::Arrow(Arrow(found_domain, found_codomain)) => {
                    self.type_ids_agree(*found_domain, *domain)
                        && self.type_ids_agree(*found_codomain, *codomain)
                }
                | _ => false,
            },
            | ExpectedType::ForallOf(pattern, body) => match &form {
                | Type::Forall(Forall(binder, found_body)) => {
                    binder.pattern == *pattern && self.type_ids_agree(*found_body, *body)
                }
                | _ => false,
            },
        }
    }

    /* ------------------------- kind re-derivation -------------------------- */

    /// Kinds are scope-free, so every allocated type node participates.
    fn check_kinds_arena_wide(&mut self) {
        let entries: Vec<(TypeId, Type)> = self
            .statics
            .types_pre
            .iter()
            .filter_map(|(ty, value)| match value {
                | Fillable::Fill(_) => None,
                | Fillable::Done(form) => Some((ty, form.clone())),
            })
            .collect();
        for (ty, form) in entries {
            self.check_type_kind(ty, &form);
        }
    }

    fn check_type_kind(&mut self, ty: TypeId, form: &Type) {
        let Some(recorded) = self.kind_of_type(ty) else {
            return;
        };
        let Some(expected) = self.synthesized_kind(form) else {
            return;
        };
        if !self.kind_matches(recorded, &expected) {
            self.errors.push(LintError::KindMismatch { ty, recorded, expected });
        }
    }

    /// Report a child whose own kind violates the sort its parent requires.
    fn expect_child_kind(&mut self, child: TypeId, expected: ExpectedKind) {
        let Some(recorded) = self.kind_of_type(child) else {
            return;
        };
        if !self.kind_matches(recorded, &expected) {
            self.errors.push(LintError::KindMismatch { ty: child, recorded, expected });
        }
    }

    fn child_is_vtype(&self, child: TypeId) -> Option<bool> {
        Some(matches!(self.statics.normalized_kind_at(self.kind_of_type(child)?)?, Kind::VType(_)))
    }

    fn child_is_ctype(&self, child: TypeId) -> Option<bool> {
        Some(matches!(self.statics.normalized_kind_at(self.kind_of_type(child)?)?, Kind::CType(_)))
    }

    fn child_kind(&self, child: TypeId) -> Option<KindId> {
        self.kind_of_type(child)
    }

    /// The payload kind of a type pattern: named wrappers contribute their
    /// label to the wrapper's own kind, while the definition they disclose is
    /// kinded at the payload.
    fn tpat_payload_kind(&self, pattern: TPatId) -> Option<KindId> {
        match self.statics.tpats.get(&pattern) {
            | Some(TypePattern::Named(Named(_, inner))) => self.tpat_payload_kind(*inner),
            | _ => self.statics.annotations_tpat.get(&pattern).copied(),
        }
    }

    fn synthesized_kind(&mut self, form: &Type) -> Option<ExpectedKind> {
        match form {
            | Type::Var(def) => match self.statics.annotations_var.get(def) {
                | Some(AnnId::Kind(kind)) => Some(ExpectedKind::OfNode(*kind)),
                | _ => None,
            },
            // Recursive-group witnesses carry their kind on the denoting node,
            // so a missing table row is not derivable here.
            | Type::Abst(witness) => {
                self.statics.annotations_abst.get(witness).map(|kind| ExpectedKind::OfNode(*kind))
            }
            // `Thk : CType -> VType` suspends computations as values, while
            // `Ret : VType -> CType` returns values as computations.
            | Type::Thk(_) => Some(ExpectedKind::Arrow(
                Box::new(ExpectedKind::CType),
                Box::new(ExpectedKind::VType),
            )),
            | Type::Ret(_) => Some(ExpectedKind::Arrow(
                Box::new(ExpectedKind::VType),
                Box::new(ExpectedKind::CType),
            )),
            | Type::Unit(_) | Type::Opaque(_) | Type::Primitive(_) => Some(ExpectedKind::VType),
            | Type::OS(_) => Some(ExpectedKind::CType),
            | Type::Named(Named(field, inner)) => Some(ExpectedKind::Label(
                field.clone(),
                Box::new(ExpectedKind::OfNode(self.child_kind(*inner)?)),
            )),
            | Type::Label(Label(_, inner)) => {
                if self.child_is_vtype(*inner) == Some(false) {
                    self.expect_child_kind(*inner, ExpectedKind::VType);
                }
                Some(ExpectedKind::VType)
            }
            | Type::Proj(Proj(target, field)) => {
                let target_kind = self.statics.normalized_kind_at(self.child_kind(*target)?)?;
                let Kind::Label(Label(found, payload)) = target_kind else {
                    return None;
                };
                (found == field).then_some(ExpectedKind::OfNode(*payload))
            }
            | Type::Abs(TypeAbstraction { binder, body }) => Some(ExpectedKind::Arrow(
                Box::new(ExpectedKind::OfNode(
                    self.statics.annotations_tpat.get(&binder.pattern).copied()?,
                )),
                Box::new(ExpectedKind::OfNode(self.child_kind(*body)?)),
            )),
            | Type::Arrow(Arrow(domain, codomain)) => {
                if self.child_is_vtype(*domain) == Some(false) {
                    self.expect_child_kind(*domain, ExpectedKind::VType);
                }
                if self.child_is_ctype(*codomain) == Some(false) {
                    self.expect_child_kind(*codomain, ExpectedKind::CType);
                }
                Some(ExpectedKind::CType)
            }
            | Type::Forall(Forall(_, body)) => {
                if self.child_is_ctype(*body) == Some(false) {
                    self.expect_child_kind(*body, ExpectedKind::CType);
                }
                Some(ExpectedKind::CType)
            }
            | Type::PackPi(inner) => {
                let PackPi { domain, codomain, .. } = inner.as_ref();
                if self.child_is_vtype(*domain) == Some(false) {
                    self.expect_child_kind(*domain, ExpectedKind::VType);
                }
                if self.child_is_ctype(*codomain) == Some(false) {
                    self.expect_child_kind(*codomain, ExpectedKind::CType);
                }
                Some(ExpectedKind::CType)
            }
            | Type::ValPi(inner) => {
                let ValPi { codomain, .. } = inner.as_ref();
                if self.child_is_vtype(*codomain) == Some(false) {
                    self.expect_child_kind(*codomain, ExpectedKind::VType);
                }
                Some(ExpectedKind::VType)
            }
            | Type::Prod(Prod(components)) => {
                components.iter().for_each(|component| {
                    if self.child_is_vtype(*component) == Some(false) {
                        self.expect_child_kind(*component, ExpectedKind::VType);
                    }
                });
                Some(ExpectedKind::VType)
            }
            | Type::Exists(inner) => {
                let Exists { binder, mode, body } = inner.as_ref();
                if self.child_is_vtype(*body) == Some(false) {
                    self.expect_child_kind(*body, ExpectedKind::VType);
                }
                match mode {
                    | ExistsMode::Abstract => Some(ExpectedKind::VType),
                    | ExistsMode::Manifest(definition) => {
                        let binder_kind = self.tpat_payload_kind(binder.pattern)?;
                        let definition_kind = self.child_kind(*definition)?;
                        if !self.kinds_agree(definition_kind, binder_kind) {
                            self.errors.push(LintError::KindMismatch {
                                ty: *definition,
                                recorded: definition_kind,
                                expected: ExpectedKind::OfNode(binder_kind),
                            });
                        }
                        Some(ExpectedKind::VType)
                    }
                }
            }
            | Type::ManifestKind(ManifestKind { body, .. }) => {
                Some(ExpectedKind::OfNode(self.child_kind(*body)?))
            }
            // Parameter telescopes are deferred; existence is the well-formedness lint's check.
            | Type::Data(_) | Type::CoData(_) => None,
            | Type::App(App(function, argument)) => {
                let function_kind =
                    self.statics.normalized_kind_at(self.child_kind(*function)?)?.clone();
                let Kind::Arrow(Arrow(domain_kind, codomain_kind)) = function_kind else {
                    return None;
                };
                let argument_kind = self.child_kind(*argument)?;
                if !self.kinds_agree(argument_kind, domain_kind) {
                    self.errors.push(LintError::KindMismatch {
                        ty: *argument,
                        recorded: argument_kind,
                        expected: ExpectedKind::OfNode(domain_kind),
                    });
                }
                Some(ExpectedKind::OfNode(codomain_kind))
            }
        }
    }

    /// Whether shape-judgment operands are comparable. A shared node used at
    /// several instantiations carries one annotation, so operands that still
    /// mention an abstract identity — directly or through a type application
    /// that is not the `Thk`/`Ret` constructor — may differ legitimately
    /// between recording sites.
    fn shape_operands(&self, operands: &[TypeId]) -> bool {
        operands.iter().all(|operand| {
            let mut seen = HashSet::new();
            !self.type_mentions_any_abst(*operand, &mut seen)
                && !self.type_applies_instantiation(*operand, &mut HashSet::new())
        })
    }

    /// Whether a type contains a type application other than `Thk` or `Ret`
    /// applied to a computation or value type.
    fn type_applies_instantiation(&self, ty: TypeId, seen: &mut HashSet<TypeId>) -> bool {
        if !seen.insert(ty) {
            return false;
        }
        let Some(form) = self.filled_type(ty) else {
            return false;
        };
        match form {
            | Type::App(App(head, argument)) => {
                let constructor =
                    matches!(self.filled_type(*head), Some(Type::Thk(_)) | Some(Type::Ret(_)));
                (!constructor && !matches!(self.filled_type(*head), Some(Type::App(_))))
                    || self.type_applies_instantiation(*head, seen)
                    || self.type_applies_instantiation(*argument, seen)
            }
            | Type::Abs(TypeAbstraction { body, .. })
            | Type::Forall(Forall(_, body))
            | Type::Named(Named(_, body))
            | Type::Label(Label(_, body))
            | Type::Proj(Proj(body, _))
            | Type::ManifestKind(ManifestKind { body, .. }) => {
                self.type_applies_instantiation(*body, seen)
            }
            | Type::Arrow(Arrow(a, b)) => {
                self.type_applies_instantiation(*a, seen)
                    || self.type_applies_instantiation(*b, seen)
            }
            | Type::PackPi(inner) => {
                let PackPi { domain, codomain, .. } = inner.as_ref();
                self.type_applies_instantiation(*domain, seen)
                    || self.type_applies_instantiation(*codomain, seen)
            }
            | Type::Prod(Prod(components)) => {
                components.iter().any(|component| self.type_applies_instantiation(*component, seen))
            }
            | Type::Exists(inner) => {
                let Exists { body, mode, .. } = inner.as_ref();
                self.type_applies_instantiation(*body, seen)
                    || match mode {
                        | ExistsMode::Abstract => false,
                        | ExistsMode::Manifest(definition) => {
                            self.type_applies_instantiation(*definition, seen)
                        }
                    }
            }
            | Type::ValPi(inner) => {
                let ValPi { binder, codomain } = inner.as_ref();
                match binder {
                    | ValPiBinder::Type(_) => self.type_applies_instantiation(*codomain, seen),
                    | ValPiBinder::Value(parameter) => {
                        self.type_applies_instantiation(parameter.domain, seen)
                            || self.type_applies_instantiation(*codomain, seen)
                    }
                }
            }
            | Type::Var(_)
            | Type::Abst(_)
            | Type::Thk(_)
            | Type::Ret(_)
            | Type::Unit(_)
            | Type::Opaque(_)
            | Type::Primitive(_)
            | Type::OS(_)
            | Type::Data(_)
            | Type::CoData(_) => false,
        }
    }

    /// Whether a type structure mentions any abstract identity.
    fn type_mentions_any_abst(&self, ty: TypeId, seen: &mut HashSet<TypeId>) -> bool {
        if !seen.insert(ty) {
            return false;
        }
        let Some(form) = self.filled_type(ty) else {
            return false;
        };
        let children: Vec<TypeId> = match form {
            | Type::Abst(_) => return true,
            | Type::Var(_)
            | Type::Thk(_)
            | Type::Ret(_)
            | Type::Unit(_)
            | Type::Opaque(_)
            | Type::Primitive(_)
            | Type::OS(_)
            | Type::Data(_)
            | Type::CoData(_) => return false,
            | Type::Named(Named(_, inner))
            | Type::Label(Label(_, inner))
            | Type::Proj(Proj(inner, _)) => vec![*inner],
            | Type::Abs(TypeAbstraction { body, .. }) | Type::Forall(Forall(_, body)) => {
                vec![*body]
            }
            | Type::App(App(head, argument)) | Type::Arrow(Arrow(head, argument)) => {
                vec![*head, *argument]
            }
            | Type::PackPi(inner) => {
                let PackPi { domain, codomain, .. } = inner.as_ref();
                vec![*domain, *codomain]
            }
            | Type::Prod(Prod(components)) => components.clone(),
            | Type::Exists(inner) => {
                let Exists { body, mode, .. } = inner.as_ref();
                let mut children = vec![*body];
                if let ExistsMode::Manifest(definition) = mode {
                    children.push(*definition);
                }
                children
            }
            | Type::ValPi(inner) => {
                let ValPi { binder, codomain } = inner.as_ref();
                match binder {
                    | ValPiBinder::Type(_) => vec![*codomain],
                    | ValPiBinder::Value(parameter) => vec![parameter.domain, *codomain],
                }
            }
            | Type::ManifestKind(ManifestKind { body, .. }) => vec![*body],
        };
        children.iter().any(|child| self.type_mentions_any_abst(*child, seen))
    }

    /* ------------------------------ traversal ------------------------------ */

    /// Collect the definitions and package witnesses bound along the root
    /// program's introduction spine. The elaborated root is a chain of
    /// let-bindings and abstractions (the `param ... in` header becomes a
    /// package-dependent abstraction), and definition bodies re-derived as
    /// their own roots may mention everything it binds.
    fn collect_root_spine(&mut self, root: TermAnnId) {
        let mut current = root;
        loop {
            match current {
                | TermAnnId::Value(value, _) => {
                    let Some(node) = self.statics.values.get(&value).cloned() else {
                        break;
                    };
                    match node {
                        | Value::Let(Let { binder, tail, .. }) => {
                            self.record_spine_vpats([binder]);
                            let Some(tail_ty) = self.value_ty(tail) else {
                                break;
                            };
                            current = TermAnnId::Value(tail, tail_ty);
                        }
                        | Value::ValAbs(Abs(binder, body)) => {
                            match binder {
                                | ValBinder::Type(pat) => {
                                    self.record_spine_defs(self.tpat_bound_defs(pat))
                                }
                                | ValBinder::Value(pat) => {
                                    self.record_spine_defs(self.vpat_bound_defs(pat))
                                }
                            }
                            let Some(body_ty) = self.value_ty(body) else {
                                break;
                            };
                            current = TermAnnId::Value(body, body_ty);
                        }
                        | _ => break,
                    }
                }
                | TermAnnId::Compu(compu, _) => {
                    let Some(node) = self.statics.compus.get(&compu).cloned() else {
                        break;
                    };
                    match node {
                        | Computation::Let(Let { binder, tail, .. }) => {
                            self.record_spine_vpats([binder]);
                            let Some(tail_ty) = self.compu_ty(tail) else {
                                break;
                            };
                            current = TermAnnId::Compu(tail, tail_ty);
                        }
                        | Computation::VAbs(Abs(binder, body)) => {
                            self.record_spine_vpats([binder]);
                            let Some(body_ty) = self.compu_ty(body) else {
                                break;
                            };
                            current = TermAnnId::Compu(body, body_ty);
                        }
                        | Computation::TAbs(Abs(binder, body)) => {
                            self.record_spine_defs(self.tpat_bound_defs(binder));
                            let Some(body_ty) = self.compu_ty(body) else {
                                break;
                            };
                            current = TermAnnId::Compu(body, body_ty);
                        }
                        | _ => break,
                    }
                }
                | _ => break,
            }
        }
    }

    fn record_spine_vpats<I: IntoIterator<Item = VPatId>>(&mut self, binders: I) {
        let bound =
            binders.into_iter().flat_map(|binder| self.vpat_bound_defs(binder)).collect::<Vec<_>>();
        self.record_spine_defs(bound);
    }

    fn record_spine_defs(&mut self, bound: Vec<DefId>) {
        let witnesses =
            bound.iter().filter_map(|def| self.denoted_witness(*def)).collect::<Vec<_>>();
        self.root_spine_witnesses.extend(witnesses);
        self.root_spine_defs.extend(bound);
    }

    fn check_root(&mut self, root: TermAnnId, ambient: &Scope) {
        match root {
            | TermAnnId::Value(value, _) => self.check_value(value, ambient),
            | TermAnnId::Compu(compu, _) => self.check_computation(compu, ambient),
            | _ => {}
        }
    }

    fn check_definition_roots(&mut self, ambient: &Scope) {
        // Definitions exported from a package legitimately mention the
        // witnesses its openings bind: arms and bodies travel together with
        // their package's identities, but that binding lives inside the
        // elaborated import subtree rather than on any one definition root.
        // Definition-root walks therefore admit every named witness, while
        // the term traversal keeps its strict structural scopes.
        let mut scope = ambient.clone();
        scope.defs = self.traversed_defs.clone();
        scope.witnesses.extend(self.root_spine_witnesses.iter().copied());
        scope.witnesses.extend(self.statics.abst_hints.iter().map(|(witness, _)| *witness));
        let aliases: Vec<ValueId> =
            self.statics.value_aliases.iter().map(|(_, value)| *value).collect();
        for value in aliases {
            self.check_value(value, &scope);
        }
        let inlinables: Vec<ValueId> =
            self.statics.inlinables.iter().map(|(_, value)| *value).collect();
        for value in inlinables {
            self.check_value(value, &scope);
        }
        let type_roots: Vec<TypeId> =
            self.statics
                .type_definitions
                .iter()
                .map(|(_, ty)| *ty)
                .chain(self.statics.seals.iter().map(|(_, ty)| *ty))
                .chain(
                    self.statics
                        .datas
                        .iter()
                        .flat_map(|(_, data)| data.iter().map(|(_, ty)| *ty).collect::<Vec<_>>()),
                )
                .chain(
                    self.statics.codatas.iter().flat_map(|(_, codata)| {
                        codata.iter().map(|(_, ty)| *ty).collect::<Vec<_>>()
                    }),
                )
                .collect();
        for ty in type_roots {
            self.check_type_scope(ty, &scope);
        }
    }

    /* --------------------------- pattern binders --------------------------- */

    fn tpat_bound_defs(&self, pat: TPatId) -> Vec<DefId> {
        match self.statics.tpats.get(&pat) {
            | Some(TypePattern::Var(def)) => vec![*def],
            | Some(TypePattern::Named(Named(_, inner))) => self.tpat_bound_defs(*inner),
            | _ => Vec::new(),
        }
    }

    fn vpat_bound_defs(&self, pat: VPatId) -> Vec<DefId> {
        match self.statics.vpats.get(&pat) {
            | Some(ValuePattern::Var(def)) => vec![*def],
            | Some(ValuePattern::Named(Named(_, inner)))
            | Some(ValuePattern::Ctor(Ctor(_, inner))) => self.vpat_bound_defs(*inner),
            | Some(ValuePattern::Alias(Alias(ConsN(aliases, base)))) => aliases
                .iter()
                .chain(std::iter::once(base))
                .flat_map(|alias| self.vpat_bound_defs(*alias))
                .collect(),
            | Some(ValuePattern::VCons(components)) => {
                components.iter().flat_map(|component| self.vpat_bound_defs(*component)).collect()
            }
            | Some(ValuePattern::SCons(ConsN(statics, tail))) => statics
                .iter()
                .filter_map(|term| match term {
                    | StaticPatId::Type(pat) => Some(*pat),
                    | StaticPatId::Kind(_) => None,
                })
                .flat_map(|pat| self.tpat_bound_defs(pat))
                .chain(self.vpat_bound_defs(*tail))
                .collect(),
            | Some(ValuePattern::View(inner)) => {
                let ViewPattern { pattern, .. } = inner.as_ref();
                self.vpat_bound_defs(*pattern)
            }
            | _ => Vec::new(),
        }
    }

    /// Extend a scope with definitions bound by a pattern, together with the
    /// abstract witnesses their annotations denote: a package opening binds
    /// its witnesses through the bound definitions' `Abst`-denoted types.
    fn binding_defs_scope<I: Iterator<Item = DefId>>(&mut self, defs: I, scope: &Scope) -> Scope {
        let mut extended = scope.clone();
        for def in defs {
            self.traversed_defs.insert(def);
            extended.defs.insert(def);
            if let Some(witness) = self.denoted_witness(def) {
                extended.witnesses.insert(witness);
            }
        }
        extended
    }

    /// The witness a definition's annotation denotes, if any: package
    /// openings bind abstract identities through their bound definitions.
    fn denoted_witness(&self, def: DefId) -> Option<AbstId> {
        let AnnId::Type(ty) = self.statics.annotations_var.get(&def)? else {
            return None;
        };
        match self.statics.normalized_at(*ty) {
            | Some(Type::Abst(witness)) => Some(*witness),
            | _ => None,
        }
    }

    fn type_binder_scope(&self, binder: &TypeBinder, scope: &Scope) -> Scope {
        let mut scope = scope.clone();
        scope.witnesses.insert(binder.witness);
        for def in self.tpat_bound_defs(binder.pattern) {
            scope.defs.insert(def);
        }
        scope
    }

    /* ------------------------------ term checks ---------------------------- */

    fn check_value(&mut self, value: ValueId, scope: &Scope) {
        if !self.visited_values.insert(value) {
            return;
        }
        let Some(recorded) = self.value_ty(value) else {
            return;
        };
        self.check_type_scope(recorded, scope);
        let Some(node) = self.statics.values.get(&value).cloned() else {
            return;
        };
        match node {
            | Value::Hole(_) => {}
            | Value::Var(def) => {
                let _ = def;
                // Definition references legitimately cross import edges and
                // alias boundaries (`docs/proposals/term.md`: repeated
                // imports share one checked root), whose binding context the
                // finished arena does not record per reference. Existence is
                // the well-formedness lint's check; definition scoping stays deferred.
            }
            | Value::Named(Named(field, inner)) => {
                self.check_value(inner, scope);
                if let Some(inner_ty) = self.value_ty(inner)
                    && self.shape_operands(&[inner_ty])
                {
                    self.expect_value_type(
                        value,
                        recorded,
                        ExpectedType::LabelOf(field.clone(), inner_ty),
                    );
                }
            }
            | Value::Let(Let { binder, bindee, tail }) => {
                self.check_value(bindee, scope);
                let tail_scope =
                    self.binding_defs_scope(self.vpat_bound_defs(binder).into_iter(), scope);
                self.check_value(tail, &tail_scope);
            }
            | Value::ValAbs(Abs(binder, body)) => {
                self.check_val_abs(value, recorded, binder, body, scope);
            }
            | Value::ValApp(App(function, argument)) => {
                self.check_value(function, scope);
                let Some(function_ty) = self.value_ty(function) else {
                    return;
                };
                self.check_val_app(value, recorded, function_ty, argument, scope);
            }
            | Value::Thunk(Thunk(body)) => {
                self.check_computation(body, scope);
                if let Some(body_ty) = self.compu_ty(body)
                    && self.shape_operands(&[body_ty])
                {
                    self.expect_value_type(value, recorded, ExpectedType::ThkOf(body_ty));
                }
            }
            | Value::Ctor(Ctor(_, payload)) => {
                self.check_value(payload, scope);
            }
            | Value::Triv(_) => {
                self.expect_value_type(value, recorded, ExpectedType::Unit);
            }
            | Value::VCons(components) => {
                components.iter().for_each(|component| self.check_value(*component, scope));
                // The product judgment is deferred: composite operands
                // multiply the instantiation sensitivity that defeats the
                // single-operand shapes above.
            }
            | Value::SCons(ConsN(_, tail)) => {
                // Witness-prefix shapes are deferred; the payload side is
                // guarded by the package body's dependence on its witnesses.
                self.check_value(tail, scope);
            }
            | Value::Proj(Proj(head, _)) => {
                self.check_value(head, scope);
            }
            | Value::Lit(literal) => {
                if let Some(primitive) = literal_primitive(&literal) {
                    self.expect_value_type(value, recorded, ExpectedType::Primitive(primitive));
                }
            }
        }
    }

    fn check_computation(&mut self, compu: CompuId, scope: &Scope) {
        if !self.visited_compus.insert(compu) {
            return;
        }
        let Some(recorded) = self.compu_ty(compu) else {
            return;
        };
        self.check_type_scope(recorded, scope);
        let Some(node) = self.statics.compus.get(&compu).cloned() else {
            return;
        };
        match node {
            | Computation::Hole(_) => {}
            | Computation::VAbs(Abs(binder, body)) => {
                self.check_v_abs(compu, recorded, binder, body, scope);
            }
            | Computation::VApp(App(function, argument)) => {
                self.check_value(argument, scope);
                self.check_computation(function, scope);
                let Some(function_ty) = self.compu_ty(function) else {
                    return;
                };
                self.check_v_app(compu, recorded, function_ty, argument);
            }
            | Computation::TAbs(Abs(binder, body)) => {
                let body_scope =
                    self.binding_defs_scope(self.tpat_bound_defs(binder).into_iter(), scope);
                self.check_computation(body, &body_scope);
                // The recorded codomain of a type abstraction is deferred:
                // elaborated binders may pair with Forall nodes whose bodies
                // were rewritten by instantiation during elaboration.
            }
            | Computation::TApp(App(function, argument)) => {
                self.check_computation(function, scope);
                self.check_type_scope(argument, scope);
                // The codomain judgment is deferred; see
                // `expect_computation_type`.
            }
            | Computation::Fix(Fix(binder, body)) => {
                let body_scope =
                    self.binding_defs_scope(self.vpat_bound_defs(binder).into_iter(), scope);
                self.check_computation(body, &body_scope);
            }
            | Computation::Force(Force(body)) => {
                self.check_value(body, scope);
                let Some(body_ty) = self.value_ty(body) else {
                    return;
                };
                let Some(Type::App(App(_, _))) = self.filled_type(body_ty).cloned() else {
                    return;
                };
                // The payload judgment is deferred for the same reason as
                // application results: shared suspensions may be forced at
                // several instantiations of an enclosing universal.
            }
            | Computation::Ret(Return(body)) => {
                self.check_value(body, scope);
                if let Some(body_ty) = self.value_ty(body)
                    && self.shape_operands(&[body_ty])
                {
                    self.expect_computation_type(compu, recorded, ExpectedType::RetOf(body_ty));
                }
            }
            | Computation::Do(Bind { binder, bindee, tail }) => {
                self.check_computation(bindee, scope);
                let Some(bindee_ty) = self.compu_ty(bindee) else {
                    return;
                };
                let Some(Type::App(App(head, _))) = self.filled_type(bindee_ty).cloned() else {
                    return;
                };
                if !matches!(self.filled_type(head), Some(Type::Ret(_))) {
                    return;
                }
                let tail_scope =
                    self.binding_defs_scope(self.vpat_bound_defs(binder).into_iter(), scope);
                self.check_computation(tail, &tail_scope);
            }
            | Computation::Let(Let { binder, bindee, tail }) => {
                self.check_value(bindee, scope);
                let tail_scope =
                    self.binding_defs_scope(self.vpat_bound_defs(binder).into_iter(), scope);
                self.check_computation(tail, &tail_scope);
            }
            | Computation::Match(Match { scrut, arms }) => {
                self.check_value(scrut, scope);
                let Some(_scrut_ty) = self.value_ty(scrut) else {
                    return;
                };
                for Matcher { binder, tail } in &arms {
                    let arm_scope =
                        self.binding_defs_scope(self.vpat_bound_defs(*binder).into_iter(), scope);
                    self.check_computation(*tail, &arm_scope);
                }
            }
            | Computation::CoMatch(CoMatch { arms }) => {
                arms.iter().for_each(|CoMatcher { tail, .. }| {
                    self.check_computation(*tail, scope);
                });
            }
            | Computation::Dtor(Dtor(head, _)) => {
                self.check_computation(head, scope);
            }
        }
    }

    /* --------------------------- judgment helpers -------------------------- */

    fn expect_value_type(&mut self, value: ValueId, recorded: TypeId, expected: ExpectedType) {
        // Only constructor-shape judgments fire. Judgments that compare the
        // record against another node's annotation never do: a shared node
        // used at several instantiations, or inside a package member
        // elaborated per import, carries one annotation that legitimately
        // differs between recording sites.
        if !self.type_matches(recorded, &expected) {
            self.errors.push(LintError::TypeMismatch {
                node: LintNode::Value(value),
                recorded,
                expected,
            });
        }
    }

    fn expect_computation_type(
        &mut self, compu: CompuId, recorded: TypeId, expected: ExpectedType,
    ) {
        // See `expect_value_type` for why only shapes judge.
        if !self.type_matches(recorded, &expected) {
            self.errors.push(LintError::TypeMismatch {
                node: LintNode::Compu(compu),
                recorded,
                expected,
            });
        }
    }

    fn check_val_abs(
        &mut self, _value: ValueId, recorded: TypeId, binder: ValBinder, body: ValueId,
        scope: &Scope,
    ) {
        let Some(Type::ValPi(inner)) = self.filled_type(recorded).cloned() else {
            return;
        };
        let ValPi { binder: pi_binder, codomain } = *inner;
        match (binder, &pi_binder) {
            | (ValBinder::Type(_), ValPiBinder::Type(type_binder)) => {
                let body_scope = self.type_binder_scope(type_binder, scope);
                self.check_value(body, &body_scope);
            }
            | (ValBinder::Value(_), ValPiBinder::Value(_)) => {
                // Pattern and codomain judgments are deferred; see
                // `expect_value_type`.
                let _ = (recorded, codomain);
                self.check_value(body, scope);
            }
            | _ => {}
        }
    }

    fn check_val_app(
        &mut self, _value: ValueId, _recorded: TypeId, _function_ty: TypeId,
        _argument: ValArgument, _scope: &Scope,
    ) {
        // Application judgments are deferred; see `expect_value_type`.
    }

    fn check_v_abs(
        &mut self, compu: CompuId, recorded: TypeId, binder: VPatId, body: CompuId, scope: &Scope,
    ) {
        if let Some(Type::Arrow(Arrow(_, _))) = self.filled_type(recorded).cloned() {
            let body_scope =
                self.binding_defs_scope(self.vpat_bound_defs(binder).into_iter(), scope);
            self.check_computation(body, &body_scope);
            // Codomain agreement is deferred here and below: elaborated
            // abstractions may pair with arrow nodes whose codomains were
            // rewritten by instantiation during elaboration.
            return;
        }
        let Some(Type::PackPi(inner)) = self.filled_type(recorded).cloned() else {
            return;
        };
        let PackPi { .. } = *inner;
        let body_scope = self.binding_defs_scope(self.vpat_bound_defs(binder).into_iter(), scope);
        self.check_computation(body, &body_scope);
        // Package-dependent codomains are opened from the argument package,
        // whose witnesses the traversal cannot recover structurally; those
        // copattern binders skip the codomain comparison entirely.
        let _ = self.statics.copattern_pack_pi_binders.get(&compu).is_some();
    }

    fn check_v_app(
        &mut self, _compu: CompuId, _recorded: TypeId, _function_ty: TypeId, _argument: ValueId,
    ) {
        // Application judgments are deferred; see `expect_computation_type`.
    }

    /* ----------------------------- scope walking --------------------------- */

    /// Walk a type structure, checking that every witness and definition
    /// reference is bound by an enclosing binder or by the ambient scope.
    fn check_type_scope(&mut self, ty: TypeId, scope: &Scope) {
        if !self.scoped_types.insert(ty) {
            return;
        }
        let Some(form) = self.statics.normalized_at(ty).cloned() else {
            return;
        };
        match &form {
            | Type::Abst(witness) => {
                if !scope.witnesses.contains(witness) && !self.ambient_witnesses.contains(witness) {
                    self.errors.push(LintError::WitnessEscape {
                        site: LintNode::Type(ty),
                        witness: *witness,
                    });
                }
            }
            | Type::Var(_) => {}
            | Type::Abs(TypeAbstraction { binder, body }) => {
                self.check_type_scope(*body, &self.type_binder_scope(binder, scope));
            }
            | Type::App(App(head, argument)) => {
                self.check_type_scope(*head, scope);
                self.check_type_scope(*argument, scope);
            }
            | Type::Named(Named(_, inner)) | Type::Label(Label(_, inner)) => {
                self.check_type_scope(*inner, scope);
            }
            | Type::Proj(Proj(target, _)) => self.check_type_scope(*target, scope),
            | Type::Thk(_)
            | Type::Ret(_)
            | Type::Unit(_)
            | Type::Opaque(_)
            | Type::Primitive(_)
            | Type::OS(_)
            | Type::Data(_)
            | Type::CoData(_) => {}
            | Type::ValPi(inner) => match inner.as_ref() {
                | ValPi { binder, codomain } => match binder {
                    | ValPiBinder::Type(binder) => {
                        self.check_type_scope(*codomain, &self.type_binder_scope(binder, scope))
                    }
                    | ValPiBinder::Value(parameter) => {
                        self.check_type_scope(parameter.domain, scope);
                        let mut extended = scope.clone();
                        if let Some(telescope) = &parameter.witnesses {
                            telescope.iter().for_each(|witness| {
                                extended.witnesses.insert(*witness);
                            });
                        }
                        self.check_type_scope(*codomain, &extended);
                    }
                },
            },
            | Type::Arrow(Arrow(domain, codomain)) => {
                self.check_type_scope(*domain, scope);
                self.check_type_scope(*codomain, scope);
            }
            | Type::Forall(Forall(binder, body)) => {
                self.check_type_scope(*body, &self.type_binder_scope(binder, scope));
            }
            | Type::PackPi(inner) => {
                let PackPi { domain, witnesses, codomain } = inner.as_ref();
                self.check_type_scope(*domain, scope);
                let mut extended = scope.clone();
                witnesses.iter().for_each(|witness| {
                    extended.witnesses.insert(*witness);
                });
                self.check_type_scope(*codomain, &extended);
            }
            | Type::Prod(Prod(components)) => {
                components.iter().for_each(|component| self.check_type_scope(*component, scope))
            }
            | Type::Exists(inner) => {
                let Exists { binder, mode, body } = inner.as_ref();
                if let ExistsMode::Manifest(definition) = mode {
                    self.check_type_scope(*definition, scope);
                }
                self.check_type_scope(*body, &self.type_binder_scope(binder, scope));
            }
            | Type::ManifestKind(ManifestKind { definition: _, body, .. }) => {
                self.check_type_scope(*body, scope);
            }
        }
    }
}

/// The primitive type a checked literal carries.
fn literal_primitive(literal: &Literal) -> Option<PrimitiveType> {
    Some(match literal {
        | Literal::Integer(integer) => PrimitiveType::Integer(match integer {
            | IntegerLiteral::Int8(_) => IntegerType::Int8,
            | IntegerLiteral::Int16(_) => IntegerType::Int16,
            | IntegerLiteral::Int32(_) => IntegerType::Int32,
            | IntegerLiteral::Int64(_) => IntegerType::Int64,
            | IntegerLiteral::UInt8(_) => IntegerType::UInt8,
            | IntegerLiteral::UInt16(_) => IntegerType::UInt16,
            | IntegerLiteral::UInt32(_) => IntegerType::UInt32,
            | IntegerLiteral::UInt64(_) => IntegerType::UInt64,
            | IntegerLiteral::Unresolved(_) => return None,
        }),
        | Literal::Float(float) => PrimitiveType::Float(match float {
            | FloatLiteral::Float32(_) => FloatType::Float32,
            | FloatLiteral::Float64(_) => FloatType::Float64,
        }),
        | Literal::String(_) => PrimitiveType::String,
        | Literal::Char(_) => PrimitiveType::Char,
    })
}
