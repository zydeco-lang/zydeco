//! Well-formedness lint over the finished typed arena.
//!
//! The lint re-establishes the arena-wide invariants that the finish phase
//! guarantees — closed holes, complete annotations, consistent annotation
//! sorts, and resolvable references — without repeating any checking rule.
//! It runs only after a check has succeeded, treats every allocated node
//! (orphans from inference retries included) the same way, and reports
//! violations as internal compiler errors rather than user diagnostics.
//! The full design, including the annotation re-derivation this pass
//! deliberately defers, is recorded in `docs/proposals/tyck-lint.md`.

use crate::arena::StaticsArena;
use crate::surface_syntax as su;
use crate::syntax::*;
use std::fmt;
use zydeco_utils::arena::ArenaAccess;

use super::rederive::{ExpectedKind, ExpectedType};

/* -------------------------------- Diagnostics ------------------------------ */

/// One typed node mentioned by a lint diagnostic.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LintNode {
    Kind(KindId),
    Type(TypeId),
    Value(ValueId),
    Compu(CompuId),
    Term(TermId),
    KPat(KPatId),
    TPat(TPatId),
    VPat(VPatId),
    Abst(AbstId),
    Fill(FillId),
    Data(DataId),
    CoData(CoDataId),
    Def(DefId),
}

impl fmt::Display for LintNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (kind, id) = match self {
            | Self::Kind(id) => ("kind", id.concise()),
            | Self::Type(id) => ("type", id.concise()),
            | Self::Value(id) => ("value", id.concise()),
            | Self::Compu(id) => ("computation", id.concise()),
            | Self::Term(TermId::Kind(id)) => ("kind term", id.concise()),
            | Self::Term(TermId::Type(id)) => ("type term", id.concise()),
            | Self::Term(TermId::Value(id)) => ("value term", id.concise()),
            | Self::Term(TermId::Compu(id)) => ("computation term", id.concise()),
            | Self::KPat(id) => ("kind pattern", id.concise()),
            | Self::TPat(id) => ("type pattern", id.concise()),
            | Self::VPat(id) => ("value pattern", id.concise()),
            | Self::Abst(id) => ("abstract type", id.concise()),
            | Self::Fill(id) => ("fill", id.concise()),
            | Self::Data(id) => ("data definition", id.concise()),
            | Self::CoData(id) => ("codata definition", id.concise()),
            | Self::Def(id) => ("definition", id.concise()),
        };
        write!(f, "{kind} {id}")
    }
}

/// The annotation sort a lint site requires.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LintSort {
    VType,
    CType,
}

impl fmt::Display for LintSort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            | Self::VType => "a value type",
            | Self::CType => "a computation type",
        })
    }
}

/// The site a paired annotation was recorded at.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LintSite {
    Root,
    Source(su::TermId),
}

impl fmt::Display for LintSite {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::Root => f.write_str("the root annotation"),
            | Self::Source(term) => {
                write!(f, "the annotation of source term {}", term.concise())
            }
        }
    }
}

/// One violated lint invariant.
#[derive(Clone, Debug, PartialEq)]
pub enum LintError {
    /// A kind cell still awaiting its hole solution after a successful check.
    UnfilledKind { kind: KindId, fill: FillId },
    /// A type cell still awaiting its hole solution after a successful check.
    UnfilledType { ty: TypeId, fill: FillId },
    /// A value hole node that is not a foreign-import placeholder.
    ResidualHoleValue { value: ValueId },
    /// A computation hole node that is not a foreign-import placeholder.
    ResidualHoleComputation { compu: CompuId },
    /// A recorded source annotation that never left its hole form.
    ResidualHoleAnnotation { site: LintSite },
    /// A node reference whose target was never allocated.
    DanglingReference { referenced_by: LintNode, node: LintNode },
    /// An allocated node missing from its annotation table.
    MissingAnnotation { node: LintNode },
    /// An annotation whose sort disagrees with the sort of its site.
    AnnotationSort { node: LintNode, ty: TypeId, kind: KindId, expected: LintSort },
    /// A paired type annotation disagreeing with the co-located kind, compared
    /// up to normalized kind structure.
    KindDisagreement {
        ty: TypeId,
        recorded: KindId,
        recorded_form: Box<Kind>,
        colocated: KindId,
        colocated_form: Box<Kind>,
    },
    /// A recorded term annotation disagreeing with the node-keyed annotation.
    AnnotationDisagreement { site: LintSite, recorded: TermAnnId, node: Option<TermAnnId> },
    /// A definition reference without any recorded annotation.
    UnresolvedDef { referenced_by: LintNode, def: DefId },
    /// A recorded term annotation that bottom-up re-derivation cannot
    /// reproduce, compared up to normalized equality.
    TypeMismatch { node: LintNode, recorded: TypeId, expected: ExpectedType },
    /// A co-located kind that structural re-derivation cannot reproduce.
    KindMismatch { ty: TypeId, recorded: KindId, expected: ExpectedKind },
    /// An abstract-type witness referenced outside its binding scope.
    WitnessEscape { site: LintNode, witness: AbstId },
    /// A definition referenced outside its binding scope.
    DefEscape { site: LintNode, def: DefId },
}

impl fmt::Display for LintError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::UnfilledKind { kind, fill } => {
                write!(f, "kind {} is still the unfilled hole {}", kind.concise(), fill.concise(),)
            }
            | Self::UnfilledType { ty, fill } => {
                write!(f, "type {} is still the unfilled hole {}", ty.concise(), fill.concise(),)
            }
            | Self::ResidualHoleValue { value } => {
                write!(f, "value {} is a hole node", value.concise())
            }
            | Self::ResidualHoleComputation { compu } => {
                write!(f, "computation {} is a hole node", compu.concise())
            }
            | Self::ResidualHoleAnnotation { site } => {
                write!(f, "{site} is still a hole annotation")
            }
            | Self::DanglingReference { referenced_by, node } => {
                write!(f, "{referenced_by} references {node}, which was never allocated")
            }
            | Self::MissingAnnotation { node } => write!(f, "{node} has no recorded annotation"),
            | Self::AnnotationSort { node, ty, kind, expected } => write!(
                f,
                "{node} is annotated {}, whose kind {} is not {expected}",
                ty.concise(),
                kind.concise(),
            ),
            | Self::KindDisagreement { ty, recorded, recorded_form, colocated, colocated_form } => {
                write!(
                    f,
                    "type {} records kind {} ({recorded_form:?}) but carries co-located kind {} ({colocated_form:?})",
                    ty.concise(),
                    recorded.concise(),
                    colocated.concise(),
                )
            }
            | Self::AnnotationDisagreement { site, recorded, node } => match node {
                | Some(node) => {
                    write!(f, "{site} records {recorded:?}, but the node carries {node:?}")
                }
                | None => {
                    write!(f, "{site} records {recorded:?}, whose node was never allocated")
                }
            },
            | Self::UnresolvedDef { referenced_by, def } => write!(
                f,
                "{referenced_by} references definition {}, which has no annotation",
                def.concise(),
            ),
            | Self::TypeMismatch { node, recorded, expected } => write!(
                f,
                "{node} is recorded as {}, but re-derivation expects {expected}",
                recorded.concise(),
            ),
            | Self::KindMismatch { ty, recorded, expected } => write!(
                f,
                "type {} carries kind {}, but re-derivation expects {expected}",
                ty.concise(),
                recorded.concise(),
            ),
            | Self::WitnessEscape { site, witness } => write!(
                f,
                "{site} uses abstract type {} outside its binding scope",
                witness.concise(),
            ),
            | Self::DefEscape { site, def } => {
                write!(f, "{site} uses definition {} outside its binding scope", def.concise(),)
            }
        }
    }
}

/* ---------------------------------- Checker -------------------------------- */

/// Well-formedness validator over one finished arena.
pub struct LintChecker<'a> {
    statics: &'a StaticsArena,
}

impl<'a> LintChecker<'a> {
    pub fn new(statics: &'a StaticsArena) -> Self {
        Self { statics }
    }

    /// Re-establish the arena-wide invariants of a successful check and
    /// re-derive every reachable annotation.
    pub fn validate(&self, root: TermAnnId) -> Vec<LintError> {
        [
            self.fill_closure(),
            self.annotation_presence(),
            self.annotation_sorts(),
            self.paired_annotations(root),
            self.type_references(),
            self.kind_references(),
            self.value_references(),
            self.computation_references(),
            self.pattern_references(),
            self.table_references(),
        ]
        .into_iter()
        .flatten()
        .chain(super::rederive::RederiveChecker::new(self.statics).validate(root))
        .collect()
    }

    /* ---------------------------- node existence --------------------------- */

    fn node_exists(&self, node: &LintNode) -> bool {
        match node {
            | LintNode::Kind(id) => self.statics.kinds_pre.get(id).is_some(),
            | LintNode::Type(id) => self.statics.types_pre.get(id).is_some(),
            | LintNode::Value(id) => self.statics.values.get(id).is_some(),
            | LintNode::Compu(id) => self.statics.compus.get(id).is_some(),
            | LintNode::Term(TermId::Kind(id)) => self.statics.kinds_pre.get(id).is_some(),
            | LintNode::Term(TermId::Type(id)) => self.statics.types_pre.get(id).is_some(),
            | LintNode::Term(TermId::Value(id)) => self.statics.values.get(id).is_some(),
            | LintNode::Term(TermId::Compu(id)) => self.statics.compus.get(id).is_some(),
            | LintNode::KPat(id) => self.statics.kpats.get(id).is_some(),
            | LintNode::TPat(id) => self.statics.tpats.get(id).is_some(),
            | LintNode::VPat(id) => self.statics.vpats.get(id).is_some(),
            | LintNode::Abst(id) => self.statics.absts.get(id).is_some(),
            | LintNode::Fill(id) => self.statics.fills.get(id).is_some(),
            | LintNode::Data(id) => self.statics.datas.get(id).is_some(),
            | LintNode::CoData(id) => self.statics.codatas.get(id).is_some(),
            | LintNode::Def(id) => self.def_exists(id),
        }
    }

    fn def_exists(&self, def: &DefId) -> bool {
        self.statics.annotations_var.get(def).is_some()
            || self.statics.generated_defs.get(def).is_some()
    }

    fn require(&self, referenced_by: LintNode, node: LintNode) -> Option<LintError> {
        (!self.node_exists(&node)).then_some(LintError::DanglingReference { referenced_by, node })
    }

    fn require_def(&self, referenced_by: LintNode, def: DefId) -> Option<LintError> {
        (!self.def_exists(&def)).then_some(LintError::UnresolvedDef { referenced_by, def })
    }

    fn require_kind(&self, referenced_by: LintNode, kind: KindId) -> Option<LintError> {
        self.require(referenced_by, LintNode::Kind(kind))
    }

    fn require_type(&self, referenced_by: LintNode, ty: TypeId) -> Option<LintError> {
        self.require(referenced_by, LintNode::Type(ty))
    }

    /// Whether two kind identifiers denote the same kind.
    ///
    /// `VType` and `CType` leaves are not canonicalized across the arena, and
    /// annotation reconciliation rebuilds arrows and labels from the lub
    /// operands, so kind identity must be compared through the normalized
    /// forms of every leaf rather than by identifier or derived equality.
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

    /* ------------------------------ check groups ---------------------------- */

    /// Every kind and type cell must have left its hole form.
    fn fill_closure(&self) -> Vec<LintError> {
        let kinds = self.statics.kinds_pre.iter().filter_map(|(kind, value)| match value {
            | Fillable::Fill(fill) => Some(LintError::UnfilledKind { kind: *kind, fill: *fill }),
            | Fillable::Done(_) => None,
        });
        let types = self.statics.types_pre.iter().filter_map(|(ty, value)| match value {
            | Fillable::Fill(fill) => Some(LintError::UnfilledType { ty, fill: *fill }),
            | Fillable::Done(_) => None,
        });
        kinds.chain(types).collect()
    }

    /// Every allocated node must appear in its annotation table.
    ///
    /// Abstract-type witnesses are exempt: their kind may live in
    /// `annotations_abst`, on the co-located kind of the `Type::Abst` node that
    /// denotes them, or in the pattern of an enclosing binder, so arena-wide
    /// presence is not an invariant the checker maintains for them.
    fn annotation_presence(&self) -> Vec<LintError> {
        let values = self
            .statics
            .values
            .iter()
            .filter(|(value, _)| self.statics.annotations_value.get(value).is_none())
            .map(|(value, _)| LintError::MissingAnnotation { node: LintNode::Value(*value) });
        let compus = self
            .statics
            .compus
            .iter()
            .filter(|(compu, _)| self.statics.annotations_compu.get(compu).is_none())
            .map(|(compu, _)| LintError::MissingAnnotation { node: LintNode::Compu(*compu) });
        let vpats = self
            .statics
            .vpats
            .iter()
            .filter(|(pat, _)| self.statics.annotations_vpat.get(pat).is_none())
            .map(|(pat, _)| LintError::MissingAnnotation { node: LintNode::VPat(*pat) });
        let tpats = self
            .statics
            .tpats
            .iter()
            .filter(|(pat, _)| self.statics.annotations_tpat.get(pat).is_none())
            .map(|(pat, _)| LintError::MissingAnnotation { node: LintNode::TPat(*pat) });
        let types = self
            .statics
            .types_pre
            .iter()
            .filter(|(ty, _)| self.statics.type_kind_at(*ty).is_none())
            .map(|(ty, _)| LintError::MissingAnnotation { node: LintNode::Type(ty) });
        values.chain(compus).chain(vpats).chain(tpats).chain(types).collect()
    }

    /// Node annotations must carry the sort of their node category.
    fn annotation_sorts(&self) -> Vec<LintError> {
        let sort_of = |ty: &TypeId| -> Option<LintSort> {
            let kind = self.statics.type_kind_at(*ty)?;
            match self.statics.normalized_kind_at(kind) {
                | Some(Kind::VType(_)) => Some(LintSort::VType),
                | Some(Kind::CType(_)) => Some(LintSort::CType),
                | _ => None,
            }
        };
        let annotation =
            |node: LintNode, ty: TypeId, expected: LintSort| match self.statics.type_kind_at(ty) {
                | Some(kind) => LintError::AnnotationSort { node, ty, kind, expected },
                | None => LintError::MissingAnnotation { node: LintNode::Type(ty) },
            };
        let values = self
            .statics
            .annotations_value
            .iter()
            .filter(|(_, ty)| sort_of(ty) != Some(LintSort::VType))
            .map(|(value, ty)| annotation(LintNode::Value(*value), *ty, LintSort::VType));
        let compus = self
            .statics
            .annotations_compu
            .iter()
            .filter(|(_, ty)| sort_of(ty) != Some(LintSort::CType))
            .map(|(compu, ty)| annotation(LintNode::Compu(*compu), *ty, LintSort::CType));
        let vpats = self
            .statics
            .annotations_vpat
            .iter()
            .filter(|(_, ty)| sort_of(ty) != Some(LintSort::VType))
            .map(|(pat, ty)| annotation(LintNode::VPat(*pat), *ty, LintSort::VType));
        values.chain(compus).chain(vpats).collect()
    }

    /// Recorded term annotations must agree with the node-keyed tables.
    fn paired_annotations(&self, root: TermAnnId) -> Vec<LintError> {
        let recorded = std::iter::once((LintSite::Root, root)).chain(
            self.statics
                .term_facts
                .iter()
                .map(|(term, facts)| (LintSite::Source(term), facts.annotation())),
        );
        recorded
            .filter_map(|(site, annotation)| self.check_paired_annotation(site, annotation))
            .collect()
    }

    fn check_paired_annotation(&self, site: LintSite, recorded: TermAnnId) -> Option<LintError> {
        match recorded {
            | TermAnnId::Hole(_) => Some(LintError::ResidualHoleAnnotation { site }),
            | TermAnnId::Kind(kind) => self.require(LintNode::Kind(kind), LintNode::Kind(kind)),
            | TermAnnId::Type(ty, recorded_kind) => match self.statics.type_kind_at(ty) {
                | None => self.require(LintNode::Type(ty), LintNode::Type(ty)),
                | Some(colocated) if colocated == recorded_kind => None,
                | Some(colocated) if !self.kinds_agree(colocated, recorded_kind) => {
                    Some(LintError::KindDisagreement {
                        ty,
                        recorded: recorded_kind,
                        recorded_form: Box::new(
                            self.statics.normalized_kind_at(recorded_kind).cloned()?,
                        ),
                        colocated,
                        colocated_form: Box::new(
                            self.statics.normalized_kind_at(colocated).cloned()?,
                        ),
                    })
                }
                | Some(_) => None,
            },
            | TermAnnId::Value(value, recorded_ty) => {
                self.require(LintNode::Value(value), LintNode::Value(value)).or_else(|| {
                    let node_ty = self.statics.annotations_value.get(&value).copied()?;
                    (node_ty != recorded_ty).then_some(LintError::AnnotationDisagreement {
                        site,
                        recorded,
                        node: Some(TermAnnId::Value(value, node_ty)),
                    })
                })
            }
            | TermAnnId::Compu(compu, recorded_ty) => {
                self.require(LintNode::Compu(compu), LintNode::Compu(compu)).or_else(|| {
                    let node_ty = self.statics.annotations_compu.get(&compu).copied()?;
                    (node_ty != recorded_ty).then_some(LintError::AnnotationDisagreement {
                        site,
                        recorded,
                        node: Some(TermAnnId::Compu(compu, node_ty)),
                    })
                })
            }
        }
    }

    /// Every reference contained in an allocated type must resolve.
    fn type_references(&self) -> Vec<LintError> {
        self.statics
            .types_pre
            .iter()
            .filter_map(|(ty, value)| match value {
                | Fillable::Fill(_) => None,
                | Fillable::Done(node) => Some(self.type_node_references(ty, node)),
            })
            .flatten()
            .collect()
    }

    fn type_node_references(&self, ty: TypeId, node: &Type) -> Vec<LintError> {
        let referenced_by = LintNode::Type(ty);
        match node {
            | Type::Var(def) => self.require_def(referenced_by, *def).into_iter().collect(),
            | Type::Abst(abst) => {
                self.require(referenced_by, LintNode::Abst(*abst)).into_iter().collect()
            }
            | Type::Abs(TypeAbstraction { binder, body }) => self
                .type_binder_references(referenced_by, binder)
                .chain(self.require_type(referenced_by, *body))
                .collect(),
            | Type::App(App(function, argument)) => [
                self.require_type(referenced_by, *function),
                self.require_type(referenced_by, *argument),
            ]
            .into_iter()
            .flatten()
            .collect(),
            | Type::Named(Named(_, inner)) | Type::Label(Label(_, inner)) => {
                self.require_type(referenced_by, *inner).into_iter().collect()
            }
            | Type::Proj(Proj(target, _)) => {
                self.require_type(referenced_by, *target).into_iter().collect()
            }
            | Type::Thk(_)
            | Type::Ret(_)
            | Type::Unit(_)
            | Type::Opaque(_)
            | Type::Primitive(_)
            | Type::OS(_) => Vec::new(),
            | Type::ValPi(inner) => self.valpi_references(referenced_by, inner),
            | Type::Arrow(Arrow(domain, codomain)) => [
                self.require_type(referenced_by, *domain),
                self.require_type(referenced_by, *codomain),
            ]
            .into_iter()
            .flatten()
            .collect(),
            | Type::Forall(Forall(binder, body)) => self
                .type_binder_references(referenced_by, binder)
                .chain(self.require_type(referenced_by, *body))
                .collect(),
            | Type::PackPi(inner) => self.packpi_references(referenced_by, inner),
            | Type::Prod(Prod(components)) => components
                .iter()
                .filter_map(|component| self.require_type(referenced_by, *component))
                .collect(),
            | Type::Exists(inner) => self.exists_references(referenced_by, inner),
            | Type::ManifestKind(ManifestKind { binder, definition, body }) => [
                self.require(referenced_by, LintNode::KPat(*binder)),
                self.require_kind(referenced_by, *definition),
                self.require_type(referenced_by, *body),
            ]
            .into_iter()
            .flatten()
            .collect(),
            | Type::Data(data) => {
                self.require(referenced_by, LintNode::Data(*data)).into_iter().collect()
            }
            | Type::CoData(codata) => {
                self.require(referenced_by, LintNode::CoData(*codata)).into_iter().collect()
            }
        }
    }

    fn type_binder_references(
        &self, referenced_by: LintNode, binder: &TypeBinder,
    ) -> impl Iterator<Item = LintError> + '_ {
        self.require(referenced_by, LintNode::TPat(binder.pattern))
            .into_iter()
            .chain(self.require(referenced_by, LintNode::Abst(binder.witness)))
    }

    fn valpi_references(&self, referenced_by: LintNode, node: &ValPi) -> Vec<LintError> {
        let ValPi { binder, codomain } = node;
        match binder {
            | ValPiBinder::Type(binder) => self
                .type_binder_references(referenced_by, binder)
                .chain(self.require_type(referenced_by, *codomain))
                .collect(),
            | ValPiBinder::Value(ValueParameter { domain, witnesses, .. }) => [
                self.require_type(referenced_by, *domain),
                self.require_type(referenced_by, *codomain),
            ]
            .into_iter()
            .flatten()
            .chain(
                witnesses
                    .iter()
                    .flat_map(|witnesses| witnesses.iter())
                    .filter_map(|witness| self.require(referenced_by, LintNode::Abst(*witness))),
            )
            .collect(),
        }
    }

    fn packpi_references(&self, referenced_by: LintNode, node: &PackPi) -> Vec<LintError> {
        let PackPi { domain, witnesses, codomain } = node;
        [self.require_type(referenced_by, *domain), self.require_type(referenced_by, *codomain)]
            .into_iter()
            .flatten()
            .chain(
                witnesses
                    .iter()
                    .filter_map(|witness| self.require(referenced_by, LintNode::Abst(*witness))),
            )
            .collect()
    }

    fn exists_references(&self, referenced_by: LintNode, node: &Exists) -> Vec<LintError> {
        let Exists { binder, mode, body } = node;
        let manifest = match mode {
            | ExistsMode::Abstract => None,
            | ExistsMode::Manifest(definition) => self.require_type(referenced_by, *definition),
        };
        self.type_binder_references(referenced_by, binder)
            .chain(manifest)
            .chain(self.require_type(referenced_by, *body))
            .collect()
    }

    /// Every reference contained in an allocated kind must resolve.
    fn kind_references(&self) -> Vec<LintError> {
        self.statics
            .kinds_pre
            .iter()
            .flat_map(|(kind, value)| {
                let referenced_by = LintNode::Kind(*kind);
                match value {
                    | Fillable::Fill(_) => Vec::new(),
                    | Fillable::Done(Kind::VType(_)) | Fillable::Done(Kind::CType(_)) => Vec::new(),
                    | Fillable::Done(Kind::Arrow(Arrow(domain, codomain))) => [
                        self.require_kind(referenced_by, *domain),
                        self.require_kind(referenced_by, *codomain),
                    ]
                    .into_iter()
                    .flatten()
                    .collect(),
                    | Fillable::Done(Kind::Label(Label(_, payload))) => {
                        self.require_kind(referenced_by, *payload).into_iter().collect()
                    }
                }
            })
            .collect()
    }

    /// Every reference contained in an allocated value must resolve.
    fn value_references(&self) -> Vec<LintError> {
        self.statics
            .values
            .iter()
            .flat_map(|(value, node)| self.value_node_references(*value, node))
            .collect()
    }

    fn value_node_references(&self, value: ValueId, node: &Value) -> Vec<LintError> {
        let referenced_by = LintNode::Value(value);
        match node {
            | Value::Hole(_) => self
                .statics
                .foreign_imports
                .get(&value)
                .is_none()
                .then_some(LintError::ResidualHoleValue { value })
                .into_iter()
                .collect(),
            | Value::Var(def) => self.require_def(referenced_by, *def).into_iter().collect(),
            | Value::Named(Named(_, inner)) => {
                self.require(referenced_by, LintNode::Value(*inner)).into_iter().collect()
            }
            | Value::Let(Let { binder, bindee, tail }) => [
                self.require(referenced_by, LintNode::VPat(*binder)),
                self.require(referenced_by, LintNode::Value(*bindee)),
                self.require(referenced_by, LintNode::Value(*tail)),
            ]
            .into_iter()
            .flatten()
            .collect(),
            | Value::ValAbs(Abs(binder, body)) => [
                match binder {
                    | ValBinder::Type(pat) => self.require(referenced_by, LintNode::TPat(*pat)),
                    | ValBinder::Value(pat) => self.require(referenced_by, LintNode::VPat(*pat)),
                },
                self.require(referenced_by, LintNode::Value(*body)),
            ]
            .into_iter()
            .flatten()
            .collect(),
            | Value::ValApp(App(function, argument)) => [
                self.require(referenced_by, LintNode::Value(*function)),
                match argument {
                    | ValArgument::Type(argument) => self.require_type(referenced_by, *argument),
                    | ValArgument::Value(argument) => {
                        self.require(referenced_by, LintNode::Value(*argument))
                    }
                },
            ]
            .into_iter()
            .flatten()
            .collect(),
            | Value::Thunk(Thunk(body)) => {
                self.require(referenced_by, LintNode::Compu(*body)).into_iter().collect()
            }
            | Value::Ctor(Ctor(_, payload)) => {
                self.require(referenced_by, LintNode::Value(*payload)).into_iter().collect()
            }
            | Value::Triv(_) | Value::Lit(_) => Vec::new(),
            | Value::VCons(components) => components
                .iter()
                .filter_map(|component| self.require(referenced_by, LintNode::Value(*component)))
                .collect(),
            | Value::SCons(ConsN(statics, tail)) => statics
                .iter()
                .filter_map(|term| match term {
                    | StaticTermId::Kind(kind) => self.require_kind(referenced_by, *kind),
                    | StaticTermId::Type(ty) => self.require_type(referenced_by, *ty),
                })
                .chain(self.require(referenced_by, LintNode::Value(*tail)))
                .collect(),
            | Value::Proj(Proj(head, ResolvedField { target: ProjTarget { products }, .. })) => {
                self.require(referenced_by, LintNode::Value(*head))
                    .into_iter()
                    .chain(products.iter().filter_map(|ProductProjection { product, .. }| {
                        self.require_type(referenced_by, *product)
                    }))
                    .collect()
            }
        }
    }

    /// Every reference contained in an allocated computation must resolve.
    fn computation_references(&self) -> Vec<LintError> {
        self.statics
            .compus
            .iter()
            .flat_map(|(compu, node)| self.computation_node_references(*compu, node))
            .collect()
    }

    fn computation_node_references(&self, compu: CompuId, node: &Computation) -> Vec<LintError> {
        let referenced_by = LintNode::Compu(compu);
        match node {
            | Computation::Hole(_) => vec![LintError::ResidualHoleComputation { compu }],
            | Computation::VAbs(Abs(binder, body)) => [
                self.require(referenced_by, LintNode::VPat(*binder)),
                self.require(referenced_by, LintNode::Compu(*body)),
            ]
            .into_iter()
            .flatten()
            .collect(),
            | Computation::VApp(App(function, argument)) => [
                self.require(referenced_by, LintNode::Compu(*function)),
                self.require(referenced_by, LintNode::Value(*argument)),
            ]
            .into_iter()
            .flatten()
            .collect(),
            | Computation::TAbs(Abs(binder, body)) => [
                self.require(referenced_by, LintNode::TPat(*binder)),
                self.require(referenced_by, LintNode::Compu(*body)),
            ]
            .into_iter()
            .flatten()
            .collect(),
            | Computation::TApp(App(function, argument)) => [
                self.require(referenced_by, LintNode::Compu(*function)),
                self.require_type(referenced_by, *argument),
            ]
            .into_iter()
            .flatten()
            .collect(),
            | Computation::Fix(Fix(binder, body)) => [
                self.require(referenced_by, LintNode::VPat(*binder)),
                self.require(referenced_by, LintNode::Compu(*body)),
            ]
            .into_iter()
            .flatten()
            .collect(),
            | Computation::Force(Force(body)) | Computation::Ret(Return(body)) => {
                self.require(referenced_by, LintNode::Value(*body)).into_iter().collect()
            }
            | Computation::Do(Bind { binder, bindee, tail }) => [
                self.require(referenced_by, LintNode::VPat(*binder)),
                self.require(referenced_by, LintNode::Compu(*bindee)),
                self.require(referenced_by, LintNode::Compu(*tail)),
            ]
            .into_iter()
            .flatten()
            .collect(),
            | Computation::Let(Let { binder, bindee, tail }) => [
                self.require(referenced_by, LintNode::VPat(*binder)),
                self.require(referenced_by, LintNode::Value(*bindee)),
                self.require(referenced_by, LintNode::Compu(*tail)),
            ]
            .into_iter()
            .flatten()
            .collect(),
            | Computation::Match(Match { scrut, arms }) => self
                .require(referenced_by, LintNode::Value(*scrut))
                .into_iter()
                .chain(arms.iter().flat_map(|Matcher { binder, tail }| {
                    [
                        self.require(referenced_by, LintNode::VPat(*binder)),
                        self.require(referenced_by, LintNode::Compu(*tail)),
                    ]
                    .into_iter()
                    .flatten()
                }))
                .collect(),
            | Computation::CoMatch(CoMatch { arms }) => arms
                .iter()
                .filter_map(|CoMatcher { tail, .. }| {
                    self.require(referenced_by, LintNode::Compu(*tail))
                })
                .collect(),
            | Computation::Dtor(Dtor(head, _)) => {
                self.require(referenced_by, LintNode::Compu(*head)).into_iter().collect()
            }
        }
    }

    /// Every reference contained in an allocated pattern must resolve.
    fn pattern_references(&self) -> Vec<LintError> {
        let kpats = self.statics.kpats.iter().filter_map(|(pat, node)| match node {
            | KindPattern::Hole(_) => None,
            | KindPattern::Var(def) => self.require_def(LintNode::KPat(*pat), *def),
        });
        let tpats = self.statics.tpats.iter().filter_map(|(pat, node)| match node {
            | TypePattern::Hole(_) => None,
            | TypePattern::Var(def) => self.require_def(LintNode::TPat(*pat), *def),
            | TypePattern::Named(Named(_, inner)) => {
                self.require(LintNode::TPat(*pat), LintNode::TPat(*inner))
            }
        });
        let vpats = self
            .statics
            .vpats
            .iter()
            .flat_map(|(pat, node)| self.value_pattern_references(*pat, node));
        kpats.chain(tpats).chain(vpats).collect()
    }

    fn value_pattern_references(&self, pat: VPatId, node: &ValuePattern) -> Vec<LintError> {
        let referenced_by = LintNode::VPat(pat);
        match node {
            | ValuePattern::Hole(_) => Vec::new(),
            | ValuePattern::Var(def) => self.require_def(referenced_by, *def).into_iter().collect(),
            | ValuePattern::Named(Named(_, inner)) | ValuePattern::Ctor(Ctor(_, inner)) => {
                self.require(referenced_by, LintNode::VPat(*inner)).into_iter().collect()
            }
            | ValuePattern::Alias(Alias(ConsN(aliases, base))) => aliases
                .iter()
                .chain(std::iter::once(base))
                .filter_map(|alias| self.require(referenced_by, LintNode::VPat(*alias)))
                .collect(),
            | ValuePattern::Triv(_) => Vec::new(),
            | ValuePattern::Lit(_) => Vec::new(),
            | ValuePattern::VCons(components) => components
                .iter()
                .filter_map(|component| self.require(referenced_by, LintNode::VPat(*component)))
                .collect(),
            | ValuePattern::SCons(ConsN(statics, tail)) => statics
                .iter()
                .filter_map(|term| match term {
                    | StaticPatId::Kind(pat) => self.require(referenced_by, LintNode::KPat(*pat)),
                    | StaticPatId::Type(pat) => self.require(referenced_by, LintNode::TPat(*pat)),
                })
                .chain(self.require(referenced_by, LintNode::VPat(*tail)))
                .collect(),
            | ValuePattern::View(inner) => {
                let ViewPattern { function, pattern } = inner.as_ref();
                [
                    self.require(referenced_by, LintNode::Value(*function)),
                    self.require(referenced_by, LintNode::VPat(*pattern)),
                ]
                .into_iter()
                .flatten()
                .collect()
            }
        }
    }

    /// Every node mentioned by an auxiliary table must resolve.
    fn table_references(&self) -> Vec<LintError> {
        let annotations_var =
            self.statics.annotations_var.iter().filter_map(|(def, ann)| match ann {
                | AnnId::Set => None,
                | AnnId::Kind(kind) => self.require(LintNode::Def(*def), LintNode::Kind(*kind)),
                | AnnId::Type(ty) => self.require(LintNode::Def(*def), LintNode::Type(*ty)),
            });
        let annotations_abst =
            self.statics.annotations_abst.iter().filter_map(|(abst, kind)| {
                self.require(LintNode::Abst(*abst), LintNode::Kind(*kind))
            });
        let annotations_tpat =
            self.statics.annotations_tpat.iter().filter_map(|(pat, kind)| {
                self.require(LintNode::TPat(*pat), LintNode::Kind(*kind))
            });
        let annotations_vpat = self
            .statics
            .annotations_vpat
            .iter()
            .filter_map(|(pat, ty)| self.require(LintNode::VPat(*pat), LintNode::Type(*ty)));
        let annotations_value =
            self.statics.annotations_value.iter().filter_map(|(value, ty)| {
                self.require(LintNode::Value(*value), LintNode::Type(*ty))
            });
        let annotations_compu =
            self.statics.annotations_compu.iter().filter_map(|(compu, ty)| {
                self.require(LintNode::Compu(*compu), LintNode::Type(*ty))
            });
        let solus = self.statics.solus.iter().filter_map(|(fill, ann)| {
            let target = match ann {
                | AnnId::Set => None,
                | AnnId::Kind(kind) => Some(LintNode::Kind(*kind)),
                | AnnId::Type(ty) => Some(LintNode::Type(*ty)),
            };
            self.require(LintNode::Fill(*fill), LintNode::Fill(*fill)).or_else(|| {
                target.filter(|target| !self.node_exists(target)).map(|target| {
                    LintError::DanglingReference {
                        referenced_by: LintNode::Fill(*fill),
                        node: target,
                    }
                })
            })
        });
        let fill_scopes = self
            .statics
            .fill_scopes
            .iter()
            .filter_map(|(fill, _)| self.require(LintNode::Fill(*fill), LintNode::Fill(*fill)));
        let data_hints = self.statics.data_hints.iter().filter_map(|(value, data)| {
            self.require(LintNode::Value(*value), LintNode::Value(*value))
                .or_else(|| self.require(LintNode::Value(*value), LintNode::Data(*data)))
        });
        let data_pat_hints = self.statics.data_pat_hints.iter().filter_map(|(pat, data)| {
            self.require(LintNode::VPat(*pat), LintNode::VPat(*pat))
                .or_else(|| self.require(LintNode::VPat(*pat), LintNode::Data(*data)))
        });
        let codata_hints = self.statics.codata_hints.iter().filter_map(|(compu, codata)| {
            self.require(LintNode::Compu(*compu), LintNode::Compu(*compu))
                .or_else(|| self.require(LintNode::Compu(*compu), LintNode::CoData(*codata)))
        });
        let copattern_matches = self.statics.copattern_matches.iter().filter_map(|(compu, ())| {
            self.require(LintNode::Compu(*compu), LintNode::Compu(*compu))
        });
        let copattern_pack_pi_binders =
            self.statics.copattern_pack_pi_binders.iter().filter_map(|(compu, pat)| {
                self.require(LintNode::Compu(*compu), LintNode::Compu(*compu))
                    .or_else(|| self.require(LintNode::Compu(*compu), LintNode::VPat(*pat)))
            });
        let package_pattern_opened_arity = self
            .statics
            .package_pattern_opened_arity
            .iter()
            .filter_map(|(pat, _)| self.require(LintNode::VPat(*pat), LintNode::VPat(*pat)));
        let value_aliases = self.statics.value_aliases.iter().filter_map(|(def, value)| {
            self.require_def(LintNode::Def(*def), *def)
                .or_else(|| self.require(LintNode::Def(*def), LintNode::Value(*value)))
        });
        let package_aliases = self.statics.package_aliases.iter().flat_map(|(def, terms)| {
            self.require_def(LintNode::Def(*def), *def).into_iter().chain(terms.iter().filter_map(
                |term| match term {
                    | StaticTermId::Kind(kind) => self.require_kind(LintNode::Def(*def), *kind),
                    | StaticTermId::Type(ty) => self.require_type(LintNode::Def(*def), *ty),
                },
            ))
        });
        let type_definitions = self.statics.type_definitions.iter().filter_map(|(def, ty)| {
            self.require_def(LintNode::Def(*def), *def)
                .or_else(|| self.require(LintNode::Def(*def), LintNode::Type(*ty)))
        });
        let inlinables = self.statics.inlinables.iter().filter_map(|(def, value)| {
            self.require_def(LintNode::Def(*def), *def)
                .or_else(|| self.require(LintNode::Def(*def), LintNode::Value(*value)))
        });
        let global_defs = self
            .statics
            .global_defs
            .iter()
            .filter_map(|(def, ())| self.require_def(LintNode::Def(*def), *def));
        let global_terms =
            self.statics.global_terms.iter().filter_map(|(term, ())| {
                self.require(LintNode::Term(*term), LintNode::Term(*term))
            });
        let def_hints = self
            .statics
            .def_hints
            .iter()
            .filter_map(|(term, def)| self.require_def(LintNode::Term(*term), *def));
        let foreign_imports = self.statics.foreign_imports.iter().filter_map(|(value, _)| {
            self.require(LintNode::Value(*value), LintNode::Value(*value))
        });
        let seals = self.statics.seals.iter().filter_map(|(abst, ty)| {
            self.require(LintNode::Abst(*abst), LintNode::Abst(*abst))
                .or_else(|| self.require(LintNode::Abst(*abst), LintNode::Type(*ty)))
        });
        let existential_skolems =
            self.statics.existential_skolems.iter().filter_map(|(abst, ())| {
                self.require(LintNode::Abst(*abst), LintNode::Abst(*abst))
            });
        let abst_hints = self.statics.abst_hints.iter().filter_map(|(abst, def)| {
            self.require(LintNode::Abst(*abst), LintNode::Abst(*abst))
                .or_else(|| self.require_def(LintNode::Abst(*abst), *def))
        });
        let datas = self.statics.datas.iter().flat_map(|(data, node)| {
            node.iter()
                .filter_map(move |(_, ty)| self.require(LintNode::Data(*data), LintNode::Type(*ty)))
        });
        let codatas = self.statics.codatas.iter().flat_map(|(codata, node)| {
            node.iter().filter_map(move |(_, ty)| {
                self.require(LintNode::CoData(*codata), LintNode::Type(*ty))
            })
        });
        annotations_var
            .chain(annotations_abst)
            .chain(annotations_tpat)
            .chain(annotations_vpat)
            .chain(annotations_value)
            .chain(annotations_compu)
            .chain(solus)
            .chain(fill_scopes)
            .chain(data_hints)
            .chain(data_pat_hints)
            .chain(codata_hints)
            .chain(copattern_matches)
            .chain(copattern_pack_pi_binders)
            .chain(package_pattern_opened_arity)
            .chain(value_aliases)
            .chain(package_aliases)
            .chain(type_definitions)
            .chain(inlinables)
            .chain(global_defs)
            .chain(global_terms)
            .chain(def_hints)
            .chain(foreign_imports)
            .chain(seals)
            .chain(existential_skolems)
            .chain(abst_hints)
            .chain(datas)
            .chain(codatas)
            .collect()
    }
}
