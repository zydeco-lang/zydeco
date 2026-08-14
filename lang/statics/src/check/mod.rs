use derive_more::{AsMut, AsRef, Deref};
use {
    super::{
        arena::StaticsArena,
        environment::{MonadicTypeBasis, TyEnv, TyEnvT},
        syntax::{AbstId, AnnId, FillId, Fillable, InferenceSite, PatAnnId, TermAnnId},
        *,
    },
    crate::surface_syntax::{PrimDefs, ScopedArena, SpanArena},
    crate::validate::CoverageChecker,
    zydeco_surface::metadata::BuiltinMeta,
    zydeco_utils::prelude::ArenaAccess,
};

/// Type-checker error definitions and reporting.
pub mod error;
pub use error::*;
/// Checker-dependent construction and projection of typed annotations.
mod annotation;
/// Least-upper-bound operations for kinds and types.
pub mod lub;
pub use lub::*;
/// Syntactic checks for annotations, seals, and usage.
pub mod syntactic;
pub use syntactic::*;
/// Debug dump helpers used by diagnostics.
mod dump;
/// Type-directed elaboration of generalized comatch clauses.
mod copattern;
use copattern::CopatternElaborator;

/// Type-checking driver that consumes scoped syntax and produces typed arenas.
#[derive(AsRef, AsMut)]
pub struct Tycker<'a> {
    /// Issuer of site-derived identifiers for this type-checking run.
    #[as_mut(DerivedAllocator)]
    allocator: DerivedAllocator,
    /// The salsa database this check runs within.
    pub db: &'a dyn crate::query::TyckDb,
    /// The name-resolved program snapshot being checked.
    pub data: crate::query::ScopedData<'a>,
    pub spans: &'a SpanArena,
    pub prim: &'a PrimDefs,
    #[as_ref(ScopedArena)]
    #[as_mut(ScopedArena)]
    pub scoped: &'a mut ScopedArena,
    #[as_ref(StaticsArena)]
    #[as_mut(StaticsArena)]
    pub statics: StaticsArena,
    /// call stack for debugging tycker and error tracking
    pub tasks: im::Vector<TyckTask>,
    /// how many times each scoped entity has been checked; supplies the
    /// derivation occurrence so re-checked entities get distinct sites
    check_counts: ArenaAssoc<su::EntityId, u32>,
    /// meta stack
    pub metas: im::Vector<su::Meta>,
    /// a writer monad for error handling
    pub errors: Vec<TyckErrorEntry>,
    pub(crate) observations: Vec<TyckObservation>,
}

pub type TyckReports = std::sync::Arc<
    Vec<ariadne::Report<'static, (zydeco_utils::span::PathDisplay, std::ops::Range<usize>)>>,
>;

/// A source-directed observation produced during type checking.
#[derive(Clone, Debug)]
pub enum TyckObservation {
    HoleSolution { site: InferenceSite, solution: Option<AnnId> },
    Debug { metadata: zydeco_syntax::Meta, result: TermAnnId },
}

/// The typed result of checking one complete source term.
#[derive(Clone, Debug)]
pub struct CheckedSource {
    pub statics: StaticsArena,
    pub root: TermAnnId,
    pub observations: Vec<TyckObservation>,
}

/// A failed source check together with the static facts established before
/// the failure.
#[derive(Clone, Debug)]
pub struct RejectedSource {
    pub statics: StaticsArena,
    pub reports: TyckReports,
    pub observations: Vec<TyckObservation>,
}

/// The recoverable result of checking one complete source term.
#[derive(Clone, Debug)]
pub enum SourceCheckOutcome {
    Checked(CheckedSource),
    Rejected(RejectedSource),
}

impl SourceCheckOutcome {
    /// Recover the conventional all-or-nothing source-checking result.
    pub fn into_result(self) -> std::result::Result<CheckedSource, TyckReports> {
        match self {
            | Self::Checked(checked) => Ok(checked),
            | Self::Rejected(RejectedSource { reports, .. }) => Err(reports),
        }
    }

    /// Retain every static fact established before either outcome.
    pub fn into_statics(self) -> StaticsArena {
        match self {
            | Self::Checked(CheckedSource { statics, .. })
            | Self::Rejected(RejectedSource { statics, .. }) => statics,
        }
    }
}

/// Non-contextual output of the pattern-checking judgment.
///
/// The surrounding [`TyEnvT`] carries the environment after the pattern. This
/// payload records only the checked pattern and the existential identities it
/// opened, in source order.
#[derive(Clone, Debug, Deref)]
pub struct PatternCheck {
    #[deref]
    annotation: PatAnnId,
    opened: Vec<AbstId>,
}

/// A checked pattern paired with the environment it makes available to its
/// body.
pub type CheckedPattern = TyEnvT<PatternCheck>;

/// Flexible pattern metavariables owned by one local inference boundary.
struct InferenceRegion {
    inherited: std::collections::HashSet<FillId>,
}

impl InferenceRegion {
    fn enter(tycker: &Tycker<'_>) -> Self {
        let inherited = tycker.statics.fills.iter().map(|(fill, _)| *fill).collect();
        Self { inherited }
    }

    fn close_k(self, tycker: &mut Tycker<'_>) -> ResultKont<()> {
        let candidates = tycker
            .statics
            .fills
            .iter()
            .filter_map(|(fill, site)| {
                (site.is_pattern() && !self.inherited.contains(fill)).then_some(*fill)
            })
            .collect::<Vec<_>>();
        let mut unconstrained = Vec::new();
        for fill in candidates {
            let Some(solution) = tycker.statics.solus.get(&fill).copied() else {
                unconstrained.push(fill);
                continue;
            };
            let AnnId::Type(solution) = solution else {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            };
            let (_, pending) = solution.solution_k(tycker)?;
            if pending.into_iter().any(|fill| !self.inherited.contains(&fill)) {
                unconstrained.push(fill);
            }
        }
        let mut sites = std::collections::HashSet::new();
        unconstrained.retain(|fill| sites.insert(tycker.statics.fills[fill]));
        if unconstrained.is_empty() {
            Ok(())
        } else {
            tycker.err_k(
                TyckError::UnconstrainedInference(unconstrained),
                std::panic::Location::caller(),
            )
        }
    }
}

impl PatternCheck {
    fn new(annotation: PatAnnId) -> Self {
        Self { annotation, opened: Vec::new() }
    }

    fn with_opened(annotation: PatAnnId, opened: Vec<AbstId>) -> Self {
        Self { annotation, opened }
    }
}

#[derive(Clone)]
struct ValueFieldCandidate {
    route: Vec<ValueFieldStep>,
    projected: ss::TypeId,
}

#[derive(Clone)]
enum ValueFieldStep {
    Named { name: FieldName, whole: ss::TypeId },
    Product { product: ss::TypeId, components: Vec<ss::TypeId>, position: usize },
}

#[derive(Clone)]
struct TypeFieldStep {
    name: FieldName,
    whole: ss::KindId,
    projected: ss::KindId,
}

#[derive(Clone)]
struct TypeFieldCandidate {
    path: Vec<TypeFieldStep>,
    projected: ss::KindId,
}

/// Structural lookup shared by named projections.
///
/// Labels are transparent search nodes. Products contribute runtime path
/// steps, and every other type constructor is an opacity boundary.
struct FieldProjectionResolver;

impl FieldProjectionResolver {
    fn value_k(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, root: ss::TypeId, field: &FieldName,
    ) -> ResultKont<ValueFieldCandidate> {
        let candidates = Self::value_candidates_k(
            tycker,
            env,
            root,
            field,
            &[],
            &mut std::collections::HashSet::new(),
        )?;
        match candidates.as_slice() {
            | [] => tycker.err_k(
                TyckError::MissingNamedField { field: field.clone(), found: root },
                std::panic::Location::caller(),
            ),
            | [candidate] => Ok(candidate.clone()),
            | _ => tycker.err_k(
                TyckError::DuplicateNamedField { field: field.clone(), found: root },
                std::panic::Location::caller(),
            ),
        }
    }

    fn value_candidates_k(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, current: ss::TypeId, field: &FieldName,
        route: &[ValueFieldStep], active: &mut std::collections::HashSet<ss::TypeId>,
    ) -> ResultKont<Vec<ValueFieldCandidate>> {
        let view = current.unroll_k(tycker)?.subst_env_k(tycker, env)?;
        if !active.insert(view) {
            return Ok(Vec::new());
        }
        let structure = tycker.type_filled_k(&view)?.to_owned();
        let candidates = match structure {
            | ss::Type::Label(ss::Label(found, projected)) => {
                let route = route
                    .iter()
                    .cloned()
                    .chain([ValueFieldStep::Named { name: found.clone(), whole: view }])
                    .collect::<Vec<_>>();
                let direct = (found == *field)
                    .then(|| ValueFieldCandidate { route: route.clone(), projected });
                let nested =
                    Self::value_candidates_k(tycker, env, projected, field, &route, active)?;
                direct.into_iter().chain(nested).collect()
            }
            | ss::Type::Prod(_) => {
                let components = Self::product_components_k(tycker, env, view)?;
                let branches = components
                    .iter()
                    .copied()
                    .enumerate()
                    .map(|(position, component)| {
                        let route = route
                            .iter()
                            .cloned()
                            .chain([ValueFieldStep::Product {
                                product: view,
                                components: components.clone(),
                                position,
                            }])
                            .collect::<Vec<_>>();
                        Self::value_candidates_k(tycker, env, component, field, &route, active)
                    })
                    .collect::<ResultKont<Vec<_>>>()?;
                branches.into_iter().flatten().collect()
            }
            | _ => Vec::new(),
        };
        active.remove(&view);
        Ok(candidates)
    }

    fn product_components_k(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, product: ss::TypeId,
    ) -> ResultKont<Vec<ss::TypeId>> {
        let mut next = Some(product);
        std::iter::from_fn(|| {
            let current = next.take()?;
            let view = match current.unroll_k(tycker).and_then(|ty| ty.subst_env_k(tycker, env)) {
                | Ok(view) => view,
                | Err(KontFailure) => return Some(Err(KontFailure)),
            };
            match tycker.type_filled_k(&view) {
                | Ok(ss::Type::Prod(ss::Prod(item, tail))) => {
                    next = Some(tail);
                    Some(Ok(item))
                }
                | Ok(_) => Some(Ok(view)),
                | Err(KontFailure) => Some(Err(KontFailure)),
            }
        })
        .collect()
    }

    fn r#type(
        tycker: &mut Tycker<'_>, root: ss::KindId, field: &FieldName,
    ) -> ResultKont<TypeFieldCandidate> {
        let candidates = Self::type_candidates_k(
            tycker,
            root,
            field,
            &[],
            &mut std::collections::HashSet::new(),
        )?;
        match candidates.as_slice() {
            | [] => tycker.err_k(
                TyckError::MissingNamedTypeField { field: field.clone(), found: root },
                std::panic::Location::caller(),
            ),
            | [candidate] => Ok(candidate.clone()),
            | _ => tycker.err_k(
                TyckError::AmbiguousNamedTypeField { field: field.clone(), found: root },
                std::panic::Location::caller(),
            ),
        }
    }

    fn type_candidates_k(
        tycker: &mut Tycker<'_>, current: ss::KindId, field: &FieldName, path: &[TypeFieldStep],
        active: &mut std::collections::HashSet<ss::KindId>,
    ) -> ResultKont<Vec<TypeFieldCandidate>> {
        if !active.insert(current) {
            return Ok(Vec::new());
        }
        let candidates = match tycker.kind_filled_k(&current)?.to_owned() {
            | ss::Kind::Label(ss::Label(found, projected)) => {
                let path = path
                    .iter()
                    .cloned()
                    .chain([TypeFieldStep { name: found.clone(), whole: current, projected }])
                    .collect::<Vec<_>>();
                let direct =
                    (found == *field).then(|| TypeFieldCandidate { path: path.clone(), projected });
                let nested = Self::type_candidates_k(tycker, projected, field, &path, active)?;
                direct.into_iter().chain(nested).collect()
            }
            | _ => Vec::new(),
        };
        active.remove(&current);
        Ok(candidates)
    }

    fn project_type_k(
        tycker: &mut Tycker<'_>, head: ss::TypeId, candidate: TypeFieldCandidate,
        projected: ss::KindId,
    ) -> ResultKont<ss::TypeId> {
        let final_step = candidate.path.len() - 1;
        candidate.path.into_iter().enumerate().try_fold(
            head,
            |head, (index, TypeFieldStep { name, projected: step_kind, .. })| {
                let step_kind = if index == final_step { projected } else { step_kind };
                let result = head.project_named(tycker, &name, step_kind);
                tycker.err_p_to_k(result)
            },
        )
    }

    fn value_target(candidate: &ValueFieldCandidate) -> ss::ProjTarget {
        let products = candidate
            .route
            .iter()
            .filter_map(|step| match step {
                | ValueFieldStep::Named { .. } => None,
                | ValueFieldStep::Product { product, position, .. } => {
                    Some(ss::ProductProjection { product: *product, position: *position })
                }
            })
            .collect();
        ss::ProjTarget { products }
    }

    fn value_pattern(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, root: ss::TypeId, candidate: ValueFieldCandidate,
        payload: ss::VPatId,
    ) -> ss::VPatId {
        candidate.route.into_iter().enumerate().rev().fold(payload, |payload, (index, step)| {
            match step {
                | ValueFieldStep::Named { name, whole } => {
                    let annotation = if index == 0 { root } else { whole };
                    Alloc::alloc(tycker, ss::Named(name, payload), annotation, env)
                }
                | ValueFieldStep::Product { product, components, position } => {
                    let patterns = components
                        .into_iter()
                        .enumerate()
                        .map(|(component_index, component)| {
                            if component_index == position {
                                payload
                            } else {
                                Alloc::alloc(tycker, ss::Hole, component, env)
                            }
                        })
                        .collect::<Vec<_>>();
                    let patterns = ss::ConsN::from_vec(patterns)
                        .expect("a product projection route has at least two components");
                    let annotation = if index == 0 { root } else { product };
                    Alloc::alloc(tycker, patterns, annotation, env)
                }
            }
        })
    }

    fn type_pattern(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, root: ss::KindId, candidate: TypeFieldCandidate,
        payload: ss::TPatId,
    ) -> ss::TPatId {
        candidate.path.into_iter().enumerate().rev().fold(
            payload,
            |payload, (index, TypeFieldStep { name, whole, .. })| {
                let annotation = if index == 0 { root } else { whole };
                Alloc::alloc(tycker, ss::Named(name, payload), annotation, env)
            },
        )
    }
}

#[derive(Clone)]
enum ExistentialProjectionMember {
    Project { source: su::PatId, field: FieldName, payload: su::PatId },
    Whole(su::PatId),
}

enum ExistentialProjectionSlot {
    Kind {
        field: Option<FieldName>,
        definition: ss::KindId,
        pattern: ss::KPatId,
    },
    Type {
        field: Option<FieldName>,
        source_pattern: ss::TPatId,
        payload_kind: ss::KindId,
        payload: ss::TypeId,
        /// The payload wrapped in the binder's named-pattern structure.
        /// Whole-package aliases need this form when instantiating a package arrow.
        full_payload: ss::TypeId,
        pattern: ss::TPatId,
        skolem: Option<ss::AbstId>,
    },
}

impl ExistentialProjectionSlot {
    fn field(&self) -> Option<&FieldName> {
        match self {
            | Self::Kind { field, .. } | Self::Type { field, .. } => field.as_ref(),
        }
    }

    fn pattern(&self) -> ss::StaticPatId {
        match self {
            | Self::Kind { pattern, .. } => (*pattern).into(),
            | Self::Type { pattern, .. } => (*pattern).into(),
        }
    }

    fn term(&self) -> ss::StaticTermId {
        match self {
            | Self::Kind { definition, .. } => (*definition).into(),
            | Self::Type { full_payload, .. } => (*full_payload).into(),
        }
    }

    fn set_pattern(&mut self, pattern: ss::StaticPatId) {
        match (self, pattern) {
            | (Self::Kind { pattern, .. }, ss::StaticPatId::Kind(selected)) => *pattern = selected,
            | (Self::Type { pattern, .. }, ss::StaticPatId::Type(selected)) => *pattern = selected,
            | _ => unreachable!("a selected package field retains its static sort"),
        }
    }
}

struct ExistentialProjectionOpening {
    expected: ss::TypeId,
    body: ss::TypeId,
    slots: Vec<ExistentialProjectionSlot>,
    env: ss::TyEnv,
    opened: Vec<ss::AbstId>,
}

/// Select fields from one package without spelling its static telescope.
///
/// Manifest kind and type entries are substituted transparently, and every
/// abstract witness is opened once under an anonymous or canonical skolem.
/// Selected static fields bind source names to those entries, while selected
/// value fields become ordinary projection patterns over the instantiated
/// package body. Whole-value alias members retain the opened witnesses so the
/// package can be forwarded without reconstructing its telescope.
struct ExistentialProjectionPattern;

impl ExistentialProjectionPattern {
    fn members(
        tycker: &Tycker<'_>, patterns: impl IntoIterator<Item = su::PatId>,
    ) -> Option<Vec<ExistentialProjectionMember>> {
        let members = patterns
            .into_iter()
            .map(|source| match tycker.scoped.pats[&source].to_owned() {
                | su::Pattern::Project(su::ProjectionPattern(field, payload)) => {
                    ExistentialProjectionMember::Project { source, field, payload }
                }
                | _ => ExistentialProjectionMember::Whole(source),
            })
            .collect::<Vec<_>>();
        members
            .iter()
            .any(|member| matches!(member, ExistentialProjectionMember::Project { .. }))
            .then_some(members)
    }

    fn applies_k(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, expected: ss::TypeId,
    ) -> ResultKont<bool> {
        let view = expected.unroll_k(tycker)?.subst_env_k(tycker, env)?;
        Ok(matches!(tycker.type_filled_k(&view)?, ss::Type::ManifestKind(_) | ss::Type::Exists(_)))
    }

    fn kind_field_name(tycker: &Tycker<'_>, pattern: ss::KPatId) -> Option<FieldName> {
        match tycker.statics.kpats[&pattern] {
            | ss::KindPattern::Var(definition) => {
                Some(tycker.scoped.defs[&definition].plain().into())
            }
            | ss::KindPattern::Hole(_) => None,
        }
    }

    fn type_field_name(tycker: &Tycker<'_>, binder: &ss::TypeBinder) -> Option<FieldName> {
        match tycker.statics.tpats[&binder.pattern].to_owned() {
            | ss::TypePattern::Named(ss::Named(field, _)) => Some(field),
            | ss::TypePattern::Var(definition) => {
                Some(tycker.scoped.defs[&definition].plain().into())
            }
            | ss::TypePattern::Hole(_) => tycker
                .statics
                .abst_hints
                .get(&binder.witness)
                .map(|definition| tycker.scoped.defs[definition].plain().into()),
        }
    }

    fn open_k(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, expected: ss::TypeId, skolems: &PatternSkolems,
    ) -> ResultKont<ExistentialProjectionOpening> {
        let mut body = expected;
        let mut body_env = env.clone();
        let mut slots = Vec::new();
        let mut opened = Vec::new();

        loop {
            let view = body.unroll_k(tycker)?.subst_env_k(tycker, &body_env)?;
            match tycker.type_filled_k(&view)?.to_owned() {
                | ss::Type::ManifestKind(ss::ManifestKind { binder, definition, body: next }) => {
                    let field = Self::kind_field_name(tycker, binder);
                    body_env =
                        TyEnvT::new(body_env, Assign(binder, definition)).tyck_k(tycker, ())?.info;
                    body = next;
                    slots.push(ExistentialProjectionSlot::Kind {
                        field,
                        definition,
                        pattern: binder,
                    });
                }
                | ss::Type::Exists(ss::Exists { binder, mode, body: next }) => {
                    let payload_kind = binder.payload_kind(tycker);
                    let field = Self::type_field_name(tycker, &binder);
                    let (payload, skolem) = match mode {
                        | ss::ExistsMode::Abstract => {
                            let skolem = match skolems.get_witness(&binder.witness) {
                                | Some(skolem) => {
                                    let canonical_kind = tycker.statics.annotations_abst[&skolem];
                                    Lub::lub_k(canonical_kind, payload_kind, tycker)?;
                                    skolem
                                }
                                | None => Alloc::alloc(tycker, None, payload_kind, &()),
                            };
                            tycker.transfer_builtin_role_k(binder.witness, skolem)?;
                            tycker.statics.existential_skolems.ensure(skolem);
                            body_env = body_env.with_skolem(skolem);
                            let payload = Alloc::alloc(tycker, skolem, payload_kind, &body_env);
                            opened.push(skolem);
                            (payload, Some(skolem))
                        }
                        | ss::ExistsMode::Manifest(definition) => {
                            let definition_kind = tycker.statics.annotations_type[&definition];
                            Lub::lub_k(payload_kind, definition_kind, tycker)?;
                            (definition, None)
                        }
                    };
                    let full_payload = binder.pattern.introduce_payload(tycker, payload);
                    let full_payload = tycker.err_p_to_k(full_payload)?;
                    body_env = TyEnvT::new(body_env, Assign(binder.pattern, full_payload))
                        .tyck_k(tycker, ())?
                        .info;
                    body = next.subst_abst_k(tycker, (binder.witness, payload))?;
                    slots.push(ExistentialProjectionSlot::Type {
                        field,
                        source_pattern: binder.pattern,
                        payload_kind,
                        payload,
                        full_payload,
                        pattern: binder.pattern,
                        skolem,
                    });
                }
                | _ => break,
            }
        }

        Ok(ExistentialProjectionOpening { expected, body, slots, env: body_env, opened })
    }

    fn wrap_type_pattern(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, source: ss::TPatId, payload: ss::TPatId,
    ) -> ss::TPatId {
        match tycker.statics.tpats[&source].to_owned() {
            | ss::TypePattern::Named(ss::Named(field, inner)) => {
                let inner = Self::wrap_type_pattern(tycker, env, inner, payload);
                let annotation = tycker.statics.annotations_tpat[&source];
                Alloc::alloc(tycker, ss::Named(field, inner), annotation, env)
            }
            | ss::TypePattern::Hole(_) | ss::TypePattern::Var(_) => payload,
        }
    }

    fn check_k(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, expected: ss::TypeId,
        members: Vec<ExistentialProjectionMember>, skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        let mut opening = Self::open_k(tycker, env, expected, &skolems)?;
        let mut body_patterns = Vec::new();
        let mut whole_patterns = Vec::new();
        let mut package_aliases = Vec::new();

        for member in members {
            let ExistentialProjectionMember::Project { source, field, payload } = member else {
                let ExistentialProjectionMember::Whole(source) = member else { unreachable!() };
                let checked = TyEnvT::new(opening.env.clone(), source).tyck_k(
                    tycker,
                    PatternAction::ana(expected.into()).with_skolems(skolems.clone()),
                )?;
                let (pattern, _) = checked.try_as_value(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                if !ValuePatternShape::is_irrefutable(tycker, pattern) {
                    tycker
                        .err_k(TyckError::RefutablePatternAlias, std::panic::Location::caller())?
                }
                opening.env = checked.info;
                opening.opened.extend(checked.inner.opened);
                if let Some(definition) = Self::whole_definition(tycker, pattern) {
                    package_aliases.push(definition);
                }
                whole_patterns.push(pattern);
                continue;
            };
            let static_candidates = opening
                .slots
                .iter()
                .enumerate()
                .filter_map(|(index, slot)| (slot.field() == Some(&field)).then_some(index))
                .collect::<Vec<_>>();
            let value_candidates = FieldProjectionResolver::value_candidates_k(
                tycker,
                &opening.env,
                opening.body,
                &field,
                &[],
                &mut std::collections::HashSet::new(),
            )?;

            match (static_candidates.as_slice(), value_candidates.as_slice()) {
                | ([], []) => tycker.err_k(
                    TyckError::MissingNamedField { field, found: expected },
                    std::panic::Location::caller(),
                )?,
                | ([slot_index], []) => {
                    let selected = match &opening.slots[*slot_index] {
                        | ExistentialProjectionSlot::Kind { definition, .. } => {
                            let checked = TyEnvT::new(opening.env.clone(), payload).tyck_k(
                                tycker,
                                PatternAction::ana(AnnId::Set).with_skolems(skolems.clone()),
                            )?;
                            let payload_pattern = checked.annotation.try_as_kind(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            opening.env =
                                TyEnvT::new(checked.info, Assign(payload_pattern, *definition))
                                    .tyck_k(tycker, ())?
                                    .info;
                            ss::StaticPatId::Kind(payload_pattern)
                        }
                        | ExistentialProjectionSlot::Type {
                            source_pattern,
                            payload_kind,
                            payload: slot_payload,
                            skolem,
                            ..
                        } => {
                            let checked = TyEnvT::new(opening.env.clone(), payload).tyck_k(
                                tycker,
                                PatternAction::ana((*payload_kind).into())
                                    .with_skolems(skolems.clone()),
                            )?;
                            let (payload_pattern, _) = checked.try_as_type(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            opening.env =
                                TyEnvT::new(checked.info, Assign(payload_pattern, *slot_payload))
                                    .tyck_k(tycker, ())?
                                    .info;
                            if let (Some(skolem), (Some(definition), _)) =
                                (*skolem, payload_pattern.try_destruct_def(tycker))
                                && tycker.statics.abst_hints.get(&skolem).is_none()
                            {
                                tycker.statics.abst_hints.insert_new(skolem, definition);
                            }
                            let full_pattern = Self::wrap_type_pattern(
                                tycker,
                                &opening.env,
                                *source_pattern,
                                payload_pattern,
                            );
                            ss::StaticPatId::Type(full_pattern)
                        }
                    };
                    opening.slots[*slot_index].set_pattern(selected);
                    let selected = match selected {
                        | ss::StaticPatId::Kind(pattern) => ss::PatId::Kind(pattern),
                        | ss::StaticPatId::Type(pattern) => ss::PatId::Type(pattern),
                    };
                    tycker.statics.pats.ensure(source, selected);
                }
                | ([], [candidate]) => {
                    let checked = TyEnvT::new(opening.env.clone(), payload).tyck_k(
                        tycker,
                        PatternAction::ana(candidate.projected.into())
                            .with_skolems(skolems.clone()),
                    )?;
                    let (payload_pattern, _) = checked.try_as_value(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    if !ValuePatternShape::is_irrefutable(tycker, payload_pattern) {
                        tycker.err_k(
                            TyckError::RefutableFieldProjectionPattern,
                            std::panic::Location::caller(),
                        )?
                    }
                    opening.env = checked.info;
                    opening.opened.extend(checked.inner.opened);
                    let pattern = FieldProjectionResolver::value_pattern(
                        tycker,
                        &opening.env,
                        opening.body,
                        candidate.clone(),
                        payload_pattern,
                    );
                    tycker.statics.pats.ensure(source, pattern.into());
                    body_patterns.push(pattern);
                }
                | _ => tycker.err_k(
                    TyckError::DuplicateNamedField { field, found: expected },
                    std::panic::Location::caller(),
                )?,
            }
        }

        let body_pattern = match body_patterns.len() {
            | 0 => Alloc::alloc(tycker, ss::Hole, opening.body, &opening.env),
            | 1 => body_patterns[0],
            | _ => {
                let patterns = ss::ConsN::from_vec(body_patterns).unwrap();
                Alloc::alloc(tycker, ss::Alias(patterns), opening.body, &opening.env)
            }
        };
        let package_terms: Vec<ss::StaticTermId> =
            opening.slots.iter().map(ExistentialProjectionSlot::term).collect();
        let static_patterns: Vec<ss::StaticPatId> =
            opening.slots.iter().map(ExistentialProjectionSlot::pattern).collect();
        let package_pattern =
            Alloc::alloc(tycker, ss::ConsN(static_patterns, body_pattern), opening.expected, env);
        for definition in package_aliases {
            let _ = tycker.statics.package_aliases.upsert(definition, package_terms.clone());
        }
        let pattern = if whole_patterns.is_empty() {
            package_pattern
        } else {
            let patterns = ss::ConsN::from_vec(
                std::iter::once(package_pattern).chain(whole_patterns).collect(),
            )
            .unwrap();
            Alloc::alloc(tycker, ss::Alias(patterns), opening.expected, env)
        };
        Ok(TyEnvT::new(
            opening.env,
            PatternCheck::with_opened(PatAnnId::Value(pattern, opening.expected), opening.opened),
        ))
    }

    fn whole_definition(tycker: &Tycker<'_>, pattern: ss::VPatId) -> Option<ss::DefId> {
        match tycker.statics.vpats[&pattern].to_owned() {
            | ss::ValuePattern::Var(definition) => Some(definition),
            | ss::ValuePattern::Named(ss::Named(_, inner)) => Self::whole_definition(tycker, inner),
            | ss::ValuePattern::Hole(_)
            | ss::ValuePattern::Ctor(_)
            | ss::ValuePattern::Alias(_)
            | ss::ValuePattern::Triv(_)
            | ss::ValuePattern::VCons(_)
            | ss::ValuePattern::SCons(_) => None,
        }
    }
}

/// The initial backend representation can repeat guaranteed destructuring,
/// but it does not yet encode conjunction between competing constructors.
struct ValuePatternShape;

impl ValuePatternShape {
    fn is_irrefutable(tycker: &Tycker<'_>, pattern: ss::VPatId) -> bool {
        match &tycker.statics.vpats[&pattern] {
            | ss::ValuePattern::Hole(_) | ss::ValuePattern::Var(_) | ss::ValuePattern::Triv(_) => {
                true
            }
            | ss::ValuePattern::Named(ss::Named(_, inner)) => Self::is_irrefutable(tycker, *inner),
            | ss::ValuePattern::Ctor(_) => false,
            | ss::ValuePattern::Alias(ss::Alias(patterns)) => {
                patterns.iter().all(|pattern| Self::is_irrefutable(tycker, *pattern))
            }
            | ss::ValuePattern::VCons(patterns) => {
                patterns.iter().all(|pattern| Self::is_irrefutable(tycker, *pattern))
            }
            | ss::ValuePattern::SCons(ss::ConsN(_, body)) => Self::is_irrefutable(tycker, *body),
        }
    }
}

impl Tycker<'_> {
    fn pattern_has_payload_annotation(&self, pattern: su::PatId) -> bool {
        match self.scoped.pats[&pattern].clone() {
            | su::Pattern::Ann(_) => true,
            | su::Pattern::Named(su::Named(_, inner)) => self.pattern_has_payload_annotation(inner),
            | su::Pattern::Project(su::ProjectionPattern(_, inner)) => {
                self.pattern_has_payload_annotation(inner)
            }
            | su::Pattern::Alias(su::Alias(patterns)) => {
                patterns.iter().any(|pattern| self.pattern_has_payload_annotation(*pattern))
            }
            | su::Pattern::Hole(_)
            | su::Pattern::Var(_)
            | su::Pattern::Ctor(_)
            | su::Pattern::Triv(_)
            | su::Pattern::Cons(_) => false,
        }
    }
}

struct MonadicBasisElaboration<'a> {
    syntax: &'a su::MonadicBasis,
    env: &'a TyEnv,
}

impl<'a> MonadicBasisElaboration<'a> {
    fn new(syntax: &'a su::MonadicBasis, env: &'a TyEnv) -> Self {
        Self { syntax, env }
    }

    fn check_k(&self, tycker: &mut Tycker<'_>) -> ResultKont<MonadicTypeBasis> {
        let monad = self.definition_k(tycker, self.syntax.monad)?;
        let algebra = self.definition_k(tycker, self.syntax.algebra)?;
        let vtype = ss::VType.build(tycker, self.env);
        let ctype = ss::CType.build(tycker, self.env);
        let monad_constructor = ss::Arrow(vtype, ctype).build(tycker, self.env);
        let monad_kind = ss::Arrow(monad_constructor, ctype).build(tycker, self.env);
        let carrier_constructor = ss::Arrow(ctype, ctype).build(tycker, self.env);
        let algebra_kind =
            ss::Arrow(monad_constructor, carrier_constructor).build(tycker, self.env);
        self.expect_kind_k(tycker, monad, monad_kind)?;
        self.expect_kind_k(tycker, algebra, algebra_kind)?;
        Ok(MonadicTypeBasis { monad, algebra })
    }

    fn definition_k(
        &self, tycker: &mut Tycker<'_>, definition: su::TermId,
    ) -> ResultKont<ss::TypeId> {
        let checked =
            TyEnvT { info: self.env.clone(), inner: definition }.tyck_k(tycker, Action::syn())?;
        let (definition, _) =
            checked.try_as_type(tycker, TyckError::SortMismatch, std::panic::Location::caller())?;
        Ok(definition)
    }

    fn expect_kind_k(
        &self, tycker: &mut Tycker<'_>, ty: ss::TypeId, expected: ss::KindId,
    ) -> ResultKont<()> {
        let actual = tycker.statics.annotations_type[&ty];
        Lub::lub_k(actual, expected, tycker)?;
        Ok(())
    }
}

trait CheckedPatternExt {
    fn with_annotation(self, annotation: PatAnnId) -> Self;
    fn close_scope_k(&self, tycker: &mut Tycker<'_>, result: ss::TypeId) -> ResultKont<()>;
    fn package_telescope_k(&self, tycker: &mut Tycker<'_>)
    -> ResultKont<Option<ss::PackTelescope>>;
}

impl CheckedPatternExt for CheckedPattern {
    fn with_annotation(self, annotation: PatAnnId) -> Self {
        let TyEnvT { info, inner } = self;
        TyEnvT::new(info, PatternCheck::with_opened(annotation, inner.opened))
    }

    #[track_caller]
    fn close_scope_k(&self, tycker: &mut Tycker<'_>, result: ss::TypeId) -> ResultKont<()> {
        if self.inner.opened.is_empty() {
            return Ok(());
        }
        let outer = self.info.skolem_scope().without(&self.inner.opened);
        result.constrain_to_scope_k(tycker, &outer)
    }

    #[track_caller]
    fn package_telescope_k(
        &self, tycker: &mut Tycker<'_>,
    ) -> ResultKont<Option<ss::PackTelescope>> {
        let Some((first, rest)) = self.inner.opened.split_first() else {
            return Ok(None);
        };
        let (pattern, _) = self.inner.annotation.try_as_value(
            tycker,
            TyckError::SortMismatch,
            std::panic::Location::caller(),
        )?;
        let boundary_arity = pattern.package_witness_arity(tycker).unwrap_or_default();
        if boundary_arity < self.inner.opened.len() {
            tycker.err_k(
                TyckError::PackageWitnessArityMismatch {
                    expected: self.inner.opened.len(),
                    found: boundary_arity,
                },
                std::panic::Location::caller(),
            )?
        }
        Ok(Some(ss::PackTelescope::new(*first, rest.iter().copied())))
    }
}

// Todo: use async to cut all tycker functions into small segments (returning futures)
// and achieve better concurrency

// Todo: use hole solution to implement the confluence checker (well-formedness checker)

impl<'a> Tycker<'a> {
    /// Create a type checker with fresh statics arenas.
    pub fn new(
        db: &'a dyn crate::query::TyckDb, data: crate::query::ScopedData<'a>, spans: &'a SpanArena,
        prim: &'a PrimDefs, scoped: &'a mut ScopedArena,
    ) -> Self {
        Self {
            allocator: DerivedAllocator::new(),
            db,
            data,
            spans,
            prim,
            scoped,
            statics: StaticsArena::default(),
            tasks: im::Vector::new(),
            check_counts: ArenaAssoc::default(),
            metas: im::Vector::new(),
            errors: Vec::new(),
            observations: Vec::new(),
        }
    }

    #[track_caller]
    pub(crate) fn transfer_builtin_role(
        &mut self, source: ss::AbstId, target: ss::AbstId,
    ) -> Result<()> {
        let Some(found) = self.statics.builtin_roles.witness(source) else {
            return Ok(());
        };
        match self.statics.builtin_roles.transfer_witness(source, target) {
            | Ok(()) => Ok(()),
            | Err(existing) => self.err(
                TyckError::ConflictingBuiltinRole { existing, found },
                std::panic::Location::caller(),
            ),
        }
    }

    #[track_caller]
    fn transfer_builtin_role_k(
        &mut self, source: ss::AbstId, target: ss::AbstId,
    ) -> ResultKont<()> {
        let result = self.transfer_builtin_role(source, target);
        self.err_p_to_k(result)
    }

    #[track_caller]
    fn validate_builtin_signature_k(&mut self, signature: &ss::PackPi) -> ResultKont<()> {
        match BuiltinSignatureValidator::new(&self.statics).validate(signature) {
            | Ok(()) => Ok(()),
            | Err(error) => self
                .err_k(TyckError::InvalidBuiltinSignature(error), std::panic::Location::caller()),
        }
    }

    /// Type-check one complete source term.
    /// Check the complete source term without the finish phase.
    pub fn run_judgments_k(&mut self, root: su::TermId) -> ResultKont<TermAnnId> {
        let env = TyEnvT::new(Default::default(), ());
        let inference = InferenceRegion::enter(self);
        let root = env.mk(root).tyck_k(self, Action::syn())?;
        if matches!(root, TermAnnId::Hole(_)) {
            self.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
        }
        inference.close_k(self)?;
        Ok(root)
    }

    pub fn run_source_k(&mut self, root: su::TermId) -> ResultKont<TermAnnId> {
        let root = self.run_judgments_k(root)?;
        self.finish_check_k()?;
        Ok(root)
    }

    /// The occurrence of the innermost allocation site, carried by the
    /// checker's allocator. Producer queries key their identifiers on it so
    /// re-checked entities derive distinct ids.
    pub fn site_occurrence(&self) -> u32 {
        self.allocator.current_site().2
    }

    /// The root site's next slot, carried between the check's phases.
    pub fn root_slot(&self) -> u32 {
        self.allocator.root_slot()
    }

    /// Resume a check after its judgment phase, restoring the accumulated
    /// errors and observations for the finish phase.
    #[allow(clippy::too_many_arguments)]
    pub fn resume(
        db: &'a dyn crate::query::TyckDb, data: crate::query::ScopedData<'a>, spans: &'a SpanArena,
        prim: &'a PrimDefs, scoped: &'a mut ScopedArena, statics: StaticsArena,
        errors: Vec<TyckErrorEntry>, observations: Vec<TyckObservation>, root_slot: u32,
    ) -> Self {
        Self {
            allocator: DerivedAllocator::resume_from_root_slot(root_slot),
            db,
            data,
            spans,
            prim,
            scoped,
            statics,
            tasks: im::Vector::new(),
            metas: im::Vector::new(),
            check_counts: ArenaAssoc::default(),
            errors,
            observations,
        }
    }

    /// Consume the checker and retain the typed identity of a complete source term.
    pub fn check_source(self, root: su::TermId) -> std::result::Result<CheckedSource, TyckReports> {
        self.check_source_outcome(root).into_result()
    }

    /// Check a source while retaining static facts from a rejected term.
    pub fn check_source_outcome(mut self, root: su::TermId) -> SourceCheckOutcome {
        // The intrinsic singletons are query-owned: materialize them before any
        // judgment so every `Construct::build` cache read hits.
        InternalTerm::fill_intrinsics(&mut self);
        match self.run_source_k(root) {
            | Ok(root) => SourceCheckOutcome::Checked(CheckedSource {
                statics: self.statics,
                root,
                observations: self.observations,
            }),
            | Err(KontFailure) => {
                let reports = self.error_reports();
                SourceCheckOutcome::Rejected(RejectedSource {
                    statics: self.statics,
                    reports,
                    observations: self.observations,
                })
            }
        }
    }

    pub(crate) fn finish_check_k(&mut self) -> ResultKont<()> {
        self.resolve_holes_and_collect();
        self.normalize_and_validate_k()
    }

    /// Resolve holes and collect their solutions, the first half of the
    /// finish phase.
    pub(crate) fn resolve_holes_and_collect(&mut self) {
        // before we go, resolve all holes with solutions (including nested ones)
        self.do_resolve_holes();
        self.collect_hole_solutions();
    }

    /// Normalize and validate the checked arena, the second half of the finish
    /// phase.
    pub(crate) fn normalize_and_validate_k(&mut self) -> ResultKont<()> {
        let mut normalizer = crate::normalize::FilledNormalizer::default();
        // normalize all kinds
        {
            let kind_ids: Vec<_> =
                self.statics.kinds_pre.iter().map(|(id, _)| id.to_owned()).collect();
            for id in kind_ids {
                normalizer.normalize_kind_k(id, self)?;
            }
        }
        // normalize all types
        {
            let type_ids: Vec<_> =
                self.statics.types_pre.iter().map(|(id, _)| id.to_owned()).collect();
            for id in type_ids {
                normalizer.normalize_type_k(id, self)?;
            }
        }
        if self.errors.is_empty() {
            let blame = std::panic::Location::caller();
            self.errors.extend(CoverageChecker::new(&self.statics).validate().into_iter().map(
                |error| TyckErrorEntry {
                    error: TyckError::Coverage(error),
                    blame,
                    stack: im::Vector::new(),
                },
            ));
        }
        if !self.errors.is_empty() {
            Err(KontFailure)?
        }
        Ok(())
    }

    pub(crate) fn error_reports(&self) -> TyckReports {
        use std::collections::HashSet;

        let mut seen_blame = HashSet::new();
        let mut seen_coverage = HashSet::new();
        std::sync::Arc::new(
            self.errors
                .iter()
                .filter(|entry| {
                    if matches!(entry.error, TyckError::Coverage(_)) {
                        let span = self
                            .error_primary_span(&entry.error)
                            .map(|(path, range)| (path, range.start, range.end));
                        seen_coverage.insert((span, self.error_message(&entry.error)))
                    } else {
                        seen_blame.insert((
                            entry.blame.file(),
                            entry.blame.line(),
                            entry.blame.column(),
                        ))
                    }
                })
                .cloned()
                .map(|entry| self.error_entry_report(entry))
                .collect(),
        )
    }

    /// Resolve all holes with solutions (including nested ones).
    #[inline]
    pub fn do_resolve_holes(&mut self) {
        let type_ids: Vec<_> = self.statics.types_pre.iter().map(|(id, _)| id.to_owned()).collect();
        let mut resolver = crate::normalize::HoleResolver::default();
        for id in type_ids {
            let solu = match resolver.resolve_k(id, self) {
                | Ok(res) => res,
                | Err(KontFailure) => continue,
            };
            if solu != id {
                let ty = self.statics.types_pre[&solu].to_owned();
                self.statics.types_pre.replace_existing(id, ty);
            }
        }
        let missing = resolver.into_missing();
        if !missing.is_empty() {
            // keep running tycker even after unsuccessful solving hole
            let _: ResultKont<()> =
                self.err_k(TyckError::MissingSolution(missing), std::panic::Location::caller());
        }
    }
    fn collect_hole_solutions(&mut self) {
        self.observations.extend(self.statics.fill_hints.iter().map(|(id, ())| {
            TyckObservation::HoleSolution {
                site: self.statics.fills[id],
                solution: self.statics.solus.get(id).copied(),
            }
        }));
    }
}

mod impl_tycker {
    use super::*;

    impl<'a> Tycker<'a> {
        /// Generalize the administrative guards using "with" pattern by placing
        /// the body of tyck function into a closure, and the with function can
        /// do all administrative work before and after calling the function.
        #[inline]
        pub(crate) fn guarded<R>(&mut self, with: impl FnOnce(&mut Self) -> R) -> R {
            let stack = self.tasks.clone();
            let res = with(self);
            self.tasks = stack;
            res
        }

        /// Start a diagnostic stack at the root of an imported source.
        ///
        /// Source assembly deliberately preserves a boundary around every
        /// imported term. Type errors inside that boundary should explain the
        /// imported source, without inheriting administrative frames from the
        /// importing term.
        #[inline]
        pub(crate) fn source_guarded<R>(
            &mut self, root: TyckTask, with: impl FnOnce(&mut Self) -> R,
        ) -> R {
            let stack = std::mem::take(&mut self.tasks);
            self.tasks.push_back(root);
            let res = with(self);
            self.tasks = stack;
            res
        }

        /// Push an error entry into the error list.
        #[inline]
        fn push_err_entry_k<T>(&mut self, entry: TyckErrorEntry) -> ResultKont<T> {
            self.errors.push(entry);
            Err(KontFailure)
        }
    }

    impl<'a> Errorable<TyckError> for Tycker<'a> {
        type Entry = Box<TyckErrorEntry>;

        /// Throw a pure error.
        #[inline]
        fn err<T>(
            &self, error: TyckError, blame: &'static std::panic::Location<'static>,
        ) -> Result<T> {
            let stack = self.tasks.clone();
            Err(Box::new(TyckErrorEntry { error, blame, stack }))
        }
        /// Throw a continuation error.
        #[inline]
        fn err_k<T>(
            &mut self, error: TyckError, blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<T> {
            let stack = self.tasks.clone();
            self.push_err_entry_k(TyckErrorEntry { error, blame, stack })
        }
        /// Convert a pure result into a continuation result.
        #[inline]
        fn err_p_to_k<T>(&mut self, res: Result<T>) -> ResultKont<T> {
            match res {
                | Ok(t) => Ok(t),
                | Err(entry) => self.push_err_entry_k(*entry),
            }
        }
    }
}

pub trait Tyck<'a> {
    type Out;
    type Action;
    /// Entry point for type checking with optional administrative wrapping.
    fn tyck_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out> {
        self.tyck_inner_k(tycker, action)
    }
    /// Core implementation for type checking.
    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out>;
}

/// Synthesis or analysis mode, optionally with an expected annotation.
#[derive(Clone, Copy, Debug)]
pub enum Switch<Ann> {
    Syn,
    Ana(Ann),
}

/// Task-stack entries used to enrich error reports.
#[derive(Clone, Debug)]
pub enum TyckTask {
    Pat(su::PatId, Switch<AnnId>),
    Term(su::TermId, Switch<AnnId>),
    Lub(AnnId, AnnId),
    SignatureGen(ss::AnnId),
    StructureGen(ss::AnnId),
    MonadicLiftPat(ss::PatId),
    MonadicLiftTerm(ss::TermId),
}

/// Wrapper for passing synthesis/analysis mode into `Tyck`.
pub struct Action<Ann> {
    pub switch: Switch<Ann>,
}

impl<Ann> Action<Ann> {
    pub fn syn() -> Self {
        Self { switch: Switch::Syn }
    }
    pub fn ana(ann: Ann) -> Self {
        Self { switch: Switch::Ana(ann) }
    }
    pub fn switch(switch: Switch<Ann>) -> Self {
        Self { switch }
    }
}

/// Validate and record the static attachment point of one typed Builtin role.
struct BuiltinAttachment {
    role: ss::BuiltinRole,
    term: TermAnnId,
}

/// Resolve one compiler-generated host type against the abstract Builtin
/// identities visible at its lexical use site.
struct BuiltinTypeResolution(ss::BuiltinTypeRole);

impl BuiltinTypeResolution {
    #[track_caller]
    fn resolve_k(self, tycker: &mut Tycker<'_>, env: &ss::TyEnv) -> ResultKont<ss::TypeId> {
        let mut witnesses = tycker
            .statics
            .builtin_roles
            .type_witnesses(self.0)
            .filter(|witness| env.skolem_scope().contains(witness))
            .collect::<Vec<_>>();
        witnesses.sort_unstable();

        match witnesses.as_slice() {
            | [] => tycker.err_k(
                TyckError::MissingBuiltinTypeRole { role: self.0 },
                std::panic::Location::caller(),
            ),
            | [witness] => {
                let kind = tycker.statics.annotations_abst[witness];
                Ok(Alloc::alloc(tycker, *witness, kind, env))
            }
            | _ => tycker.err_k(
                TyckError::AmbiguousBuiltinTypeRole { role: self.0, witnesses },
                std::panic::Location::caller(),
            ),
        }
    }
}

/// Give compiler-generated primitive syntax its intrinsic or lexical static
/// meaning without routing it through a source-level definition name.
pub(crate) struct InternalTerm(su::Internal, su::TermId);

impl InternalTerm {
    /// Materialize the query-owned intrinsic singletons into `IntrinsicStatics`.
    pub(crate) fn fill_intrinsics(tycker: &mut Tycker<'_>) {
        use zydeco_syntax::PrimitiveType;
        let mut keys = vec![
            crate::query::IntrinsicKey::VType,
            crate::query::IntrinsicKey::CType,
            crate::query::IntrinsicKey::Thk,
            crate::query::IntrinsicKey::Ret,
            crate::query::IntrinsicKey::Unit,
        ];
        keys.extend(
            PrimitiveType::ALL
                .iter()
                .map(|primitive| crate::query::IntrinsicKey::Primitive(*primitive)),
        );
        for key in keys {
            let interned = crate::query::InternedIntrinsic::new(tycker.db, key);
            let singleton = crate::query::intrinsic_singleton(tycker.db, tycker.data, interned);
            match singleton {
                | crate::query::IntrinsicSingleton::Kind { id, kind } => {
                    tycker.statics.kinds_pre.insert_new(id, ss::Fillable::Done(kind));
                    match key {
                        | crate::query::IntrinsicKey::VType => {
                            tycker.statics.intrinsics.vtype = Some(id)
                        }
                        | crate::query::IntrinsicKey::CType => {
                            tycker.statics.intrinsics.ctype = Some(id)
                        }
                        | _ => {}
                    }
                }
                | crate::query::IntrinsicSingleton::Type { kinds, ty: (ty, ty_node), ann } => {
                    for (id, kind) in kinds {
                        tycker.statics.kinds_pre.insert_new(id, ss::Fillable::Done(kind));
                    }
                    tycker.statics.types_pre.insert_new(ty, ss::Fillable::Done(ty_node));
                    tycker.statics.annotations_type.insert_new(ty, ann);
                    tycker.statics.env_type.insert_new(ty, TyEnv::default());
                    match key {
                        | crate::query::IntrinsicKey::Thk => {
                            tycker.statics.intrinsics.thk = Some(ty)
                        }
                        | crate::query::IntrinsicKey::Ret => {
                            tycker.statics.intrinsics.ret = Some(ty)
                        }
                        | crate::query::IntrinsicKey::Unit => {
                            tycker.statics.intrinsics.unit = Some(ty)
                        }
                        | crate::query::IntrinsicKey::Primitive(primitive) => {
                            tycker.statics.intrinsics.primitives.insert(primitive, ty);
                        }
                        | _ => {}
                    }
                }
            }
        }
    }

    #[track_caller]
    fn tyck_k(
        self, tycker: &mut Tycker<'_>, env: &ss::TyEnv, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        let synthesized = match self.0 {
            | su::Internal::VType => TermAnnId::Kind(ss::VType.build(tycker, env)),
            | su::Internal::CType => TermAnnId::Kind(ss::CType.build(tycker, env)),
            | su::Internal::Thk => {
                let ty = ss::ThkTy.build(tycker, env);
                TermAnnId::Type(ty, tycker.statics.annotations_type[&ty])
            }
            | su::Internal::Ret => {
                let ty = ss::RetTy.build(tycker, env);
                TermAnnId::Type(ty, tycker.statics.annotations_type[&ty])
            }
            | su::Internal::Unit => {
                let ty = ss::UnitTy.build(tycker, env);
                TermAnnId::Type(ty, tycker.statics.annotations_type[&ty])
            }
            | su::Internal::Primitive(primitive) => {
                let ty = ss::PrimitiveTy(primitive).build(tycker, env);
                TermAnnId::Type(ty, tycker.statics.annotations_type[&ty])
            }
            | su::Internal::OS => self.builtin_type_k(tycker, env, ss::BuiltinTypeRole::OS)?,
            | su::Internal::Monad | su::Internal::Algebra => {
                let term = crate::query::InternedTerm::new(tycker.db, self.1);
                let env_data = crate::query::EnvData::new(tycker.db, env.clone());
                if let Some(error) =
                    crate::query::internal_judgment(tycker.db, tycker.data, term, env_data)
                {
                    tycker.err_k(error, std::panic::Location::caller())?
                } else {
                    unreachable!("intrinsic rejections are query-produced")
                }
            }
        };
        self.reconcile_k(tycker, synthesized, switch)
    }

    #[track_caller]
    fn builtin_type_k(
        &self, tycker: &mut Tycker<'_>, env: &ss::TyEnv, role: ss::BuiltinTypeRole,
    ) -> ResultKont<TermAnnId> {
        let ty = BuiltinTypeResolution(role).resolve_k(tycker, env)?;
        Ok(TermAnnId::Type(ty, tycker.statics.annotations_type[&ty]))
    }

    #[track_caller]
    fn reconcile_k(
        &self, tycker: &mut Tycker<'_>, synthesized: TermAnnId, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        let annotation = match synthesized {
            | TermAnnId::Kind(_) => AnnId::Set,
            | TermAnnId::Type(_, kind) => AnnId::Kind(kind),
            | TermAnnId::Hole(_) | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                unreachable!("internal terms synthesize only kinds and types")
            }
        };
        let annotation = match switch {
            | Switch::Syn => annotation,
            | Switch::Ana(expected) => Lub::lub_k(annotation, expected, tycker)?,
        };

        match (synthesized, annotation) {
            | (TermAnnId::Kind(kind), AnnId::Set) => Ok(TermAnnId::Kind(kind)),
            | (TermAnnId::Type(ty, _), AnnId::Kind(kind)) => Ok(TermAnnId::Type(ty, kind)),
            | _ => unreachable!("annotation reconciliation preserves the internal term sort"),
        }
    }
}

impl BuiltinAttachment {
    fn new(role: ss::BuiltinRole, term: TermAnnId) -> Self {
        Self { role, term }
    }

    #[track_caller]
    fn register_k(self, tycker: &mut Tycker<'_>, env: &ss::TyEnv) -> ResultKont<()> {
        match self.role {
            | ss::BuiltinRole::Type(role) => {
                let expected = match role.universe() {
                    | ss::BuiltinTypeUniverse::Value => "an abstract existential value-type entry",
                    | ss::BuiltinTypeUniverse::Computation => {
                        "an abstract existential computation-type entry"
                    }
                };
                let (witness, kind) = self.existential_witness_k(tycker, expected)?;
                let expected_kind = match role.universe() {
                    | ss::BuiltinTypeUniverse::Value => ss::VType.build(tycker, env),
                    | ss::BuiltinTypeUniverse::Computation => ss::CType.build(tycker, env),
                };
                Lub::lub_k(kind, expected_kind, tycker)?;
                tycker.statics.builtin_roles.attach_type(witness, role).map_err(|existing| {
                    tycker.errors.push(TyckErrorEntry {
                        error: TyckError::ConflictingBuiltinRole { existing, found: self.role },
                        blame: std::panic::Location::caller(),
                        stack: tycker.tasks.clone(),
                    });
                    KontFailure
                })
            }
            | ss::BuiltinRole::Value(role) => {
                let TermAnnId::Type(entry, _) = self.term else {
                    return tycker.err_k(
                        TyckError::InvalidBuiltinAttachment {
                            role: self.role,
                            expected: "a named value classifier",
                        },
                        std::panic::Location::caller(),
                    );
                };
                let ss::Type::Label(_) = tycker.type_filled_k(&entry)?.to_owned() else {
                    return tycker.err_k(
                        TyckError::InvalidBuiltinAttachment {
                            role: self.role,
                            expected: "a named value classifier",
                        },
                        std::panic::Location::caller(),
                    );
                };
                tycker.statics.builtin_roles.attach_value(entry, role).map_err(|existing| {
                    tycker.errors.push(TyckErrorEntry {
                        error: TyckError::ConflictingBuiltinRole {
                            existing: ss::BuiltinRole::Value(existing),
                            found: self.role,
                        },
                        blame: std::panic::Location::caller(),
                        stack: tycker.tasks.clone(),
                    });
                    KontFailure
                })
            }
        }
    }

    #[track_caller]
    fn existential_witness_k(
        &self, tycker: &mut Tycker<'_>, expected: &'static str,
    ) -> ResultKont<(ss::AbstId, ss::KindId)> {
        let TermAnnId::Type(entry, _) = self.term else {
            return tycker.err_k(
                TyckError::InvalidBuiltinAttachment { role: self.role, expected },
                std::panic::Location::caller(),
            );
        };
        let ss::Type::Exists(exists) = tycker.type_filled_k(&entry)?.to_owned() else {
            return tycker.err_k(
                TyckError::InvalidBuiltinAttachment { role: self.role, expected },
                std::panic::Location::caller(),
            );
        };
        if !matches!(exists.mode, ss::ExistsMode::Abstract) {
            return tycker.err_k(
                TyckError::InvalidBuiltinAttachment { role: self.role, expected },
                std::panic::Location::caller(),
            );
        }
        Ok((exists.binder.witness, exists.binder.payload_kind(tycker)))
    }
}

/// Canonical existential identities assigned to package-pattern components.
#[derive(Clone, Debug, Default)]
struct PatternSkolems {
    patterns: im::HashMap<su::PatId, ss::AbstId>,
    witnesses: im::HashMap<ss::AbstId, ss::AbstId>,
}

impl PatternSkolems {
    fn new(
        patterns: impl IntoIterator<Item = (su::PatId, ss::AbstId)>,
        witnesses: impl IntoIterator<Item = (ss::AbstId, ss::AbstId)>,
    ) -> Self {
        Self {
            patterns: patterns.into_iter().collect(),
            witnesses: witnesses.into_iter().collect(),
        }
    }

    fn get(&self, pattern: &su::PatId) -> Option<ss::AbstId> {
        self.patterns.get(pattern).copied()
    }

    fn get_witness(&self, witness: &ss::AbstId) -> Option<ss::AbstId> {
        self.witnesses.get(witness).copied()
    }
}

/// Pattern-checking mode together with any canonical package witnesses.
#[derive(Clone, Debug)]
pub struct PatternAction {
    switch: Switch<AnnId>,
    skolems: PatternSkolems,
}

impl PatternAction {
    pub fn syn() -> Self {
        Self { switch: Switch::Syn, skolems: PatternSkolems::default() }
    }

    pub fn ana(ann: AnnId) -> Self {
        Self { switch: Switch::Ana(ann), skolems: PatternSkolems::default() }
    }

    pub fn switch(switch: Switch<AnnId>) -> Self {
        Self { switch, skolems: PatternSkolems::default() }
    }

    fn with_skolems(mut self, skolems: PatternSkolems) -> Self {
        self.skolems = skolems;
        self
    }
}

pub struct Assign<Br, Be>(pub Br, pub Be);
pub struct FixPoint<T>(pub T);

/// Formation input for a pure or computational function type with an
/// elaborated value-pattern domain.
struct ValuePiFormation {
    binder: CheckedPattern,
    codomain: su::TermId,
}

/// Instantiate the bound witnesses of a package-dependent arrow.
struct PackPiInstantiation {
    signature: PackageSignature,
    witnesses: Vec<ss::StaticTermId>,
}

/// The common static information carried by pure and computational
/// package-dependent arrows.
#[derive(Clone)]
struct PackageSignature {
    domain: ss::TypeId,
    witnesses: ss::PackTelescope,
    codomain: ss::TypeId,
}

impl From<ss::PackPi> for PackageSignature {
    fn from(signature: ss::PackPi) -> Self {
        let ss::PackPi { domain, witnesses, codomain } = signature;
        Self { domain, witnesses, codomain }
    }
}

impl From<ss::ValuePackPi> for PackageSignature {
    fn from(signature: ss::ValuePackPi) -> Self {
        let ss::ValuePackPi { domain, witnesses, codomain } = signature;
        Self { domain, witnesses, codomain }
    }
}

/// State for recursively instantiating a `PackPi` through the physical
/// existential prefix of its package argument.
struct PackPiInstantiationState<'a> {
    domain: ss::TypeId,
    codomain: ss::TypeId,
    canonical: &'a [ss::AbstId],
    actual: &'a [ss::StaticTermId],
    expected: usize,
    found: usize,
}

impl<'a> PackPiInstantiationState<'a> {
    fn new(
        signature: &PackageSignature, canonical: &'a [ss::AbstId], actual: &'a [ss::StaticTermId],
    ) -> Self {
        Self {
            domain: signature.domain,
            codomain: signature.codomain,
            canonical,
            actual,
            expected: canonical.len(),
            found: actual.len(),
        }
    }

    fn instantiate_k(self, tycker: &mut Tycker<'_>, env: &ss::TyEnv) -> ResultKont<ss::TypeId> {
        if self.canonical.is_empty() {
            return Ok(self.codomain);
        }
        let view = self.domain.unroll_k(tycker)?.subst_env_k(tycker, env)?;
        match tycker.type_filled_k(&view)?.to_owned() {
            | ss::Type::ManifestKind(ss::ManifestKind { definition, body, .. }) => {
                let Some((&ss::StaticTermId::Kind(actual_kind), actual)) =
                    self.actual.split_first()
                else {
                    return self.mismatch_k(tycker);
                };
                Lub::lub_k(definition, actual_kind, tycker)?;
                Self {
                    domain: body,
                    codomain: self.codomain,
                    canonical: self.canonical,
                    actual,
                    expected: self.expected,
                    found: self.found,
                }
                .instantiate_k(tycker, env)
            }
            | ss::Type::Exists(ss::Exists { binder, mode, body }) => {
                let Some((&ss::StaticTermId::Type(witness), actual)) = self.actual.split_first()
                else {
                    return self.mismatch_k(tycker);
                };
                let payload = binder.pattern.bind_argument_k(tycker, witness)?;
                match mode {
                    | ss::ExistsMode::Abstract => {
                        let Some((&canonical, remaining)) = self.canonical.split_first() else {
                            unreachable!()
                        };
                        let canonical_kind = tycker.statics.annotations_abst[&canonical];
                        let payload_kind = tycker.statics.annotations_type[&payload];
                        Lub::lub_k(canonical_kind, payload_kind, tycker)?;
                        let codomain = self.codomain.subst_abst_k(tycker, (canonical, payload))?;
                        let domain = body.subst_abst_k(tycker, (binder.witness, payload))?;
                        Self {
                            domain,
                            codomain,
                            canonical: remaining,
                            actual,
                            expected: self.expected,
                            found: self.found,
                        }
                        .instantiate_k(tycker, env)
                    }
                    | ss::ExistsMode::Manifest(definition) => {
                        let payload = Lub::lub_k(definition, payload, tycker)?;
                        let domain = body.subst_abst_k(tycker, (binder.witness, payload))?;
                        Self {
                            domain,
                            codomain: self.codomain,
                            canonical: self.canonical,
                            actual,
                            expected: self.expected,
                            found: self.found,
                        }
                        .instantiate_k(tycker, env)
                    }
                }
            }
            | _ => self.mismatch_k(tycker),
        }
    }

    #[track_caller]
    fn mismatch_k<T>(&self, tycker: &mut Tycker<'_>) -> ResultKont<T> {
        tycker.err_k(
            TyckError::PackageWitnessArityMismatch { expected: self.expected, found: self.found },
            std::panic::Location::caller(),
        )
    }
}

/// Check a value abstraction against a package-dependent arrow.
struct PackPiIntroduction {
    binder: su::PatId,
    body: su::TermId,
    signature: ss::PackPi,
}

/// Check a value abstraction against a pure package-dependent arrow.
struct ValuePackPiIntroduction {
    binder: su::PatId,
    body: su::TermId,
    signature: ss::ValuePackPi,
}

/// Associate a package pattern's leading type components with a `PackPi`
/// telescope.
struct PackPiPatternSkolems {
    pattern: su::PatId,
    signature: PackageSignature,
}

/// Associate each abstract witness in a package domain with the canonical
/// identity stored by its package-dependent arrow.
struct PackPiWitnessSkolems<'a> {
    witnesses: &'a [ss::AbstId],
    domain: ss::TypeId,
    expected: usize,
}

impl<'a> PackPiWitnessSkolems<'a> {
    fn new(witnesses: &'a [ss::AbstId], domain: ss::TypeId) -> Self {
        Self { witnesses, domain, expected: witnesses.len() }
    }

    fn collect_k(
        self, tycker: &mut Tycker<'_>, env: &ss::TyEnv,
    ) -> ResultKont<Vec<(ss::AbstId, ss::AbstId)>> {
        if self.witnesses.is_empty() {
            return Ok(Vec::new());
        }
        let view = self.domain.unroll_k(tycker)?.subst_env_k(tycker, env)?;
        match tycker.type_filled_k(&view)?.to_owned() {
            | ss::Type::ManifestKind(ss::ManifestKind { body, .. }) => {
                Self { witnesses: self.witnesses, domain: body, expected: self.expected }
                    .collect_k(tycker, env)
            }
            | ss::Type::Exists(ss::Exists { binder, mode: ss::ExistsMode::Abstract, body }) => {
                let Some((&canonical, witnesses)) = self.witnesses.split_first() else {
                    unreachable!()
                };
                let kind = tycker.statics.annotations_abst[&canonical];
                let payload = Alloc::alloc(tycker, canonical, kind, env);
                let domain = body.subst_abst_k(tycker, (binder.witness, payload))?;
                let tail =
                    Self { witnesses, domain, expected: self.expected }.collect_k(tycker, env)?;
                Ok(std::iter::once((binder.witness, canonical)).chain(tail).collect())
            }
            | ss::Type::Exists(ss::Exists {
                binder,
                mode: ss::ExistsMode::Manifest(definition),
                body,
            }) => {
                let domain = body.subst_abst_k(tycker, (binder.witness, definition))?;
                Self { witnesses: self.witnesses, domain, expected: self.expected }
                    .collect_k(tycker, env)
            }
            | _ => tycker.err_k(
                TyckError::PackageWitnessArityMismatch {
                    expected: self.expected,
                    found: self.expected - self.witnesses.len(),
                },
                std::panic::Location::caller(),
            ),
        }
    }
}

/// Traverse a package pattern and its domain in lockstep until every abstract
/// witness in a `PackPi` has a corresponding pattern component.
struct PackPiPatternAssignments<'a> {
    items: &'a [su::PatId],
    witnesses: &'a [ss::AbstId],
    domain: ss::TypeId,
    expected: usize,
    found: usize,
}

impl<'a> PackPiPatternAssignments<'a> {
    fn new(items: &'a [su::PatId], witnesses: &'a [ss::AbstId], domain: ss::TypeId) -> Self {
        Self { items, witnesses, domain, expected: witnesses.len(), found: items.len() }
    }

    fn collect_k(
        self, tycker: &mut Tycker<'_>, env: &ss::TyEnv,
    ) -> ResultKont<Vec<(su::PatId, ss::AbstId)>> {
        if self.witnesses.is_empty() {
            return Ok(Vec::new());
        }
        let Some((&item, items)) = self.items.split_first() else {
            return self.mismatch_k(tycker);
        };
        let view = self.domain.unroll_k(tycker)?.subst_env_k(tycker, env)?;
        match tycker.type_filled_k(&view)?.to_owned() {
            | ss::Type::ManifestKind(ss::ManifestKind { body, .. }) => Self {
                items,
                witnesses: self.witnesses,
                domain: body,
                expected: self.expected,
                found: self.found,
            }
            .collect_k(tycker, env),
            | ss::Type::Exists(ss::Exists { binder, mode: ss::ExistsMode::Abstract, body }) => {
                let Some((&witness, witnesses)) = self.witnesses.split_first() else {
                    unreachable!()
                };
                let kind = tycker.statics.annotations_abst[&witness];
                let payload = Alloc::alloc(tycker, witness, kind, env);
                let domain = body.subst_abst_k(tycker, (binder.witness, payload))?;
                let tail =
                    Self { items, witnesses, domain, expected: self.expected, found: self.found }
                        .collect_k(tycker, env)?;
                Ok(std::iter::once((item, witness)).chain(tail).collect())
            }
            | ss::Type::Exists(ss::Exists {
                binder,
                mode: ss::ExistsMode::Manifest(definition),
                body,
            }) => {
                let domain = body.subst_abst_k(tycker, (binder.witness, definition))?;
                Self {
                    items,
                    witnesses: self.witnesses,
                    domain,
                    expected: self.expected,
                    found: self.found,
                }
                .collect_k(tycker, env)
            }
            | _ => self.mismatch_k(tycker),
        }
    }

    #[track_caller]
    fn mismatch_k<T>(&self, tycker: &mut Tycker<'_>) -> ResultKont<T> {
        tycker.err_k(
            TyckError::PackageWitnessArityMismatch { expected: self.expected, found: self.found },
            std::panic::Location::caller(),
        )
    }
}

impl PackPiPatternSkolems {
    fn assignments_k(
        &self, tycker: &mut Tycker<'_>, env: &ss::TyEnv, pattern: su::PatId,
    ) -> ResultKont<Vec<(su::PatId, ss::AbstId)>> {
        match tycker.scoped.pats[&pattern].to_owned() {
            | su::Pattern::Ann(su::Ann { tm, .. }) => self.assignments_k(tycker, env, tm),
            | su::Pattern::Named(su::Named(_, inner)) => self.assignments_k(tycker, env, inner),
            | su::Pattern::Cons(su::ConsN(items, _)) => {
                let witnesses = self.signature.witnesses.iter().copied().collect::<Vec<_>>();
                PackPiPatternAssignments::new(&items, &witnesses, self.signature.domain)
                    .collect_k(tycker, env)
            }
            | su::Pattern::Project(_) => Ok(Vec::new()),
            | su::Pattern::Alias(su::Alias(patterns))
                if ExistentialProjectionPattern::members(tycker, patterns.iter().copied())
                    .is_some() =>
            {
                Ok(Vec::new())
            }
            | su::Pattern::Hole(_)
            | su::Pattern::Var(_)
            | su::Pattern::Ctor(_)
            | su::Pattern::Alias(_)
            | su::Pattern::Triv(_) => tycker.err_k(
                TyckError::PackageWitnessArityMismatch {
                    expected: self.signature.witnesses.len(),
                    found: 0,
                },
                std::panic::Location::caller(),
            ),
        }
    }
}

/// Apply a package-dependent function to a package with manifest witnesses.
struct PackPiElimination {
    function: ss::CompuId,
    argument: su::TermId,
    signature: ss::PackPi,
}

/// Apply a pure package-dependent function to a package with manifest
/// witnesses.
struct ValuePackPiElimination {
    function: ss::ValueId,
    argument: su::TermId,
    signature: ss::ValuePackPi,
}

impl<'a> Tyck<'a> for TyEnvT<PackPiPatternSkolems> {
    type Out = PatternSkolems;
    type Action = ();

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        let patterns = self.inner.assignments_k(tycker, &self.info, self.inner.pattern)?;
        let canonical = self.inner.signature.witnesses.iter().copied().collect::<Vec<_>>();
        let witnesses = PackPiWitnessSkolems::new(&canonical, self.inner.signature.domain)
            .collect_k(tycker, &self.info)?;
        Ok(PatternSkolems::new(patterns, witnesses))
    }
}

/// Type check one acyclic context binding.
impl<'a> Tyck<'a> for TyEnvT<su::Binding> {
    type Out = TyEnvT<()>;
    type Action = ();

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<TyEnvT<()>> {
        let mut env = self.mk(());
        let su::BindingForm::Definition(su::Definition { binder, bindee }) = self.inner.inner
        else {
            unreachable!()
        };
        let surface_bindee = bindee;
        let (bindee, is_sealed) = match bindee.syntactically_sealed(tycker) {
            | Some(bindee) => (bindee, true),
            | None => (bindee, false),
        };
        // synthesize the bindee
        let out_ann = env.mk(bindee).tyck_k(tycker, Action::syn())?;
        let env = match out_ann {
            | TermAnnId::Hole(_) | TermAnnId::Kind(_) => unreachable!(),
            | TermAnnId::Type(ty, kd) => {
                let bindee = ty;
                let binder = env.mk(binder).tyck_k(tycker, PatternAction::ana(kd.into()))?;
                let (binder, _kd) = binder.as_type();

                if let (Some(def), _) = binder.try_destruct_def(tycker) {
                    let _ = tycker.statics.type_definitions.upsert(def, bindee);
                }

                // seal the type if needed
                let bindee = if is_sealed {
                    let abst: AbstId = tycker.fresh();
                    tycker.statics.absts.insert_new(abst, ());
                    if let (Some(def), _kd) = binder.try_destruct_def(tycker) {
                        tycker.statics.abst_hints.insert_new(abst, def);
                    }
                    tycker.statics.seals.insert_new(abst, ty);
                    Alloc::alloc(tycker, abst, kd, &env.info)
                } else {
                    bindee
                };

                // add the type into the environment
                let TyEnvT { info: new_env, inner: () } =
                    env.mk(Assign(binder, bindee)).tyck_k(tycker, ())?;
                env.info = new_env;
                // should also be added to global if it only depends on global definitions
                match binder.try_destruct_def(tycker) {
                    | (Some(def), _) => {
                        // coctx defines what the bindee is using that is not local
                        if (tycker.scoped.coctxs_term_local[&surface_bindee].clone())
                            .into_iter()
                            .all(|id| tycker.statics.global_defs.get(&id).is_some())
                        {
                            tycker.statics.global_defs.ensure(def);
                        }
                    }
                    | (None, _) => {}
                }
                env
            }
            | TermAnnId::Value(bindee, ty) => {
                let binder_elaboration =
                    env.mk(binder).tyck_k(tycker, PatternAction::ana(ty.into()))?;
                let (binder, _) = binder_elaboration.as_value();
                // Existential package patterns introduce abstract types whose
                // scope extends over the following term.
                env.info = binder_elaboration.info;
                // should also be added to global if it only depends on global definitions
                match binder.try_destruct_def(tycker) {
                    | (Some(def), _) => {
                        let _ = tycker.statics.value_aliases.upsert(def, bindee);
                        // coctx defines what the bindee is using that is not local
                        if (tycker.scoped.coctxs_term_local[&surface_bindee].clone())
                            .into_iter()
                            .all(|id| tycker.statics.global_defs.get(&id).is_some())
                        {
                            tycker.statics.global_defs.ensure(def);
                            // consider adding it to the inlinables as well
                            let _ = tycker.statics.inlinables.upsert(def, bindee);
                        }
                    }
                    | (None, _) => {}
                }
                env
            }
            | TermAnnId::Compu(_, _) => {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            }
        };

        Ok(env)
    }
}

/// Type check a self-recursive or mutually recursive context node.
impl<'a> Tyck<'a> for FixPoint<TyEnvT<Vec<su::Binding>>> {
    type Out = TyEnvT<()>;
    type Action = ();

    fn tyck_inner_k<'f>(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        let FixPoint(group_under_env) = self;
        let bindings = &group_under_env.inner;
        let mut env = group_under_env.mk(());

        use std::collections::HashMap;

        let mut binder_map = HashMap::new();
        let mut abst_map = HashMap::new();
        for binding in bindings {
            let id = binding.id;
            let su::BindingForm::Definition(su::Definition { binder, bindee }) = binding.inner
            else {
                unreachable!("recursive groups contain definitions")
            };
            // the bindee must be sealed
            let Some(bindee) = bindee.syntactically_sealed(tycker) else {
                tycker.err_k(TyckError::MissingSeal, std::panic::Location::caller())?
            };
            // the type definition is self referencing, need to get the annotation
            let Some(syn_ann) = bindee.syntactically_annotated(tycker) else {
                tycker.err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?
            };
            // try synthesizing the kind
            let ann = env.mk(syn_ann).tyck_k(tycker, Action::syn())?;
            // the binder should be a type; register it before analyzing the bindee
            let kd =
                ann.try_as_kind(tycker, TyckError::SortMismatch, std::panic::Location::caller())?;
            let binder = env.mk(binder).tyck_k(tycker, PatternAction::ana(kd.into()))?;
            let (binder, _kd) = binder.as_type();
            binder_map.insert(id, binder);
            // register the def with abstract type
            let (def, kd) = binder.try_destruct_def(tycker);
            if let Some(def) = def {
                let abst: AbstId = tycker.fresh();
                tycker.statics.absts.insert_new(abst, ());
                tycker.statics.abst_hints.insert_new(abst, def);
                let abst_ty = Alloc::alloc(tycker, abst, kd, &env.info);
                env.info += [(def, abst_ty.into())];
                abst_map.insert(id, (abst, kd));
            }
        }
        for binding in bindings {
            let id = binding.id;
            let su::BindingForm::Definition(su::Definition { binder: _, bindee }) = binding.inner
            else {
                unreachable!("recursive groups contain definitions")
            };
            let binder = binder_map[&id];
            // should not be added to global because they are mutually recursive
            // match binder.try_destruct_def(tycker) {
            //     | (Some(def), _) => {
            //         tycker.statics.global_defs.insert(def, ());
            //     }
            //     | (None, _) => {}
            // }
            // remove seal
            let Some(bindee) = bindee.syntactically_sealed(tycker) else { unreachable!() };
            let bindee = env.mk(bindee).tyck_k(tycker, Action::syn())?;
            let (bindee, _kd) = bindee.try_as_type(
                tycker,
                TyckError::SortMismatch,
                std::panic::Location::caller(),
            )?;
            // subst vars in bindee
            let bindee_subst = bindee.subst_env_k(tycker, &env.info)?;
            if let (Some(def), _) = binder.try_destruct_def(tycker) {
                let _ = tycker.statics.type_definitions.upsert(def, bindee_subst);
            }
            // add the types to the seal arena
            let (abst, kd) = abst_map[&id];
            tycker.statics.seals.insert_new(abst, bindee_subst);
            let abst_ty = Alloc::alloc(tycker, abst, kd, &env.info);
            // add the type into the environment
            let TyEnvT { info: new_env, inner: () } =
                env.mk(Assign(binder, abst_ty)).tyck_k(tycker, ())?;
            env.info = new_env;
        }
        Ok(env)
    }
}

impl<'a> Tyck<'a> for TyEnvT<su::PatId> {
    type Out = CheckedPattern;
    type Action = PatternAction;

    fn tyck_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out> {
        tycker.guarded(|tycker| {
            // administrative
            tycker.tasks.push_back(TyckTask::Pat(self.inner, action.switch));
            let entity = su::EntityId::Pat(self.inner);
            let occurrence = tycker.check_counts.get(&entity).copied().unwrap_or(0);
            let _ = tycker.check_counts.upsert(entity, occurrence + 1);
            tycker.allocator.enter(
                self.inner.key_space().as_u64(),
                self.inner.raw().into_u32(),
                occurrence,
            );
            let result = self.tyck_inner_k(tycker, action);
            tycker.allocator.exit();
            result
        })
    }

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out> {
        let PatternAction { switch, skolems } = action;
        use su::Pattern as Pat;
        let elaboration = match tycker.scoped.pats[&self.inner].clone() {
            | Pat::Ann(pat) => {
                let su::Ann { tm, ty } = pat;
                let ty_out_ann = self.mk(ty).tyck_k(tycker, Action::syn())?;
                let ty_tm: AnnId = match ty_out_ann {
                    | TermAnnId::Kind(kd) => kd.into(),
                    | TermAnnId::Type(ty, _) => ty.into(),
                    | TermAnnId::Hole(_) => {
                        // Fixme: I forgor
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                };
                match switch {
                    | Switch::Syn => self
                        .mk(tm)
                        .tyck_k(tycker, PatternAction::ana(ty_tm).with_skolems(skolems.clone()))?,
                    | Switch::Ana(ty_ana) => {
                        let ty = Lub::lub_k(ty_tm, ty_ana, tycker)?;

                        self.mk(tm)
                            .tyck_k(tycker, PatternAction::ana(ty).with_skolems(skolems.clone()))?
                    }
                }
            }
            | Pat::Hole(pat) => {
                let su::Hole = pat;
                match switch {
                    | Switch::Syn => {
                        let pat = crate::query::InternedPat::new(tycker.db, self.inner);
                        let Some(error) =
                            crate::query::pat_hole_syn_judgment(tycker.db, tycker.data, pat)
                        else {
                            unreachable!("hole pattern judgments are query-produced")
                        };
                        tycker.err_k(error, std::panic::Location::caller())?
                    }
                    | Switch::Ana(ann) => {
                        self.mk(PatternCheck::new(PatAnnId::mk_hole(tycker, &self.info, ann)))
                    }
                }
            }
            | Pat::Var(def) => {
                let ann = match switch {
                    | Switch::Syn => match tycker.statics.annotations_var.get(&def) {
                        | Some(ann) => ann.to_owned(),
                        | None => {
                            let vtype = ss::VType.build(tycker, &self.info);
                            let fill = Alloc::alloc(tycker, self.inner, (), &());
                            let inferred: ss::TypeId =
                                Alloc::alloc(tycker, fill, vtype, &self.info);
                            inferred.into()
                        }
                    },
                    | Switch::Ana(ann) => ann,
                };
                let ann = match ann {
                    | AnnId::Set => AnnId::Set,
                    | AnnId::Kind(kd) => kd.into(),
                    | AnnId::Type(ty) => {
                        let vtype = ss::VType.build(tycker, &self.info);
                        let kd = tycker.statics.annotations_type[&ty].to_owned();
                        Lub::lub_k(vtype, kd, tycker)?;
                        ty.into()
                    }
                };
                if let Some(ann_) = tycker.statics.annotations_var.insert_or_get(def, ann) {
                    let ann = Lub::lub_k(ann_, ann, tycker)?;
                    tycker.statics.annotations_var.replace_existing(def, ann);
                }

                self.mk(PatternCheck::new(PatAnnId::mk_var(tycker, &self.info, def, ann)))
            }
            | Pat::Named(pat) => {
                let su::Named(name, inner) = pat;
                match switch {
                    | Switch::Syn => {
                        let checked = self
                            .mk(inner)
                            .tyck_k(tycker, PatternAction::syn().with_skolems(skolems.clone()))?;
                        match checked.annotation {
                            | inner_out @ (PatAnnId::Kind(_) | PatAnnId::Type(_, _)) => {
                                let pat = crate::query::InternedPat::new(tycker.db, self.inner);
                                let inner_interned =
                                    crate::query::InternedPatAnn::new(tycker.db, inner_out);
                                let Some(outcome) = crate::query::pat_named_syn_judgment(
                                    tycker.db,
                                    tycker.data,
                                    pat,
                                    inner_interned,
                                    tycker.site_occurrence(),
                                ) else {
                                    unreachable!(
                                        "the type and rejection arms of named pattern judgments are query-produced"
                                    )
                                };
                                match outcome {
                                    | crate::query::PatNamedSynOutcome::Type {
                                        kind_id,
                                        kind,
                                        named_id,
                                        named,
                                    } => {
                                        tycker
                                            .statics
                                            .kinds_pre
                                            .insert_new(kind_id, ss::Fillable::Done(kind));
                                        tycker.statics.tpats.insert_new(named_id, named);
                                        tycker
                                            .statics
                                            .annotations_tpat
                                            .insert_new(named_id, kind_id);
                                        tycker
                                            .statics
                                            .env_tpat
                                            .insert_new(named_id, self.info.clone());
                                        checked.with_annotation(PatAnnId::Type(named_id, kind_id))
                                    }
                                    | crate::query::PatNamedSynOutcome::Error(error) => {
                                        tycker.err_k(error, std::panic::Location::caller())?
                                    }
                                }
                            }
                            | PatAnnId::Value(inner, inner_ty) => {
                                let inner_kind = tycker.statics.annotations_type[&inner_ty];
                                let vtype = ss::VType.build(tycker, &self.info);
                                Lub::lub_k(vtype, inner_kind, tycker)?;
                                let named_ty = Alloc::alloc(
                                    tycker,
                                    ss::Label(name.clone(), inner_ty),
                                    vtype,
                                    &self.info,
                                );
                                let named = Alloc::alloc(
                                    tycker,
                                    ss::Named(name, inner),
                                    named_ty,
                                    &self.info,
                                );
                                checked.with_annotation(PatAnnId::Value(named, named_ty))
                            }
                        }
                    }
                    | Switch::Ana(AnnId::Kind(expected)) => {
                        let ss::Kind::Label(ss::Label(expected_name, inner_kind)) =
                            tycker.kind_filled_k(&expected)?.to_owned()
                        else {
                            tycker.err_k(TyckError::KindMismatch, std::panic::Location::caller())?
                        };
                        if name != expected_name {
                            tycker.err_k(
                                TyckError::NamedLabelMismatch {
                                    expected: expected_name,
                                    found: name.clone(),
                                },
                                std::panic::Location::caller(),
                            )?
                        }
                        let checked = self.mk(inner).tyck_k(
                            tycker,
                            PatternAction::ana(inner_kind.into()).with_skolems(skolems.clone()),
                        )?;
                        let (inner, _) = checked.try_as_type(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                        let named =
                            Alloc::alloc(tycker, ss::Named(name, inner), expected, &self.info);
                        checked.with_annotation(PatAnnId::Type(named, expected))
                    }
                    | Switch::Ana(AnnId::Type(expected)) => {
                        let expected_view =
                            expected.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                        let ss::Type::Label(ss::Label(expected_name, inner_ty)) =
                            tycker.type_filled_k(&expected_view)?.to_owned()
                        else {
                            tycker.err_k(
                                TyckError::TypeExpected {
                                    expected: "a named value type".to_string(),
                                    found: expected,
                                },
                                std::panic::Location::caller(),
                            )?
                        };
                        if name != expected_name {
                            tycker.err_k(
                                TyckError::NamedLabelMismatch {
                                    expected: expected_name,
                                    found: name.clone(),
                                },
                                std::panic::Location::caller(),
                            )?
                        }
                        let checked = self.mk(inner).tyck_k(
                            tycker,
                            PatternAction::ana(inner_ty.into()).with_skolems(skolems.clone()),
                        )?;
                        let (inner, _) = checked.try_as_value(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                        let named =
                            Alloc::alloc(tycker, ss::Named(name, inner), expected, &self.info);
                        checked.with_annotation(PatAnnId::Value(named, expected))
                    }
                    | Switch::Ana(AnnId::Set) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                }
            }
            | Pat::Project(su::ProjectionPattern(field, inner)) => match switch {
                | Switch::Syn => {
                    tycker.err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?
                }
                | Switch::Ana(AnnId::Kind(expected)) => {
                    let candidate = FieldProjectionResolver::r#type(tycker, expected, &field)?;
                    let checked = self.mk(inner).tyck_k(
                        tycker,
                        PatternAction::ana(candidate.projected.into())
                            .with_skolems(skolems.clone()),
                    )?;
                    let (payload, _) = checked.try_as_type(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    let pattern = FieldProjectionResolver::type_pattern(
                        tycker, &self.info, expected, candidate, payload,
                    );
                    checked.with_annotation(PatAnnId::Type(pattern, expected))
                }
                | Switch::Ana(AnnId::Type(expected)) => {
                    if ExistentialProjectionPattern::applies_k(tycker, &self.info, expected)? {
                        let members = ExistentialProjectionPattern::members(
                            tycker,
                            std::iter::once(self.inner),
                        )
                        .unwrap();
                        ExistentialProjectionPattern::check_k(
                            tycker,
                            &self.info,
                            expected,
                            members,
                            skolems.clone(),
                        )?
                    } else {
                        let candidate =
                            FieldProjectionResolver::value_k(tycker, &self.info, expected, &field)?;
                        let checked = self.mk(inner).tyck_k(
                            tycker,
                            PatternAction::ana(candidate.projected.into())
                                .with_skolems(skolems.clone()),
                        )?;
                        let (payload, _) = checked.try_as_value(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                        if !ValuePatternShape::is_irrefutable(tycker, payload) {
                            tycker.err_k(
                                TyckError::RefutableFieldProjectionPattern,
                                std::panic::Location::caller(),
                            )?
                        }
                        let pattern = FieldProjectionResolver::value_pattern(
                            tycker, &self.info, expected, candidate, payload,
                        );
                        checked.with_annotation(PatAnnId::Value(pattern, expected))
                    }
                }
                | Switch::Ana(AnnId::Set) => {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            },
            | Pat::Ctor(pat) => match switch {
                | Switch::Syn => {
                    let pat = crate::query::InternedPat::new(tycker.db, self.inner);
                    let Some(error) =
                        crate::query::pat_ctor_syn_judgment(tycker.db, tycker.data, pat)
                    else {
                        unreachable!("constructor pattern judgments are query-produced")
                    };
                    tycker.err_k(error, std::panic::Location::caller())?
                }
                | Switch::Ana(ann) => {
                    let AnnId::Type(ann_ty) = ann else {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    let ann_ty_unroll = ann_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                    let ss::Type::Data(data_id) = &tycker.type_filled_k(&ann_ty_unroll)? else {
                        tycker.err_k(
                            TyckError::TypeExpected {
                                expected: "data type definition".to_string(),
                                found: ann_ty_unroll,
                            },
                            std::panic::Location::caller(),
                        )?
                    };
                    let su::Ctor(ctor, args) = pat;
                    use std::collections::HashMap;
                    let arm_ty = match tycker.statics.datas[data_id]
                        .clone()
                        .into_iter()
                        .collect::<HashMap<_, _>>()
                        .get(&ctor)
                        .cloned()
                    {
                        | Some(ty) => ty,
                        | None => tycker.err_k(
                            TyckError::UnknownDataConstructor(ctor.clone()),
                            std::panic::Location::caller(),
                        )?,
                    };
                    let args_out_ann = self.mk(args).tyck_k(
                        tycker,
                        PatternAction::ana(arm_ty.to_owned().into()).with_skolems(skolems.clone()),
                    )?;
                    let (args, _) = args_out_ann.as_value();
                    let pat =
                        Alloc::alloc(tycker, ss::Ctor(ctor.to_owned(), args), ann_ty, &self.info);
                    tycker.statics.data_pat_hints.insert_new(pat, data_id.to_owned());
                    args_out_ann.with_annotation(PatAnnId::Value(pat, ann_ty))
                }
            },
            | Pat::Alias(su::Alias(patterns)) => match switch {
                | Switch::Syn => {
                    let pat = crate::query::InternedPat::new(tycker.db, self.inner);
                    let Some(error) =
                        crate::query::pat_alias_syn_judgment(tycker.db, tycker.data, pat)
                    else {
                        unreachable!("alias pattern judgments are query-produced")
                    };
                    tycker.err_k(error, std::panic::Location::caller())?
                }
                | Switch::Ana(AnnId::Type(expected)) => {
                    let members =
                        ExistentialProjectionPattern::members(tycker, patterns.iter().copied());
                    if let Some(members) = members
                        && ExistentialProjectionPattern::applies_k(tycker, &self.info, expected)?
                    {
                        ExistentialProjectionPattern::check_k(
                            tycker,
                            &self.info,
                            expected,
                            members,
                            skolems.clone(),
                        )?
                    } else {
                        let initial = (self.info.clone(), Vec::new(), Vec::new());
                        let (pattern_env, output, opened) = patterns.into_iter().try_fold(
                            initial,
                            |(pattern_env, mut output, mut opened), pattern| -> ResultKont<_> {
                                let checked = TyEnvT::new(pattern_env, pattern).tyck_k(
                                    tycker,
                                    PatternAction::ana(expected.into())
                                        .with_skolems(skolems.clone()),
                                )?;
                                let TyEnvT { info, inner } = checked;
                                let (pattern, _) = inner.annotation.try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                if !ValuePatternShape::is_irrefutable(tycker, pattern) {
                                    tycker.err_k(
                                        TyckError::RefutablePatternAlias,
                                        std::panic::Location::caller(),
                                    )?
                                }
                                output.push(pattern);
                                opened.extend(inner.opened);
                                Ok((info, output, opened))
                            },
                        )?;
                        let patterns = ss::ConsN::from_vec(output).unwrap();
                        let alias = Alloc::alloc(tycker, ss::Alias(patterns), expected, &self.info);
                        TyEnvT::new(
                            pattern_env,
                            PatternCheck::with_opened(PatAnnId::Value(alias, expected), opened),
                        )
                    }
                }
                | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => tycker
                    .err_k(TyckError::PatternAliasRequiresValue, std::panic::Location::caller())?,
            },
            | Pat::Triv(su::Triv) => match switch {
                | Switch::Syn => {
                    let pat = crate::query::InternedPat::new(tycker.db, self.inner);
                    let Some(outcome) = crate::query::pat_triv_syn_judgment(
                        tycker.db,
                        tycker.data,
                        pat,
                        tycker.site_occurrence(),
                    ) else {
                        unreachable!("trivial pattern judgments are query-produced")
                    };
                    let crate::query::PatTrivSynOutcome { id, value, ty } = outcome;
                    tycker.statics.vpats.insert_new(id, value);
                    tycker.statics.annotations_vpat.insert_new(id, ty);
                    tycker.statics.env_vpat.insert_new(id, self.info.clone());
                    self.mk(PatternCheck::new(PatAnnId::Value(id, ty)))
                }
                | Switch::Ana(AnnId::Type(ana)) => {
                    let unit = ss::UnitTy.build(tycker, &self.info);
                    let ann = Lub::lub_k(unit, ana, tycker)?;
                    let triv = Alloc::alloc(tycker, ss::Triv, ann, &self.info);
                    self.mk(PatternCheck::new(PatAnnId::Value(triv, ann)))
                }
                | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            },
            | Pat::Cons(pat) => {
                let su::ConsN(items, tail) = pat;
                match switch {
                    | Switch::Syn => {
                        let initial = (self.info.clone(), Vec::new(), Vec::new(), Vec::new());
                        let (pattern_env, output, annotations, mut opened) = items
                            .into_iter()
                            .try_fold(initial, |state, item| -> ResultKont<_> {
                                let (pattern_env, mut output, mut annotations, mut opened) = state;
                                let checked = TyEnvT::new(pattern_env, item).tyck_k(
                                    tycker,
                                    PatternAction::syn().with_skolems(skolems.clone()),
                                )?;
                                let TyEnvT { info, inner } = checked;
                                let (item, annotation) = inner.annotation.try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                output.push(item);
                                annotations.push(annotation);
                                opened.extend(inner.opened);
                                Ok((info, output, annotations, opened))
                            })?;

                        let checked = TyEnvT::new(pattern_env, tail)
                            .tyck_k(tycker, PatternAction::syn().with_skolems(skolems.clone()))?;
                        let TyEnvT { info: pattern_env, inner } = checked;
                        let (tail, tail_annotation) = inner.annotation.try_as_value(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                        opened.extend(inner.opened);
                        let item_outcomes = output
                            .iter()
                            .zip(&annotations)
                            .map(|(vpat, ty)| PatAnnId::Value(*vpat, *ty))
                            .collect::<Vec<_>>();
                        let pat_interned = crate::query::InternedPat::new(tycker.db, self.inner);
                        let items_interned =
                            crate::query::InternedPatItems::new(tycker.db, item_outcomes);
                        let tail_interned = crate::query::InternedPatAnn::new(
                            tycker.db,
                            PatAnnId::Value(tail, tail_annotation),
                        );
                        let Some(outcome) = crate::query::pat_cons_syn_judgment(
                            tycker.db,
                            tycker.data,
                            pat_interned,
                            items_interned,
                            tail_interned,
                            tycker.site_occurrence(),
                        ) else {
                            unreachable!("consumed pattern judgments are query-produced")
                        };
                        for (id, prod) in outcome.prods {
                            tycker.statics.types_pre.insert_new(id, ss::Fillable::Done(prod));
                            tycker.statics.annotations_type.insert_new(id, outcome.vtype);
                            tycker.statics.env_type.insert_new(id, pattern_env.clone());
                        }
                        tycker.statics.vpats.insert_new(outcome.pat_id, outcome.pat);
                        tycker.statics.annotations_vpat.insert_new(outcome.pat_id, outcome.ann);
                        tycker.statics.env_vpat.insert_new(outcome.pat_id, self.info.clone());
                        TyEnvT::new(
                            pattern_env,
                            PatternCheck::with_opened(
                                PatAnnId::Value(outcome.pat_id, outcome.ann),
                                opened,
                            ),
                        )
                    }
                    | Switch::Ana(AnnId::Type(expected)) => {
                        let expected_view = expected;
                        match expected_view.reveal_or_refine_product_k(tycker, &self.info)? {
                            | ss::Type::Prod(_) => {
                                let mut expected_item = expected_view;
                                let mut pattern_env = self.info.clone();
                                let mut opened = Vec::new();
                                let mut output = Vec::with_capacity(items.len());
                                let mut annotations = Vec::with_capacity(items.len());
                                for item in items {
                                    let ss::Prod(item_ty, next_ty) =
                                        expected_item.view_product_k(tycker, &pattern_env)?;
                                    expected_item = next_ty;
                                    let checked = TyEnvT::new(pattern_env.clone(), item).tyck_k(
                                        tycker,
                                        PatternAction::ana(item_ty.into())
                                            .with_skolems(skolems.clone()),
                                    )?;
                                    let (item, annotation) = checked.try_as_value(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    pattern_env = checked.info;
                                    opened.extend(checked.inner.opened);
                                    output.push(item);
                                    annotations.push(annotation);
                                }

                                let checked = TyEnvT::new(pattern_env.clone(), tail).tyck_k(
                                    tycker,
                                    PatternAction::ana(expected_item.into())
                                        .with_skolems(skolems.clone()),
                                )?;
                                let (tail, ann) = checked.try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                pattern_env = checked.info;
                                opened.extend(checked.inner.opened);
                                let vtype = ss::VType.build(tycker, &pattern_env);
                                let ann = annotations.into_iter().rev().fold(ann, |ann, head| {
                                    Alloc::alloc(tycker, ss::Prod(head, ann), vtype, &pattern_env)
                                });
                                let cons =
                                    Alloc::alloc(tycker, ss::ConsN(output, tail), ann, &self.info);
                                TyEnvT::new(
                                    pattern_env,
                                    PatternCheck::with_opened(PatAnnId::Value(cons, ann), opened),
                                )
                            }
                            | ss::Type::Exists(_) | ss::Type::ManifestKind(_) => {
                                let mut body_env = self.info.clone();
                                let mut body_ty = expected;
                                let mut body_index = items.len();
                                let mut static_patterns: Vec<ss::StaticPatId> = Vec::new();
                                let mut opened = Vec::new();
                                for (index, item) in items.iter().copied().enumerate() {
                                    let view =
                                        body_ty.unroll_k(tycker)?.subst_env_k(tycker, &body_env)?;
                                    match tycker.type_filled_k(&view)?.to_owned() {
                                        | ss::Type::ManifestKind(ss::ManifestKind {
                                            binder,
                                            definition,
                                            body,
                                        }) => {
                                            let checked = TyEnvT::new(body_env.clone(), item)
                                                .tyck_k(
                                                    tycker,
                                                    PatternAction::ana(AnnId::Set)
                                                        .with_skolems(skolems.clone()),
                                                )?;
                                            let pattern = checked.annotation.try_as_kind(
                                                tycker,
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?;
                                            body_env =
                                                TyEnvT::new(body_env, Assign(pattern, definition))
                                                    .tyck_k(tycker, ())?
                                                    .info;
                                            body_ty = body;
                                            static_patterns.push(pattern.into());
                                            let _ = binder;
                                        }
                                        | ss::Type::Exists(ss::Exists {
                                            binder: source_binder,
                                            mode,
                                            body: next_ty,
                                        }) => {
                                            let domain_kind = source_binder.domain_kind(tycker);
                                            let payload_kind = source_binder.payload_kind(tycker);
                                            let checked = TyEnvT::new(body_env.clone(), item)
                                                .tyck_k(
                                                    tycker,
                                                    PatternAction::ana(domain_kind.into())
                                                        .with_skolems(skolems.clone()),
                                                )?;
                                            let (witness, _) = checked.try_as_type(
                                                tycker,
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?;
                                            body_env = checked.info;
                                            opened.extend(checked.inner.opened);

                                            match mode {
                                                | ss::ExistsMode::Abstract => {
                                                    // Ordinary elimination is fresh. Checking a PackPi
                                                    // introduction instead reuses the signature's
                                                    // canonical identity for this pattern component.
                                                    let skolem = match skolems.get(&item) {
                                                        | Some(skolem) => {
                                                            let expected = tycker
                                                                .statics
                                                                .annotations_abst[&skolem];
                                                            Lub::lub_k(
                                                                expected,
                                                                payload_kind,
                                                                tycker,
                                                            )?;
                                                            skolem
                                                        }
                                                        | None => {
                                                            let (def, _) =
                                                                witness.try_destruct_def(tycker);
                                                            Alloc::alloc(
                                                                tycker,
                                                                def,
                                                                payload_kind,
                                                                &(),
                                                            )
                                                        }
                                                    };
                                                    tycker.transfer_builtin_role_k(
                                                        source_binder.witness,
                                                        skolem,
                                                    )?;
                                                    tycker
                                                        .statics
                                                        .existential_skolems
                                                        .ensure(skolem);
                                                    body_env = body_env.with_skolem(skolem);
                                                    let abstract_ty = Alloc::alloc(
                                                        tycker,
                                                        skolem,
                                                        payload_kind,
                                                        &body_env,
                                                    );
                                                    let full_witness = source_binder
                                                        .pattern
                                                        .introduce_payload(tycker, abstract_ty);
                                                    let full_witness =
                                                        tycker.err_p_to_k(full_witness)?;
                                                    body_env = TyEnvT::new(
                                                        body_env,
                                                        Assign(witness, full_witness),
                                                    )
                                                    .tyck_k(tycker, ())?
                                                    .info;
                                                    body_ty = next_ty.subst_abst_k(
                                                        tycker,
                                                        (source_binder.witness, abstract_ty),
                                                    )?;
                                                    opened.push(skolem);
                                                }
                                                | ss::ExistsMode::Manifest(definition) => {
                                                    let definition_kind = tycker
                                                        .statics
                                                        .annotations_type[&definition];
                                                    Lub::lub_k(
                                                        payload_kind,
                                                        definition_kind,
                                                        tycker,
                                                    )?;
                                                    let full_definition = source_binder
                                                        .pattern
                                                        .introduce_payload(tycker, definition);
                                                    let full_definition =
                                                        tycker.err_p_to_k(full_definition)?;
                                                    body_env = TyEnvT::new(
                                                        body_env,
                                                        Assign(witness, full_definition),
                                                    )
                                                    .tyck_k(tycker, ())?
                                                    .info;
                                                    body_ty = next_ty.subst_abst_k(
                                                        tycker,
                                                        (source_binder.witness, definition),
                                                    )?;
                                                }
                                            }
                                            static_patterns.push(witness.into());
                                        }
                                        | _ => {
                                            body_index = index;
                                            break;
                                        }
                                    }
                                }

                                if static_patterns.is_empty() {
                                    tycker.err_k(
                                        TyckError::TypeExpected {
                                            expected: "an existential package".to_string(),
                                            found: expected,
                                        },
                                        std::panic::Location::caller(),
                                    )?
                                }

                                let body_items = &items[body_index..];
                                let body = if body_items.is_empty() {
                                    let checked = TyEnvT::new(body_env.clone(), tail).tyck_k(
                                        tycker,
                                        PatternAction::ana(body_ty.into())
                                            .with_skolems(skolems.clone()),
                                    )?;
                                    let (body, _) = checked.try_as_value(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    body_env = checked.info;
                                    opened.extend(checked.inner.opened);
                                    body
                                } else {
                                    let body_view = body_ty;
                                    let _ = body_view.view_product_k(tycker, &body_env)?;

                                    let mut expected_item = body_view;
                                    let mut output = Vec::with_capacity(body_items.len());
                                    let mut annotations = Vec::with_capacity(body_items.len());
                                    for item in body_items.iter().copied() {
                                        let ss::Prod(item_ty, next_ty) =
                                            expected_item.view_product_k(tycker, &body_env)?;
                                        expected_item = next_ty;
                                        let checked = TyEnvT::new(body_env.clone(), item).tyck_k(
                                            tycker,
                                            PatternAction::ana(item_ty.into())
                                                .with_skolems(skolems.clone()),
                                        )?;
                                        let (item, annotation) = checked.try_as_value(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        body_env = checked.info;
                                        opened.extend(checked.inner.opened);
                                        output.push(item);
                                        annotations.push(annotation);
                                    }

                                    let checked = TyEnvT::new(body_env.clone(), tail).tyck_k(
                                        tycker,
                                        PatternAction::ana(expected_item.into())
                                            .with_skolems(skolems.clone()),
                                    )?;
                                    let (tail, ann) = checked.try_as_value(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    body_env = checked.info;
                                    opened.extend(checked.inner.opened);
                                    let vtype = ss::VType.build(tycker, &body_env);
                                    let ann =
                                        annotations.into_iter().rev().fold(ann, |ann, head| {
                                            Alloc::alloc(
                                                tycker,
                                                ss::Prod(head, ann),
                                                vtype,
                                                &body_env,
                                            )
                                        });
                                    Alloc::alloc(tycker, ss::ConsN(output, tail), ann, &body_env)
                                };
                                let cons = Alloc::alloc(
                                    tycker,
                                    ss::ConsN(static_patterns, body),
                                    expected,
                                    &self.info,
                                );
                                TyEnvT::new(
                                    body_env,
                                    PatternCheck::with_opened(
                                        PatAnnId::Value(cons, expected),
                                        opened,
                                    ),
                                )
                            }
                            | _ => tycker.err_k(
                                TyckError::TypeExpected {
                                    expected: "one of `_ * _` or `exists _ . _`".to_string(),
                                    found: expected,
                                },
                                std::panic::Location::caller(),
                            )?,
                        }
                    }
                    | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                }
            }
        };

        // maintain back mapping
        tycker.statics.pats.ensure(self.inner, elaboration.annotation.as_pat());

        Ok(elaboration)
    }
}

impl<'a> Tyck<'a> for TyEnvT<Assign<ss::KPatId, ss::KindId>> {
    type Out = TyEnvT<()>;
    type Action = ();

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        let Assign(assigner, assignee) = self.inner;
        match tycker.statics.kpats[&assigner] {
            | ss::KindPattern::Hole(_) => Ok(self.mk(())),
            | ss::KindPattern::Var(definition) => {
                let mut env = self.info.clone();
                env += [(definition, assignee.into())];
                Ok(TyEnvT::new(env, ()))
            }
        }
    }
}

impl<'a> Tyck<'a> for TyEnvT<Assign<ss::TPatId, ss::TypeId>> {
    type Out = TyEnvT<()>;
    type Action = ();

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        use ss::TypePattern as TPat;
        let Assign(assigner, assignee) = self.inner;
        let pat = tycker.statics.tpats[&assigner].to_owned();
        match pat {
            | TPat::Hole(_) => Ok(self.mk(())),
            | TPat::Var(def) => {
                // defensive programming: def should be in ctx and should be a kind;
                let def_kd = {
                    let ann = tycker.statics.annotations_var[&def];
                    ann.as_kind()
                };
                // def_kd should correctly be the type of assignee
                let assignee_kd = { tycker.statics.annotations_type[&assignee].to_owned() };
                Lub::lub_k(def_kd, assignee_kd, tycker)?;
                let mut env = self.info.clone();
                env += [(def, assignee.into())];
                Ok(TyEnvT { info: env, inner: () })
            }
            | TPat::Named(ss::Named(name, inner)) => {
                let payload_kind = tycker.statics.annotations_tpat[&inner];
                let payload = assignee.project_named(tycker, &name, payload_kind);
                let payload = tycker.err_p_to_k(payload)?;
                self.mk(Assign(inner, payload)).tyck_k(tycker, ())
            }
        }
    }
}

impl<'a> Tyck<'a> for TyEnvT<ValuePiFormation> {
    type Out = TermAnnId;
    type Action = Action<AnnId>;

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out> {
        let ValuePiFormation { binder, codomain } = &self.inner;
        let (pattern, domain) =
            binder.try_as_value(tycker, TyckError::SortMismatch, std::panic::Location::caller())?;
        if pattern.syntactically_used(tycker) {
            tycker.err_k(
                TyckError::Expressivity("dependent types are not supported yet"),
                std::panic::Location::caller(),
            )?
        }

        let domain_kind = tycker.statics.annotations_type[&domain];
        let vtype = ss::VType.build(tycker, &self.info);
        Lub::lub_k(vtype, domain_kind, tycker)?;

        let codomain = TyEnvT::new(binder.info.clone(), *codomain).tyck_k(tycker, action)?;
        let (codomain, codomain_kind) = codomain.try_as_type(
            tycker,
            TyckError::SortMismatch,
            std::panic::Location::caller(),
        )?;
        let pi = match tycker.kind_filled_k(&codomain_kind)?.to_owned() {
            | ss::Kind::VType(_) => match binder.package_telescope_k(tycker)? {
                | None => {
                    binder.close_scope_k(tycker, codomain)?;
                    Alloc::alloc(
                        tycker,
                        ss::ValueArrow(domain, codomain),
                        codomain_kind,
                        &self.info,
                    )
                }
                | Some(witnesses) => {
                    let signature = ss::ValuePackPi { domain, witnesses, codomain };
                    tycker.validate_builtin_signature_k(&ss::PackPi {
                        domain: signature.domain,
                        witnesses: signature.witnesses.clone(),
                        codomain: signature.codomain,
                    })?;
                    Alloc::alloc(tycker, signature, codomain_kind, &self.info)
                }
            },
            | ss::Kind::CType(_) => match binder.package_telescope_k(tycker)? {
                | None => {
                    binder.close_scope_k(tycker, codomain)?;
                    Alloc::alloc(tycker, ss::Arrow(domain, codomain), codomain_kind, &self.info)
                }
                | Some(witnesses) => {
                    let signature = ss::PackPi { domain, witnesses, codomain };
                    tycker.validate_builtin_signature_k(&signature)?;
                    Alloc::alloc(tycker, signature, codomain_kind, &self.info)
                }
            },
            | ss::Kind::Arrow(_) | ss::Kind::Label(_) => {
                tycker.err_k(TyckError::KindMismatch, std::panic::Location::caller())?
            }
        };
        Ok(TermAnnId::Type(pi, codomain_kind))
    }
}

impl<'a> Tyck<'a> for TyEnvT<PackPiInstantiation> {
    type Out = ss::TypeId;
    type Action = ();

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        let PackPiInstantiation { signature, witnesses } = &self.inner;
        let canonical = signature.witnesses.iter().copied().collect::<Vec<_>>();
        PackPiInstantiationState::new(signature, &canonical, witnesses)
            .instantiate_k(tycker, &self.info)
    }
}

impl<'a> Tyck<'a> for TyEnvT<PackPiIntroduction> {
    type Out = TermAnnId;
    type Action = ();

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        let PackPiIntroduction { binder, body, signature } = &self.inner;
        let skolems = self
            .mk(PackPiPatternSkolems { pattern: *binder, signature: signature.clone().into() })
            .tyck_k(tycker, ())?;
        let binder = self
            .mk(*binder)
            .tyck_k(tycker, PatternAction::ana(signature.domain.into()).with_skolems(skolems))?;
        let (pattern, domain) =
            binder.try_as_value(tycker, TyckError::SortMismatch, std::panic::Location::caller())?;
        Lub::lub_k(signature.domain, domain, tycker)?;

        let Some(witnesses) = binder.package_telescope_k(tycker)? else {
            tycker.err_k(
                TyckError::PackageWitnessArityMismatch {
                    expected: signature.witnesses.len(),
                    found: 0,
                },
                std::panic::Location::caller(),
            )?
        };
        let body = TyEnvT::new(binder.info.clone(), *body)
            .tyck_k(tycker, Action::ana(signature.codomain.into()))?;
        let (body, codomain) =
            body.try_as_compu(tycker, TyckError::SortMismatch, std::panic::Location::caller())?;

        let ctype = ss::CType.build(tycker, &self.info);
        let signature =
            Alloc::alloc(tycker, ss::PackPi { domain, witnesses, codomain }, ctype, &self.info);
        signature.constrain_to_scope_k(tycker, self.info.skolem_scope())?;
        let abstraction = Alloc::alloc(tycker, ss::Abs(pattern, body), signature, &self.info);
        Ok(TermAnnId::Compu(abstraction, signature))
    }
}

impl<'a> Tyck<'a> for TyEnvT<ValuePackPiIntroduction> {
    type Out = TermAnnId;
    type Action = ();

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        let ValuePackPiIntroduction { binder, body, signature } = &self.inner;
        let skolems = self
            .mk(PackPiPatternSkolems { pattern: *binder, signature: signature.clone().into() })
            .tyck_k(tycker, ())?;
        let binder = self
            .mk(*binder)
            .tyck_k(tycker, PatternAction::ana(signature.domain.into()).with_skolems(skolems))?;
        let (pattern, domain) =
            binder.try_as_value(tycker, TyckError::SortMismatch, std::panic::Location::caller())?;
        Lub::lub_k(signature.domain, domain, tycker)?;

        let Some(witnesses) = binder.package_telescope_k(tycker)? else {
            tycker.err_k(
                TyckError::PackageWitnessArityMismatch {
                    expected: signature.witnesses.len(),
                    found: 0,
                },
                std::panic::Location::caller(),
            )?
        };
        let body = TyEnvT::new(binder.info.clone(), *body)
            .tyck_k(tycker, Action::ana(signature.codomain.into()))?;
        let (body, codomain) =
            body.try_as_value(tycker, TyckError::SortMismatch, std::panic::Location::caller())?;

        let vtype = ss::VType.build(tycker, &self.info);
        let signature = Alloc::alloc(
            tycker,
            ss::ValuePackPi { domain, witnesses, codomain },
            vtype,
            &self.info,
        );
        signature.constrain_to_scope_k(tycker, self.info.skolem_scope())?;
        let abstraction: ss::ValueId =
            Alloc::alloc(tycker, ss::Abs(pattern, body), signature, &self.info);
        Ok(TermAnnId::Value(abstraction, signature))
    }
}

impl<'a> Tyck<'a> for TyEnvT<PackPiElimination> {
    type Out = TermAnnId;
    type Action = Action<AnnId>;

    fn tyck_inner_k(
        &self, tycker: &mut Tycker<'a>, Action { switch }: Self::Action,
    ) -> ResultKont<Self::Out> {
        let PackPiElimination { function, argument, signature } = &self.inner;
        let argument = self.mk(*argument).tyck_k(tycker, Action::ana(signature.domain.into()))?;
        let (argument, _) = argument.try_as_value(
            tycker,
            TyckError::SortMismatch,
            std::panic::Location::caller(),
        )?;
        let Some(witnesses) = argument.package_witnesses(tycker) else {
            tycker.err_k(
                TyckError::PackageWitnessesUnavailable { package: argument },
                std::panic::Location::caller(),
            )?
        };
        let codomain = self
            .mk(PackPiInstantiation { signature: signature.clone().into(), witnesses })
            .tyck_k(tycker, ())?;
        codomain.constrain_to_scope_k(tycker, self.info.skolem_scope())?;
        let codomain = match switch {
            | Switch::Syn => codomain,
            | Switch::Ana(AnnId::Type(expected)) => Lub::lub_k(codomain, expected, tycker)?,
            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            }
        };
        let application = Alloc::alloc(tycker, ss::App(*function, argument), codomain, &self.info);
        Ok(TermAnnId::Compu(application, codomain))
    }
}

impl<'a> Tyck<'a> for TyEnvT<ValuePackPiElimination> {
    type Out = TermAnnId;
    type Action = Action<AnnId>;

    fn tyck_inner_k(
        &self, tycker: &mut Tycker<'a>, Action { switch }: Self::Action,
    ) -> ResultKont<Self::Out> {
        let ValuePackPiElimination { function, argument, signature } = &self.inner;
        let argument = self.mk(*argument).tyck_k(tycker, Action::ana(signature.domain.into()))?;
        let (argument, _) = argument.try_as_value(
            tycker,
            TyckError::SortMismatch,
            std::panic::Location::caller(),
        )?;
        let Some(witnesses) = argument.package_witnesses(tycker) else {
            tycker.err_k(
                TyckError::PackageWitnessesUnavailable { package: argument },
                std::panic::Location::caller(),
            )?
        };
        let codomain = self
            .mk(PackPiInstantiation { signature: signature.clone().into(), witnesses })
            .tyck_k(tycker, ())?;
        codomain.constrain_to_scope_k(tycker, self.info.skolem_scope())?;
        let codomain = match switch {
            | Switch::Syn => codomain,
            | Switch::Ana(AnnId::Type(expected)) => Lub::lub_k(codomain, expected, tycker)?,
            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            }
        };
        let application: ss::ValueId =
            Alloc::alloc(tycker, ss::App(*function, argument), codomain, &self.info);
        Ok(TermAnnId::Value(application, codomain))
    }
}

impl<'a> Tyck<'a> for TyEnvT<su::TermId> {
    type Out = TermAnnId;
    type Action = Action<AnnId>;

    fn tyck_k(
        &self, tycker: &mut Tycker<'a>, Action { switch }: Self::Action,
    ) -> ResultKont<Self::Out> {
        tycker.guarded(|tycker| {
            // administrative
            tycker.tasks.push_back(TyckTask::Term(self.inner, switch));
            let entity = su::EntityId::Term(self.inner);
            let occurrence = tycker.check_counts.get(&entity).copied().unwrap_or(0);
            let _ = tycker.check_counts.upsert(entity, occurrence + 1);
            tycker.allocator.enter(
                self.inner.key_space().as_u64(),
                self.inner.raw().into_u32(),
                occurrence,
            );
            let result = self.tyck_inner_k(tycker, Action { switch });
            tycker.allocator.exit();
            result
        })
    }

    fn tyck_inner_k(
        &self, tycker: &mut Tycker<'a>, Action { mut switch }: Self::Action,
    ) -> ResultKont<Self::Out> {
        // check if we're analyzing against an unfilled type
        match switch {
            | Switch::Syn => {}
            | Switch::Ana(ana) => match ana {
                | AnnId::Set => {}
                | AnnId::Kind(kd) => match tycker.statics.kinds_pre[&kd].to_owned() {
                    | Fillable::Fill(fill) => match self.tyck_k(tycker, Action::syn())? {
                        | TermAnnId::Type(ty, kd) => {
                            let kd = fill.fill_k(tycker, kd.into())?.as_kind();
                            return Ok(TermAnnId::Type(ty, kd));
                        }
                        | TermAnnId::Hole(_)
                        | TermAnnId::Kind(_)
                        | TermAnnId::Value(_, _)
                        | TermAnnId::Compu(_, _) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    },
                    | _ => {}
                },
                | AnnId::Type(ty) => match tycker.statics.types_pre[&ty].to_owned() {
                    | Fillable::Fill(fill) => match self.tyck_k(tycker, Action::syn())? {
                        | TermAnnId::Value(v, ty) => {
                            let ty = fill.fill_k(tycker, ty.into())?.as_type();
                            return Ok(TermAnnId::Value(v, ty));
                        }
                        | TermAnnId::Compu(c, ty) => {
                            let ty = fill.fill_k(tycker, ty.into())?.as_type();
                            return Ok(TermAnnId::Compu(c, ty));
                        }
                        | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Type(_, _) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    },
                    | _ => {
                        let kd = tycker.statics.annotations_type[&ty].to_owned();
                        switch = Switch::Ana(
                            ty.subst_env_k(tycker, &self.info)?.normalize_k(tycker, kd)?.into(),
                        )
                    }
                },
            },
        }
        // the switch should contain no unfilled from here on

        use su::Term as Tm;
        let out_ann = match tycker.scoped.terms[&self.inner].to_owned() {
            | Tm::Meta(term) => {
                let su::MetaT(meta, term) = term;
                let res = self.mk(term).tyck_k(tycker, Action::switch(switch))?;
                if let Some(meta) = meta
                    .specialize::<BuiltinMeta>()
                    .expect("builtin metadata is validated during desugaring")
                {
                    BuiltinAttachment::new(meta.role, res).register_k(tycker, &self.info)?;
                }
                if meta.is("debug") {
                    tycker
                        .observations
                        .push(TyckObservation::Debug { metadata: meta, result: res });
                }
                res
            }
            | Tm::SourceBoundary(su::SourceBoundary(term)) => {
                tycker.source_guarded(TyckTask::Term(term, switch), |tycker| {
                    let inference = InferenceRegion::enter(tycker);
                    let checked = self.mk(term).tyck_inner_k(tycker, Action::switch(switch))?;
                    inference.close_k(tycker)?;
                    Ok(checked)
                })?
            }
            | Tm::SignatureBoundary(su::SignatureBoundary(term)) => {
                tycker.source_guarded(TyckTask::Term(term, switch), |tycker| {
                    let inference = InferenceRegion::enter(tycker);
                    let checked = self.mk(term).tyck_inner_k(tycker, Action::switch(switch))?;
                    let checked = match checked {
                        | TermAnnId::Type(_, _) => checked,
                        | TermAnnId::Hole(_)
                        | TermAnnId::Kind(_)
                        | TermAnnId::Value(_, _)
                        | TermAnnId::Compu(_, _) => tycker
                            .err_k(TyckError::SignatureNotType, std::panic::Location::caller())?,
                    };
                    inference.close_k(tycker)?;
                    Ok(checked)
                })?
            }
            | Tm::Internal(internal) => {
                InternalTerm(internal, self.inner).tyck_k(tycker, &self.info, switch)?
            }
            | Tm::Sealed(_) => unreachable!(),
            | Tm::Ann(term) => {
                let su::Ann { tm, ty } = term;
                // if the ty is a hole, we should stay in current switch
                match tycker.scoped.terms[&ty] {
                    | Tm::Hole(su::Hole) => {
                        let res = self.mk(tm).tyck_k(tycker, Action::switch(switch))?;
                        return Ok(res);
                    }
                    | _ => {}
                }
                let ty_out_ann = self.mk(ty).tyck_k(tycker, Action::syn())?;
                let ty_ann = match ty_out_ann {
                    | TermAnnId::Kind(kd) => kd.into(),
                    | TermAnnId::Type(ty, _kd) => ty.into(),
                    | TermAnnId::Hole(_) | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                };
                let ann = match switch {
                    | Switch::Syn => ty_ann,
                    | Switch::Ana(ty_ana) => Lub::lub_k(ty_ann, ty_ana, tycker)?,
                };

                self.mk(tm).tyck_k(tycker, Action::ana(ann))?
            }
            | Tm::Hole(term) => {
                let su::Hole = term;
                match switch {
                    | Switch::Syn => {
                        let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                        let Some(fill) = crate::query::term_hole_syn_judgment(
                            tycker.db,
                            tycker.data,
                            term,
                            tycker.site_occurrence(),
                        ) else {
                            unreachable!("hole judgments are query-produced")
                        };
                        tycker.statics.fills.insert_new(fill, ss::InferenceSite::Term(self.inner));
                        TermAnnId::Hole(fill)
                    }
                    | Switch::Ana(AnnId::Set) => {
                        // can't deduce kind for now
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | Switch::Ana(AnnId::Kind(kd)) => {
                        // a type hole, with a specific kind in mind
                        let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                        let input = crate::query::InternedHoleAna::new(
                            tycker.db,
                            crate::query::HoleAnaKind::Type { kd },
                        );
                        let Some(crate::query::HoleAnaOutcome::Type { fill, ty, kd }) =
                            crate::query::hole_ana_judgment(
                                tycker.db,
                                tycker.data,
                                term,
                                input,
                                tycker.site_occurrence(),
                            )
                        else {
                            unreachable!("the kind arm of hole judgments is query-produced")
                        };
                        tycker.statics.fills.insert_new(fill, ss::InferenceSite::Term(self.inner));
                        tycker.statics.types_pre.insert_new(ty, ss::Fillable::Fill(fill));
                        tycker.statics.annotations_type.insert_new(ty, kd);
                        tycker.statics.env_type.insert_new(ty, self.info.clone());
                        let scope = self.info.skolem_scope().clone();
                        if let Some(existing) =
                            tycker.statics.fill_scopes.insert_or_get(fill, scope.clone())
                        {
                            tycker
                                .statics
                                .fill_scopes
                                .replace_existing(fill, existing.intersection(&scope));
                        }
                        TermAnnId::Type(ty, kd)
                    }
                    | Switch::Ana(AnnId::Type(ty)) => {
                        // a hole in either value or computation; like undefined in Haskell
                        let kd = tycker.statics.annotations_type[&ty].to_owned();
                        match tycker.kind_filled_k(&kd)?.to_owned() {
                            | ss::Kind::VType(ss::VType) => {
                                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedHoleAna::new(
                                    tycker.db,
                                    crate::query::HoleAnaKind::Value { ty },
                                );
                                let Some(crate::query::HoleAnaOutcome::Value {
                                    fill,
                                    id,
                                    value,
                                    ann,
                                }) = crate::query::hole_ana_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                                else {
                                    unreachable!(
                                        "the value arm of hole judgments is query-produced"
                                    )
                                };
                                tycker
                                    .statics
                                    .fills
                                    .insert_new(fill, ss::InferenceSite::Term(self.inner));
                                fill.fill_k(tycker, ty.into())?;
                                tycker.statics.fill_hints.insert_new(fill, ());
                                tycker.statics.values.insert_new(id, value);
                                tycker.statics.annotations_value.insert_new(id, ann);
                                tycker.statics.env_value.insert_new(id, self.info.clone());
                                TermAnnId::Value(id, ann)
                            }
                            | ss::Kind::CType(ss::CType) => {
                                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedHoleAna::new(
                                    tycker.db,
                                    crate::query::HoleAnaKind::Compu { ty },
                                );
                                let Some(crate::query::HoleAnaOutcome::Compu {
                                    fill,
                                    id,
                                    compu,
                                    ann,
                                }) = crate::query::hole_ana_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                                else {
                                    unreachable!(
                                        "the computation arm of hole judgments is query-produced"
                                    )
                                };
                                tycker
                                    .statics
                                    .fills
                                    .insert_new(fill, ss::InferenceSite::Term(self.inner));
                                fill.fill_k(tycker, ty.into())?;
                                tycker.statics.fill_hints.insert_new(fill, ());
                                tycker.statics.compus.insert_new(id, compu);
                                tycker.statics.annotations_compu.insert_new(id, ann);
                                tycker.statics.env_compu.insert_new(id, self.info.clone());
                                TermAnnId::Compu(id, ann)
                            }
                            | ss::Kind::Arrow(_) | ss::Kind::Label(_) => tycker
                                .err_k(TyckError::SortMismatch, std::panic::Location::caller())?,
                        }
                    }
                }
            }
            | Tm::Var(def) => {
                let annotation =
                    tycker.statics.annotations_var.get(&def).copied().unwrap_or_else(|| {
                        panic!(
                            "resolved variable `{}` reached the checker before its binder",
                            tycker.scoped.defs[&def].plain()
                        )
                    });
                let ann = {
                    match switch {
                        | Switch::Syn => annotation,
                        | Switch::Ana(ana) => Lub::lub_k(annotation, ana, tycker)?,
                    }
                };
                match ann {
                    | AnnId::Set => {
                        let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                        let env_data = crate::query::EnvData::new(tycker.db, self.info.clone());
                        let annotation = crate::query::InternedAnn::new(tycker.db, ann);
                        let Some(crate::query::VarSynOutcome::Kind { id }) =
                            crate::query::var_syn_judgment(
                                tycker.db,
                                tycker.data,
                                env_data,
                                term,
                                annotation,
                                tycker.site_occurrence(),
                            )
                        else {
                            unreachable!("the set arm of variable judgments is query-produced")
                        };
                        TermAnnId::Kind(id)
                    }
                    | AnnId::Kind(kd) => match self.info.recursively_get_type(tycker, &def) {
                        | Some(&ann) => {
                            let AnnId::Type(ty) = ann else { unreachable!() };
                            TermAnnId::Type(ty, kd)
                        }
                        | None => {
                            let ty = Alloc::alloc(tycker, def, kd, &self.info);
                            TermAnnId::Type(ty, kd)
                        }
                    },
                    | AnnId::Type(_) => {
                        let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                        let env_data = crate::query::EnvData::new(tycker.db, self.info.clone());
                        let annotation = crate::query::InternedAnn::new(tycker.db, ann);
                        let Some(crate::query::VarSynOutcome::Value { id, value, ty }) =
                            crate::query::var_syn_judgment(
                                tycker.db,
                                tycker.data,
                                env_data,
                                term,
                                annotation,
                                tycker.site_occurrence(),
                            )
                        else {
                            unreachable!("the type arm of variable judgments is query-produced")
                        };
                        tycker.statics.values.insert_new(id, value);
                        tycker.statics.annotations_value.insert_new(id, ty);
                        tycker.statics.env_value.insert_new(id, self.info.clone());
                        TermAnnId::Value(id, ty)
                    }
                }
            }
            | Tm::Named(term) => {
                let su::Named(name, inner) = term;
                match switch {
                    | Switch::Syn => match self.mk(inner).tyck_k(tycker, Action::syn())? {
                        | inner_out @ (TermAnnId::Type(..)
                        | TermAnnId::Hole(_)
                        | TermAnnId::Kind(_)
                        | TermAnnId::Compu(_, _)) => {
                            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                            let inner_interned =
                                crate::query::InternedTermAnn::new(tycker.db, inner_out);
                            let Some(outcome) = crate::query::named_syn_judgment(
                                tycker.db,
                                tycker.data,
                                term,
                                inner_interned,
                                tycker.site_occurrence(),
                            ) else {
                                unreachable!(
                                    "the type and rejection arms of named judgments are query-produced"
                                )
                            };
                            match outcome {
                                | crate::query::NamedSynOutcome::Type {
                                    kind_id,
                                    kind,
                                    named_id,
                                    named,
                                } => {
                                    tycker
                                        .statics
                                        .kinds_pre
                                        .insert_new(kind_id, ss::Fillable::Done(kind));
                                    tycker
                                        .statics
                                        .types_pre
                                        .insert_new(named_id, ss::Fillable::Done(named));
                                    tycker.statics.annotations_type.insert_new(named_id, kind_id);
                                    tycker.statics.env_type.insert_new(named_id, self.info.clone());
                                    TermAnnId::Type(named_id, kind_id)
                                }
                                | crate::query::NamedSynOutcome::Error(error) => {
                                    tycker.err_k(error, std::panic::Location::caller())?
                                }
                            }
                        }
                        | TermAnnId::Value(inner, inner_ty) => {
                            let inner_kind = tycker.statics.annotations_type[&inner_ty];
                            let vtype = ss::VType.build(tycker, &self.info);
                            Lub::lub_k(vtype, inner_kind, tycker)?;
                            let named_ty = Alloc::alloc(
                                tycker,
                                ss::Label(name.clone(), inner_ty),
                                vtype,
                                &self.info,
                            );
                            let named =
                                Alloc::alloc(tycker, ss::Named(name, inner), named_ty, &self.info);
                            TermAnnId::Value(named, named_ty)
                        }
                    },
                    | Switch::Ana(AnnId::Kind(kd)) => {
                        let ss::Kind::Label(ss::Label(expected_name, inner_kind)) =
                            tycker.kind_filled_k(&kd)?.to_owned()
                        else {
                            tycker.err_k(TyckError::KindMismatch, std::panic::Location::caller())?
                        };
                        if name != expected_name {
                            tycker.err_k(
                                TyckError::NamedLabelMismatch {
                                    expected: expected_name,
                                    found: name.clone(),
                                },
                                std::panic::Location::caller(),
                            )?
                        }
                        let checked =
                            self.mk(inner).tyck_k(tycker, Action::ana(inner_kind.into()))?;
                        let (inner, _) = checked.try_as_type(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                        let named = Alloc::alloc(tycker, ss::Named(name, inner), kd, &self.info);
                        TermAnnId::Type(named, kd)
                    }
                    | Switch::Ana(AnnId::Type(expected)) => {
                        let expected_view =
                            expected.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                        let ss::Type::Label(ss::Label(expected_name, inner_ty)) =
                            tycker.type_filled_k(&expected_view)?.to_owned()
                        else {
                            tycker.err_k(
                                TyckError::TypeExpected {
                                    expected: "a named value type".to_string(),
                                    found: expected,
                                },
                                std::panic::Location::caller(),
                            )?
                        };
                        if name != expected_name {
                            tycker.err_k(
                                TyckError::NamedLabelMismatch {
                                    expected: expected_name,
                                    found: name.clone(),
                                },
                                std::panic::Location::caller(),
                            )?
                        }
                        let checked =
                            self.mk(inner).tyck_k(tycker, Action::ana(inner_ty.into()))?;
                        let (inner, _) = checked.try_as_value(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                        let named =
                            Alloc::alloc(tycker, ss::Named(name, inner), expected, &self.info);
                        TermAnnId::Value(named, expected)
                    }
                    | Switch::Ana(AnnId::Set) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                }
            }
            | Tm::Label(term) => {
                let su::Label(name, inner) = term;
                match switch {
                    | Switch::Syn => match self.mk(inner).tyck_k(tycker, Action::syn())? {
                        | inner_out @ (TermAnnId::Kind(_)
                        | TermAnnId::Hole(_)
                        | TermAnnId::Value(_, _)
                        | TermAnnId::Compu(_, _)) => {
                            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                            let inner_interned =
                                crate::query::InternedTermAnn::new(tycker.db, inner_out);
                            let Some(outcome) = crate::query::label_syn_judgment(
                                tycker.db,
                                tycker.data,
                                term,
                                inner_interned,
                                tycker.site_occurrence(),
                            ) else {
                                unreachable!(
                                    "the kind and rejection arms of label judgments are query-produced"
                                )
                            };
                            match outcome {
                                | crate::query::LabelSynOutcome::Kind { id, kind } => {
                                    tycker
                                        .statics
                                        .kinds_pre
                                        .insert_new(id, ss::Fillable::Done(kind));
                                    TermAnnId::Kind(id)
                                }
                                | crate::query::LabelSynOutcome::Error(error) => {
                                    tycker.err_k(error, std::panic::Location::caller())?
                                }
                            }
                        }
                        | TermAnnId::Type(inner, kind) => {
                            let vtype = ss::VType.build(tycker, &self.info);
                            Lub::lub_k(vtype, kind, tycker)?;
                            let label =
                                Alloc::alloc(tycker, ss::Label(name, inner), vtype, &self.info);
                            TermAnnId::Type(label, vtype)
                        }
                    },
                    | Switch::Ana(AnnId::Set) => {
                        let inner =
                            self.mk(inner).tyck_k(tycker, Action::ana(AnnId::Set))?.try_as_kind(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                        let label = Alloc::alloc(tycker, ss::Label(name, inner), (), &());
                        TermAnnId::Kind(label)
                    }
                    | Switch::Ana(AnnId::Kind(kind)) => {
                        let vtype = ss::VType.build(tycker, &self.info);
                        Lub::lub_k(vtype, kind, tycker)?;
                        let (inner, _) =
                            self.mk(inner).tyck_k(tycker, Action::ana(vtype.into()))?.try_as_type(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                        let label = Alloc::alloc(tycker, ss::Label(name, inner), vtype, &self.info);
                        TermAnnId::Type(label, vtype)
                    }
                    | Switch::Ana(AnnId::Type(_)) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                }
            }
            | Tm::Triv(su::Triv) => match switch {
                | Switch::Syn => {
                    let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                    let Some(outcome) = crate::query::triv_syn_judgment(
                        tycker.db,
                        tycker.data,
                        term,
                        tycker.site_occurrence(),
                    ) else {
                        unreachable!("trivial judgments are query-produced")
                    };
                    let crate::query::TrivSynOutcome { id, value, ty } = outcome;
                    tycker.statics.values.insert_new(id, value);
                    tycker.statics.annotations_value.insert_new(id, ty);
                    tycker.statics.env_value.insert_new(id, self.info.clone());
                    TermAnnId::Value(id, ty)
                }
                | Switch::Ana(AnnId::Type(ana)) => {
                    let unit = ss::UnitTy.build(tycker, &self.info);
                    let ann = Lub::lub_k(unit, ana, tycker)?;
                    let triv = Alloc::alloc(tycker, ss::Triv, ann, &self.info);
                    TermAnnId::Value(triv, ann)
                }
                | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            },
            | Tm::Cons(term) => {
                let su::ConsN(items, tail) = term;
                match switch {
                    | Switch::Syn => {
                        let (output, annotations): (Vec<_>, Vec<_>) = items
                            .into_iter()
                            .map(|item| -> ResultKont<_> {
                                match self.mk(item).tyck_k(tycker, Action::syn())? {
                                    | TermAnnId::Value(item, item_ty) => Ok((item, item_ty)),
                                    | TermAnnId::Type(_, _) => tycker.err_k(
                                        TyckError::MissingAnnotation,
                                        std::panic::Location::caller(),
                                    ),
                                    | TermAnnId::Hole(_)
                                    | TermAnnId::Kind(_)
                                    | TermAnnId::Compu(_, _) => tycker.err_k(
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    ),
                                }
                            })
                            .collect::<ResultKont<Vec<_>>>()?
                            .into_iter()
                            .unzip();
                        let checked = self.mk(tail).tyck_k(tycker, Action::syn())?;
                        match checked {
                            | TermAnnId::Value(_, _) => {}
                            | TermAnnId::Type(_, _) => tycker.err_k(
                                TyckError::MissingAnnotation,
                                std::panic::Location::caller(),
                            )?,
                            | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Compu(_, _) => {
                                tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?
                            }
                        };
                        let item_outcomes = output
                            .iter()
                            .zip(&annotations)
                            .map(|(value, ty)| TermAnnId::Value(*value, *ty))
                            .collect::<Vec<_>>();
                        let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                        let items_interned =
                            crate::query::InternedConsItems::new(tycker.db, item_outcomes);
                        let tail_interned = crate::query::InternedTermAnn::new(tycker.db, checked);
                        let Some(outcome) = crate::query::cons_syn_judgment(
                            tycker.db,
                            tycker.data,
                            term,
                            items_interned,
                            tail_interned,
                            tycker.site_occurrence(),
                        ) else {
                            unreachable!("consumed judgments are query-produced")
                        };
                        for (id, prod) in outcome.prods {
                            tycker.statics.types_pre.insert_new(id, ss::Fillable::Done(prod));
                            tycker.statics.annotations_type.insert_new(id, outcome.vtype);
                            tycker.statics.env_type.insert_new(id, self.info.clone());
                        }
                        tycker.statics.values.insert_new(outcome.cons_id, outcome.cons);
                        tycker.statics.annotations_value.insert_new(outcome.cons_id, outcome.ann);
                        tycker.statics.env_value.insert_new(outcome.cons_id, self.info.clone());
                        TermAnnId::Value(outcome.cons_id, outcome.ann)
                    }
                    | Switch::Ana(AnnId::Type(expected)) => {
                        let expected_view = expected;
                        match expected_view.reveal_or_refine_product_k(tycker, &self.info)? {
                            | ss::Type::Prod(_) => {
                                let mut expected_item = expected_view;
                                let (output, annotations): (Vec<_>, Vec<_>) = items
                                    .into_iter()
                                    .map(|item| -> ResultKont<_> {
                                        let ss::Prod(item_ty, next_ty) =
                                            expected_item.view_product_k(tycker, &self.info)?;
                                        expected_item = next_ty;
                                        let checked = self
                                            .mk(item)
                                            .tyck_k(tycker, Action::ana(item_ty.into()))?;
                                        checked.try_as_value(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )
                                    })
                                    .collect::<ResultKont<Vec<_>>>()?
                                    .into_iter()
                                    .unzip();

                                let checked = self
                                    .mk(tail)
                                    .tyck_k(tycker, Action::ana(expected_item.into()))?;
                                let (tail, ann) = checked.try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let vtype = ss::VType.build(tycker, &self.info);
                                let ann = annotations.into_iter().rev().fold(ann, |ann, head| {
                                    Alloc::alloc(tycker, ss::Prod(head, ann), vtype, &self.info)
                                });
                                let cons =
                                    Alloc::alloc(tycker, ss::ConsN(output, tail), ann, &self.info);
                                TermAnnId::Value(cons, ann)
                            }
                            | ss::Type::Exists(_) | ss::Type::ManifestKind(_) => {
                                let mut body_ty = expected;
                                let mut body_index = items.len();

                                let witnesses = items
                                    .iter()
                                    .copied()
                                    .enumerate()
                                    .map_while(|(index, item)| {
                                        (|| -> ResultKont<Option<ss::StaticTermId>> {
                                            let view = body_ty
                                                .unroll_k(tycker)?
                                                .subst_env_k(tycker, &self.info)?;
                                            match tycker.type_filled_k(&view)?.to_owned() {
                                                | ss::Type::ManifestKind(ss::ManifestKind {
                                                    definition,
                                                    body,
                                                    ..
                                                }) => {
                                                    let checked = self
                                                        .mk(item)
                                                        .tyck_k(tycker, Action::ana(AnnId::Set))?;
                                                    let witness = checked.try_as_kind(
                                                        tycker,
                                                        TyckError::SortMismatch,
                                                        std::panic::Location::caller(),
                                                    )?;
                                                    let witness =
                                                        Lub::lub_k(definition, witness, tycker)?;
                                                    body_ty = body;
                                                    Ok(Some(witness.into()))
                                                }
                                                | ss::Type::Exists(ss::Exists {
                                                    binder,
                                                    mode,
                                                    body: next_ty,
                                                }) => {
                                                    let domain_kind = binder.domain_kind(tycker);
                                                    let checked = self.mk(item).tyck_k(
                                                        tycker,
                                                        Action::ana(domain_kind.into()),
                                                    )?;
                                                    let (witness, _) = checked.try_as_type(
                                                        tycker,
                                                        TyckError::SortMismatch,
                                                        std::panic::Location::caller(),
                                                    )?;
                                                    let payload = binder
                                                        .pattern
                                                        .bind_argument_k(tycker, witness)?;
                                                    let payload = match mode {
                                                        | ss::ExistsMode::Abstract => payload,
                                                        | ss::ExistsMode::Manifest(definition) => {
                                                            Lub::lub_k(definition, payload, tycker)?
                                                        }
                                                    };
                                                    body_ty = next_ty.subst_abst_k(
                                                        tycker,
                                                        (binder.witness, payload),
                                                    )?;
                                                    Ok(Some(witness.into()))
                                                }
                                                | _ => {
                                                    body_index = index;
                                                    Ok(None)
                                                }
                                            }
                                        })()
                                        .transpose()
                                    })
                                    .collect::<ResultKont<Vec<_>>>()?;

                                if witnesses.is_empty() {
                                    tycker.err_k(
                                        TyckError::TypeExpected {
                                            expected: "an existential package".to_string(),
                                            found: expected,
                                        },
                                        std::panic::Location::caller(),
                                    )?
                                }

                                let body_items = &items[body_index..];
                                let body = if body_items.is_empty() {
                                    let checked = self
                                        .mk(tail)
                                        .tyck_k(tycker, Action::ana(body_ty.into()))?;
                                    checked
                                        .try_as_value(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?
                                        .0
                                } else {
                                    let body_view = body_ty;
                                    let _ = body_view.view_product_k(tycker, &self.info)?;

                                    let mut expected_item = body_view;
                                    let (output, annotations): (Vec<_>, Vec<_>) = body_items
                                        .iter()
                                        .copied()
                                        .map(|item| -> ResultKont<_> {
                                            let ss::Prod(item_ty, next_ty) =
                                                expected_item.view_product_k(tycker, &self.info)?;
                                            expected_item = next_ty;
                                            let checked = self
                                                .mk(item)
                                                .tyck_k(tycker, Action::ana(item_ty.into()))?;
                                            checked.try_as_value(
                                                tycker,
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )
                                        })
                                        .collect::<ResultKont<Vec<_>>>()?
                                        .into_iter()
                                        .unzip();

                                    let checked = self
                                        .mk(tail)
                                        .tyck_k(tycker, Action::ana(expected_item.into()))?;
                                    let (tail, ann) = checked.try_as_value(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    let vtype = ss::VType.build(tycker, &self.info);
                                    let ann =
                                        annotations.into_iter().rev().fold(ann, |ann, head| {
                                            Alloc::alloc(
                                                tycker,
                                                ss::Prod(head, ann),
                                                vtype,
                                                &self.info,
                                            )
                                        });
                                    Alloc::alloc(tycker, ss::ConsN(output, tail), ann, &self.info)
                                };
                                let cons = Alloc::alloc(
                                    tycker,
                                    ss::ConsN(witnesses, body),
                                    expected,
                                    &self.info,
                                );
                                TermAnnId::Value(cons, expected)
                            }
                            | _ => tycker.err_k(
                                TyckError::TypeExpected {
                                    expected: "one of `_ * _` or `exists _ . _`".to_string(),
                                    found: expected,
                                },
                                std::panic::Location::caller(),
                            )?,
                        }
                    }
                    | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                }
            }
            | Tm::Abs(term) => {
                let su::Abs(pat, body) = term;
                match switch {
                    | Switch::Syn => {
                        let pat_out_ann = self.mk(pat).tyck_k(tycker, PatternAction::syn())?;
                        match pat_out_ann.annotation {
                            | PatAnnId::Kind(_) => {
                                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedAbsSyn::new(
                                    tycker.db,
                                    crate::query::AbsSynArm::Expressivity,
                                );
                                let Some(crate::query::AbsSynOutcome::Error(error)) =
                                    crate::query::abs_syn_judgment(
                                        tycker.db,
                                        tycker.data,
                                        term,
                                        input,
                                        tycker.site_occurrence(),
                                    )
                                else {
                                    unreachable!(
                                        "the kind arm of abstraction judgments is query-produced"
                                    )
                                };
                                tycker.err_k(error, std::panic::Location::caller())?
                            }
                            | PatAnnId::Type(tpat, kd) => {
                                // could be either type-polymorphic function or type function
                                let abst = Alloc::alloc(tycker, tpat, (), &());
                                let subst_vec = {
                                    let mut subst_vec = Vec::new();
                                    if let (Some(def), kd) = tpat.try_destruct_def(tycker) {
                                        let ty_abst = Alloc::alloc(tycker, abst, kd, &self.info);
                                        subst_vec.push((def, ty_abst.into()));
                                    }
                                    subst_vec
                                };
                                let body_out_ann =
                                    self.mk_add(subst_vec, body).tyck_k(tycker, Action::syn())?;
                                match body_out_ann {
                                    | TermAnnId::Type(ty, body_kd) => {
                                        // a type function
                                        // recover abst in ty
                                        let ty = if let (Some(def), _kd) =
                                            tpat.try_destruct_def(tycker)
                                        {
                                            let def_ty = Alloc::alloc(tycker, def, kd, &self.info);
                                            ty.subst_abst_k(tycker, (abst, def_ty))?
                                        } else {
                                            ty
                                        };
                                        let term =
                                            crate::query::InternedTerm::new(tycker.db, self.inner);
                                        let input = crate::query::InternedAbsSyn::new(
                                            tycker.db,
                                            crate::query::AbsSynArm::TypeFunction {
                                                tpat,
                                                kd,
                                                body_kd,
                                                ty,
                                            },
                                        );
                                        let Some(crate::query::AbsSynOutcome::TypeFunction {
                                            arrow_id,
                                            arrow,
                                            abs_id,
                                            abs,
                                        }) = crate::query::abs_syn_judgment(
                                            tycker.db,
                                            tycker.data,
                                            term,
                                            input,
                                            tycker.site_occurrence(),
                                        )
                                        else {
                                            unreachable!(
                                                "the type function arm of abstraction judgments is query-produced"
                                            )
                                        };
                                        tycker
                                            .statics
                                            .kinds_pre
                                            .insert_new(arrow_id, ss::Fillable::Done(arrow));
                                        tycker
                                            .statics
                                            .types_pre
                                            .insert_new(abs_id, ss::Fillable::Done(abs));
                                        tycker
                                            .statics
                                            .annotations_type
                                            .insert_new(abs_id, arrow_id);
                                        tycker
                                            .statics
                                            .env_type
                                            .insert_new(abs_id, self.info.clone());
                                        TermAnnId::Type(abs_id, arrow_id)
                                    }
                                    | TermAnnId::Compu(compu, body_ty) => {
                                        // a type-polymorphic function
                                        let term =
                                            crate::query::InternedTerm::new(tycker.db, self.inner);
                                        let input = crate::query::InternedAbsSyn::new(
                                            tycker.db,
                                            crate::query::AbsSynArm::PolymorphicCompu {
                                                tpat,
                                                abst,
                                                compu,
                                                body_ty,
                                            },
                                        );
                                        let Some(crate::query::AbsSynOutcome::TAbsCompu {
                                            ann_id,
                                            ann,
                                            kd,
                                            abs_id,
                                            abs,
                                        }) = crate::query::abs_syn_judgment(
                                            tycker.db,
                                            tycker.data,
                                            term,
                                            input,
                                            tycker.site_occurrence(),
                                        )
                                        else {
                                            unreachable!(
                                                "the polymorphic computation arm of abstraction judgments is query-produced"
                                            )
                                        };
                                        tycker
                                            .statics
                                            .types_pre
                                            .insert_new(ann_id, ss::Fillable::Done(ann));
                                        tycker.statics.annotations_type.insert_new(ann_id, kd);
                                        tycker
                                            .statics
                                            .env_type
                                            .insert_new(ann_id, self.info.clone());
                                        tycker.statics.compus.insert_new(abs_id, abs);
                                        tycker.statics.annotations_compu.insert_new(abs_id, ann_id);
                                        tycker
                                            .statics
                                            .env_compu
                                            .insert_new(abs_id, self.info.clone());
                                        TermAnnId::Compu(abs_id, ann_id)
                                    }
                                    | TermAnnId::Value(value, body_ty) => {
                                        let term =
                                            crate::query::InternedTerm::new(tycker.db, self.inner);
                                        let input = crate::query::InternedAbsSyn::new(
                                            tycker.db,
                                            crate::query::AbsSynArm::PolymorphicValue {
                                                tpat,
                                                abst,
                                                value,
                                                body_ty,
                                            },
                                        );
                                        let Some(crate::query::AbsSynOutcome::TAbsValue {
                                            ann_id,
                                            ann,
                                            kd,
                                            abs_id,
                                            abs,
                                        }) = crate::query::abs_syn_judgment(
                                            tycker.db,
                                            tycker.data,
                                            term,
                                            input,
                                            tycker.site_occurrence(),
                                        )
                                        else {
                                            unreachable!(
                                                "the polymorphic value arm of abstraction judgments is query-produced"
                                            )
                                        };
                                        tycker
                                            .statics
                                            .types_pre
                                            .insert_new(ann_id, ss::Fillable::Done(ann));
                                        tycker.statics.annotations_type.insert_new(ann_id, kd);
                                        tycker
                                            .statics
                                            .env_type
                                            .insert_new(ann_id, self.info.clone());
                                        tycker.statics.values.insert_new(abs_id, abs);
                                        tycker.statics.annotations_value.insert_new(abs_id, ann_id);
                                        tycker
                                            .statics
                                            .env_value
                                            .insert_new(abs_id, self.info.clone());
                                        TermAnnId::Value(abs_id, ann_id)
                                    }
                                    | TermAnnId::Hole(_) | TermAnnId::Kind(_) => {
                                        let term =
                                            crate::query::InternedTerm::new(tycker.db, self.inner);
                                        let input = crate::query::InternedAbsSyn::new(
                                            tycker.db,
                                            crate::query::AbsSynArm::SortMismatch,
                                        );
                                        let Some(crate::query::AbsSynOutcome::Error(error)) =
                                            crate::query::abs_syn_judgment(
                                                tycker.db,
                                                tycker.data,
                                                term,
                                                input,
                                                tycker.site_occurrence(),
                                            )
                                        else {
                                            unreachable!(
                                                "the sort arm of abstraction judgments is query-produced"
                                            )
                                        };
                                        tycker.err_k(error, std::panic::Location::caller())?
                                    }
                                }
                            }
                            | PatAnnId::Value(vpat, ty) => {
                                // A value-pattern abstraction is pure when its body is a value
                                // and computational when its body is a computation.
                                let body_out_ann = TyEnvT::new(pat_out_ann.info.clone(), body)
                                    .tyck_k(tycker, Action::syn())?;
                                match body_out_ann {
                                    | TermAnnId::Value(value, body_ty) => {
                                        let arm = match pat_out_ann.package_telescope_k(tycker)? {
                                            | None => {
                                                pat_out_ann.close_scope_k(tycker, body_ty)?;
                                                crate::query::AbsSynArm::ValueArrow {
                                                    vpat,
                                                    ty,
                                                    value,
                                                    body_ty,
                                                }
                                            }
                                            | Some(witnesses) => {
                                                let pack_pi = ss::ValuePackPi {
                                                    domain: ty,
                                                    witnesses,
                                                    codomain: body_ty,
                                                };
                                                tycker.validate_builtin_signature_k(
                                                    &ss::PackPi {
                                                        domain: pack_pi.domain,
                                                        witnesses: pack_pi.witnesses.clone(),
                                                        codomain: pack_pi.codomain,
                                                    },
                                                )?;
                                                let mut iter = pack_pi.witnesses.iter();
                                                let first = *iter.next().expect("a package telescope opens at least one witness");
                                                let rest = iter.copied().collect::<Vec<_>>();
                                                crate::query::AbsSynArm::ValuePackPi {
                                                    vpat,
                                                    domain: ty,
                                                    first,
                                                    rest,
                                                    codomain: body_ty,
                                                    value,
                                                }
                                            }
                                        };
                                        let term =
                                            crate::query::InternedTerm::new(tycker.db, self.inner);
                                        let input =
                                            crate::query::InternedAbsSyn::new(tycker.db, arm);
                                        let Some(crate::query::AbsSynOutcome::VAbsValue {
                                            ann_id,
                                            ann,
                                            kd,
                                            abs_id,
                                            abs,
                                        }) = crate::query::abs_syn_judgment(
                                            tycker.db,
                                            tycker.data,
                                            term,
                                            input,
                                            tycker.site_occurrence(),
                                        )
                                        else {
                                            unreachable!(
                                                "the value arrow arm of abstraction judgments is query-produced"
                                            )
                                        };
                                        let is_pack_pi = matches!(
                                            &ann,
                                            ss::Type::VPackPi(_) | ss::Type::PackPi(_)
                                        );
                                        tycker
                                            .statics
                                            .types_pre
                                            .insert_new(ann_id, ss::Fillable::Done(ann));
                                        tycker.statics.annotations_type.insert_new(ann_id, kd);
                                        tycker
                                            .statics
                                            .env_type
                                            .insert_new(ann_id, self.info.clone());
                                        if is_pack_pi {
                                            ann_id.constrain_to_scope_k(
                                                tycker,
                                                self.info.skolem_scope(),
                                            )?;
                                        }
                                        tycker.statics.values.insert_new(abs_id, abs);
                                        tycker.statics.annotations_value.insert_new(abs_id, ann_id);
                                        tycker
                                            .statics
                                            .env_value
                                            .insert_new(abs_id, self.info.clone());
                                        TermAnnId::Value(abs_id, ann_id)
                                    }
                                    | TermAnnId::Compu(compu, body_ty) => {
                                        let arm = match pat_out_ann.package_telescope_k(tycker)? {
                                            | None => {
                                                pat_out_ann.close_scope_k(tycker, body_ty)?;
                                                crate::query::AbsSynArm::CompuArrow {
                                                    vpat,
                                                    ty,
                                                    compu,
                                                    body_ty,
                                                }
                                            }
                                            | Some(witnesses) => {
                                                let pack_pi = ss::PackPi {
                                                    domain: ty,
                                                    witnesses,
                                                    codomain: body_ty,
                                                };
                                                tycker.validate_builtin_signature_k(&pack_pi)?;
                                                let mut iter = pack_pi.witnesses.iter();
                                                let first = *iter.next().expect("a package telescope opens at least one witness");
                                                let rest = iter.copied().collect::<Vec<_>>();
                                                crate::query::AbsSynArm::CompuPackPi {
                                                    vpat,
                                                    domain: ty,
                                                    first,
                                                    rest,
                                                    codomain: body_ty,
                                                    compu,
                                                }
                                            }
                                        };
                                        let term =
                                            crate::query::InternedTerm::new(tycker.db, self.inner);
                                        let input =
                                            crate::query::InternedAbsSyn::new(tycker.db, arm);
                                        let Some(crate::query::AbsSynOutcome::VAbsCompu {
                                            ann_id,
                                            ann,
                                            kd,
                                            abs_id,
                                            abs,
                                        }) = crate::query::abs_syn_judgment(
                                            tycker.db,
                                            tycker.data,
                                            term,
                                            input,
                                            tycker.site_occurrence(),
                                        )
                                        else {
                                            unreachable!(
                                                "the computation arrow arm of abstraction judgments is query-produced"
                                            )
                                        };
                                        let is_pack_pi = matches!(
                                            &ann,
                                            ss::Type::VPackPi(_) | ss::Type::PackPi(_)
                                        );
                                        tycker
                                            .statics
                                            .types_pre
                                            .insert_new(ann_id, ss::Fillable::Done(ann));
                                        tycker.statics.annotations_type.insert_new(ann_id, kd);
                                        tycker
                                            .statics
                                            .env_type
                                            .insert_new(ann_id, self.info.clone());
                                        if is_pack_pi {
                                            ann_id.constrain_to_scope_k(
                                                tycker,
                                                self.info.skolem_scope(),
                                            )?;
                                        }
                                        tycker.statics.compus.insert_new(abs_id, abs);
                                        tycker.statics.annotations_compu.insert_new(abs_id, ann_id);
                                        tycker
                                            .statics
                                            .env_compu
                                            .insert_new(abs_id, self.info.clone());
                                        TermAnnId::Compu(abs_id, ann_id)
                                    }
                                    | TermAnnId::Hole(_)
                                    | TermAnnId::Kind(_)
                                    | TermAnnId::Type(_, _) => {
                                        let term =
                                            crate::query::InternedTerm::new(tycker.db, self.inner);
                                        let input = crate::query::InternedAbsSyn::new(
                                            tycker.db,
                                            crate::query::AbsSynArm::SortMismatch,
                                        );
                                        let Some(crate::query::AbsSynOutcome::Error(error)) =
                                            crate::query::abs_syn_judgment(
                                                tycker.db,
                                                tycker.data,
                                                term,
                                                input,
                                                tycker.site_occurrence(),
                                            )
                                        else {
                                            unreachable!(
                                                "the sort arm of abstraction judgments is query-produced"
                                            )
                                        };
                                        tycker.err_k(error, std::panic::Location::caller())?
                                    }
                                }
                            }
                        }
                    }
                    | Switch::Ana(ana) => {
                        match ana {
                            | AnnId::Set => tycker
                                .err_k(TyckError::SortMismatch, std::panic::Location::caller())?,
                            | AnnId::Kind(kd) => {
                                // type function in f omega
                                // expecting a kind arrow
                                let ss::Kind::Arrow(kd_arr) = tycker.kind_filled_k(&kd)?.to_owned()
                                else {
                                    tycker.err_k(
                                        TyckError::KindMismatch,
                                        std::panic::Location::caller(),
                                    )?
                                };
                                let ss::Arrow(kd_1, kd_2) = kd_arr;
                                let binder =
                                    self.mk(pat).tyck_k(tycker, PatternAction::ana(kd_1.into()))?;
                                let (binder, binder_kd) = binder.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let body_out_ann =
                                    self.mk(body).tyck_k(tycker, Action::ana(kd_2.into()))?;
                                let (body_out, body_kd) = body_out_ann.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let ann =
                                    Alloc::alloc(tycker, ss::Arrow(binder_kd, body_kd), (), &());
                                let abs = Alloc::alloc(
                                    tycker,
                                    ss::Abs(binder, body_out),
                                    ann,
                                    &self.info,
                                );
                                TermAnnId::Type(abs, ann)
                            }
                            | AnnId::Type(ty) => {
                                // could be either a term function or a type-polymorphic term function
                                let kind = tycker.statics.annotations_type[&ty];
                                let expected = match tycker.kind_filled_k(&kind)?.to_owned() {
                                    | ss::Kind::VType(_) => ty
                                        .normalize_k(tycker, kind)?
                                        .reveal_or_refine_value_arrow_k(tycker, &self.info)?,
                                    | ss::Kind::CType(_) => tycker.type_filled_k(&ty)?.to_owned(),
                                    | ss::Kind::Arrow(_) | ss::Kind::Label(_) => tycker.err_k(
                                        TyckError::KindMismatch,
                                        std::panic::Location::caller(),
                                    )?,
                                };
                                match expected {
                                    | ss::Type::VArrow(ss::ValueArrow(ty_1, ty_2)) => {
                                        let binder_elaboration = self
                                            .mk(pat)
                                            .tyck_k(tycker, PatternAction::ana(ty_1.into()))?;
                                        let (binder, binder_ty) = binder_elaboration.try_as_value(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        let body_out_ann =
                                            TyEnvT::new(binder_elaboration.info.clone(), body)
                                                .tyck_k(tycker, Action::ana(ty_2.into()))?;
                                        let (body_out, body_ty) = body_out_ann.try_as_value(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        binder_elaboration.close_scope_k(tycker, body_ty)?;
                                        let vtype = ss::VType.build(tycker, &self.info);
                                        let ann = Alloc::alloc(
                                            tycker,
                                            ss::ValueArrow(binder_ty, body_ty),
                                            vtype,
                                            &self.info,
                                        );
                                        let abs: ss::ValueId = Alloc::alloc(
                                            tycker,
                                            ss::Abs(binder, body_out),
                                            ann,
                                            &self.info,
                                        );
                                        TermAnnId::Value(abs, ann)
                                    }
                                    | ss::Type::VPackPi(signature) => self
                                        .mk(ValuePackPiIntroduction {
                                            binder: pat,
                                            body,
                                            signature,
                                        })
                                        .tyck_k(tycker, ())?,
                                    | ss::Type::VForall(ty) => {
                                        let ss::ValueForall(source_binder, ty_body) = ty;
                                        let domain_kind = source_binder.domain_kind(tycker);
                                        let binder = self.mk(pat).tyck_k(
                                            tycker,
                                            PatternAction::ana(domain_kind.into()),
                                        )?;
                                        let (binder, _binder_kd) = binder.try_as_type(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        let payload_kind = source_binder.payload_kind(tycker);
                                        let abst_ty = Alloc::alloc(
                                            tycker,
                                            source_binder.witness,
                                            payload_kind,
                                            &self.info,
                                        );
                                        let full_argument = source_binder
                                            .pattern
                                            .introduce_payload(tycker, abst_ty);
                                        let full_argument = tycker.err_p_to_k(full_argument)?;
                                        let env = self
                                            .mk(Assign(binder, full_argument))
                                            .tyck_k(tycker, ())?
                                            .info;
                                        let body_out_ann = TyEnvT { info: env, inner: body }
                                            .tyck_k(tycker, Action::ana(ty_body.into()))?;
                                        let (body_out, body_ty) = body_out_ann.try_as_value(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        let vtype = ss::VType.build(tycker, &self.info);
                                        let ann = Alloc::alloc(
                                            tycker,
                                            ss::ValueForall(source_binder, body_ty),
                                            vtype,
                                            &self.info,
                                        );
                                        let abs: ss::ValueId = Alloc::alloc(
                                            tycker,
                                            ss::Abs(binder, body_out),
                                            ann,
                                            &self.info,
                                        );
                                        TermAnnId::Value(abs, ann)
                                    }
                                    | ss::Type::Arrow(ty) => {
                                        // a term-term function
                                        let ss::Arrow(ty_1, ty_2) = ty;
                                        let binder_elaboration = self
                                            .mk(pat)
                                            .tyck_k(tycker, PatternAction::ana(ty_1.into()))?;
                                        let (binder, binder_ty) = binder_elaboration.try_as_value(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        let body_out_ann =
                                            TyEnvT::new(binder_elaboration.info.clone(), body)
                                                .tyck_k(tycker, Action::ana(ty_2.into()))?;
                                        let (body_out, body_ty) = body_out_ann.try_as_compu(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        binder_elaboration.close_scope_k(tycker, body_ty)?;
                                        let ctype = ss::CType.build(tycker, &self.info);
                                        let ann = Alloc::alloc(
                                            tycker,
                                            ss::Arrow(binder_ty, body_ty),
                                            ctype,
                                            &self.info,
                                        );
                                        let abs = Alloc::alloc(
                                            tycker,
                                            ss::Abs(binder, body_out),
                                            ann,
                                            &self.info,
                                        );
                                        TermAnnId::Compu(abs, ann)
                                    }
                                    | ss::Type::PackPi(signature) => self
                                        .mk(PackPiIntroduction { binder: pat, body, signature })
                                        .tyck_k(tycker, ())?,
                                    | ss::Type::Forall(ty) => {
                                        let ss::Forall(source_binder, ty_body) = ty;
                                        let domain_kind = source_binder.domain_kind(tycker);
                                        let binder = self.mk(pat).tyck_k(
                                            tycker,
                                            PatternAction::ana(domain_kind.into()),
                                        )?;
                                        let (binder, _binder_kd) = binder.try_as_type(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        let payload_kind = source_binder.payload_kind(tycker);
                                        let abst_ty = Alloc::alloc(
                                            tycker,
                                            source_binder.witness,
                                            payload_kind,
                                            &self.info,
                                        );
                                        let full_argument = source_binder
                                            .pattern
                                            .introduce_payload(tycker, abst_ty);
                                        let full_argument = tycker.err_p_to_k(full_argument)?;
                                        let env = self
                                            .mk(Assign(binder, full_argument))
                                            .tyck_k(tycker, ())?
                                            .info;
                                        let body_out_ann = TyEnvT { info: env, inner: body }
                                            .tyck_k(tycker, Action::ana(ty_body.into()))?;
                                        // throwing _body_ty away because it has been substituted
                                        // Todo: reuse _body_ty by substituting abst back
                                        let (body_out, body_ty) = body_out_ann.try_as_compu(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        let ctype = ss::CType.build(tycker, &self.info);
                                        let ann = Alloc::alloc(
                                            tycker,
                                            ss::Forall(source_binder, body_ty),
                                            ctype,
                                            &self.info,
                                        );
                                        let abs = Alloc::alloc(
                                            tycker,
                                            ss::Abs(binder, body_out),
                                            ann,
                                            &self.info,
                                        );
                                        TermAnnId::Compu(abs, ann)
                                    }
                                    | _ => tycker.err_k(
                                        TyckError::TypeExpected {
                                            expected: "one of `_ -> _`, a package-dependent \
                                                       arrow, or `forall _ . _`"
                                                .to_string(),
                                            found: ty,
                                        },
                                        std::panic::Location::caller(),
                                    )?,
                                }
                            }
                        }
                    }
                }
            }
            | Tm::App(term) => {
                let su::App(f, a) = term;
                let f_out_ann = self.mk(f).tyck_k(tycker, Action::syn())?;
                match f_out_ann {
                    | TermAnnId::Hole(_) | TermAnnId::Kind(_) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | TermAnnId::Type(f_ty, f_kd) => {
                        // type application in f omega
                        // f_kd should be a kind arrow
                        let ss::Kind::Arrow(kd_arr) = tycker.kind_filled_k(&f_kd)?.to_owned()
                        else {
                            tycker.err_k(TyckError::KindMismatch, std::panic::Location::caller())?
                        };
                        let ss::Arrow(a_kd, kd_out) = kd_arr;
                        let a_out_ann = self.mk(a).tyck_k(tycker, Action::ana(a_kd.into()))?;
                        let (a_ty, _a_kd) = a_out_ann.try_as_type(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                        // check kd_out is the same as the analyzed kind
                        let kd_out = {
                            match switch {
                                | Switch::Syn => kd_out,
                                | Switch::Ana(ana) => match ana {
                                    | AnnId::Kind(kd_ana) => Lub::lub_k(kd_out, kd_ana, tycker)?,
                                    | AnnId::Set | AnnId::Type(_) => tycker.err_k(
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?,
                                },
                            }
                        };
                        // normalize the application
                        let body_ty_norm = f_ty.normalize_app_k(tycker, a_ty, kd_out)?;
                        TermAnnId::Type(body_ty_norm, kd_out)
                    }
                    | TermAnnId::Value(f_out, f_ty) => {
                        let f_kd = tycker.statics.annotations_type[&f_ty];
                        let f_ty = f_ty.normalize_k(tycker, f_kd)?;
                        match f_ty.reveal_or_refine_value_arrow_k(tycker, &self.info)? {
                            | ss::Type::VForall(ss::ValueForall(binder, ty_body)) => {
                                let domain_kind = binder.domain_kind(tycker);
                                let a_out_ann =
                                    self.mk(a).tyck_k(tycker, Action::ana(domain_kind.into()))?;
                                let (a_ty, _a_kd) = a_out_ann.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let payload = binder.pattern.bind_argument_k(tycker, a_ty)?;
                                let body_ty =
                                    ty_body.subst_abst_k(tycker, (binder.witness, payload))?;
                                let ty_out = match switch {
                                    | Switch::Syn => body_ty,
                                    | Switch::Ana(AnnId::Type(expected)) => {
                                        Lub::lub_k(body_ty, expected, tycker)?
                                    }
                                    | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => tycker.err_k(
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?,
                                };
                                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedAppInput::new(
                                    tycker.db,
                                    crate::query::AppKind::ValueType {
                                        function: f_out,
                                        argument: a_ty,
                                    },
                                    ty_out,
                                    ty_out,
                                );
                                let Some(crate::query::AppSynOutcome::Value {
                                    id,
                                    value,
                                    ann,
                                    reported,
                                }) = crate::query::app_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                                else {
                                    unreachable!("value-type applications are query-produced")
                                };
                                tycker.statics.values.insert_new(id, value);
                                tycker.statics.annotations_value.insert_new(id, ann);
                                tycker.statics.env_value.insert_new(id, self.info.clone());
                                TermAnnId::Value(id, reported)
                            }
                            | ss::Type::VPackPi(signature) => self
                                .mk(ValuePackPiElimination {
                                    function: f_out,
                                    argument: a,
                                    signature,
                                })
                                .tyck_k(tycker, Action::switch(switch))?,
                            | ss::Type::VArrow(ss::ValueArrow(ty_arg, ty_out)) => {
                                let a_out_ann =
                                    self.mk(a).tyck_k(tycker, Action::ana(ty_arg.into()))?;
                                let (a_out, _a_ty) = a_out_ann.try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let ty_out = match switch {
                                    | Switch::Syn => ty_out,
                                    | Switch::Ana(AnnId::Type(ty_ana)) => {
                                        Lub::lub_k(ty_out, ty_ana, tycker)?
                                    }
                                    | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => tycker.err_k(
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?,
                                };
                                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedAppInput::new(
                                    tycker.db,
                                    crate::query::AppKind::ValueValue {
                                        function: f_out,
                                        argument: a_out,
                                    },
                                    ty_out,
                                    ty_out,
                                );
                                let Some(crate::query::AppSynOutcome::Value {
                                    id,
                                    value,
                                    ann,
                                    reported,
                                }) = crate::query::app_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                                else {
                                    unreachable!("value applications are query-produced")
                                };
                                tycker.statics.values.insert_new(id, value);
                                tycker.statics.annotations_value.insert_new(id, ann);
                                tycker.statics.env_value.insert_new(id, self.info.clone());
                                TermAnnId::Value(id, reported)
                            }
                            | _ => tycker.err_k(
                                TyckError::TypeExpected {
                                    expected: "a pure value arrow, package-dependent arrow, or \
                                               universal"
                                        .to_string(),
                                    found: f_ty,
                                },
                                std::panic::Location::caller(),
                            )?,
                        }
                    }
                    | TermAnnId::Compu(f_out, f_ty) => {
                        let f_kd = tycker.statics.annotations_type[&f_ty].to_owned();
                        let f_ty = f_ty.normalize_k(tycker, f_kd)?;
                        // either a term-term application or a type-polymorphic term application
                        match f_ty.reveal_or_refine_arrow_k(tycker, &self.info)? {
                            | ss::Type::Arrow(ty) => {
                                // a term-term application
                                let ss::Arrow(ty_arg, ty_out) = ty;
                                let a_out_ann =
                                    self.mk(a).tyck_k(tycker, Action::ana(ty_arg.into()))?;
                                let (a_out, _a_ty) = a_out_ann.try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                // check ty_out is the same as the analyzed type
                                let ty_out = {
                                    match switch {
                                        | Switch::Syn => ty_out,
                                        | Switch::Ana(ana) => match ana {
                                            | AnnId::Type(ty_ana) => {
                                                Lub::lub_k(ty_out, ty_ana, tycker)?
                                            }
                                            | AnnId::Set | AnnId::Kind(_) => tycker.err_k(
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?,
                                        },
                                    }
                                };
                                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedAppInput::new(
                                    tycker.db,
                                    crate::query::AppKind::CompuValue {
                                        function: f_out,
                                        argument: a_out,
                                    },
                                    ty_out,
                                    ty_out,
                                );
                                let Some(crate::query::AppSynOutcome::Compu {
                                    id,
                                    compu,
                                    ann,
                                    reported,
                                }) = crate::query::app_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                                else {
                                    unreachable!("computation applications are query-produced")
                                };
                                tycker.statics.compus.insert_new(id, compu);
                                tycker.statics.annotations_compu.insert_new(id, ann);
                                tycker.statics.env_compu.insert_new(id, self.info.clone());
                                TermAnnId::Compu(id, reported)
                            }
                            | ss::Type::Forall(ty) => {
                                // a type-polymorphic term application
                                let ss::Forall(binder, ty_body) = ty;
                                let domain_kind = binder.domain_kind(tycker);
                                let a_out_ann =
                                    self.mk(a).tyck_k(tycker, Action::ana(domain_kind.into()))?;
                                let (a_ty, _a_kd) = a_out_ann.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let payload = binder.pattern.bind_argument_k(tycker, a_ty)?;
                                let body_ty_subst =
                                    ty_body.subst_abst_k(tycker, (binder.witness, payload))?;
                                let ty_out = {
                                    match switch {
                                        | Switch::Syn => body_ty_subst,
                                        | Switch::Ana(ana) => match ana {
                                            | AnnId::Type(ty_ana) => {
                                                Lub::lub_k(body_ty_subst, ty_ana, tycker)?
                                            }
                                            | AnnId::Set | AnnId::Kind(_) => tycker.err_k(
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?,
                                        },
                                    }
                                };
                                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedAppInput::new(
                                    tycker.db,
                                    crate::query::AppKind::CompuType {
                                        function: f_out,
                                        argument: a_ty,
                                    },
                                    ty_out,
                                    body_ty_subst,
                                );
                                let Some(crate::query::AppSynOutcome::Compu {
                                    id,
                                    compu,
                                    ann,
                                    reported,
                                }) = crate::query::app_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                                else {
                                    unreachable!(
                                        "polymorphic computation applications are query-produced"
                                    )
                                };
                                tycker.statics.compus.insert_new(id, compu);
                                tycker.statics.annotations_compu.insert_new(id, ann);
                                tycker.statics.env_compu.insert_new(id, self.info.clone());
                                TermAnnId::Compu(id, reported)
                            }
                            | ss::Type::PackPi(signature) => self
                                .mk(PackPiElimination { function: f_out, argument: a, signature })
                                .tyck_k(tycker, Action::switch(switch))?,
                            | _ => tycker.err_k(
                                TyckError::TypeExpected {
                                    expected: "one of `_ -> _`, a package-dependent arrow, or \
                                               `forall _ . _`"
                                        .to_string(),
                                    found: f_ty,
                                },
                                std::panic::Location::caller(),
                            )?,
                        }
                    }
                }
            }
            | Tm::Fix(term) => {
                let su::Fix(pat, body) = term;
                let binder_elaboration = {
                    let switch = {
                        match switch {
                            | Switch::Ana(AnnId::Type(ty)) => {
                                let thunk_app_ty: ss::TypeId =
                                    cs::Thk(ty).build(tycker, &self.info);
                                Switch::Ana(thunk_app_ty.into())
                            }
                            | _ => switch,
                        }
                    };
                    self.mk(pat).tyck_k(tycker, PatternAction::switch(switch))?
                };
                let (binder, binder_ty) = binder_elaboration.try_as_value(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let (binder, binder_ty) = {
                    let ss::Type::App(ret_app_body_ty) = tycker.type_filled_k(&binder_ty)? else {
                        unreachable!()
                    };
                    let ss::App(_ret_ty, body_ty) = ret_app_body_ty;
                    (binder, body_ty)
                };
                let body_out_ann = TyEnvT::new(binder_elaboration.info.clone(), body)
                    .tyck_k(tycker, Action::ana(binder_ty.into()))?;
                let (body_out, fix_ty) = body_out_ann.try_as_compu(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                binder_elaboration.close_scope_k(tycker, fix_ty)?;
                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                let input =
                    crate::query::InternedFixInput::new(tycker.db, binder, body_out, fix_ty);
                let Some(outcome) = crate::query::fix_judgment(
                    tycker.db,
                    tycker.data,
                    term,
                    input,
                    tycker.site_occurrence(),
                ) else {
                    unreachable!("fixpoint judgments are query-produced")
                };
                tycker.statics.compus.insert_new(outcome.id, outcome.compu);
                tycker.statics.annotations_compu.insert_new(outcome.id, outcome.ann);
                tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
                TermAnnId::Compu(outcome.id, outcome.ann)
            }
            | Tm::Pi(term) => {
                let su::Pi(binder, body) = term;
                match switch {
                    | Switch::Syn => {
                        let binder_out_ann =
                            self.mk(binder).tyck_k(tycker, PatternAction::syn())?;
                        match binder_out_ann.annotation {
                            | PatAnnId::Kind(_) => tycker.err_k(
                                TyckError::Expressivity("kind quantification is not supported"),
                                std::panic::Location::caller(),
                            )?,
                            | PatAnnId::Type(tpat, kd_1) => {
                                let abst = Alloc::alloc(tycker, tpat, (), &());
                                let subst_vec = {
                                    let mut subst_vec = Vec::new();
                                    if let (Some(def), kd) = tpat.try_destruct_def(tycker) {
                                        let ty_abst = Alloc::alloc(tycker, abst, kd, &self.info);
                                        subst_vec.push((def, ty_abst.into()));
                                    }
                                    subst_vec
                                };
                                let body =
                                    self.mk_add(subst_vec, body).tyck_k(tycker, Action::syn())?;
                                match body {
                                    | TermAnnId::Kind(kd_2) => {
                                        // kind arrow; no tpat should be used
                                        if tpat.syntactically_used(tycker) {
                                            tycker.err_k(
                                                TyckError::Expressivity(
                                                    "dependent kinds are not supported yet",
                                                ),
                                                std::panic::Location::caller(),
                                            )?
                                        }
                                        let term =
                                            crate::query::InternedTerm::new(tycker.db, self.inner);
                                        let input = crate::query::InternedPiSyn::new(
                                            tycker.db,
                                            crate::query::PiSynArm::KindArrow { kd_1, kd_2 },
                                            tpat,
                                            abst,
                                        );
                                        let Some(crate::query::PiSynOutcome::Kind { id, kind }) =
                                            crate::query::pi_syn_judgment(
                                                tycker.db,
                                                tycker.data,
                                                term,
                                                input,
                                                tycker.site_occurrence(),
                                            )
                                        else {
                                            unreachable!(
                                                "the kind arrow of pi judgments is query-produced"
                                            )
                                        };
                                        tycker
                                            .statics
                                            .kinds_pre
                                            .insert_new(id, ss::Fillable::Done(kind));
                                        TermAnnId::Kind(id)
                                    }
                                    | TermAnnId::Type(ty_2, kd_2) => {
                                        let arm = match tycker.kind_filled_k(&kd_2)?.to_owned() {
                                            | ss::Kind::VType(_) => {
                                                crate::query::PiSynArm::ValueForall { ty_2, kd_2 }
                                            }
                                            | ss::Kind::CType(_) => {
                                                crate::query::PiSynArm::Forall { ty_2, kd_2 }
                                            }
                                            | ss::Kind::Arrow(_) | ss::Kind::Label(_) => {
                                                crate::query::PiSynArm::KindMismatch
                                            }
                                        };
                                        let term =
                                            crate::query::InternedTerm::new(tycker.db, self.inner);
                                        let input = crate::query::InternedPiSyn::new(
                                            tycker.db, arm, tpat, abst,
                                        );
                                        match crate::query::pi_syn_judgment(
                                            tycker.db,
                                            tycker.data,
                                            term,
                                            input,
                                            tycker.site_occurrence(),
                                        ) {
                                            | Some(crate::query::PiSynOutcome::Type {
                                                id,
                                                ty,
                                                kd,
                                            }) => {
                                                tycker
                                                    .statics
                                                    .types_pre
                                                    .insert_new(id, ss::Fillable::Done(ty));
                                                tycker.statics.annotations_type.insert_new(id, kd);
                                                tycker
                                                    .statics
                                                    .env_type
                                                    .insert_new(id, self.info.clone());
                                                TermAnnId::Type(id, kd)
                                            }
                                            | Some(crate::query::PiSynOutcome::Error(error)) => {
                                                tycker
                                                    .err_k(error, std::panic::Location::caller())?
                                            }
                                            | _ => unreachable!(
                                                "the type arm of pi judgments is query-produced"
                                            ),
                                        }
                                    }
                                    | TermAnnId::Hole(_) => {
                                        let term =
                                            crate::query::InternedTerm::new(tycker.db, self.inner);
                                        let input = crate::query::InternedPiSyn::new(
                                            tycker.db,
                                            crate::query::PiSynArm::MissingAnnotation,
                                            tpat,
                                            abst,
                                        );
                                        let Some(crate::query::PiSynOutcome::Error(error)) =
                                            crate::query::pi_syn_judgment(
                                                tycker.db,
                                                tycker.data,
                                                term,
                                                input,
                                                tycker.site_occurrence(),
                                            )
                                        else {
                                            unreachable!(
                                                "the hole arm of pi judgments is query-produced"
                                            )
                                        };
                                        tycker.err_k(error, std::panic::Location::caller())?
                                    }
                                    | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                                        let term =
                                            crate::query::InternedTerm::new(tycker.db, self.inner);
                                        let input = crate::query::InternedPiSyn::new(
                                            tycker.db,
                                            crate::query::PiSynArm::SortMismatch,
                                            tpat,
                                            abst,
                                        );
                                        let Some(crate::query::PiSynOutcome::Error(error)) =
                                            crate::query::pi_syn_judgment(
                                                tycker.db,
                                                tycker.data,
                                                term,
                                                input,
                                                tycker.site_occurrence(),
                                            )
                                        else {
                                            unreachable!(
                                                "the sort arm of pi judgments is query-produced"
                                            )
                                        };
                                        tycker.err_k(error, std::panic::Location::caller())?
                                    }
                                }
                            }
                            | PatAnnId::Value(_, _) => self
                                .mk(ValuePiFormation { binder: binder_out_ann, codomain: body })
                                .tyck_k(tycker, Action::syn())?,
                        }
                    }
                    | Switch::Ana(ana) => match ana {
                        | AnnId::Set => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                        | AnnId::Kind(kd) => {
                            match tycker.kind_filled_k(&kd)?.to_owned() {
                                | ss::Kind::VType(_) => {
                                    let binder_out_ann =
                                        self.mk(binder).tyck_k(tycker, PatternAction::syn())?;
                                    match binder_out_ann.annotation {
                                        | PatAnnId::Value(_, _) => {
                                            let vtype = ss::VType.build(tycker, &self.info);
                                            self.mk(ValuePiFormation {
                                                binder: binder_out_ann,
                                                codomain: body,
                                            })
                                            .tyck_k(tycker, Action::ana(vtype.into()))?
                                        }
                                        | PatAnnId::Type(tpat, _kd_1) => {
                                            let vtype = ss::VType.build(tycker, &self.info);
                                            let abst = Alloc::alloc(tycker, tpat, (), &());
                                            let subst_vec = {
                                                let mut subst_vec = Vec::new();
                                                if let (Some(def), kd) =
                                                    tpat.try_destruct_def(tycker)
                                                {
                                                    let ty_abst =
                                                        Alloc::alloc(tycker, abst, kd, &self.info);
                                                    subst_vec.push((def, ty_abst.into()));
                                                }
                                                subst_vec
                                            };
                                            let ty_2 = self
                                                .mk_add(subst_vec, body)
                                                .tyck_k(tycker, Action::ana(vtype.into()))?;
                                            let (ty_2, _vtype) = ty_2.try_as_type(
                                                tycker,
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?;
                                            let binder =
                                                ss::TypeBinder { pattern: tpat, witness: abst };
                                            let forall = Alloc::alloc(
                                                tycker,
                                                ss::ValueForall(binder, ty_2),
                                                vtype,
                                                &self.info,
                                            );
                                            TermAnnId::Type(forall, vtype)
                                        }
                                        | PatAnnId::Kind(_) => tycker.err_k(
                                            TyckError::Expressivity(
                                                "kind quantification is not supported",
                                            ),
                                            std::panic::Location::caller(),
                                        )?,
                                    }
                                }
                                | ss::Kind::CType(ss::CType) => {
                                    // could be forall or type arrow
                                    // synthesize the binder
                                    let binder_out_ann =
                                        self.mk(binder).tyck_k(tycker, PatternAction::syn())?;
                                    match binder_out_ann.annotation {
                                        | PatAnnId::Kind(_) => tycker.err_k(
                                            TyckError::Expressivity(
                                                "kind quantification is not supported",
                                            ),
                                            std::panic::Location::caller(),
                                        )?,
                                        | PatAnnId::Type(tpat, _kd_1) => {
                                            // forall
                                            let ctype = ss::CType.build(tycker, &self.info);
                                            let abst = Alloc::alloc(tycker, tpat, (), &());
                                            let subst_vec = {
                                                let mut subst_vec = Vec::new();
                                                if let (Some(def), kd) =
                                                    tpat.try_destruct_def(tycker)
                                                {
                                                    let ty_abst =
                                                        Alloc::alloc(tycker, abst, kd, &self.info);
                                                    subst_vec.push((def, ty_abst.into()));
                                                }
                                                subst_vec
                                            };
                                            let ty_2 = self
                                                .mk_add(subst_vec, body)
                                                .tyck_k(tycker, Action::ana(ctype.into()))?;
                                            let (ty_2, _ctype) = ty_2.try_as_type(
                                                tycker,
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?;
                                            let binder =
                                                ss::TypeBinder { pattern: tpat, witness: abst };
                                            let forall = Alloc::alloc(
                                                tycker,
                                                ss::Forall(binder, ty_2),
                                                ctype,
                                                &self.info,
                                            );
                                            TermAnnId::Type(forall, ctype)
                                        }
                                        | PatAnnId::Value(_, _) => {
                                            let ctype = ss::CType.build(tycker, &self.info);
                                            self.mk(ValuePiFormation {
                                                binder: binder_out_ann,
                                                codomain: body,
                                            })
                                            .tyck_k(tycker, Action::ana(ctype.into()))?
                                        }
                                    }
                                }
                                | ss::Kind::Arrow(kd_arr) => {
                                    // kind arrow
                                    let ss::Arrow(kd_1, kd_2) = kd_arr;
                                    // ana binder with kd_1
                                    let binder_out_ann = self
                                        .mk(binder)
                                        .tyck_k(tycker, PatternAction::ana(kd_1.into()))?;
                                    let (tpat, kd_1) = binder_out_ann.try_as_type(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    if tpat.syntactically_used(tycker) {
                                        tycker.err_k(
                                            TyckError::Expressivity(
                                                "dependent kinds are not supported yet",
                                            ),
                                            std::panic::Location::caller(),
                                        )?
                                    }
                                    // ana body with kd_2
                                    let body =
                                        self.mk(body).tyck_k(tycker, Action::ana(kd_2.into()))?;
                                    let kd_2 = body.try_as_kind(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    let arr = Alloc::alloc(tycker, ss::Arrow(kd_1, kd_2), (), &());
                                    TermAnnId::Kind(arr)
                                }
                                | ss::Kind::Label(_) => tycker.err_k(
                                    TyckError::KindMismatch,
                                    std::panic::Location::caller(),
                                )?,
                            }
                        }
                        | AnnId::Type(_) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    },
                }
            }
            | Tm::Sigma(term) => {
                let su::Sigma(binder, body) = term;
                match switch {
                    | Switch::Syn => {
                        // either a prod or an exists
                        let binder_out_ann =
                            self.mk(binder).tyck_k(tycker, PatternAction::syn())?;
                        match binder_out_ann.annotation {
                            | PatAnnId::Kind(_) => {
                                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedSigmaSyn::new(
                                    tycker.db,
                                    crate::query::SigmaSynArm::Expressivity,
                                );
                                let Some(crate::query::SigmaSynOutcome::Error(error)) =
                                    crate::query::sigma_syn_judgment(
                                        tycker.db,
                                        tycker.data,
                                        term,
                                        input,
                                        tycker.site_occurrence(),
                                    )
                                else {
                                    unreachable!(
                                        "the kind arm of sigma judgments is query-produced"
                                    )
                                };
                                tycker.err_k(error, std::panic::Location::caller())?
                            }
                            | PatAnnId::Type(tpat, _kd) => {
                                // exists
                                let abst = Alloc::alloc(tycker, tpat, (), &());
                                let subst_vec = {
                                    let mut subst_vec = Vec::new();
                                    if let (Some(def), kd) = tpat.try_destruct_def(tycker) {
                                        let ty_abst = Alloc::alloc(tycker, abst, kd, &self.info);
                                        subst_vec.push((def, ty_abst.into()));
                                    }
                                    subst_vec
                                };
                                let body =
                                    self.mk_add(subst_vec, body).tyck_k(tycker, Action::syn())?;
                                let (body_ty, body_kd) = body.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                // body_kd should be of vtype
                                let vtype = ss::VType.build(tycker, &self.info);
                                Lub::lub_k(vtype, body_kd, tycker)?;
                                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedSigmaSyn::new(
                                    tycker.db,
                                    crate::query::SigmaSynArm::Exists { tpat, abst, body_ty },
                                );
                                let Some(crate::query::SigmaSynOutcome::Type { id, ty, kd }) =
                                    crate::query::sigma_syn_judgment(
                                        tycker.db,
                                        tycker.data,
                                        term,
                                        input,
                                        tycker.site_occurrence(),
                                    )
                                else {
                                    unreachable!(
                                        "the exists arm of sigma judgments is query-produced"
                                    )
                                };
                                tycker.statics.types_pre.insert_new(id, ss::Fillable::Done(ty));
                                tycker.statics.annotations_type.insert_new(id, kd);
                                tycker.statics.env_type.insert_new(id, self.info.clone());
                                TermAnnId::Type(id, kd)
                            }
                            | PatAnnId::Value(vpat, ty_1) => {
                                // prod; vpat should not be used
                                if vpat.syntactically_used(tycker) {
                                    tycker.err_k(
                                        TyckError::Expressivity(
                                            "dependent types are not supported yet",
                                        ),
                                        std::panic::Location::caller(),
                                    )?
                                }
                                // ty should be of vtype
                                let kd_1 = tycker.statics.annotations_type[&ty_1].to_owned();
                                let vtype = ss::VType.build(tycker, &self.info);
                                Lub::lub_k(vtype, kd_1, tycker)?;
                                let ty_2 = self.mk(body).tyck_k(tycker, Action::syn())?;
                                let (ty_2, kd_2) = ty_2.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                // kd_2 should be of vtype
                                Lub::lub_k(vtype, kd_2, tycker)?;
                                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedSigmaSyn::new(
                                    tycker.db,
                                    crate::query::SigmaSynArm::Prod { ty_1, ty_2 },
                                );
                                let Some(crate::query::SigmaSynOutcome::Type { id, ty, kd }) =
                                    crate::query::sigma_syn_judgment(
                                        tycker.db,
                                        tycker.data,
                                        term,
                                        input,
                                        tycker.site_occurrence(),
                                    )
                                else {
                                    unreachable!(
                                        "the product arm of sigma judgments is query-produced"
                                    )
                                };
                                tycker.statics.types_pre.insert_new(id, ss::Fillable::Done(ty));
                                tycker.statics.annotations_type.insert_new(id, kd);
                                tycker.statics.env_type.insert_new(id, self.info.clone());
                                TermAnnId::Type(id, kd)
                            }
                        }
                    }
                    | Switch::Ana(ana) => match ana {
                        | AnnId::Kind(kd) => {
                            let vtype = ss::VType.build(tycker, &self.info);
                            // prod or exists; should be of vtype
                            Lub::lub_k(vtype, kd, tycker)?;
                            // just synthesize the whole thing
                            self.tyck_k(tycker, Action::syn())?
                        }
                        | AnnId::Set | AnnId::Type(_) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    },
                }
            }
            | Tm::ManifestExists(term) => {
                let su::ManifestExists { binder, definition, body } = term;
                match switch {
                    | Switch::Syn => {
                        let definition = self.mk(definition).tyck_k(tycker, Action::syn())?;
                        match definition {
                            | TermAnnId::Kind(definition) => {
                                let binder = self
                                    .mk(binder)
                                    .tyck_k(tycker, PatternAction::ana(AnnId::Set))?;
                                let pattern = binder.annotation.try_as_kind(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let body_env =
                                    self.mk(Assign(pattern, definition)).tyck_k(tycker, ())?.info;
                                let body =
                                    TyEnvT::new(body_env, body).tyck_k(tycker, Action::syn())?;
                                let (body, body_kind) = body.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let vtype = ss::VType.build(tycker, &self.info);
                                Lub::lub_k(vtype, body_kind, tycker)?;
                                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedManifestSyn::new(
                                    tycker.db,
                                    crate::query::ManifestSynArm::Kind {
                                        pattern,
                                        definition,
                                        body,
                                    },
                                );
                                let Some(crate::query::ManifestSynOutcome::Type { id, ty, kd }) =
                                    crate::query::manifest_exists_syn_judgment(
                                        tycker.db,
                                        tycker.data,
                                        term,
                                        input,
                                        tycker.site_occurrence(),
                                    )
                                else {
                                    unreachable!(
                                        "the kind arm of manifest-exists judgments is query-produced"
                                    )
                                };
                                tycker.statics.types_pre.insert_new(id, ss::Fillable::Done(ty));
                                tycker.statics.annotations_type.insert_new(id, kd);
                                tycker.statics.env_type.insert_new(id, self.info.clone());
                                TermAnnId::Type(id, kd)
                            }
                            | TermAnnId::Type(definition, definition_kind) => {
                                let binder_action = if tycker.pattern_has_payload_annotation(binder)
                                {
                                    PatternAction::syn()
                                } else {
                                    PatternAction::ana(definition_kind.into())
                                };
                                let binder = self.mk(binder).tyck_k(tycker, binder_action)?;
                                let (pattern, _domain_kind) = binder.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let witness = Alloc::alloc(tycker, pattern, (), &());
                                let payload_kind = tycker.statics.annotations_abst[&witness];
                                Lub::lub_k(payload_kind, definition_kind, tycker)?;

                                let full_definition = pattern.introduce_payload(tycker, definition);
                                let full_definition = tycker.err_p_to_k(full_definition)?;
                                let body_env = self
                                    .mk(Assign(pattern, full_definition))
                                    .tyck_k(tycker, ())?
                                    .info;
                                let body =
                                    TyEnvT::new(body_env, body).tyck_k(tycker, Action::syn())?;
                                let (body, body_kind) = body.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let vtype = ss::VType.build(tycker, &self.info);
                                Lub::lub_k(vtype, body_kind, tycker)?;

                                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedManifestSyn::new(
                                    tycker.db,
                                    crate::query::ManifestSynArm::Type {
                                        pattern,
                                        witness,
                                        definition,
                                        body,
                                    },
                                );
                                let Some(crate::query::ManifestSynOutcome::Type { id, ty, kd }) =
                                    crate::query::manifest_exists_syn_judgment(
                                        tycker.db,
                                        tycker.data,
                                        term,
                                        input,
                                        tycker.site_occurrence(),
                                    )
                                else {
                                    unreachable!(
                                        "the type arm of manifest-exists judgments is query-produced"
                                    )
                                };
                                tycker.statics.types_pre.insert_new(id, ss::Fillable::Done(ty));
                                tycker.statics.annotations_type.insert_new(id, kd);
                                tycker.statics.env_type.insert_new(id, self.info.clone());
                                TermAnnId::Type(id, kd)
                            }
                            | TermAnnId::Hole(_)
                            | TermAnnId::Value(_, _)
                            | TermAnnId::Compu(_, _) => {
                                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedManifestSyn::new(
                                    tycker.db,
                                    crate::query::ManifestSynArm::SortMismatch,
                                );
                                let Some(crate::query::ManifestSynOutcome::Error(error)) =
                                    crate::query::manifest_exists_syn_judgment(
                                        tycker.db,
                                        tycker.data,
                                        term,
                                        input,
                                        tycker.site_occurrence(),
                                    )
                                else {
                                    unreachable!(
                                        "the sort arm of manifest-exists judgments is query-produced"
                                    )
                                };
                                tycker.err_k(error, std::panic::Location::caller())?
                            }
                        }
                    }
                    | Switch::Ana(AnnId::Kind(kind)) => {
                        let vtype = ss::VType.build(tycker, &self.info);
                        Lub::lub_k(vtype, kind, tycker)?;
                        self.tyck_k(tycker, Action::syn())?
                    }
                    | Switch::Ana(AnnId::Set | AnnId::Type(_)) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                }
            }
            | Tm::Thunk(term) => {
                let su::Thunk(body) = term;
                let ana = match switch {
                    | Switch::Syn => tycker.thk_hole(&self.info, self.inner).into(),
                    | Switch::Ana(ana) => ana,
                };
                let AnnId::Type(ana_ty) = ana else {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let thunk_app_hole = tycker.thk_hole(&self.info, body);
                let ty = Lub::lub_k(ana_ty, thunk_app_hole, tycker)?;
                let ss::Type::App(thunk_app_body_ty) = tycker.type_filled_k(&ty)?.to_owned() else {
                    unreachable!()
                };
                let ss::App(_thunk_ty, body_ty) = thunk_app_body_ty;
                let body_out_ann = self.mk(body).tyck_k(tycker, Action::ana(body_ty.into()))?;
                let (body_out, body_ty) = body_out_ann.try_as_compu(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                let body_interned = crate::query::InternedTermAnn::new(
                    tycker.db,
                    TermAnnId::Compu(body_out, body_ty),
                );
                let Some(outcome) = crate::query::thunk_judgment(
                    tycker.db,
                    tycker.data,
                    term,
                    body_interned,
                    tycker.site_occurrence(),
                ) else {
                    unreachable!("thunk judgments are query-produced")
                };
                tycker
                    .statics
                    .types_pre
                    .insert_new(outcome.thk_ty_id, ss::Fillable::Done(outcome.thk_ty));
                tycker.statics.annotations_type.insert_new(outcome.thk_ty_id, outcome.vtype);
                tycker.statics.env_type.insert_new(outcome.thk_ty_id, self.info.clone());
                tycker.statics.values.insert_new(outcome.thunk_id, outcome.thunk);
                tycker.statics.annotations_value.insert_new(outcome.thunk_id, outcome.thk_ty_id);
                tycker.statics.env_value.insert_new(outcome.thunk_id, self.info.clone());
                TermAnnId::Value(outcome.thunk_id, outcome.thk_ty_id)
            }
            | Tm::Force(term) => {
                let su::Force(body) = term;
                let body_ty = {
                    match switch {
                        | Switch::Syn => {
                            // if syn, then ana the body with thunk_app_hole

                            tycker.thk_hole(&self.info, body)
                        }
                        | Switch::Ana(ana) => {
                            let ana_ty = match ana {
                                | AnnId::Set | AnnId::Kind(_) => tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?,
                                | AnnId::Type(ty) => ty,
                            };
                            // check ana_ty is computation type
                            let ctype = ss::CType.build(tycker, &self.info);
                            let ana_ty_kd = tycker.statics.annotations_type[&ana_ty].to_owned();
                            Lub::lub_k(ctype, ana_ty_kd, tycker)?;
                            // if ana, then ana the body with thunked body_ty
                            cs::Thk(ana_ty).build(tycker, &self.info)
                        }
                    }
                };
                let (body, body_ty) = {
                    let body_out_ann = self.mk(body).tyck_k(tycker, Action::ana(body_ty.into()))?;
                    let (body_out, body_ty) = body_out_ann.try_as_value(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    (body_out, body_ty)
                };
                let force_ty = {
                    let ss::Type::App(thunk_app_body_ty) =
                        tycker.type_filled_k(&body_ty)?.to_owned()
                    else {
                        unreachable!()
                    };
                    let ss::App(_thunk_ty, force_ty) = thunk_app_body_ty;
                    force_ty
                };
                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                let input = crate::query::InternedForceInput::new(tycker.db, body, force_ty);
                let Some(outcome) = crate::query::force_judgment(
                    tycker.db,
                    tycker.data,
                    term,
                    input,
                    tycker.site_occurrence(),
                ) else {
                    unreachable!("force judgments are query-produced")
                };
                tycker.statics.compus.insert_new(outcome.id, outcome.compu);
                tycker.statics.annotations_compu.insert_new(outcome.id, outcome.ann);
                tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
                TermAnnId::Compu(outcome.id, outcome.ann)
            }
            | Tm::Ret(term) => {
                let su::Return(body) = term;
                let ana = match switch {
                    | Switch::Syn => tycker.ret_hole(&self.info, self.inner).into(),
                    | Switch::Ana(ana) => ana,
                };
                let AnnId::Type(ana_ty) = ana else {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let ret_app_hole = tycker.ret_hole(&self.info, self.inner);
                let ty = Lub::lub_k(ana_ty, ret_app_hole, tycker)?;
                let ss::Type::App(ret_app_body_ty) = tycker.type_filled_k(&ty)?.to_owned() else {
                    unreachable!()
                };
                let ss::App(_ret_ty, body_ty) = ret_app_body_ty;
                let body_out_ann = self.mk(body).tyck_k(tycker, Action::ana(body_ty.into()))?;
                let (body_out, body_ty) = body_out_ann.try_as_value(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                let body_interned = crate::query::InternedTermAnn::new(
                    tycker.db,
                    TermAnnId::Value(body_out, body_ty),
                );
                let Some(outcome) = crate::query::ret_judgment(
                    tycker.db,
                    tycker.data,
                    term,
                    body_interned,
                    tycker.site_occurrence(),
                ) else {
                    unreachable!("return judgments are query-produced")
                };
                tycker
                    .statics
                    .types_pre
                    .insert_new(outcome.ret_ty_id, ss::Fillable::Done(outcome.ret_ty));
                tycker.statics.annotations_type.insert_new(outcome.ret_ty_id, outcome.vtype);
                tycker.statics.env_type.insert_new(outcome.ret_ty_id, self.info.clone());
                tycker.statics.compus.insert_new(outcome.ret_id, outcome.ret);
                tycker.statics.annotations_compu.insert_new(outcome.ret_id, outcome.ret_ty_id);
                tycker.statics.env_compu.insert_new(outcome.ret_id, self.info.clone());
                TermAnnId::Compu(outcome.ret_id, outcome.ret_ty_id)
            }
            | Tm::Do(term) => {
                let su::Bind { binder, bindee, tail } = term;
                // first, ana bindee with ret_app_hole, and we get a compu that should be ret_app_body_ty
                let (bindee_out, bindee_ty) = {
                    let ret_app_hole = tycker.ret_hole(&self.info, bindee);
                    let bindee_out_ann =
                        self.mk(bindee).tyck_k(tycker, Action::ana(ret_app_hole.into()))?;
                    bindee_out_ann.try_as_compu(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?
                };
                // then we get the binder_ty from bindee_ty and ana binder with it
                let ss::Type::App(ret_app_binder_ty) = tycker.type_filled_k(&bindee_ty)?.to_owned()
                else {
                    unreachable!()
                };
                let ss::App(_ret_ty, binder_ty) = ret_app_binder_ty;
                let binder_elaboration =
                    self.mk(binder).tyck_k(tycker, PatternAction::ana(binder_ty.into()))?;
                let (binder_out, _binder_ty) = binder_elaboration.as_value();
                // finally, we tyck the tail
                let (tail_out, tail_ty) = {
                    let tail_out_ann = TyEnvT::new(binder_elaboration.info.clone(), tail)
                        .tyck_k(tycker, Action::switch(switch))?;
                    tail_out_ann.try_as_compu(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?
                };
                binder_elaboration.close_scope_k(tycker, tail_ty)?;
                let bind_ty = tail_ty;
                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                let input = crate::query::InternedDoInput::new(
                    tycker.db, binder_out, bindee_out, tail_out, bind_ty,
                );
                let Some(outcome) = crate::query::do_judgment(
                    tycker.db,
                    tycker.data,
                    term,
                    input,
                    tycker.site_occurrence(),
                ) else {
                    unreachable!("bind judgments are query-produced")
                };
                tycker.statics.compus.insert_new(outcome.id, outcome.compu);
                tycker.statics.annotations_compu.insert_new(outcome.id, outcome.ann);
                tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
                TermAnnId::Compu(outcome.id, outcome.ann)
            }
            | Tm::Let(term) => {
                let su::Let { binder, bindee, tail } = term;
                let (bindee, is_sealed) = match bindee.syntactically_sealed(tycker) {
                    | Some(bindee) => (bindee, true),
                    | None => (bindee, false),
                };
                // first, synthesize bindee
                let bindee_out_ann = self.mk(bindee).tyck_k(tycker, Action::syn())?;
                match bindee_out_ann {
                    | TermAnnId::Type(bindee_out, bindee_kd) => {
                        // a type alias
                        // then, ana binder with bindee_kd
                        let binder_out_ann =
                            self.mk(binder).tyck_k(tycker, PatternAction::ana(bindee_kd.into()))?;
                        let (binder_out, _binder_kd) = binder_out_ann.as_type();
                        if let (Some(def), _) = binder_out.try_destruct_def(tycker) {
                            let _ = tycker.statics.type_definitions.upsert(def, bindee_out);
                        }
                        let bindee_out = if is_sealed {
                            let abst: AbstId = tycker.fresh();
                            tycker.statics.absts.insert_new(abst, ());
                            if let (Some(def), _) = binder_out.try_destruct_def(tycker) {
                                tycker.statics.abst_hints.insert_new(abst, def);
                            }
                            tycker.statics.seals.insert_new(abst, bindee_out);
                            Alloc::alloc(tycker, abst, bindee_kd, &self.info)
                        } else {
                            bindee_out
                        };
                        // and then assign bindee_out to binder_out;
                        // the type is effectively inlined
                        let env = self.mk(Assign(binder_out, bindee_out)).tyck_k(tycker, ())?;
                        match binder_out.try_destruct_def(tycker) {
                            | (Some(def), _) => {
                                // consider adding it to the globals if bindee is global
                                if tycker.statics.global_terms.get(&bindee_out.into()).is_some() {
                                    tycker.statics.global_defs.ensure(def);
                                }
                            }
                            | (None, _) => {}
                        }
                        // finally, we tyck the tail
                        let tail_out_ann = env.mk(tail).tyck_k(tycker, Action::switch(switch))?;
                        match tail_out_ann {
                            | TermAnnId::Type(tail_out, tail_kd) => {
                                // the resulting type will be the tail
                                TermAnnId::Type(tail_out, tail_kd)
                            }
                            | TermAnnId::Value(tail_out, tail_ty) => {
                                // Type aliases and definitions are static, so the
                                // resulting value is the checked tail itself.
                                TermAnnId::Value(tail_out, tail_ty)
                            }
                            | TermAnnId::Compu(tail_out, tail_ty) => {
                                // the resulting computation will only be the tail
                                TermAnnId::Compu(tail_out, tail_ty)
                            }
                            | TermAnnId::Hole(_) | TermAnnId::Kind(_) => tycker
                                .err_k(TyckError::SortMismatch, std::panic::Location::caller())?,
                        }
                    }
                    | TermAnnId::Value(bindee_out, bindee_ty) => {
                        // a value alias
                        // then, ana binder with bindee_ty
                        let binder_elaboration =
                            self.mk(binder).tyck_k(tycker, PatternAction::ana(bindee_ty.into()))?;
                        let (binder_out, _binder_ty) = binder_elaboration.as_value();
                        match binder_out.try_destruct_def(tycker) {
                            | (Some(def), _) => {
                                let _ = tycker.statics.value_aliases.upsert(def, bindee_out);
                                // consider adding it to the globals if bindee is global
                                if tycker.statics.global_terms.get(&bindee_out.into()).is_some() {
                                    tycker.statics.global_defs.ensure(def);
                                    // consider adding it to the inlinables as well
                                    let _ = tycker.statics.inlinables.upsert(def, bindee_out);
                                }
                            }
                            | (None, _) => {}
                        }
                        // finally, we tyck the tail
                        let tail_out_ann = TyEnvT::new(binder_elaboration.info.clone(), tail)
                            .tyck_k(tycker, Action::switch(switch))?;
                        match tail_out_ann {
                            | TermAnnId::Value(tail_out, tail_ty) => {
                                binder_elaboration.close_scope_k(tycker, tail_ty)?;
                                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                                let binder = crate::query::InternedVPat::new(tycker.db, binder_out);
                                let bindee =
                                    crate::query::InternedValue::new(tycker.db, bindee_out);
                                let tail = crate::query::InternedTermAnn::new(
                                    tycker.db,
                                    TermAnnId::Value(tail_out, tail_ty),
                                );
                                let Some(crate::query::LetSynOutcome::Value { id, value, ann }) =
                                    crate::query::let_judgment(
                                        tycker.db,
                                        tycker.data,
                                        term,
                                        binder,
                                        bindee,
                                        tail,
                                        tycker.site_occurrence(),
                                    )
                                else {
                                    unreachable!(
                                        "the value tail of let judgments is query-produced"
                                    )
                                };
                                tycker.statics.values.insert_new(id, value);
                                tycker.statics.annotations_value.insert_new(id, ann);
                                tycker.statics.env_value.insert_new(id, self.info.clone());
                                TermAnnId::Value(id, ann)
                            }
                            | TermAnnId::Compu(tail_out, tail_ty) => {
                                binder_elaboration.close_scope_k(tycker, tail_ty)?;
                                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                                let binder = crate::query::InternedVPat::new(tycker.db, binder_out);
                                let bindee =
                                    crate::query::InternedValue::new(tycker.db, bindee_out);
                                let tail = crate::query::InternedTermAnn::new(
                                    tycker.db,
                                    TermAnnId::Compu(tail_out, tail_ty),
                                );
                                let Some(crate::query::LetSynOutcome::Compu { id, compu, ann }) =
                                    crate::query::let_judgment(
                                        tycker.db,
                                        tycker.data,
                                        term,
                                        binder,
                                        bindee,
                                        tail,
                                        tycker.site_occurrence(),
                                    )
                                else {
                                    unreachable!(
                                        "the computation tail of let judgments is query-produced"
                                    )
                                };
                                tycker.statics.compus.insert_new(id, compu);
                                tycker.statics.annotations_compu.insert_new(id, ann);
                                tycker.statics.env_compu.insert_new(id, self.info.clone());
                                TermAnnId::Compu(id, ann)
                            }
                            | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Type(_, _) => {
                                tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?
                            }
                        }
                    }
                    | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Compu(_, _) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                }
            }
            | Tm::MobileParam(_) | Tm::MobileBind(_) => {
                unreachable!("mobile syntax must be eliminated during name resolution")
            }
            | Tm::Residual(term) => {
                let su::Residual(body) = term;
                self.mk(body).tyck_k(tycker, Action::switch(switch))?
            }
            | Tm::Block(term) => {
                let su::Block(body) = term;
                let inference = InferenceRegion::enter(tycker);
                let checked = self.mk(body).tyck_k(tycker, Action::switch(switch))?;
                inference.close_k(tycker)?;
                checked
            }
            | Tm::RecGroup(term) => {
                let su::RecGroup { definitions, tail } = term;
                let bindings = definitions
                    .into_iter()
                    .enumerate()
                    .map(|(source_order, su::RecursiveDefinition { binder, bindee })| {
                        su::Binding::from_term(
                            bindee,
                            su::BindingForm::Definition(su::Definition { binder, bindee }),
                            source_order,
                        )
                    })
                    .collect::<Vec<_>>();
                let env = FixPoint(self.mk(bindings)).tyck_k(tycker, ())?;
                env.mk(tail).tyck_k(tycker, Action::switch(switch))?
            }
            | Tm::MoBlock(term) => {
                let su::MoBlock { body, basis: lexical_basis } = term;
                let basis =
                    MonadicBasisElaboration::new(&lexical_basis, &self.info).check_k(tycker)?;

                // tyck the body with an (almost) empty env
                let ty_env = TyEnv::monadic_new(tycker, &self.info);
                let body_out_ann = TyEnvT { info: ty_env.to_owned(), inner: body }
                    .tyck_k(tycker, Action::syn())?;
                let (body, _body_ty) = body_out_ann.try_as_compu(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;

                let monad_ty_kd: ss::KindId =
                    ss::Arrow(ss::VType, ss::CType).build(tycker, &self.info);
                let monad_ty_var =
                    Alloc::alloc(tycker, ss::VarName("M".to_string()), monad_ty_kd.into(), &());
                let abst: ss::AbstId = Alloc::alloc(tycker, monad_ty_var, monad_ty_kd, &());
                let monad_ty = cs::Type(cs::Ann(abst, monad_ty_kd)).build(tycker, &self.info);
                let ctype = ss::CType.build(tycker, &self.info);
                let monad_application =
                    Alloc::alloc(tycker, ss::App(basis.monad, monad_ty), ctype, &self.info);
                let monad_impl_ty = cs::Thk(cs::Type(monad_application)).build(tycker, &self.info);
                let monad_impl_var =
                    Alloc::alloc(tycker, ss::VarName("mo".to_string()), monad_impl_ty.into(), &());
                let monad_impl = cs::Value(monad_impl_var).build(tycker, &self.info);

                use crate::environment::*;
                let (_menv, body_lift) = cs::TermLift { tm: body }.mbuild_k(
                    tycker,
                    MonEnv {
                        ty: ty_env,
                        subst: SubstEnv::new(),
                        subst_abst: SubstAbstEnv::new(),
                        structure: StrEnv::new(),
                        basis,
                        monad_ty,
                        monad_impl,
                    },
                )?;
                let body_lift_ty = cs::TypeOf(body_lift).build(tycker, &self.info);

                // <monad_impl_to_body_lift> = fn (mo: Thk (Monad M)) => Lift(body)
                let monad_impl_vpat: ss::VPatId =
                    Alloc::alloc(tycker, monad_impl_var, monad_impl_ty, &self.info);
                let monad_impl_to_body_lift_ty =
                    Alloc::alloc(tycker, ss::Arrow(monad_impl_ty, body_lift_ty), ctype, &self.info);
                let monad_impl_to_body_lift = Alloc::alloc(
                    tycker,
                    ss::Abs(monad_impl_vpat, body_lift),
                    monad_impl_to_body_lift_ty,
                    &self.info,
                );

                // fn (M : VType -> CType) => <monad_impl_to_body_lift>
                let monad_ty_tpat: ss::TPatId =
                    Alloc::alloc(tycker, monad_ty_var, monad_ty_kd, &self.info);
                let res_body_ty = Alloc::alloc(
                    tycker,
                    ss::Forall(
                        ss::TypeBinder { pattern: monad_ty_tpat, witness: abst },
                        monad_impl_to_body_lift_ty,
                    ),
                    ctype,
                    &self.info,
                );
                let res_body = Alloc::alloc(
                    tycker,
                    ss::Abs(monad_ty_tpat, monad_impl_to_body_lift),
                    res_body_ty,
                    &self.info,
                );

                TermAnnId::Compu(res_body, res_body_ty)
            }
            | Tm::Data(term) => {
                let su::Data { arms } = term;
                let vtype = ss::VType.build(tycker, &self.info);
                let vtype = match switch {
                    | Switch::Syn => vtype,
                    | Switch::Ana(ann) => {
                        let AnnId::Kind(ann_kd) = ann else {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        Lub::lub_k(vtype, ann_kd, tycker)?
                    }
                };
                let mut arms_vec = im::Vector::new();
                for su::DataArm { name, param } in arms {
                    let param = self.mk(param).tyck_k(tycker, Action::ana(vtype.into()))?;
                    let TermAnnId::Type(ty, _kd) = param else {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    arms_vec.push_back((name, ty));
                }
                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                let arms_interned = crate::query::InternedDataArms::new(
                    tycker.db,
                    arms_vec.into_iter().collect::<Vec<_>>(),
                );
                let kd_interned = crate::query::InternedKind::new(tycker.db, vtype);
                let Some(outcome) = crate::query::data_syn_judgment(
                    tycker.db,
                    tycker.data,
                    term,
                    arms_interned,
                    kd_interned,
                    tycker.site_occurrence(),
                ) else {
                    unreachable!("data declaration judgments are query-produced")
                };
                tycker.statics.datas.insert_new(outcome.data_id, outcome.data);
                tycker.statics.types_pre.insert_new(outcome.ty_id, ss::Fillable::Done(outcome.ty));
                tycker.statics.annotations_type.insert_new(outcome.ty_id, outcome.kd);
                tycker.statics.env_type.insert_new(outcome.ty_id, self.info.clone());
                TermAnnId::Type(outcome.ty_id, outcome.kd)
            }
            | Tm::CoData(term) => {
                let su::CoData { arms } = term;
                let ctype = ss::CType.build(tycker, &self.info);
                let ctype = match switch {
                    | Switch::Syn => ctype,
                    | Switch::Ana(ann) => {
                        let AnnId::Kind(ann_kd) = ann else {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        Lub::lub_k(ctype, ann_kd, tycker)?
                    }
                };
                let mut arms_vec = im::Vector::new();
                for su::CoDataArm { name, out } in arms {
                    let out = self.mk(out).tyck_k(tycker, Action::ana(ctype.into()))?;
                    let TermAnnId::Type(ty, _kd) = out else {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    arms_vec.push_back((name, ty));
                }
                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                let arms_interned = crate::query::InternedCoDataArms::new(
                    tycker.db,
                    arms_vec.into_iter().collect::<Vec<_>>(),
                );
                let kd_interned = crate::query::InternedKind::new(tycker.db, ctype);
                let Some(outcome) = crate::query::codata_syn_judgment(
                    tycker.db,
                    tycker.data,
                    term,
                    arms_interned,
                    kd_interned,
                    tycker.site_occurrence(),
                ) else {
                    unreachable!("codata declaration judgments are query-produced")
                };
                tycker.statics.codatas.insert_new(outcome.codata_id, outcome.codata);
                tycker.statics.types_pre.insert_new(outcome.ty_id, ss::Fillable::Done(outcome.ty));
                tycker.statics.annotations_type.insert_new(outcome.ty_id, outcome.kd);
                tycker.statics.env_type.insert_new(outcome.ty_id, self.info.clone());
                TermAnnId::Type(outcome.ty_id, outcome.kd)
            }
            | Tm::Ctor(term) => {
                let su::Ctor(ctor, arg) = term;
                let ana_ty = match switch {
                    | Switch::Syn => tycker
                        .err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                    | Switch::Ana(ann) => ann,
                };
                let AnnId::Type(ana_ty) = ana_ty else {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let ana_ty_unroll = ana_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                let ss::Type::Data(data_id) = tycker.type_filled_k(&ana_ty_unroll)? else {
                    tycker.err_k(
                        TyckError::TypeExpected {
                            expected: "data type definition".to_string(),
                            found: ana_ty_unroll,
                        },
                        std::panic::Location::caller(),
                    )?
                };
                let arg_ty = match tycker.statics.datas[&data_id].get(&ctor) {
                    | Some(ty) => ty.to_owned(),
                    | None => tycker.err_k(
                        TyckError::UnknownDataConstructor(ctor.clone()),
                        std::panic::Location::caller(),
                    )?,
                };
                let arg_out_ann = self.mk(arg).tyck_k(tycker, Action::ana(arg_ty.into()))?;
                let TermAnnId::Value(arg, _arg_ty) = arg_out_ann else { unreachable!() };
                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                let input = crate::query::InternedCtorInput::new(
                    tycker.db,
                    ctor.to_owned(),
                    arg,
                    ana_ty,
                    data_id,
                );
                let Some(outcome) = crate::query::ctor_syn_judgment(
                    tycker.db,
                    tycker.data,
                    term,
                    input,
                    tycker.site_occurrence(),
                ) else {
                    unreachable!("constructor judgments are query-produced")
                };
                tycker.statics.values.insert_new(outcome.id, outcome.value);
                tycker.statics.annotations_value.insert_new(outcome.id, outcome.ann);
                tycker.statics.env_value.insert_new(outcome.id, self.info.clone());
                // hint the ctor to be associated with the definition name
                tycker.statics.data_hints.insert_new(outcome.id, data_id);
                TermAnnId::Value(outcome.id, outcome.ann)
            }
            | Tm::Match(term) => {
                let su::Match { scrut, arms } = term;
                let scrut_out_ann = self.mk(scrut).tyck_k(tycker, Action::syn())?;
                let (scrut, scrut_ty) = scrut_out_ann.try_as_value(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let scrut_ty_unroll = scrut_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                // hint the scrut to be associated with the data type
                match tycker.type_filled_k(&scrut_ty_unroll)? {
                    | ss::Type::Data(data_id) => {
                        let _ = tycker.statics.data_hints.upsert(scrut, data_id);
                    }
                    | _ => {}
                }
                let mut matchers = Vec::new();
                let mut arms_ty = Vec::new();
                for su::Matcher { binder, tail } in arms {
                    let binder_elaboration = self
                        .mk(binder)
                        .tyck_k(tycker, PatternAction::ana(scrut_ty_unroll.into()))?;
                    let (binder, _ty) = binder_elaboration.try_as_value(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    match switch {
                        | Switch::Syn => {
                            let tail_out_ann = TyEnvT::new(binder_elaboration.info.clone(), tail)
                                .tyck_k(tycker, Action::syn())?;
                            let TermAnnId::Compu(tail, ty) = tail_out_ann else {
                                tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?
                            };
                            binder_elaboration.close_scope_k(tycker, ty)?;
                            matchers.push(ss::Matcher { binder, tail });
                            arms_ty.push(ty);
                        }
                        | Switch::Ana(ana_ty) => {
                            let tail_out_ann = TyEnvT::new(binder_elaboration.info.clone(), tail)
                                .tyck_k(tycker, Action::ana(ana_ty))?;
                            let TermAnnId::Compu(tail, ty) = tail_out_ann else {
                                tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?
                            };
                            binder_elaboration.close_scope_k(tycker, ty)?;
                            matchers.push(ss::Matcher { binder, tail });
                            arms_ty.push(ty);
                        }
                    }
                }
                // Note: use hole
                if arms_ty.is_empty() {
                    match switch {
                        | Switch::Syn => tycker
                            .err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                        | Switch::Ana(ana_ty) => match ana_ty {
                            | AnnId::Set | AnnId::Kind(_) => tycker
                                .err_k(TyckError::SortMismatch, std::panic::Location::caller())?,
                            | AnnId::Type(ana_ty) => {
                                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                                let input = crate::query::InternedMatchInput::new(
                                    tycker.db,
                                    scrut,
                                    matchers
                                        .iter()
                                        .map(|matcher| (matcher.binder, matcher.tail))
                                        .collect::<Vec<_>>(),
                                    ana_ty,
                                );
                                let Some(outcome) = crate::query::match_syn_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                ) else {
                                    unreachable!("the empty-arms match judgment is query-produced")
                                };
                                tycker.statics.compus.insert_new(outcome.id, outcome.compu);
                                tycker
                                    .statics
                                    .annotations_compu
                                    .insert_new(outcome.id, outcome.ann);
                                tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
                                TermAnnId::Compu(outcome.id, outcome.ann)
                            }
                        },
                    }
                } else {
                    // make sure that each arm has the same type
                    let mut iter = arms_ty.into_iter();
                    let mut res = iter.next().unwrap();
                    for ty in iter {
                        res = Lub::lub_k(res, ty, tycker)?;
                    }
                    let whole_ty = res;
                    let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                    let input = crate::query::InternedMatchInput::new(
                        tycker.db,
                        scrut,
                        matchers
                            .iter()
                            .map(|matcher| (matcher.binder, matcher.tail))
                            .collect::<Vec<_>>(),
                        whole_ty,
                    );
                    let Some(outcome) = crate::query::match_syn_judgment(
                        tycker.db,
                        tycker.data,
                        term,
                        input,
                        tycker.site_occurrence(),
                    ) else {
                        unreachable!("match judgments are query-produced")
                    };
                    tycker.statics.compus.insert_new(outcome.id, outcome.compu);
                    tycker.statics.annotations_compu.insert_new(outcome.id, outcome.ann);
                    tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
                    TermAnnId::Compu(outcome.id, outcome.ann)
                }
            }
            | Tm::CoMatchClauses(term) => {
                let expected = match switch {
                    | Switch::Syn => tycker
                        .err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                    | Switch::Ana(AnnId::Type(expected)) => expected,
                    | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                };
                let computation = CopatternElaborator::new(self.inner, term, expected, &self.info)
                    .elaborate_k(tycker)?;
                TermAnnId::Compu(computation, expected)
            }
            | Tm::CoMatch(term) => {
                let su::CoMatch { arms: comatchers } = term;
                let ana_ty = match switch {
                    | Switch::Syn => tycker
                        .err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                    | Switch::Ana(ana) => match ana {
                        | AnnId::Set | AnnId::Kind(_) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                        | AnnId::Type(ana_ty) => ana_ty,
                    },
                };
                let ana_ty_unroll = ana_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                let ss::Type::CoData(codata_id) = tycker.type_filled_k(&ana_ty_unroll)? else {
                    tycker.err_k(
                        TyckError::TypeExpected {
                            expected: "codata type definition".to_string(),
                            found: ana_ty_unroll,
                        },
                        std::panic::Location::caller(),
                    )?
                };
                let arms = tycker.statics.codatas[&codata_id].clone();
                let mut comatchers_new = Vec::new();
                for su::CoMatcher { dtor, tail } in comatchers {
                    let arm_ty = match arms.get(&dtor) {
                        | Some(arm_ty) => arm_ty,
                        | None => tycker.err_k(
                            TyckError::UnknownCoDataDestructor(dtor.clone()),
                            std::panic::Location::caller(),
                        )?,
                    };
                    let tail_out_ann = self.mk(tail).tyck_k(tycker, Action::ana(arm_ty.into()))?;
                    let TermAnnId::Compu(tail, _ty) = tail_out_ann else {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    comatchers_new.push(ss::CoMatcher { dtor, tail });
                }
                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                let input = crate::query::InternedCoMatchInput::new(
                    tycker.db,
                    comatchers_new
                        .iter()
                        .map(|comatcher| (comatcher.dtor.clone(), comatcher.tail))
                        .collect::<Vec<_>>(),
                    ana_ty,
                );
                let Some(outcome) = crate::query::comatch_syn_judgment(
                    tycker.db,
                    tycker.data,
                    term,
                    input,
                    tycker.site_occurrence(),
                ) else {
                    unreachable!("comatch judgments are query-produced")
                };
                tycker.statics.compus.insert_new(outcome.id, outcome.compu);
                tycker.statics.annotations_compu.insert_new(outcome.id, outcome.ann);
                tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
                // hint the whole computation to be associated with the codata type
                tycker.statics.codata_hints.insert_new(outcome.id, codata_id);
                TermAnnId::Compu(outcome.id, outcome.ann)
            }
            | Tm::Dtor(term) => {
                let su::Dtor(body, dtor) = term;
                let body_out_ann = self.mk(body).tyck_k(tycker, Action::syn())?;
                let TermAnnId::Compu(body, ty_body) = body_out_ann else {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let ty_body_unroll = ty_body.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                let ss::Type::CoData(codata_id) = tycker.type_filled_k(&ty_body_unroll)? else {
                    tycker.err_k(
                        TyckError::TypeExpected {
                            expected: "codata type definition".to_string(),
                            found: ty_body_unroll,
                        },
                        std::panic::Location::caller(),
                    )?
                };
                // hint the body to be associated with the codata type
                let _ = tycker.statics.codata_hints.upsert(body, codata_id);
                let whole_ty = match tycker.statics.codatas[&codata_id].get(&dtor) {
                    | Some(ty) => ty.to_owned(),
                    | None => tycker.err_k(
                        TyckError::UnknownCoDataDestructor(dtor.clone()),
                        std::panic::Location::caller(),
                    )?,
                };
                match switch {
                    | Switch::Syn => {
                        let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                        let input =
                            crate::query::InternedDtorInput::new(tycker.db, body, dtor, whole_ty);
                        let Some(outcome) = crate::query::dtor_syn_judgment(
                            tycker.db,
                            tycker.data,
                            term,
                            input,
                            tycker.site_occurrence(),
                        ) else {
                            unreachable!("destructor judgments are query-produced")
                        };
                        tycker.statics.compus.insert_new(outcome.id, outcome.compu);
                        tycker.statics.annotations_compu.insert_new(outcome.id, outcome.ann);
                        tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
                        TermAnnId::Compu(outcome.id, outcome.ann)
                    }
                    | Switch::Ana(ana) => {
                        let AnnId::Type(ana_ty) = ana else {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        let whole_ty = Lub::lub_k(whole_ty, ana_ty, tycker)?;
                        let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                        let input =
                            crate::query::InternedDtorInput::new(tycker.db, body, dtor, whole_ty);
                        let Some(outcome) = crate::query::dtor_syn_judgment(
                            tycker.db,
                            tycker.data,
                            term,
                            input,
                            tycker.site_occurrence(),
                        ) else {
                            unreachable!("destructor judgments are query-produced")
                        };
                        tycker.statics.compus.insert_new(outcome.id, outcome.compu);
                        tycker.statics.annotations_compu.insert_new(outcome.id, outcome.ann);
                        tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
                        TermAnnId::Compu(outcome.id, outcome.ann)
                    }
                }
            }
            | Tm::Proj(term) => {
                let su::Proj(head, name) = term;
                let checked = self.mk(head).tyck_k(tycker, Action::syn())?;
                match checked {
                    | TermAnnId::Type(head, head_kind) => {
                        let candidate = FieldProjectionResolver::r#type(tycker, head_kind, &name)?;
                        let payload_kind = match switch {
                            | Switch::Syn => candidate.projected,
                            | Switch::Ana(AnnId::Kind(expected)) => {
                                Lub::lub_k(candidate.projected, expected, tycker)?
                            }
                            | Switch::Ana(AnnId::Set | AnnId::Type(_)) => tycker
                                .err_k(TyckError::SortMismatch, std::panic::Location::caller())?,
                        };
                        let projected = FieldProjectionResolver::project_type_k(
                            tycker,
                            head,
                            candidate,
                            payload_kind,
                        )?;
                        TermAnnId::Type(projected, payload_kind)
                    }
                    | TermAnnId::Value(head, head_ty) => {
                        let candidate =
                            FieldProjectionResolver::value_k(tycker, &self.info, head_ty, &name)?;
                        let target = FieldProjectionResolver::value_target(&candidate);
                        let projected_ty = candidate.projected;
                        let projected_ty = match switch {
                            | Switch::Syn => projected_ty,
                            | Switch::Ana(AnnId::Type(expected)) => {
                                Lub::lub_k(projected_ty, expected, tycker)?
                            }
                            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => tycker
                                .err_k(TyckError::SortMismatch, std::panic::Location::caller())?,
                        };
                        let field = ss::ResolvedField { name, target };
                        let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                        let input = crate::query::InternedProjInput::new(
                            tycker.db,
                            head,
                            field.name,
                            field
                                .target
                                .products
                                .iter()
                                .map(|product| (product.product, product.position))
                                .collect::<Vec<_>>(),
                            projected_ty,
                        );
                        let Some(outcome) = crate::query::proj_syn_judgment(
                            tycker.db,
                            tycker.data,
                            term,
                            input,
                            tycker.site_occurrence(),
                        ) else {
                            unreachable!("projection judgments are query-produced")
                        };
                        tycker.statics.values.insert_new(outcome.id, outcome.value);
                        tycker.statics.annotations_value.insert_new(outcome.id, outcome.ann);
                        tycker.statics.env_value.insert_new(outcome.id, self.info.clone());
                        TermAnnId::Value(outcome.id, outcome.ann)
                    }
                    | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Compu(_, _) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                }
            }
            | Tm::Lit(lit) => match switch {
                | Switch::Syn => {
                    let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                    let Some(outcome) = crate::query::literal_syn_judgment(
                        tycker.db,
                        tycker.data,
                        term,
                        tycker.site_occurrence(),
                    ) else {
                        unreachable!("literal judgments are query-produced")
                    };
                    match outcome {
                        | crate::query::LiteralSynOutcome::Value { id, value, ty } => {
                            tycker.statics.values.insert_new(id, value);
                            tycker.statics.annotations_value.insert_new(id, ty);
                            tycker.statics.env_value.insert_new(id, self.info.clone());
                            TermAnnId::Value(id, ty)
                        }
                        | crate::query::LiteralSynOutcome::Error(error) => {
                            tycker.err_k(error, std::panic::Location::caller())?
                        }
                    }
                }
                | Switch::Ana(annotation) => {
                    let switch = Switch::Ana(annotation);
                    fn primitive_type(
                        tycker: &Tycker<'_>, ty: ss::TypeId,
                    ) -> Option<ss::PrimitiveType> {
                        match tycker.statics.types_pre.get(&ty)?.to_owned() {
                            | ss::Fillable::Fill(fill) => match tycker.statics.solus.get(&fill) {
                                | Some(ss::AnnId::Type(solution)) => {
                                    primitive_type(tycker, *solution)
                                }
                                | _ => None,
                            },
                            | ss::Fillable::Done(ss::Type::Primitive(ss::PrimitiveTy(
                                primitive,
                            ))) => Some(primitive),
                            | ss::Fillable::Done(ss::Type::Named(ss::Named(_, inner))) => {
                                primitive_type(tycker, inner)
                            }
                            | ss::Fillable::Done(_) => None,
                        }
                    }

                    fn literal_type_k(
                        tycker: &mut Tycker<'_>, env: &ss::TyEnv, switch: Switch<AnnId>,
                        primitive: ss::PrimitiveType,
                    ) -> ResultKont<ss::TypeId> {
                        let literal_ty = ss::PrimitiveTy(primitive).build(tycker, env);
                        match switch {
                            | Switch::Syn => unreachable!("the synth path is query-produced"),
                            | Switch::Ana(annotation) => {
                                let AnnId::Type(ty) = annotation else {
                                    tycker.err_k(
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?
                                };
                                Lub::lub_k(literal_ty, ty, tycker)
                            }
                        }
                    }
                    use zydeco_syntax::Literal as Lit;
                    let (lit, ty) = match lit {
                        | Lit::Integer(i) => {
                            let (ty, integer_type) = match switch {
                                | Switch::Syn => unreachable!("the synth path is query-produced"),
                                | Switch::Ana(AnnId::Type(ty)) => {
                                    match primitive_type(tycker, ty) {
                                        | Some(ss::PrimitiveType::Integer(integer_type)) => {
                                            (ty, integer_type)
                                        }
                                        | Some(_) | None => {
                                            let default = ss::PrimitiveTy(
                                                ss::PrimitiveType::Integer(ss::IntegerType::Int64),
                                            )
                                            .build(tycker, &self.info);
                                            let ty = Lub::lub_k(default, ty, tycker)?;
                                            (ty, ss::IntegerType::Int64)
                                        }
                                    }
                                }
                                | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?,
                            };
                            let value = i.value();
                            let Some(i) = i.with_type(integer_type) else {
                                tycker.err_k(
                                    TyckError::IntegerLiteralOutOfRange { value, integer_type },
                                    std::panic::Location::caller(),
                                )?
                            };
                            (Lit::Integer(i), ty)
                        }
                        | Lit::Float(value) => {
                            let (ty, float_type) = match switch {
                                | Switch::Syn => unreachable!("the synth path is query-produced"),
                                | Switch::Ana(AnnId::Type(ty)) => {
                                    match primitive_type(tycker, ty) {
                                        | Some(ss::PrimitiveType::Float(float_type)) => {
                                            (ty, float_type)
                                        }
                                        | Some(_) | None => {
                                            let default = ss::PrimitiveTy(
                                                ss::PrimitiveType::Float(ss::FloatType::Float64),
                                            )
                                            .build(tycker, &self.info);
                                            let ty = Lub::lub_k(default, ty, tycker)?;
                                            (ty, ss::FloatType::Float64)
                                        }
                                    }
                                }
                                | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?,
                            };
                            let original = value;
                            let Some(value) = value.with_type(float_type) else {
                                tycker.err_k(
                                    TyckError::FloatLiteralOutOfRange {
                                        value: original.value(),
                                        float_type,
                                    },
                                    std::panic::Location::caller(),
                                )?
                            };
                            (Lit::Float(value), ty)
                        }
                        | Lit::String(s) => {
                            let ty = literal_type_k(
                                tycker,
                                &self.info,
                                switch,
                                ss::PrimitiveType::String,
                            )?;
                            (Lit::String(s), ty)
                        }
                        | Lit::Char(c) => {
                            let ty = literal_type_k(
                                tycker,
                                &self.info,
                                switch,
                                ss::PrimitiveType::Char,
                            )?;
                            (Lit::Char(c), ty)
                        }
                    };
                    let lit = Alloc::alloc(tycker, lit, ty, &self.info);
                    TermAnnId::Value(lit, ty)
                }
            },
        };

        if let Some(out) = out_ann.as_term() {
            // maintain back mapping
            tycker.statics.terms.ensure(self.inner, out);

            // check if the term is global
            let global = tycker.scoped.coctxs_term_local[&self.inner]
                .iter()
                .all(|def| tycker.statics.global_defs.get(def).is_some());

            if global {
                tycker.statics.global_terms.ensure(out);
            }
        }

        Ok(out_ann)
    }
}

#[cfg(test)]
mod source_boundary_tests {
    use super::*;
    use std::sync::{Arc, Mutex};

    #[salsa::db]
    #[derive(Clone)]
    struct TestDb {
        storage: salsa::Storage<Self>,
        pending: Arc<Mutex<Option<Arc<crate::query::PendingParts>>>>,
    }

    impl Default for TestDb {
        fn default() -> Self {
            Self { storage: salsa::Storage::default(), pending: Arc::new(Mutex::new(None)) }
        }
    }

    #[salsa::db]
    impl salsa::Database for TestDb {}

    #[salsa::db]
    impl crate::query::TyckDb for TestDb {
        fn pending_parts(&self) -> &Arc<Mutex<Option<Arc<crate::query::PendingParts>>>> {
            &self.pending
        }
    }
    use crate::environment::TyEnv;

    #[test]
    fn expected_kinds_flow_through_source_boundaries() {
        let mut allocator = IdAllocator::<su::ScopedScope>::new();
        let hole = allocator.alloc();
        let boundary = allocator.alloc();
        let mut scoped = su::ScopedArena::default();
        scoped.terms.insert_new(hole, su::Hole.into());
        scoped.terms.insert_new(boundary, su::SourceBoundary(hole).into());
        [hole, boundary].into_iter().for_each(|term| {
            scoped.ctxs_term.insert_new(term, su::Context::new());
            scoped.coctxs_term_local.insert_new(term, su::CoContext::new());
        });

        let spans = su::SpanArena::default();
        let prim = su::PrimDefs::default();
        let db = TestDb::default();
        *db.pending.lock().unwrap() = Some(Arc::new(crate::query::PendingParts {
            spans: spans.clone(),
            prim: prim.clone(),
            scoped: scoped.clone(),
            root: hole,
        }));
        let data = crate::query::intern_pending(&db);
        let mut tycker = Tycker::new(&db, data, &spans, &prim, &mut scoped);
        let env = TyEnv::default();
        let expected = Alloc::alloc(&mut tycker, ss::VType, (), &());
        let checked =
            TyEnvT::new(env, boundary).tyck_k(&mut tycker, Action::ana(expected.into())).unwrap();

        assert!(matches!(checked, TermAnnId::Type(_, kind) if kind == expected));
        assert!(tycker.errors.is_empty());
    }

    #[test]
    fn imported_source_errors_exclude_the_importing_task_stack() {
        let mut allocator = IdAllocator::<su::ScopedScope>::new();
        let hole = allocator.alloc();
        let boundary = allocator.alloc();
        let mut scoped = su::ScopedArena::default();
        scoped.terms.insert_new(hole, su::Hole.into());
        scoped.terms.insert_new(boundary, su::SourceBoundary(hole).into());
        [hole, boundary].into_iter().for_each(|term| {
            scoped.ctxs_term.insert_new(term, su::Context::new());
            scoped.coctxs_term_local.insert_new(term, su::CoContext::new());
        });

        let spans = su::SpanArena::default();
        let prim = su::PrimDefs::default();
        let db = TestDb::default();
        *db.pending.lock().unwrap() = Some(Arc::new(crate::query::PendingParts {
            spans: spans.clone(),
            prim: prim.clone(),
            scoped: scoped.clone(),
            root: hole,
        }));
        let data = crate::query::intern_pending(&db);
        let mut tycker = Tycker::new(&db, data, &spans, &prim, &mut scoped);
        let result =
            TyEnvT::new(TyEnv::default(), boundary).tyck_k(&mut tycker, Action::ana(AnnId::Set));

        assert!(result.is_err());
        let [entry] = tycker.errors.as_slice() else { panic!("expected one type-checking error") };
        assert!(matches!(entry.error, TyckError::SortMismatch));
        assert_eq!(entry.stack.len(), 1);
        assert!(matches!(
            entry.stack.front(),
            Some(TyckTask::Term(term, Switch::Ana(AnnId::Set))) if *term == hole
        ));
    }
}
