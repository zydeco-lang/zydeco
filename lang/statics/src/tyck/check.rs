use derive_more::{AsMut, AsRef, Deref};
use {
    super::{
        arena::StaticsScope,
        env::{MonadicTypeBasis, TyEnv},
        syntax::{AbstId, AnnId, FillId, Fillable, PatAnnId, StaticsArena, TermAnnId, TyEnvT},
        *,
    },
    crate::{
        surface_syntax::{PrimDefs, ScopedArena, SpanArena},
        *,
    },
    zydeco_utils::prelude::{ArenaAccess, IdAllocator},
};

/// Type-checking driver that consumes scoped syntax and produces typed arenas.
#[derive(AsRef, AsMut)]
pub struct Tycker<'a> {
    /// Sequential issuer scoped to this type-checking run.
    #[as_mut(IdAllocator<StaticsScope>)]
    allocator: IdAllocator<StaticsScope>,
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
    /// meta stack
    pub metas: im::Vector<su::Meta>,
    /// a writer monad for error handling
    pub errors: Vec<TyckErrorEntry>,
    hole_solution_output: HoleSolutionOutput,
}

pub type TyckReports =
    Vec<ariadne::Report<'static, (zydeco_utils::span::PathDisplay, std::ops::Range<usize>)>>;

/// How a type-checking run exposes inferred solutions for source holes.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub enum HoleSolutionOutput {
    #[default]
    Stdout,
    Silent,
}

/// The typed result of checking one complete source term.
pub struct CheckedSource {
    pub statics: StaticsArena,
    pub root: TermAnnId,
}

/// A failed source check together with the static facts established before
/// the failure.
pub struct RejectedSource {
    pub statics: StaticsArena,
    pub reports: TyckReports,
}

/// The recoverable result of checking one complete source term.
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
        let inherited = tycker.statics.fills.iter().map(|(fill, _)| fill).collect();
        Self { inherited }
    }

    fn close_k(self, tycker: &mut Tycker<'_>) -> ResultKont<()> {
        let candidates = tycker
            .statics
            .fills
            .iter()
            .filter_map(|(fill, site)| {
                (site.is_pattern() && !self.inherited.contains(&fill)).then_some(fill)
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

impl Tycker<'_> {
    fn pattern_has_payload_annotation(&self, pattern: su::PatId) -> bool {
        match self.scoped.pats[&pattern] {
            | su::Pattern::Ann(_) => true,
            | su::Pattern::Named(su::Named(_, inner)) => self.pattern_has_payload_annotation(inner),
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

// Todo: implement a better coverage checker
// Todo: use hole solution to implement the confluence checker (well-formedness checker)

impl<'a> Tycker<'a> {
    /// Create a type checker with fresh statics arenas.
    pub fn new(spans: &'a SpanArena, prim: &'a PrimDefs, scoped: &'a mut ScopedArena) -> Self {
        Self {
            allocator: IdAllocator::new(),
            spans,
            prim,
            scoped,
            statics: StaticsArena::default(),
            tasks: im::Vector::new(),
            metas: im::Vector::new(),
            errors: Vec::new(),
            hole_solution_output: HoleSolutionOutput::default(),
        }
    }

    /// Select how inferred source-hole solutions are exposed by this run.
    pub fn with_hole_solution_output(mut self, output: HoleSolutionOutput) -> Self {
        self.hole_solution_output = output;
        self
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
    pub fn run_source_k(&mut self, root: su::TermId) -> ResultKont<TermAnnId> {
        let env = TyEnvT::new(Default::default(), ());
        let inference = InferenceRegion::enter(self);
        let root = env.mk(root).tyck_k(self, Action::syn())?;
        if matches!(root, TermAnnId::Hole(_)) {
            self.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
        }
        inference.close_k(self)?;
        self.finish_check_k()?;
        Ok(root)
    }

    /// Consume the checker and retain the typed identity of a complete source term.
    pub fn check_source(self, root: su::TermId) -> std::result::Result<CheckedSource, TyckReports> {
        self.check_source_outcome(root).into_result()
    }

    /// Check a source while retaining static facts from a rejected term.
    pub fn check_source_outcome(mut self, root: su::TermId) -> SourceCheckOutcome {
        match self.run_source_k(root) {
            | Ok(root) => {
                SourceCheckOutcome::Checked(CheckedSource { statics: self.statics, root })
            }
            | Err(()) => {
                let reports = self.error_reports();
                SourceCheckOutcome::Rejected(RejectedSource { statics: self.statics, reports })
            }
        }
    }

    fn finish_check_k(&mut self) -> ResultKont<()> {
        // before we go, resolve all holes with solutions (including nested ones)
        self.do_resolve_holes();
        // and also, print all hole solutions as a reference for the user
        self.do_print_hole_solutions();
        // normalize all kinds
        {
            let kind_ids: Vec<_> =
                self.statics.kinds_pre.iter().map(|(id, _)| id.to_owned()).collect();
            for id in kind_ids {
                id.do_normalize_filled_k(self)?;
            }
        }
        // normalize all types
        {
            let type_ids: Vec<_> =
                self.statics.types_pre.iter().map(|(id, _)| id.to_owned()).collect();
            for id in type_ids {
                id.do_normalize_filled_k(self)?;
            }
        }
        if !self.errors.is_empty() {
            Err(())?
        }
        Ok(())
    }

    fn error_reports(&self) -> TyckReports {
        use std::collections::HashSet;

        let mut seen = HashSet::new();
        self.errors
            .iter()
            .filter(|error| {
                seen.insert((error.blame.file(), error.blame.line(), error.blame.column()))
            })
            .cloned()
            .map(|entry| self.error_entry_report(entry))
            .collect()
    }

    /// Resolve all holes with solutions (including nested ones).
    #[inline]
    pub fn do_resolve_holes(&mut self) {
        let type_ids: Vec<_> = self.statics.types_pre.iter().map(|(id, _)| id.to_owned()).collect();
        for id in type_ids {
            let (solu, mut fills) = match id.solution_k(self) {
                | Ok(res) => res,
                | Err(()) => continue,
            };
            if !fills.is_empty() {
                fills.sort_unstable();
                fills.dedup();
                // keep running tycker even after unsuccessful solving hole
                let _: ResultKont<()> =
                    self.err_k(TyckError::MissingSolution(fills), std::panic::Location::caller());
            }
            let ty = self.statics.types_pre[&solu].to_owned();
            self.statics.types_pre.replace_existing(id, ty);
        }
    }
    /// Print all hole solutions as a reference for the user.
    #[inline]
    pub fn do_print_hole_solutions(&self) {
        if self.hole_solution_output == HoleSolutionOutput::Silent {
            return;
        }
        if self.statics.fill_hints.len() > 0 {
            println!("Hole Solutions:");
        }
        for (id, ()) in &self.statics.fill_hints {
            let site = self.statics.fills[id];
            let site_text = {
                use zydeco_surface::scoped::fmt::*;
                match site {
                    | ss::InferenceSite::Term(term) => term.ugly(&Formatter::new(self.scoped)),
                    | ss::InferenceSite::Pattern(pattern) => {
                        pattern.ugly(&Formatter::new(self.scoped))
                    }
                }
            };
            let site_span = {
                use zydeco_syntax::*;
                match site {
                    | ss::InferenceSite::Term(term) => term.span(self),
                    | ss::InferenceSite::Pattern(pattern) => pattern.span(self),
                }
            };
            let site_solu = match self.statics.solus.get(id) {
                | Some(ann) => {
                    use super::fmt::*;
                    ann.ugly(&Formatter::new(self.scoped, &self.statics))
                }
                | None => "???".to_string(),
            };
            println!(
                "{} {} {} : {}",
                site_text,
                {
                    use colored::Colorize;
                    "@".green()
                },
                site_span,
                site_solu
            );
        }
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

        /// Push an error entry into the error list.
        #[inline]
        fn push_err_entry_k<T>(&mut self, entry: TyckErrorEntry) -> ResultKont<T> {
            self.errors.push(entry);
            Err(())
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
struct InternalTerm(su::Internal);

impl InternalTerm {
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
            | su::Internal::Int => self.builtin_type_k(tycker, env, ss::BuiltinTypeRole::Int)?,
            | su::Internal::Char => self.builtin_type_k(tycker, env, ss::BuiltinTypeRole::Char)?,
            | su::Internal::String => {
                self.builtin_type_k(tycker, env, ss::BuiltinTypeRole::String)?
            }
            | su::Internal::OS => self.builtin_type_k(tycker, env, ss::BuiltinTypeRole::OS)?,
            | su::Internal::Monad | su::Internal::Algebra => tycker.err_k(
                TyckError::Expressivity(
                    "`Monad` and `Algebra` are ordinary library bindings, not intrinsic terms",
                ),
                std::panic::Location::caller(),
            )?,
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
struct PatternSkolems(im::HashMap<su::PatId, ss::AbstId>);

impl PatternSkolems {
    fn new(assignments: impl IntoIterator<Item = (su::PatId, ss::AbstId)>) -> Self {
        Self(assignments.into_iter().collect())
    }

    fn get(&self, pattern: &su::PatId) -> Option<ss::AbstId> {
        self.0.get(pattern).copied()
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
            | su::Pattern::Hole(_)
            | su::Pattern::Var(_)
            | su::Pattern::Ctor(_)
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
        let assignments = self.inner.assignments_k(tycker, &self.info, self.inner.pattern)?;
        Ok(PatternSkolems::new(assignments))
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
                    let abst = tycker.statics.absts.alloc(());
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
                let abst = tycker.statics.absts.alloc(());
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
            self.tyck_inner_k(tycker, action)
        })
    }

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out> {
        let PatternAction { switch, skolems } = action;
        // // Debug: print
        // {
        //     use colored::Colorize;
        //     use zydeco_surface::scoped::fmt::*;
        //     println!("{}", "=".repeat(80));
        //     println!(
        //         "\t{}",
        //         &tycker.scoped.ctxs_pat_local[&self.inner].ugly(&Formatter::new(&tycker.scoped))
        //     );
        //     println!("   {}\t{}", "|-".green(), self.inner.ugly(&Formatter::new(&tycker.scoped)));
        //     println!("{}", "=".repeat(80));
        //     println!();
        // }
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
                    | Switch::Syn => tycker
                        .err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?,
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
                            | PatAnnId::Kind(_) => tycker.err_k(
                                TyckError::Expressivity("named kind components are not supported"),
                                std::panic::Location::caller(),
                            )?,
                            | PatAnnId::Type(inner, inner_kind) => {
                                let named_kind = Alloc::alloc(
                                    tycker,
                                    ss::Label(name.clone(), inner_kind),
                                    (),
                                    &(),
                                );
                                let named = Alloc::alloc(
                                    tycker,
                                    ss::Named(name, inner),
                                    named_kind,
                                    &self.info,
                                );
                                checked.with_annotation(PatAnnId::Type(named, named_kind))
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
            | Pat::Ctor(pat) => match switch {
                | Switch::Syn => {
                    tycker.err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?
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
                            TyckError::MissingDataArm(ctor.clone()),
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
            | Pat::Triv(su::Triv) => {
                let unit = ss::UnitTy.build(tycker, &self.info);
                let ann = match switch {
                    | Switch::Syn => unit,
                    | Switch::Ana(AnnId::Type(ana)) => Lub::lub_k(unit, ana, tycker)?,
                    | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                };
                let triv = Alloc::alloc(tycker, ss::Triv, ann, &self.info);
                self.mk(PatternCheck::new(PatAnnId::Value(triv, ann)))
            }
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
                        let vtype = ss::VType.build(tycker, &pattern_env);
                        let annotation =
                            annotations.into_iter().rev().fold(tail_annotation, |tail, head| {
                                Alloc::alloc(tycker, ss::Prod(head, tail), vtype, &pattern_env)
                            });
                        let pattern =
                            Alloc::alloc(tycker, ss::ConsN(output, tail), annotation, &self.info);
                        TyEnvT::new(
                            pattern_env,
                            PatternCheck::with_opened(PatAnnId::Value(pattern, annotation), opened),
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
            self.tyck_inner_k(tycker, Action { switch })
        })
    }

    fn tyck_inner_k(
        &self, tycker: &mut Tycker<'a>, Action { mut switch }: Self::Action,
    ) -> ResultKont<Self::Out> {
        // // Debug: print
        // {
        //     use zydeco_surface::scoped::fmt::*;
        //     println!("{}", "=".repeat(80));
        //     // use colored::Colorize;
        //     // println!(
        //     //     "\t{}",
        //     //     &tycker.scoped.coctxs_term_local[&self.inner].ugly(&Formatter::new(&tycker.scoped))
        //     // );
        //     // println!("   {}\t{}", "-|".green(), self.inner.ugly(&Formatter::new(&tycker.scoped)));

        //     use zydeco_syntax::SpanView;
        //     println!(
        //         "{} @ ({})",
        //         self.inner.ugly(&Formatter::new(&tycker.scoped)),
        //         self.inner.span(tycker)
        //     );
        //     match switch {
        //         | Switch::Syn => {
        //             println!("\t>> (syn)")
        //         }
        //         | Switch::Ana(ana) => {
        //             use crate::fmt::*;
        //             println!(
        //                 "\t>> (ana: {})",
        //                 ana.ugly(&Formatter::new(&tycker.scoped, &tycker.statics))
        //             )
        //         }
        //     }
        //     println!("{}", "=".repeat(80));
        //     println!();
        // }

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

        // // Debug: print
        // {
        //     use zydeco_surface::scoped::fmt::*;
        //     use zydeco_syntax::SpanView;
        //     println!(
        //         "{} @ ({})",
        //         self.inner.ugly(&Formatter::new(&tycker.scoped)),
        //         self.inner.span(tycker)
        //     );
        //     match switch {
        //         | Switch::Syn => {
        //             println!("\t>> (syn)")
        //         }
        //         | Switch::Ana(ana) => {
        //             use crate::fmt::*;
        //             println!(
        //                 "\t>> (ana: {})",
        //                 ana.ugly(&Formatter::new(&tycker.scoped, &tycker.statics))
        //             )
        //         }
        //     }
        // }

        use su::Term as Tm;
        let out_ann = match tycker.scoped.terms[&self.inner].to_owned() {
            | Tm::Meta(term) => {
                let su::MetaT(meta, term) = term;
                let res = self.mk(term).tyck_k(tycker, Action::switch(switch))?;
                if let Some(role) = meta.as_builtin() {
                    BuiltinAttachment::new(role, res).register_k(tycker, &self.info)?;
                }
                if meta.is("debug") {
                    print!("[debug printing] ");
                    for argument in meta.arguments() {
                        print!("{argument}");
                    }
                    match res {
                        | TermAnnId::Hole(fill) => {
                            println!(" (hole): {}", fill.concise());
                        }
                        | TermAnnId::Kind(kind) => {
                            println!(" (kind): {}", tycker.pretty_statics(kind));
                        }
                        | TermAnnId::Type(ty, kd) => {
                            println!(
                                " (type):{}\nof kind:{}",
                                tycker.pretty_statics_nested(ty, "\t"),
                                tycker.pretty_statics_nested(kd, "\t"),
                            );
                        }
                        | TermAnnId::Value(val, ty) => {
                            println!(
                                " (value):{}\nof type:{}",
                                tycker.pretty_statics_nested(val, "\t"),
                                tycker.pretty_statics_nested(ty, "\t"),
                            );
                        }
                        | TermAnnId::Compu(compu, ty) => {
                            println!(
                                " (computation):{}\nof type:{}",
                                tycker.pretty_statics_nested(compu, "\t"),
                                tycker.pretty_statics_nested(ty, "\t"),
                            );
                        }
                    }
                }
                res
            }
            | Tm::SourceBoundary(su::SourceBoundary(term)) => {
                let inference = InferenceRegion::enter(tycker);
                let checked = self.mk(term).tyck_k(tycker, Action::switch(switch))?;
                inference.close_k(tycker)?;
                checked
            }
            | Tm::Internal(internal) => {
                InternalTerm(internal).tyck_k(tycker, &self.info, switch)?
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
                        let fill = Alloc::alloc(tycker, self.inner, (), &());
                        TermAnnId::Hole(fill)
                    }
                    | Switch::Ana(AnnId::Set) => {
                        // can't deduce kind for now
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | Switch::Ana(AnnId::Kind(kd)) => {
                        // a type hole, with a specific kind in mind
                        let fill = Alloc::alloc(tycker, self.inner, (), &());
                        let fill_out = Alloc::alloc(tycker, fill, kd, &self.info);
                        TermAnnId::Type(fill_out, kd)
                    }
                    | Switch::Ana(AnnId::Type(ty)) => {
                        // a hole in either value or computation; like undefined in Haskell
                        let kd = tycker.statics.annotations_type[&ty].to_owned();
                        match tycker.kind_filled_k(&kd)?.to_owned() {
                            | ss::Kind::VType(ss::VType) => {
                                let hole = Alloc::alloc(tycker, self.inner, (), &());
                                hole.fill_k(tycker, ty.into())?;
                                tycker.statics.fill_hints.insert_new(hole, ());
                                let hole = Alloc::alloc(tycker, ss::Hole, ty, &self.info);
                                TermAnnId::Value(hole, ty)
                            }
                            | ss::Kind::CType(ss::CType) => {
                                let hole = Alloc::alloc(tycker, self.inner, (), &());
                                hole.fill_k(tycker, ty.into())?;
                                tycker.statics.fill_hints.insert_new(hole, ());
                                let hole = Alloc::alloc(tycker, ss::Hole, ty, &self.info);
                                TermAnnId::Compu(hole, ty)
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
                        let ann = self.info[&def];
                        let AnnId::Kind(kd) = ann else { unreachable!() };
                        TermAnnId::Kind(kd)
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
                    | AnnId::Type(ty) => {
                        let val = Alloc::alloc(tycker, def, ty, &self.info);
                        TermAnnId::Value(val, ty)
                    }
                }
            }
            | Tm::Named(term) => {
                let su::Named(name, inner) = term;
                match switch {
                    | Switch::Syn => match self.mk(inner).tyck_k(tycker, Action::syn())? {
                        | TermAnnId::Type(inner, kd) => {
                            let named_kind =
                                Alloc::alloc(tycker, ss::Label(name.clone(), kd), (), &());
                            let named = Alloc::alloc(
                                tycker,
                                ss::Named(name, inner),
                                named_kind,
                                &self.info,
                            );
                            TermAnnId::Type(named, named_kind)
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
                        | TermAnnId::Hole(_) => tycker
                            .err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                        | TermAnnId::Kind(_) | TermAnnId::Compu(_, _) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
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
                        | TermAnnId::Kind(inner) => {
                            let label = Alloc::alloc(tycker, ss::Label(name, inner), (), &());
                            TermAnnId::Kind(label)
                        }
                        | TermAnnId::Type(inner, kind) => {
                            let vtype = ss::VType.build(tycker, &self.info);
                            Lub::lub_k(vtype, kind, tycker)?;
                            let label =
                                Alloc::alloc(tycker, ss::Label(name, inner), vtype, &self.info);
                            TermAnnId::Type(label, vtype)
                        }
                        | TermAnnId::Hole(_) => tycker
                            .err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                        | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
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
            | Tm::Triv(su::Triv) => {
                let unit = ss::UnitTy.build(tycker, &self.info);
                let ann = match switch {
                    | Switch::Syn => unit,
                    | Switch::Ana(AnnId::Type(ana)) => Lub::lub_k(unit, ana, tycker)?,
                    | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                };
                let triv = Alloc::alloc(tycker, ss::Triv, ann, &self.info);
                TermAnnId::Value(triv, ann)
            }
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
                        let (tail, ann) = match checked {
                            | TermAnnId::Value(tail, tail_ty) => (tail, tail_ty),
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
                        let vtype = ss::VType.build(tycker, &self.info);
                        let ann = annotations.into_iter().rev().fold(ann, |ann, head| {
                            Alloc::alloc(tycker, ss::Prod(head, ann), vtype, &self.info)
                        });
                        let cons = Alloc::alloc(tycker, ss::ConsN(output, tail), ann, &self.info);
                        TermAnnId::Value(cons, ann)
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
                            | PatAnnId::Kind(_) => tycker.err_k(
                                TyckError::Expressivity(
                                    "functions cannot abstract over the meta-level `Set`",
                                ),
                                std::panic::Location::caller(),
                            )?,
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
                                        let ann =
                                            Alloc::alloc(tycker, ss::Arrow(kd, body_kd), (), &());
                                        // recover abst in ty
                                        let ty = if let (Some(def), _kd) =
                                            tpat.try_destruct_def(tycker)
                                        {
                                            let def_ty = Alloc::alloc(tycker, def, kd, &self.info);
                                            ty.subst_abst_k(tycker, (abst, def_ty))?
                                        } else {
                                            ty
                                        };
                                        let abs = Alloc::alloc(
                                            tycker,
                                            ss::Abs(tpat, ty),
                                            ann,
                                            &self.info,
                                        );
                                        TermAnnId::Type(abs, ann)
                                    }
                                    | TermAnnId::Compu(compu, body_ty) => {
                                        // a type-polymorphic function
                                        let ctype = ss::CType.build(tycker, &self.info);
                                        let binder =
                                            ss::TypeBinder { pattern: tpat, witness: abst };
                                        let ann = Alloc::alloc(
                                            tycker,
                                            ss::Forall(binder, body_ty),
                                            ctype,
                                            &self.info,
                                        );
                                        let abs = Alloc::alloc(
                                            tycker,
                                            ss::Abs(tpat, compu),
                                            ann,
                                            &self.info,
                                        );
                                        TermAnnId::Compu(abs, ann)
                                    }
                                    | TermAnnId::Value(value, body_ty) => {
                                        let vtype = ss::VType.build(tycker, &self.info);
                                        let binder =
                                            ss::TypeBinder { pattern: tpat, witness: abst };
                                        let ann = Alloc::alloc(
                                            tycker,
                                            ss::ValueForall(binder, body_ty),
                                            vtype,
                                            &self.info,
                                        );
                                        let abs: ss::ValueId = Alloc::alloc(
                                            tycker,
                                            ss::Abs(tpat, value),
                                            ann,
                                            &self.info,
                                        );
                                        TermAnnId::Value(abs, ann)
                                    }
                                    | TermAnnId::Hole(_) | TermAnnId::Kind(_) => tycker.err_k(
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?,
                                }
                            }
                            | PatAnnId::Value(vpat, ty) => {
                                // A value-pattern abstraction is pure when its body is a value
                                // and computational when its body is a computation.
                                let body_out_ann = TyEnvT::new(pat_out_ann.info.clone(), body)
                                    .tyck_k(tycker, Action::syn())?;
                                match body_out_ann {
                                    | TermAnnId::Value(value, body_ty) => {
                                        let vtype = ss::VType.build(tycker, &self.info);
                                        let ann = match pat_out_ann.package_telescope_k(tycker)? {
                                            | None => {
                                                pat_out_ann.close_scope_k(tycker, body_ty)?;
                                                Alloc::alloc(
                                                    tycker,
                                                    ss::ValueArrow(ty, body_ty),
                                                    vtype,
                                                    &self.info,
                                                )
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
                                                let signature = Alloc::alloc(
                                                    tycker, pack_pi, vtype, &self.info,
                                                );
                                                signature.constrain_to_scope_k(
                                                    tycker,
                                                    self.info.skolem_scope(),
                                                )?;
                                                signature
                                            }
                                        };
                                        let abs: ss::ValueId = Alloc::alloc(
                                            tycker,
                                            ss::Abs(vpat, value),
                                            ann,
                                            &self.info,
                                        );
                                        TermAnnId::Value(abs, ann)
                                    }
                                    | TermAnnId::Compu(compu, body_ty) => {
                                        let ctype = ss::CType.build(tycker, &self.info);
                                        let ann = match pat_out_ann.package_telescope_k(tycker)? {
                                            | None => {
                                                pat_out_ann.close_scope_k(tycker, body_ty)?;
                                                Alloc::alloc(
                                                    tycker,
                                                    ss::Arrow(ty, body_ty),
                                                    ctype,
                                                    &self.info,
                                                )
                                            }
                                            | Some(witnesses) => {
                                                let pack_pi = ss::PackPi {
                                                    domain: ty,
                                                    witnesses,
                                                    codomain: body_ty,
                                                };
                                                tycker.validate_builtin_signature_k(&pack_pi)?;
                                                let signature = Alloc::alloc(
                                                    tycker, pack_pi, ctype, &self.info,
                                                );
                                                signature.constrain_to_scope_k(
                                                    tycker,
                                                    self.info.skolem_scope(),
                                                )?;
                                                signature
                                            }
                                        };
                                        let abs = Alloc::alloc(
                                            tycker,
                                            ss::Abs(vpat, compu),
                                            ann,
                                            &self.info,
                                        );
                                        TermAnnId::Compu(abs, ann)
                                    }
                                    | TermAnnId::Hole(_)
                                    | TermAnnId::Kind(_)
                                    | TermAnnId::Type(_, _) => tycker.err_k(
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?,
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
                                let app: ss::ValueId =
                                    Alloc::alloc(tycker, ss::App(f_out, a_ty), ty_out, &self.info);
                                TermAnnId::Value(app, ty_out)
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
                                let app: ss::ValueId =
                                    Alloc::alloc(tycker, ss::App(f_out, a_out), ty_out, &self.info);
                                TermAnnId::Value(app, ty_out)
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
                                // // Debug: print
                                // {
                                //     use crate::fmt::*;
                                //     println!(
                                //         "Applying\n\t{}\nwith\n\t{}\ngetting\n\t{}\n\n",
                                //         f_ty.ugly(&Formatter::new(&tycker.scoped, &tycker.statics)),
                                //         _a_ty
                                //             .ugly(&Formatter::new(&tycker.scoped, &tycker.statics)),
                                //         ty_out
                                //             .ugly(&Formatter::new(&tycker.scoped, &tycker.statics))
                                //     );
                                // }
                                let app =
                                    Alloc::alloc(tycker, ss::App(f_out, a_out), ty_out, &self.info);
                                TermAnnId::Compu(app, ty_out)
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
                                // // Debug: print
                                // {
                                //     println!(
                                //         "Substituting\n\t{}\nwith\n\t{}\ngetting\n\t{}\n\n",
                                //         tycker.dump_statics(f_ty),
                                //         tycker.dump_statics(a_ty),
                                //         tycker.dump_statics(body_ty_subst),
                                //     );
                                // }
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
                                let app =
                                    Alloc::alloc(tycker, ss::App(f_out, a_ty), ty_out, &self.info);
                                TermAnnId::Compu(app, body_ty_subst)
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
                let fix = Alloc::alloc(tycker, ss::Fix(binder, body_out), fix_ty, &self.info);
                TermAnnId::Compu(fix, fix_ty)
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
                                        let arr =
                                            Alloc::alloc(tycker, ss::Arrow(kd_1, kd_2), (), &());
                                        TermAnnId::Kind(arr)
                                    }
                                    | TermAnnId::Type(ty_2, kd_2) => {
                                        let binder =
                                            ss::TypeBinder { pattern: tpat, witness: abst };
                                        match tycker.kind_filled_k(&kd_2)?.to_owned() {
                                            | ss::Kind::VType(_) => {
                                                let forall = Alloc::alloc(
                                                    tycker,
                                                    ss::ValueForall(binder, ty_2),
                                                    kd_2,
                                                    &self.info,
                                                );
                                                TermAnnId::Type(forall, kd_2)
                                            }
                                            | ss::Kind::CType(_) => {
                                                let forall = Alloc::alloc(
                                                    tycker,
                                                    ss::Forall(binder, ty_2),
                                                    kd_2,
                                                    &self.info,
                                                );
                                                TermAnnId::Type(forall, kd_2)
                                            }
                                            | ss::Kind::Arrow(_) | ss::Kind::Label(_) => tycker
                                                .err_k(
                                                    TyckError::KindMismatch,
                                                    std::panic::Location::caller(),
                                                )?,
                                        }
                                    }
                                    | TermAnnId::Hole(_) => tycker.err_k(
                                        TyckError::MissingAnnotation,
                                        std::panic::Location::caller(),
                                    )?,
                                    | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => tycker
                                        .err_k(
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?,
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
                            | PatAnnId::Kind(_) => tycker.err_k(
                                TyckError::Expressivity(
                                    "abstract existential kinds are not supported",
                                ),
                                std::panic::Location::caller(),
                            )?,
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
                                let binder = ss::TypeBinder { pattern: tpat, witness: abst };
                                let exists = Alloc::alloc(
                                    tycker,
                                    ss::Exists::new(binder, body_ty),
                                    vtype,
                                    &self.info,
                                );
                                TermAnnId::Type(exists, vtype)
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
                                let prod =
                                    Alloc::alloc(tycker, ss::Prod(ty_1, ty_2), vtype, &self.info);
                                TermAnnId::Type(prod, vtype)
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
                                let manifest =
                                    ss::ManifestKind { binder: pattern, definition, body };
                                let manifest = Alloc::alloc(tycker, manifest, vtype, &self.info);
                                TermAnnId::Type(manifest, vtype)
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

                                let binder = ss::TypeBinder { pattern, witness };
                                let exists = Alloc::alloc(
                                    tycker,
                                    ss::Exists::with_manifest(binder, definition, body),
                                    vtype,
                                    &self.info,
                                );
                                TermAnnId::Type(exists, vtype)
                            }
                            | TermAnnId::Hole(_)
                            | TermAnnId::Value(_, _)
                            | TermAnnId::Compu(_, _) => tycker
                                .err_k(TyckError::SortMismatch, std::panic::Location::caller())?,
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
                let thunk_app_body_ty = cs::Thk(body_ty).build(tycker, &self.info);
                let thunk =
                    Alloc::alloc(tycker, ss::Thunk(body_out), thunk_app_body_ty, &self.info);
                TermAnnId::Value(thunk, thunk_app_body_ty)
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
                let force = Alloc::alloc(tycker, ss::Force(body), force_ty, &self.info);
                TermAnnId::Compu(force, force_ty)
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
                let ret_app_body_ty = cs::Ret(body_ty).build(tycker, &self.info);
                let ret = Alloc::alloc(tycker, ss::Return(body_out), ret_app_body_ty, &self.info);
                TermAnnId::Compu(ret, ret_app_body_ty)
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
                let bind = Alloc::alloc(
                    tycker,
                    ss::Bind { binder: binder_out, bindee: bindee_out, tail: tail_out },
                    bind_ty,
                    &self.info,
                );
                TermAnnId::Compu(bind, bind_ty)
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
                            let abst = tycker.statics.absts.alloc(());
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
                                let bind = Alloc::alloc(
                                    tycker,
                                    ss::Let {
                                        binder: binder_out,
                                        bindee: bindee_out,
                                        tail: tail_out,
                                    },
                                    tail_ty,
                                    &self.info,
                                );
                                TermAnnId::Value(bind, tail_ty)
                            }
                            | TermAnnId::Compu(tail_out, tail_ty) => {
                                binder_elaboration.close_scope_k(tycker, tail_ty)?;
                                let bind = Alloc::alloc(
                                    tycker,
                                    ss::Let {
                                        binder: binder_out,
                                        bindee: bindee_out,
                                        tail: tail_out,
                                    },
                                    tail_ty,
                                    &self.info,
                                );
                                TermAnnId::Compu(bind, tail_ty)
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

                use super::env::*;
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

                // <monad_impl_to_body_lift> = fn (mo: Thk (Monad M)) -> Lift(body)
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

                // fn (M : VType -> CType) -> <monad_impl_to_body_lift>
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

                // // Debug: print
                // {
                //     println!("{}", ">".repeat(40));
                //     println!("{}", tycker.pretty_statics(body));
                //     println!("{}", "=".repeat(40));
                //     println!("{}", tycker.pretty_statics(res_body));
                //     println!("{}", "<".repeat(40));
                // }

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
                let id = tycker.statics.datas.alloc(ss::Data::new(arms_vec));
                let data = Alloc::alloc(tycker, id, vtype, &self.info);
                TermAnnId::Type(data, vtype)
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
                let id = tycker.statics.codatas.alloc(ss::CoData::new(arms_vec));
                let codata = Alloc::alloc(tycker, id, ctype, &self.info);
                TermAnnId::Type(codata, ctype)
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
                        TyckError::MissingDataArm(ctor.clone()),
                        std::panic::Location::caller(),
                    )?,
                };
                let arg_out_ann = self.mk(arg).tyck_k(tycker, Action::ana(arg_ty.into()))?;
                let TermAnnId::Value(arg, _arg_ty) = arg_out_ann else { unreachable!() };
                let ctor = Alloc::alloc(tycker, ss::Ctor(ctor.to_owned(), arg), ana_ty, &self.info);
                // hint the ctor to be associated with the definition name
                tycker.statics.data_hints.insert_new(ctor, data_id);
                TermAnnId::Value(ctor, ana_ty)
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
                                let whole_term = Alloc::alloc(
                                    tycker,
                                    ss::Match { scrut, arms: matchers },
                                    ana_ty,
                                    &self.info,
                                );
                                TermAnnId::Compu(whole_term, ana_ty)
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
                    let whole_term = Alloc::alloc(
                        tycker,
                        ss::Match { scrut, arms: matchers },
                        whole_ty,
                        &self.info,
                    );
                    TermAnnId::Compu(whole_term, whole_ty)
                }
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
                use std::collections::HashMap;
                let mut arms = tycker.statics.codatas[&codata_id]
                    .clone()
                    .into_iter()
                    .collect::<HashMap<_, _>>();
                let mut comatchers_new = Vec::new();
                for su::CoMatcher { dtor, tail } in comatchers {
                    let arm_ty = match arms.remove(&dtor) {
                        | Some(arm_ty) => arm_ty,
                        | None => tycker.err_k(
                            TyckError::MissingCoDataArm(dtor.clone()),
                            std::panic::Location::caller(),
                        )?,
                    };
                    let tail_out_ann = self.mk(tail).tyck_k(tycker, Action::ana(arm_ty.into()))?;
                    let TermAnnId::Compu(tail, _ty) = tail_out_ann else {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    comatchers_new.push(ss::CoMatcher { dtor, tail });
                }
                if !arms.is_empty() {
                    tycker.err_k(
                        TyckError::NonExhaustiveCoDataArms(arms),
                        std::panic::Location::caller(),
                    )?
                }
                let whole_term =
                    Alloc::alloc(tycker, ss::CoMatch { arms: comatchers_new }, ana_ty, &self.info);
                // hint the whole computation to be associated with the codata type
                tycker.statics.codata_hints.insert_new(whole_term, codata_id);
                TermAnnId::Compu(whole_term, ana_ty)
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
                        TyckError::MissingCoDataArm(dtor.clone()),
                        std::panic::Location::caller(),
                    )?,
                };
                match switch {
                    | Switch::Syn => {
                        let whole =
                            Alloc::alloc(tycker, ss::Dtor(body, dtor), whole_ty, &self.info);
                        TermAnnId::Compu(whole, whole_ty)
                    }
                    | Switch::Ana(ana) => {
                        let AnnId::Type(ana_ty) = ana else {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        let whole_ty = Lub::lub_k(whole_ty, ana_ty, tycker)?;
                        let whole =
                            Alloc::alloc(tycker, ss::Dtor(body, dtor), whole_ty, &self.info);
                        TermAnnId::Compu(whole, whole_ty)
                    }
                }
            }
            | Tm::Proj(term) => {
                let su::Proj(head, name) = term;
                let checked = self.mk(head).tyck_k(tycker, Action::syn())?;
                match checked {
                    | TermAnnId::Type(head, head_kind) => {
                        let ss::Kind::Label(ss::Label(found, payload_kind)) =
                            tycker.kind_filled_k(&head_kind)?.to_owned()
                        else {
                            tycker.err_k(TyckError::KindMismatch, std::panic::Location::caller())?
                        };
                        if name != found {
                            tycker.err_k(
                                TyckError::NamedLabelMismatch {
                                    expected: found,
                                    found: name.clone(),
                                },
                                std::panic::Location::caller(),
                            )?
                        }
                        let payload_kind = match switch {
                            | Switch::Syn => payload_kind,
                            | Switch::Ana(AnnId::Kind(expected)) => {
                                Lub::lub_k(payload_kind, expected, tycker)?
                            }
                            | Switch::Ana(AnnId::Set | AnnId::Type(_)) => tycker
                                .err_k(TyckError::SortMismatch, std::panic::Location::caller())?,
                        };
                        let projected = head.project_named(tycker, &name, payload_kind);
                        let projected = tycker.err_p_to_k(projected)?;
                        TermAnnId::Type(projected, payload_kind)
                    }
                    | TermAnnId::Value(head, head_ty) => {
                        let head_view =
                            head_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                        let (target, projected_ty) =
                            match tycker.type_filled_k(&head_view)?.to_owned() {
                                | ss::Type::Label(ss::Label(found, projected_ty)) => {
                                    if name != found {
                                        tycker.err_k(
                                            TyckError::MissingNamedField {
                                                field: name.clone(),
                                                found: head_ty,
                                            },
                                            std::panic::Location::caller(),
                                        )?
                                    }
                                    (ss::ProjTarget::Direct, projected_ty)
                                }
                                | ss::Type::Prod(_) => {
                                    let mut next = Some(head_view);
                                    let components = std::iter::from_fn(|| {
                                        let current = next.take()?;
                                        let view = match current
                                            .unroll_k(tycker)
                                            .and_then(|ty| ty.subst_env_k(tycker, &self.info))
                                        {
                                            | Ok(view) => view,
                                            | Err(()) => return Some(Err(())),
                                        };
                                        match tycker.type_filled_k(&view) {
                                            | Ok(ss::Type::Prod(ss::Prod(item, tail))) => {
                                                next = Some(tail);
                                                Some(Ok(item))
                                            }
                                            | Ok(_) => Some(Ok(view)),
                                            | Err(()) => Some(Err(())),
                                        }
                                    })
                                    .collect::<ResultKont<Vec<_>>>()?;
                                    let matches = components
                                        .into_iter()
                                        .enumerate()
                                        .map(|(position, component)| -> ResultKont<_> {
                                            let view = component
                                                .unroll_k(tycker)?
                                                .subst_env_k(tycker, &self.info)?;
                                            Ok(match tycker.type_filled_k(&view)?.to_owned() {
                                                | ss::Type::Label(ss::Label(
                                                    found,
                                                    projected_ty,
                                                )) if found == name => {
                                                    Some((position, projected_ty))
                                                }
                                                | _ => None,
                                            })
                                        })
                                        .collect::<ResultKont<Vec<_>>>()?
                                        .into_iter()
                                        .flatten()
                                        .collect::<Vec<_>>();
                                    match matches.as_slice() {
                                        | [] => tycker.err_k(
                                            TyckError::MissingNamedField {
                                                field: name.clone(),
                                                found: head_ty,
                                            },
                                            std::panic::Location::caller(),
                                        )?,
                                        | [(position, projected_ty)] => {
                                            (ss::ProjTarget::Product(*position), *projected_ty)
                                        }
                                        | _ => tycker.err_k(
                                            TyckError::DuplicateNamedField {
                                                field: name.clone(),
                                                found: head_ty,
                                            },
                                            std::panic::Location::caller(),
                                        )?,
                                    }
                                }
                                | _ => tycker.err_k(
                                    TyckError::MissingNamedField {
                                        field: name.clone(),
                                        found: head_ty,
                                    },
                                    std::panic::Location::caller(),
                                )?,
                            };
                        let projected_ty = match switch {
                            | Switch::Syn => projected_ty,
                            | Switch::Ana(AnnId::Type(expected)) => {
                                Lub::lub_k(projected_ty, expected, tycker)?
                            }
                            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => tycker
                                .err_k(TyckError::SortMismatch, std::panic::Location::caller())?,
                        };
                        let field = ss::ResolvedField { name, target };
                        let projected =
                            Alloc::alloc(tycker, ss::Proj(head, field), projected_ty, &self.info);
                        TermAnnId::Value(projected, projected_ty)
                    }
                    | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Compu(_, _) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                }
            }
            | Tm::Lit(lit) => {
                fn literal_type_k(
                    tycker: &mut Tycker<'_>, env: &ss::TyEnv, switch: Switch<AnnId>,
                    role: ss::BuiltinTypeRole,
                ) -> ResultKont<ss::TypeId> {
                    match switch {
                        | Switch::Syn => BuiltinTypeResolution(role).resolve_k(tycker, env),
                        | Switch::Ana(annotation) => {
                            let AnnId::Type(ty) = annotation else {
                                tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?
                            };
                            Ok(ty)
                        }
                    }
                }
                use zydeco_syntax::Literal as Lit;
                let (lit, ty) = match lit {
                    | Lit::Int(i) => {
                        let ty =
                            literal_type_k(tycker, &self.info, switch, ss::BuiltinTypeRole::Int)?;
                        (Lit::Int(i), ty)
                    }
                    | Lit::String(s) => {
                        let ty = literal_type_k(
                            tycker,
                            &self.info,
                            switch,
                            ss::BuiltinTypeRole::String,
                        )?;
                        (Lit::String(s), ty)
                    }
                    | Lit::Char(c) => {
                        let ty =
                            literal_type_k(tycker, &self.info, switch, ss::BuiltinTypeRole::Char)?;
                        (Lit::Char(c), ty)
                    }
                };
                let lit = Alloc::alloc(tycker, lit, ty, &self.info);
                TermAnnId::Value(lit, ty)
            }
        };

        if let Some(out) = out_ann.as_term() {
            // maintain back mapping
            tycker.statics.terms.ensure(self.inner, out);

            // check if the term is global
            let coctx = tycker.scoped.coctxs_term_local[&self.inner].to_owned();

            let mut non_global = Vec::new();
            for def in coctx.into_iter() {
                if tycker.statics.global_defs.get(&def).is_none() {
                    non_global.push(def);
                }
            }
            let global = non_global.is_empty();
            // // a better way to check if the term is global
            // let global = 'out: {
            //     for (def, ()) in coctx.into_iter() {
            //         if !tycker.statics.global_defs.get(&def).is_some() {
            //             break 'out false;
            //         }
            //     }
            //     true
            // };

            if global {
                tycker.statics.global_terms.ensure(out);
            }
            // if !global {
            //     // Debug: print
            //     {
            //         println!(
            //             "non-global term: {}",
            //             tycker.dump_statics(out)
            //         );
            //         println!(
            //             "non-global defs: {}",
            //             non_global
            //                 .iter()
            //                 .map(|def| tycker.dump_statics(def))
            //                 .collect::<Vec<_>>()
            //                 .join(", ")
            //         );
            //         println!();
            //     }
            // }
        }

        Ok(out_ann)
    }
}

#[cfg(test)]
mod source_boundary_tests {
    use super::*;
    use crate::tyck::env::TyEnv;

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
        let mut tycker = Tycker::new(&spans, &prim, &mut scoped);
        let env = TyEnv::default();
        let expected = Alloc::alloc(&mut tycker, ss::VType, (), &());
        let checked =
            TyEnvT::new(env, boundary).tyck_k(&mut tycker, Action::ana(expected.into())).unwrap();

        assert!(matches!(checked, TermAnnId::Type(_, kind) if kind == expected));
        assert!(tycker.errors.is_empty());
    }
}
