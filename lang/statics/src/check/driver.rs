//! Checker lifecycle, allocation sites, finalization, and diagnostic guards.

use super::*;
use crate::check::intrinsics::InternalTerm;
use crate::check::judgment::Action;
use crate::check::projection::DeferredEnvMaterializationCache;
use crate::check::source::{
    CheckedSource, CheckedTermRepository, InferenceRegion, RejectedSource, SourceCheckOutcome,
    TyckDiagnostics, TyckObservation,
};

impl<'a> Tycker<'a> {
    /// Create a type checker with fresh statics arenas.
    pub fn new(
        db: &'a dyn crate::query::TyckDb, data: crate::query::ScopedData<'a>, spans: &'a SpanArena,
        prim: &'a PrimDefs, scoped: &'a ScopedArena,
    ) -> Self {
        let mut statics = StaticsArena::default();
        statics.reserve(scoped);
        let source_contexts = TermContexts::collect(scoped, data.root(db));
        Self {
            allocator: DerivedAllocator::new(),
            db,
            data,
            spans,
            prim,
            scoped,
            source_contexts,
            statics,
            tasks: rpds::VectorSync::new_sync(),
            check_counts: ArenaAssoc::default(),
            checked_terms: CheckedTermRepository::default(),
            metas: rpds::VectorSync::new_sync(),
            field_materializations: DeferredEnvMaterializationCache::default(),
            completion: None,
            lub_probe: None,
            errors: Vec::new(),
            observations: Vec::new(),
            pending_foreign_imports: Vec::new(),
        }
    }

    pub(crate) fn invalidate_field_materializations(&mut self) {
        self.field_materializations.clear();
    }

    pub(super) fn record_seal(&mut self, witness: ss::AbstId, definition: ss::TypeId) {
        self.statics.seals.insert_new(witness, definition);
        self.invalidate_field_materializations();
    }

    /// Resolve a source or elaboration-generated definition name.
    pub fn def_name(&self, id: &su::DefId) -> &su::VarName {
        self.statics.def_name(self.scoped, id)
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
        let root = self.run_judgments_k(root);
        self.finish_judgments();
        let root = root?;
        self.finish_check_k()?;
        self.elaborate_static_root_k(root)?;
        Ok(root)
    }

    /// Release transient judgment inputs and right-size source facts before
    /// hole resolution and normalization begin.
    pub(crate) fn finish_judgments(&mut self) {
        self.source_contexts = TermContexts::default();
        self.statics.shrink_source_provenance();
    }

    pub(super) fn source_free_variables(&self, term: &su::TermId) -> &su::CoContext {
        self.source_contexts.at(term)
    }

    /// The occurrence of the innermost allocation site, carried by the
    /// checker's allocator. Producer queries key their identifiers on it so
    /// re-checked entities derive distinct ids.
    pub fn site_occurrence(&self) -> u32 {
        self.allocator.current_site().2
    }

    /// Derive one identifier in the producer allocation family without
    /// retaining a salsa query solely to own that identifier.
    pub(super) fn query_derived_id<Id: ArenaId>(&self, slot: u32) -> Id {
        let (space, raw, occurrence) = self.allocator.current_site();
        derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, space, raw, occurrence), slot)
    }

    /// The innermost allocation site as a salsa key, for queries of auxiliary
    /// entities that allocate at the enclosing entity's site.
    pub fn query_site(&self) -> crate::query::InternedSite<'a> {
        let (space, raw, occurrence) = self.allocator.current_site();
        crate::query::InternedSite::new(self.db, space, raw, occurrence)
    }

    /// Consume the checker and retain the typed identity of a complete source term.
    pub fn check_source(
        self, root: su::TermId,
    ) -> std::result::Result<CheckedSource, TyckDiagnostics> {
        self.check_source_outcome(root).into_result()
    }

    /// Check a source while retaining static facts from a rejected term.
    pub fn check_source_outcome(mut self, root: su::TermId) -> SourceCheckOutcome {
        // The intrinsic singletons are query-owned: materialize them before any
        // judgment so every `Construct::build` cache read hits.
        InternalTerm::fill_intrinsics(&mut self);
        match self.run_source_k(root) {
            | Ok(root) => {
                self.strip_checker_state();
                SourceCheckOutcome::Checked(CheckedSource {
                    statics: std::sync::Arc::new(self.statics),
                    root,
                    observations: self.observations,
                })
            }
            | Err(KontFailure) => {
                let diagnostics = self.error_diagnostics();
                self.strip_checker_state();
                SourceCheckOutcome::Rejected(RejectedSource {
                    statics: std::sync::Arc::new(self.statics),
                    diagnostics,
                    observations: self.observations,
                })
            }
        }
    }

    pub(crate) fn finish_check_k(&mut self) -> ResultKont<()> {
        self.resolve_holes_and_collect();
        self.normalize_and_validate_k()
    }

    pub(crate) fn elaborate_static_root_k(&mut self, root: TermAnnId) -> ResultKont<()> {
        let elaboration = crate::elaborate::static_values::StaticElaborator::run(self, root)?;
        self.statics.static_elaboration = Some(elaboration);
        Ok(())
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
            self.validate_foreign_imports();
            // Retain one normalized classifier per distinct top annotation so
            // editor facts can answer without the occurrence payload.
            self.statics.retain_normalized_annotations();
        }
        if self.errors.is_empty() {
            let blame = std::panic::Location::caller();
            let coverage = CoverageChecker::new(&self.statics).validate();
            self.statics.coverage_errors = coverage.clone();
            self.errors.extend(coverage.into_iter().map(|error| TyckErrorEntry {
                error: TyckError::Coverage(error),
                blame,
                stack: rpds::VectorSync::new_sync(),
            }));
        }
        if !self.errors.is_empty() {
            Err(KontFailure)?
        }
        Ok(())
    }

    /// Intern one typing environment into the arena's environment cache and
    /// record it as the environment of one type node.
    pub(crate) fn store_env(&mut self, id: TypeId, env: &TyEnv) {
        let env = self.statics.intern_env(env);
        self.statics.env_type.insert_new(id, env);
    }

    /// Drop the checker-internal typing environments from the finished arena.
    ///
    /// Environments are consumed entirely by the checking phases: judgment
    /// synthesis, hole resolution, and normalization. No downstream pass and no
    /// editor query reads them, but they are the largest retained structure in a
    /// standard-library-sized arena, so they are stripped before the outcome
    /// leaves the checker.
    pub(crate) fn strip_checker_state(&mut self) {
        self.statics.types_pre.strip_kind_index();
        self.statics.env_kpat = Default::default();
        self.statics.env_tpat = Default::default();
        self.statics.env_type = Default::default();
        self.statics.env_interner = Default::default();
        self.statics.env_vpat = Default::default();
        self.statics.env_value = Default::default();
        self.statics.env_compu = Default::default();
    }

    pub(crate) fn error_diagnostics(&self) -> TyckDiagnostics {
        use std::collections::HashSet;

        let mut seen = HashSet::new();
        let diagnostics = self
            .errors
            .iter()
            .cloned()
            .map(|entry| self.error_entry_diagnostic(entry))
            .filter(|diagnostic| seen.insert(diagnostic.clone()))
            .collect::<Vec<_>>();
        TyckDiagnostics { diagnostics: std::sync::Arc::from(diagnostics) }
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
    pub(super) fn collect_hole_solutions(&mut self) {
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
        pub(crate) fn source_guarded<R>(&mut self, with: impl FnOnce(&mut Self) -> R) -> R {
            let stack = std::mem::take(&mut self.tasks);
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
