//! Checker lifecycle, allocation sites, finalization, and diagnostic guards.

use super::*;
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
        scoped: &'a ScopedArena,
    ) -> Self {
        let mut statics = StaticsArena::default();
        let intrinsics = IntrinsicStatics::new(&mut statics);
        Self::with_preloaded(
            db,
            data,
            spans,
            scoped,
            statics,
            intrinsics,
            PreloadedState::default(),
        )
    }

    /// Create a type checker seeded with preloaded dependency interfaces or bodies.
    pub(crate) fn with_preloaded(
        db: &'a dyn crate::query::TyckDb, data: crate::query::ScopedData<'a>, spans: &'a SpanArena,
        scoped: &'a ScopedArena, mut statics: StaticsArena, intrinsics: IntrinsicStatics,
        preloaded: PreloadedState,
    ) -> Self {
        statics.reserve(scoped);
        let source_contexts = TermContexts::collect(scoped, data.root(db));
        Self::build_tycker(db, data, spans, scoped, statics, intrinsics, source_contexts, preloaded)
    }

    /// Reconstruct a post-judgment checker over an already-assembled `StaticsArena`
    /// without scanning `TermContexts` again.
    pub(crate) fn from_assembled(
        db: &'a dyn crate::query::TyckDb, data: crate::query::ScopedData<'a>, spans: &'a SpanArena,
        scoped: &'a ScopedArena, statics: StaticsArena, intrinsics: IntrinsicStatics,
    ) -> Self {
        Self::build_tycker(
            db,
            data,
            spans,
            scoped,
            statics,
            intrinsics,
            TermContexts::default(),
            PreloadedState::default(),
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn build_tycker(
        db: &'a dyn crate::query::TyckDb, data: crate::query::ScopedData<'a>, spans: &'a SpanArena,
        scoped: &'a ScopedArena, statics: StaticsArena, intrinsics: IntrinsicStatics,
        source_contexts: TermContexts, preloaded: PreloadedState,
    ) -> Self {
        Self {
            allocator: DefGenAllocator::new(db.generated_def_spaces(), data.root(db)),
            db,
            data,
            spans,
            scoped,
            source_contexts,
            statics,
            intrinsics,
            tasks: Vec::new(),
            visited_terms: Default::default(),
            kind_bindings: Default::default(),
            type_bindings: Default::default(),
            settled_pattern_annotations: ArenaAssoc::default(),
            checked_terms: CheckedTermRepository::default(),
            fill_trail: Vec::new(),
            fill_depth: 0,
            solus_epoch: 0,
            lub_cache: Default::default(),
            metas: Vec::new(),
            field_materializations: DeferredEnvMaterializationCache::default(),
            completion: None,
            lub_probe: None,
            prechecked_boundaries: preloaded.boundaries,
            boundary_owned_defs: preloaded.boundary_owned_defs,
            dep_value_ranges: preloaded.dep_value_ranges,
            errors: Vec::new(),
            observations: Vec::new(),
            monadic_families: preloaded.monadic_families,
            monadic_memo: None,
            pending_foreign_imports: Vec::new(),
        }
    }

    pub(crate) fn invalidate_field_materializations(&mut self) {
        self.field_materializations.clear();
        self.lub_cache.clear();
    }

    pub(crate) fn begin_fill_transaction(&mut self) -> usize {
        self.fill_depth += 1;
        self.fill_trail.len()
    }

    pub(crate) fn commit_fill_transaction(&mut self, _checkpoint: usize) {
        self.fill_depth -= 1;
        if self.fill_depth == 0 {
            self.fill_trail.clear();
        }
    }

    pub(crate) fn rollback_fill_transaction(&mut self, checkpoint: usize) {
        self.fill_depth -= 1;
        while self.fill_trail.len() > checkpoint {
            match self.fill_trail.pop().expect("trail length exceeded checkpoint") {
                | FillUndo::Solus { id, prev } => {
                    match prev {
                        | Some(ann) => {
                            let _ = self.statics.solus.upsert(id, ann);
                        }
                        | None => {
                            let _ = self.statics.solus.remove(id);
                        }
                    }
                    self.solus_epoch = self.solus_epoch.wrapping_add(1);
                    self.invalidate_field_materializations();
                }
                | FillUndo::FillScope { id, prev } => match prev {
                    | Some(scope) => {
                        let _ = self.statics.fill_scopes.upsert(id, scope);
                    }
                    | None => {
                        let _ = self.statics.fill_scopes.remove(id);
                    }
                },
            }
        }
        if self.fill_depth == 0 {
            self.fill_trail.clear();
        }
    }

    pub(crate) fn record_solution(&mut self, id: FillId, solution: AnnId) {
        let prev = self.statics.solus.upsert(id, solution);
        if self.fill_depth > 0 {
            self.fill_trail.push(FillUndo::Solus { id, prev });
        }
        self.solus_epoch = self.solus_epoch.wrapping_add(1);
        self.invalidate_field_materializations();
    }

    pub(crate) fn record_fill_scope(&mut self, id: FillId, scope: SkolemScope) {
        let prev = self.statics.fill_scopes.upsert(id, scope);
        if self.fill_depth > 0 {
            self.fill_trail.push(FillUndo::FillScope { id, prev });
        }
    }

    pub(super) fn record_seal(&mut self, witness: ss::AbstId, definition: ss::TypeId) {
        self.statics.seals.insert_new(witness, definition);
        self.invalidate_field_materializations();
    }

    /// Resolve a source or elaboration-generated definition name.
    pub fn def_name(&self, id: &su::DefId) -> &su::VarName {
        self.statics.def_name(self.scoped, id)
    }

    /// Check the complete source term without the finish phase.
    pub fn run_judgments(&mut self, root: su::TermId) -> Result<TermAnnId> {
        let checked = self.guarded(|tycker| {
            let env = TyEnvT::new(Default::default(), ());
            let inference = InferenceRegion::enter(tycker);
            let root = env.mk(root).tyck(tycker, Action::syn())?;
            if matches!(root, TermAnnId::Hole(_)) {
                tycker.err(TyckError::SortMismatch)?
            }
            inference.close(tycker)?;
            Ok(root)
        });
        if checked.is_err() {
            self.check_independent_sources(root);
        }
        checked
    }

    pub fn run_source(&mut self, root: su::TermId) -> Result<TermAnnId> {
        let root = self.run_judgments(root);
        self.finish_judgments();
        let root = root?;
        self.finish_check()?;
        self.elaborate_static_root(root)?;
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

    /// Consume the checker and retain the typed identity of a complete source term.
    pub fn check_source(
        self, root: su::TermId,
    ) -> std::result::Result<CheckedSource, TyckDiagnostics> {
        self.check_source_outcome(root).into_result()
    }

    /// Check a source while retaining static facts from a rejected term.
    pub fn check_source_outcome(mut self, root: su::TermId) -> SourceCheckOutcome {
        match self.run_source(root) {
            | Ok(root) => {
                self.strip_checker_state();
                SourceCheckOutcome::Checked(CheckedSource {
                    statics: std::sync::Arc::new(self.statics),
                    root,
                    observations: self.observations,
                })
            }
            | Err(ReportedError) => {
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

    pub(crate) fn finish_check(&mut self) -> Result<()> {
        self.resolve_holes_and_collect();
        self.normalize_and_validate()
    }

    pub fn elaborate_static_root(&mut self, root: TermAnnId) -> Result<()> {
        let core = crate::elaborate::static_values::StaticElaborator::run(self, root)?;
        self.statics.core = core.map(std::sync::Arc::new);
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
    pub(crate) fn normalize_and_validate(&mut self) -> Result<()> {
        self.normalize_and_validate_from(0, 0, 0, 0)
    }

    pub(crate) fn normalize_and_validate_from(
        &mut self, kind_start: usize, type_start: usize, compu_start: usize, value_start: usize,
    ) -> Result<()> {
        let mut normalizer =
            crate::normalize::FilledNormalizer::with_starts(kind_start, type_start);
        let kind_ids: Vec<_> = self.statics.kinds_pre.ids().skip(kind_start).collect();
        let kinds = kind_ids
            .into_iter()
            .map(|id| normalizer.normalize_kind(id, self).map(|_| ()))
            .collect_reported::<()>();
        let type_ids: Vec<_> = self.statics.types_pre.ids().skip(type_start).collect();
        let types = type_ids
            .into_iter()
            .map(|id| normalizer.normalize_type(id, self).map(|_| ()))
            .collect_reported::<()>();
        kinds?;
        types?;
        self.validate_foreign_imports();
        // Retain one normalized classifier per distinct top annotation so
        // editor facts can answer without the occurrence payload.
        self.statics.retain_normalized_annotations();
        if self.errors.is_empty() {
            let coverage =
                CoverageChecker::new(&self.statics).validate_from(compu_start, value_start);
            self.statics.coverage_errors = coverage.clone();
            self.errors.extend(coverage.into_iter().map(|error| TyckErrorEntry {
                error: TyckError::Coverage(error),
                stack: std::sync::Arc::from([]),
            }));
        }
        if !self.errors.is_empty() {
            Err(ReportedError)?
        }
        Ok(())
    }

    /// Release construction-only checker indexes before the outcome leaves the
    /// checker.
    pub(crate) fn strip_checker_state(&mut self) {
        self.statics.types_pre.strip_kind_index();
    }

    pub(crate) fn error_diagnostics(&self) -> TyckDiagnostics {
        use std::collections::HashSet;

        let primary = self
            .errors
            .iter()
            .filter(|entry| !matches!(entry.error, TyckError::MissingSolution(_)))
            .cloned()
            .map(|entry| self.error_entry_diagnostic(entry))
            .collect::<Vec<_>>();
        let rejected = primary
            .iter()
            .flat_map(|diagnostic| {
                diagnostic.primary.iter().chain(
                    diagnostic
                        .related
                        .iter()
                        .filter(|_| diagnostic.code == TyckDiagnosticCode::UnconstrainedInference),
                )
            })
            .map(|label| label.span)
            .collect::<Vec<_>>();
        let missing = self.errors.iter().filter_map(|entry| {
            let TyckError::MissingSolution(fills) = &entry.error else { return None };
            let fills = fills
                .iter()
                .copied()
                .filter(|fill| {
                    let owner = self.inference_site_source_span(self.statics.fills[fill]);
                    // The classifier of a failed expression cannot be inferred reliably.
                    // Keep holes belonging to independent expressions in the same source.
                    !rejected.iter().any(|span| owner.lo() <= span.lo() && span.hi() <= owner.hi())
                })
                .collect::<Vec<_>>();
            (!fills.is_empty()).then(|| {
                self.error_entry_diagnostic(TyckErrorEntry {
                    error: TyckError::MissingSolution(fills),
                    stack: entry.stack.clone(),
                })
            })
        });
        let mut seen = HashSet::new();
        let diagnostics = primary
            .into_iter()
            .chain(missing)
            .filter(|diagnostic| seen.insert(diagnostic.clone()))
            .collect::<Vec<_>>();
        TyckDiagnostics { diagnostics: std::sync::Arc::from(diagnostics) }
    }

    /// Resolve all holes with solutions (including nested ones).
    #[inline]
    pub fn do_resolve_holes(&mut self) {
        self.do_resolve_holes_from(0);
    }

    pub(crate) fn do_resolve_holes_from(&mut self, type_start: usize) {
        let type_ids: Vec<_> = self.statics.types_pre.ids().skip(type_start).collect();
        let mut resolver = crate::normalize::HoleResolver::with_start(type_start);
        for id in type_ids {
            let solu = match resolver.resolve(id, self) {
                | Ok(res) => res,
                | Err(ReportedError) => continue,
            };
            if solu != id {
                let ty = self.statics.types_pre[&solu].to_owned();
                self.statics.types_pre.replace_existing(id, ty);
            }
        }
        let missing = resolver.into_missing();
        if !missing.is_empty() {
            // keep running tycker even after unsuccessful solving hole
            let _: Result<()> = self.err(TyckError::MissingSolution(missing));
        }
    }
    pub(crate) fn collect_hole_solutions(&mut self) {
        self.observations.extend(self.statics.fill_hints.iter().map(|id| {
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
            let depth = self.tasks.len();
            let res = zydeco_foundation::stack::maybe_grow(|| with(self));
            self.tasks.truncate(depth);
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
            let res = zydeco_foundation::stack::maybe_grow(|| with(self));
            self.tasks = stack;
            res
        }

        /// Run `attempt` as a probe, keeping none of the diagnostics it reports when it fails.
        ///
        /// A probe asks whether a judgment holds; its failure answers the question
        /// instead of rejecting the program.
        pub(crate) fn speculate<T>(
            &mut self, attempt: impl FnOnce(&mut Self) -> Result<T>,
        ) -> Option<T> {
            let errors = self.errors.len();
            let result = attempt(self);
            if result.is_err() {
                self.errors.truncate(errors);
            }
            result.ok()
        }
    }

    impl Report<TyckError> for Tycker<'_> {
        /// Record `error` in the context of the current checking tasks.
        #[inline]
        fn report(&mut self, error: TyckError) -> ReportedError {
            let stack = std::sync::Arc::from(self.tasks.as_slice());
            self.errors.push(TyckErrorEntry { error, stack });
            ReportedError
        }
    }
}
