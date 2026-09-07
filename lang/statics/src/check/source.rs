//! Checked source outcomes, inference regions, and reuse of synthesized source terms.

use super::*;
use crate::check::judgment::Switch;

/// Immutable source diagnostics produced by one rejected check.
#[derive(Clone, Debug)]
pub struct TyckDiagnostics {
    pub(super) diagnostics: std::sync::Arc<[TyckDiagnostic]>,
}

impl TyckDiagnostics {
    pub fn iter(&self) -> std::slice::Iter<'_, TyckDiagnostic> {
        self.diagnostics.iter()
    }

    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    pub fn len(&self) -> usize {
        self.diagnostics.len()
    }
}

/// A source-directed observation produced during type checking.
#[derive(Clone, Debug)]
pub enum TyckObservation {
    HoleSolution { site: InferenceSite, solution: Option<AnnId> },
    Debug { metadata: zydeco_syntax::Meta, result: TermAnnId },
}

/// The typed result of checking one complete source term.
///
/// The arena is shared behind an [`std::sync::Arc`] so that read-only consumers (editor
/// facts and lowerers) can read individual nodes without cloning hundreds of
/// megabytes. Later phases keep their synthesized metadata in phase-local
/// deltas rather than extending this arena.
#[derive(Clone, Debug)]
pub struct CheckedSource {
    pub statics: std::sync::Arc<StaticsArena>,
    pub root: TermAnnId,
    pub observations: Vec<TyckObservation>,
}

/// A failed source check together with the static facts established before
/// the failure.
#[derive(Clone, Debug)]
pub struct RejectedSource {
    pub statics: std::sync::Arc<StaticsArena>,
    pub diagnostics: TyckDiagnostics,
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
    pub fn into_result(self) -> std::result::Result<CheckedSource, TyckDiagnostics> {
        match self {
            | Self::Checked(checked) => Ok(checked),
            | Self::Rejected(RejectedSource { diagnostics, .. }) => Err(diagnostics),
        }
    }

    /// A cheap shared handle to the arena, for read-only consumers.
    pub fn statics_arc(&self) -> std::sync::Arc<StaticsArena> {
        match self {
            | Self::Checked(CheckedSource { statics, .. })
            | Self::Rejected(RejectedSource { statics, .. }) => statics.clone(),
        }
    }
}

/// Flexible pattern metavariables owned by one local inference boundary.
pub(super) struct InferenceRegion {
    pub(super) inherited: std::collections::HashSet<FillId>,
}

/// One immutable handle to an already synthesized typed term.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) struct CheckedTerm(pub(super) TermAnnId);

/// The one checked root retained for a resolved term. Later references may
/// extend this context, but must preserve its existing bindings and witnesses.
#[derive(Clone, Debug)]
struct CheckedTermEntry {
    environment: TyEnv,
    checked: CheckedTerm,
}

/// Checker-global repository of typed term roots.
#[derive(Default)]
pub(super) struct CheckedTermRepository {
    entries: std::collections::HashMap<su::TermId, CheckedTermEntry>,
}

impl CheckedTerm {
    pub(super) fn root(self) -> TermAnnId {
        self.0
    }

    #[track_caller]
    pub(super) fn require_complete_k(
        self, tycker: &mut Tycker<'_>, error: TyckError,
    ) -> ResultKont<Self> {
        match self.0 {
            | TermAnnId::Hole(_) => tycker.err_k(error, std::panic::Location::caller()),
            | TermAnnId::Kind(_)
            | TermAnnId::Type(_, _)
            | TermAnnId::Value(_, _)
            | TermAnnId::Compu(_, _) => Ok(self),
        }
    }

    /// Compare a use-site expectation with the term's canonical synthesized
    /// classifier without changing the shared checked term.
    #[track_caller]
    pub(super) fn reconcile_k(
        self, tycker: &mut Tycker<'_>, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        let expected = match switch {
            | Switch::Syn => return Ok(self.0),
            | Switch::Ana(expected) => expected,
        };
        let synthesized = match self.0 {
            | TermAnnId::Kind(_) => AnnId::Set,
            | TermAnnId::Type(_, kind) => AnnId::Kind(kind),
            | TermAnnId::Value(_, ty) | TermAnnId::Compu(_, ty) => AnnId::Type(ty),
            | TermAnnId::Hole(_) => {
                return tycker.err_k(TyckError::MissingAnnotation, std::panic::Location::caller());
            }
        };
        Lub::lub_k(synthesized, expected, tycker)?;
        Ok(self.0)
    }
}

impl CheckedTermRepository {
    pub(super) fn get(&self, term: su::TermId, environment: &TyEnv) -> Option<CheckedTerm> {
        let entry = self.entries.get(&term)?;
        assert!(
            environment.is_extension_of(&entry.environment),
            "one resolved term was requested outside its original typing context"
        );
        Some(entry.checked)
    }

    /// Retain the canonical result after synthesis. A nested elaborator may
    /// have completed the same request while the outer synthesis was running;
    /// in that case both paths must have produced the same arena root.
    pub(super) fn retain(
        &mut self, term: su::TermId, environment: TyEnv, checked: CheckedTerm,
    ) -> CheckedTerm {
        use std::collections::hash_map::Entry;
        match self.entries.entry(term) {
            | Entry::Vacant(entry) => {
                entry.insert(CheckedTermEntry { environment, checked });
                checked
            }
            | Entry::Occupied(entry) => {
                let canonical = entry.get();
                assert!(
                    environment.is_extension_of(&canonical.environment),
                    "one resolved term was requested outside its original typing context"
                );
                assert_eq!(
                    canonical.checked, checked,
                    "one resolved term produced distinct checked roots"
                );
                canonical.checked
            }
        }
    }
}

impl InferenceRegion {
    pub(super) fn enter(tycker: &Tycker<'_>) -> Self {
        let inherited = tycker.statics.fills.iter().map(|(fill, _)| *fill).collect();
        Self { inherited }
    }

    pub(super) fn close_k(self, tycker: &mut Tycker<'_>) -> ResultKont<()> {
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

impl Tycker<'_> {
    /// Synthesize one resolved term in its lexical context, retaining the
    /// resulting typed root for every later reference to that term.
    pub(super) fn synthesize_once_k(
        &mut self, term: su::TermId, environment: &TyEnv,
        synthesize: impl FnOnce(&mut Self) -> ResultKont<TermAnnId>,
    ) -> ResultKont<CheckedTerm> {
        if let Some(checked) = self.checked_terms.get(term, environment) {
            return Ok(checked);
        }
        let checked = CheckedTerm(synthesize(self)?);
        Ok(self.checked_terms.retain(term, environment.clone(), checked))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::intrinsics::InternalTerm;
    use crate::check::tests::{TestDb, with_tycker};
    use std::sync::Arc;

    #[test]
    fn expected_kinds_do_not_elaborate_imported_sources() {
        let mut allocator = IdAllocator::<su::ScopedScope>::new();
        let hole = allocator.alloc();
        let boundary = allocator.alloc();
        let mut scoped = su::ScopedArena::default();
        scoped.terms.insert_new(hole, su::Hole.into());
        scoped.terms.insert_new(boundary, su::SourceBoundary(hole).into());

        let spans = su::SpanArena::default();
        let prim = su::PrimDefs::default();
        let db = TestDb::default();
        *db.pending.lock().unwrap() = Some(Arc::new(crate::query::PendingParts {
            spans: spans.clone(),
            prim: prim.clone(),
            scoped: scoped.clone(),
            root: boundary,
        }));
        let data = crate::query::intern_pending(&db);
        let mut tycker = Tycker::new(&db, data, &spans, &prim, &scoped);
        let env = TyEnv::default();
        let expected = Alloc::alloc(&mut tycker, ss::VType, (), &());
        let checked = TyEnvT::new(env, boundary).tyck_k(&mut tycker, Action::ana(expected.into()));

        assert!(checked.is_err());
        let [entry] = tycker.errors.as_slice() else { panic!("expected one type-checking error") };
        assert!(matches!(entry.error, TyckError::MissingAnnotation));
        assert_eq!(entry.stack.len(), 1);
        assert!(matches!(
            entry.stack.first(),
            Some(TyckTask::Term(term, Switch::Syn)) if *term == hole
        ));
    }

    #[test]
    fn imported_source_errors_exclude_the_importing_task_stack() {
        let mut allocator = IdAllocator::<su::ScopedScope>::new();
        let hole = allocator.alloc();
        let boundary = allocator.alloc();
        let mut scoped = su::ScopedArena::default();
        scoped.terms.insert_new(hole, su::Hole.into());
        scoped.terms.insert_new(boundary, su::SourceBoundary(hole).into());

        let spans = su::SpanArena::default();
        let prim = su::PrimDefs::default();
        let db = TestDb::default();
        *db.pending.lock().unwrap() = Some(Arc::new(crate::query::PendingParts {
            spans: spans.clone(),
            prim: prim.clone(),
            scoped: scoped.clone(),
            root: boundary,
        }));
        let data = crate::query::intern_pending(&db);
        let mut tycker = Tycker::new(&db, data, &spans, &prim, &scoped);
        let result =
            TyEnvT::new(TyEnv::default(), boundary).tyck_k(&mut tycker, Action::ana(AnnId::Set));

        assert!(result.is_err());
        let [entry] = tycker.errors.as_slice() else { panic!("expected one type-checking error") };
        assert!(matches!(entry.error, TyckError::MissingAnnotation));
        assert_eq!(entry.stack.len(), 1);
        assert!(matches!(
            entry.stack.first(),
            Some(TyckTask::Term(term, Switch::Syn)) if *term == hole
        ));
    }

    #[test]
    fn repeated_source_boundaries_check_and_materialize_the_provider_once() {
        with_tycker(
            |allocator, scoped| {
                let provider = allocator.alloc();
                let first = allocator.alloc();
                let second = allocator.alloc();
                let root = allocator.alloc();
                scoped.terms.insert_new(provider, su::Triv.into());
                scoped.terms.insert_new(first, su::SourceBoundary(provider).into());
                scoped.terms.insert_new(second, su::SourceBoundary(provider).into());
                scoped.terms.insert_new(root, su::Term::Cons(vec![first, second]));
                (root, (provider, first, second))
            },
            |tycker, (provider, first, second)| {
                InternalTerm::fill_intrinsics(tycker);
                let root = tycker.data.root(tycker.db);

                let checked =
                    TyEnvT::new(TyEnv::new(), root).tyck_k(tycker, Action::syn()).unwrap();

                assert!(matches!(checked, TermAnnId::Value(_, _)));
                assert_eq!(tycker.check_counts.get(&su::EntityId::Term(provider)), Some(&1),);
                assert_eq!(
                    tycker.statics.term_annotation(first),
                    tycker.statics.term_annotation(second)
                );
                assert_eq!(
                    tycker.statics.term_annotation(first),
                    tycker.statics.term_annotation(provider),
                );
                let typed_provider = tycker
                    .statics
                    .term_annotation(provider)
                    .and_then(TermAnnId::as_term)
                    .expect("the provider has a typed root");
                assert_eq!(tycker.statics.terms.source(&typed_provider), Some(provider));
                assert_eq!(tycker.checked_terms.entries.len(), 1);
                assert!(tycker.checked_terms.entries.contains_key(&provider));
                assert!(tycker.errors.is_empty());
            },
        );
    }

    #[test]
    fn nested_checked_term_synthesis_retains_the_term_specific_result() {
        with_tycker(
            |allocator, scoped| {
                let provider = allocator.alloc();
                scoped.terms.insert_new(provider, su::Triv.into());
                (provider, provider)
            },
            |tycker, provider| {
                InternalTerm::fill_intrinsics(tycker);
                let environment = TyEnv::new();

                let checked = tycker
                    .synthesize_once_k(provider, &environment, |tycker| {
                        let checked =
                            tycker.synthesize_once_k(provider, &environment, |tycker| {
                                TyEnvT::new(environment.clone(), provider)
                                    .tyck_k(tycker, Action::syn())
                            })?;
                        Ok(checked.root())
                    })
                    .unwrap();

                assert_eq!(Some(checked.root()), tycker.statics.term_annotation(provider),);
                assert_eq!(tycker.check_counts.get(&su::EntityId::Term(provider)), Some(&1),);
                assert_eq!(tycker.checked_terms.entries.len(), 1);
                assert!(tycker.errors.is_empty());
            },
        );
    }

    #[test]
    fn classifier_queries_reuse_the_operand_after_context_extension() {
        with_tycker(
            |allocator, scoped| {
                let operand = allocator.alloc();
                scoped.terms.insert_new(operand, su::Triv.into());
                let query = allocator.alloc();
                scoped.terms.insert_new(query, su::TypeOf(operand).into());
                let definition: su::DefId = allocator.alloc();
                (query, (query, operand, definition))
            },
            |tycker, (query, operand, definition)| {
                InternalTerm::fill_intrinsics(tycker);
                let original =
                    TyEnvT::new(TyEnv::new(), query).tyck_k(tycker, Action::syn()).unwrap();
                let TermAnnId::Type(ty, _) = original else {
                    panic!("typeof unit must produce a type")
                };
                let environment = TyEnv::new() + [(definition, AnnId::Type(ty))];
                let repeated =
                    TyEnvT::new(environment, query).tyck_k(tycker, Action::syn()).unwrap();
                assert_eq!(original, repeated);
                assert_eq!(tycker.check_counts.get(&su::EntityId::Term(operand)), Some(&1));
                assert!(tycker.errors.is_empty());
            },
        );
    }
}
