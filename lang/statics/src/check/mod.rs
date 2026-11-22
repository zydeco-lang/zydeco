use derive_more::{AsMut, AsRef, Deref};
use zydeco_surface::scoped::context::TermContexts;
use {
    super::{
        arena::StaticsArena,
        environment::{MonadicTypeBasis, TyEnv, TyEnvT},
        syntax::{AbstId, AnnId, FillId, Fillable, InferenceSite, PatAnnId, TermAnnId},
        *,
    },
    crate::surface_syntax::{ScopedArena, SpanArena},
    crate::validate::CoverageChecker,
};

/// Checker lifecycle and source finalization.
mod driver;
mod library;
pub use library::{CheckedExport, CheckedLibrary, CheckedUnit, LibraryCheckError, UnitInitializer};
/// Checked source results and synthesis reuse.
pub(crate) mod source;
use source::CheckedTermRepository;
pub use source::{
    CheckedSource, RejectedSource, SourceCheckOutcome, TyckDiagnostics, TyckObservation,
};
/// Common checking protocol and expected annotations.
pub(crate) mod judgment;
pub use judgment::{Action, Switch, Tyck, TyckTask};
/// Pattern rules and lexical opening scopes.
mod pattern;
pub use pattern::{CheckedPattern, PatternAction, PatternCheck};
/// Declaration groups and typed pattern assignments.
mod binding;
pub use binding::{Assign, FixPoint};
/// Field search and selective package opening.
mod projection;
use projection::DeferredEnvMaterializationCache;
/// Function classifiers and dependent application.
mod functions;
/// Source-term checking rules.
mod term;
/// Intrinsic and foreign attachment points.
pub(crate) mod intrinsics;
pub(crate) use intrinsics::IntrinsicStatics;
use intrinsics::PendingForeignImport;
/// Monadic basis checking and algebra translation.
mod monadic;

/// Type-checker error definitions and reporting.
pub mod error;
pub use error::*;
/// Checker-dependent construction and projection of typed annotations.
mod annotation;
/// Least-upper-bound operations for kinds and types.
pub mod lub;
pub use lub::*;
/// Request-local expected annotations and completion compatibility evidence.
mod completion;
pub use completion::CompletionTyping;
/// Syntactic checks for annotations and seals.
pub mod syntactic;
pub use syntactic::*;
/// Debug dump helpers used by diagnostics.
mod dump;
/// Type-directed elaboration of generalized comatch clauses.
mod copattern;
use copattern::CopatternElaborator;

/// Undo entry recorded while unifying inside [`FillId::fill`].
pub(crate) enum FillUndo {
    Solus { id: FillId, prev: Option<AnnId> },
    FillScope { id: FillId, prev: Option<SkolemScope> },
}

/// Type-checking driver that consumes scoped syntax and produces typed arenas.
#[derive(AsRef, AsMut)]
pub struct Tycker<'a> {
    /// Issuer of generated [`DefId`]s for this type-checking run.
    #[as_mut(DefGenAllocator)]
    allocator: DefGenAllocator,
    /// The salsa database this check runs within.
    pub db: &'a dyn crate::query::TyckDb,
    /// The name-resolved program snapshot being checked.
    pub data: crate::query::ScopedData<'a>,
    pub spans: &'a SpanArena,
    #[as_ref(ScopedArena)]
    pub scoped: &'a ScopedArena,
    source_contexts: TermContexts,
    #[as_ref(StaticsArena)]
    #[as_mut(StaticsArena)]
    pub statics: StaticsArena,
    pub(crate) intrinsics: IntrinsicStatics,
    /// call stack for debugging tycker and error tracking
    pub tasks: Vec<TyckTask>,
    /// Source terms visited during judgment checking, used by recovery.
    visited_terms: rustc_hash::FxHashSet<su::TermId>,
    /// Current transparent kind-pattern bindings active during checking/elaboration.
    pub(crate) kind_bindings: rustc_hash::FxHashMap<DefId, KindId>,
    /// Current transparent type-pattern bindings active during checking/elaboration.
    pub(crate) type_bindings: rustc_hash::FxHashMap<DefId, TypeId>,
    /// Classifiers of pattern annotations decided before their pattern is
    /// checked.
    ///
    /// Inside a call-by-value block the `A` in a binder's `(p : A)` is a
    /// source type, which ordinary synthesis cannot read, so the block
    /// translates it first and records the result here; the annotation rule
    /// takes a recorded classifier instead of synthesizing one. A type
    /// binder's `(X : K)` records the kind the block's own kind judgment
    /// decided, for the same reason. Empty outside such blocks.
    settled_pattern_annotations: ArenaAssoc<su::TermId, AnnId>,
    /// Resolved terms selected for canonical synthesis, including imported
    /// providers, classifier queries, and monadic blocks.
    checked_terms: CheckedTermRepository,
    /// Monotonic rollback log for speculative fill and scope updates during unification.
    pub(crate) fill_trail: Vec<FillUndo>,
    pub(crate) fill_depth: usize,
    /// Incremented on every inference solution commit or rollback to invalidate caches.
    pub(crate) solus_epoch: u64,
    /// Non-diverged structural LUB cache, cleared whenever inference state changes.
    pub(crate) lub_cache: rustc_hash::FxHashMap<(TypeId, TypeId), TypeId>,
    /// meta stack
    pub metas: Vec<su::Meta>,
    /// Results of field-search materializations under the current inference state.
    field_materializations: DeferredEnvMaterializationCache,
    completion: Option<completion::CompletionCapture>,
    lub_probe: Option<lub::LubProbe>,
    /// Pre-checked imported `SourceBoundary` outcomes supplied by separate compilation.
    pub(crate) prechecked_boundaries:
        rustc_hash::FxHashMap<su::TermId, std::result::Result<TermAnnId, ()>>,
    /// Definitions transitively owned by each imported `SourceBoundary` term.
    pub(crate) boundary_owned_defs: rustc_hash::FxHashMap<su::TermId, std::sync::Arc<[su::DefId]>>,
    /// Value index ranges per dependency mapped to that dependency's transitively owned defs.
    pub(crate) dep_value_ranges: Vec<(std::ops::Range<usize>, std::sync::Arc<[su::DefId]>)>,
    /// a writer monad for error handling
    pub errors: Vec<TyckErrorEntry>,
    pub(crate) observations: Vec<TyckObservation>,
    pub(crate) monadic_families:
        std::collections::BTreeMap<ss::AbstId, crate::elaborate::monadic::nominal::NominalFamily>,
    pub(crate) monadic_memo: Option<crate::elaborate::monadic::MonadicMemo>,
    pending_foreign_imports: Vec<PendingForeignImport>,
}

/// Dependency artifacts preloaded into a separate-compilation unit check.
#[derive(Default)]
pub(crate) struct PreloadedState {
    pub boundaries: rustc_hash::FxHashMap<su::TermId, std::result::Result<TermAnnId, ()>>,
    pub monadic_families:
        std::collections::BTreeMap<ss::AbstId, crate::elaborate::monadic::nominal::NominalFamily>,
    pub boundary_owned_defs: rustc_hash::FxHashMap<su::TermId, std::sync::Arc<[su::DefId]>>,
    pub dep_value_ranges: Vec<(std::ops::Range<usize>, std::sync::Arc<[su::DefId]>)>,
}

#[cfg(test)]
pub(crate) mod tests;
