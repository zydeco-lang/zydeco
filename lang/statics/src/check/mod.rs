use derive_more::{AsMut, AsRef, Deref};
use {
    super::{
        arena::StaticsArena,
        environment::{MonadicTypeBasis, TyEnv, TyEnvT},
        syntax::{AbstId, AnnId, FillId, Fillable, InferenceSite, PatAnnId, TermAnnId},
        *,
    },
    crate::surface_syntax::{PrimDefs, ScopedArena, SpanArena, TermContexts},
    crate::validate::CoverageChecker,
    zydeco_surface::metadata::{BuiltinMeta, FfiMeta, MetadataKind},
    zydeco_utils::prelude::ArenaAccess,
};

/// Checker lifecycle and source finalization.
mod driver;
/// Checked source results and synthesis reuse.
mod source;
use source::CheckedTermRepository;
pub use source::{
    CheckedSource, RejectedSource, SourceCheckOutcome, TyckDiagnostics, TyckObservation,
};
/// Common checking protocol and expected annotations.
mod judgment;
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
mod intrinsics;
pub(crate) use intrinsics::InternalTerm;
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
    pub scoped: &'a ScopedArena,
    source_contexts: TermContexts,
    #[as_ref(StaticsArena)]
    #[as_mut(StaticsArena)]
    pub statics: StaticsArena,
    /// call stack for debugging tycker and error tracking
    pub tasks: rpds::VectorSync<TyckTask>,
    /// how many times each scoped entity has been checked; supplies the
    /// derivation occurrence so re-checked entities get distinct sites
    check_counts: ArenaAssoc<su::EntityId, u32>,
    /// Resolved terms selected for canonical synthesis, including imported
    /// providers, classifier queries, and monadic blocks.
    checked_terms: CheckedTermRepository,
    /// meta stack
    pub metas: rpds::VectorSync<su::Meta>,
    /// Results of field-search materializations under the current inference state.
    field_materializations: DeferredEnvMaterializationCache,
    completion: Option<completion::CompletionCapture>,
    lub_probe: Option<lub::LubProbe>,
    /// a writer monad for error handling
    pub errors: Vec<TyckErrorEntry>,
    pub(crate) observations: Vec<TyckObservation>,
    pending_foreign_imports: Vec<PendingForeignImport>,
}

#[cfg(test)]
mod tests;
