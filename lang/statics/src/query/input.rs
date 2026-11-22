//! Database inputs and shared interned keys for source checking queries.

use super::*;

/// Databases that can answer type-checking queries.
///
/// Implemented by the session database; this trait carries the query ingredients
/// declared in this module, so the queries join the session's salsa graph.
#[salsa::db]
pub trait TyckDb: salsa::Database {
    /// Per-root key-space registry for definitions synthesized by checking/elaboration.
    fn generated_def_spaces(
        &self,
    ) -> &std::sync::Arc<zydeco_foundation::arena::KeySpaceRegistry<su::TermId>>;

    /// The slot through which programs assembled outside the source pipeline
    /// cross into the query graph; salsa requires an active query to create
    /// tracked structs. See [`intern_pending`].
    fn pending_parts(
        &self,
    ) -> &std::sync::Arc<std::sync::Mutex<Option<std::sync::Arc<PendingParts>>>>;
}

/// A resolved program assembled outside the source pipeline, waiting to enter
/// the query graph through [`intern_pending`].
pub struct PendingParts {
    pub spans: su::SpanArena,
    pub scoped: su::ScopedArena,
    pub root: su::TermId,
}

/// The name-resolved program of one source snapshot.
#[salsa::tracked]
pub struct ScopedData<'db> {
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub spans: std::sync::Arc<su::SpanArena>,
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub scoped: std::sync::Arc<su::ScopedArena>,
    #[tracked]
    #[returns(copy)]
    pub root: su::TermId,
}

/// An interned typed type node, for use as a salsa query key.
#[salsa::interned]
pub struct InternedType<'db> {
    #[returns(clone)]
    pub id: ss::TypeId,
}

/// An interned scoped definition, for use as a salsa query key.
#[salsa::interned]
pub struct InternedDef<'db> {
    #[returns(clone)]
    pub id: su::DefId,
}

/// An interned scoped term, for use as a salsa query key.
#[salsa::interned]
pub struct InternedTerm<'db> {
    #[returns(clone)]
    pub id: su::TermId,
}

/// An interned hole-filling site, for use as a salsa query key.
#[salsa::interned]
pub struct InternedFill<'db> {
    #[returns(clone)]
    pub id: ss::FillId,
}
