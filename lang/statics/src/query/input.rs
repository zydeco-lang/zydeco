//! Database inputs and shared interned keys for source checking and judgment queries.

use super::*;

/// Databases that can answer type-checking queries.
///
/// Implemented by the session database; this trait carries the query ingredients
/// declared in this module, so the queries join the session's salsa graph.
#[salsa::db]
pub trait TyckDb: salsa::Database {
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
    pub prim: su::PrimDefs,
    pub scoped: su::ScopedArena,
    pub root: su::TermId,
}

/// The name-resolved program of one source snapshot.
///
/// Tracked-struct fields need neither `Eq` nor `Hash`, which the arenas do not
/// provide, so this is how the checker's inputs enter the salsa graph.
#[salsa::tracked]
pub struct ScopedData<'db> {
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub spans: std::sync::Arc<su::SpanArena>,
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub prim: su::PrimDefs,
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

/// An interned typed kind node, for use as a salsa query key.
#[salsa::interned]
pub struct InternedKind<'db> {
    #[returns(clone)]
    pub id: ss::KindId,
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

/// The type-checking environment at a judgment site, salsa-visible so that
/// environment-dependent judgments can key on it.
#[salsa::tracked]
pub struct EnvData<'db> {
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub env: TyEnv,
}

/// An interned type annotation, for use as a salsa query key.
#[salsa::interned]
pub struct InternedAnn<'db> {
    #[returns(clone)]
    pub id: ss::AnnId,
}

/// An interned allocation site, for use as a salsa query key.
///
/// Auxiliary checker entities (package-pi introductions and eliminations)
/// allocate at the enclosing entity's site without being scoped entities
/// themselves; this wrapper keys their queries on the site directly.
#[salsa::interned]
pub struct InternedSite<'db> {
    #[returns(clone)]
    pub space: u64,
    #[returns(clone)]
    pub raw: u32,
    #[returns(clone)]
    pub occurrence: u32,
}

/// An interned scoped pattern, for use as a salsa query key.
#[salsa::interned]
pub struct InternedPat<'db> {
    #[returns(clone)]
    pub id: su::PatId,
}

/// An interned term annotation, for use as a salsa query key.
#[salsa::interned]
pub struct InternedTermAnn<'db> {
    #[returns(clone)]
    pub id: ss::TermAnnId,
}

/// An interned pattern annotation, for use as a salsa query key.
#[salsa::interned]
pub struct InternedPatAnn<'db> {
    #[returns(clone)]
    pub id: ss::PatAnnId,
}

/// An interned value pattern identifier, for use as a salsa query key.
#[salsa::interned]
pub struct InternedVPat<'db> {
    #[returns(clone)]
    pub id: ss::VPatId,
}

/// An interned value identifier, for use as a salsa query key.
#[salsa::interned]
pub struct InternedValue<'db> {
    #[returns(clone)]
    pub id: ss::ValueId,
}

/// An interned hole-filling site, for use as a salsa query key.
#[salsa::interned]
pub struct InternedFill<'db> {
    #[returns(clone)]
    pub id: ss::FillId,
}
