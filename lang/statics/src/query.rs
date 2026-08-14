//! Salsa-backed query entry points for type checking.
//!
//! [`TyckDb`] extends the session's salsa graph with type-checking queries. The
//! name-resolved program is held as a salsa tracked struct ([`ScopedData`]) so that
//! queries can key on it within the same database; finer-grained judgment queries
//! replace the wholesale [`check_source`] piece by piece (see
//! `docs/logs/query-based-tyck.md`).

use crate::alloc::QUERY_DERIVATION_TAG;
use crate::check::{SourceCheckOutcome, Tycker};
use crate::surface_syntax as su;
use crate::syntax as ss;
use zydeco_utils::arena::ArenaAccess;
use zydeco_utils::arena::{ArenaId, KeySpaceId, derived_id};

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
    pub spans: su::SpanArena,
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub prim: su::PrimDefs,
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub scoped: su::ScopedArena,
    #[tracked]
    pub root: su::TermId,
}

/// An interned typed type node, for use as a salsa query key.
#[salsa::interned]
pub struct InternedType<'db> {
    pub id: ss::TypeId,
}

/// An interned typed kind node, for use as a salsa query key.
#[salsa::interned]
pub struct InternedKind<'db> {
    pub id: ss::KindId,
}

/// An interned scoped definition, for use as a salsa query key.
#[salsa::interned]
pub struct InternedDef<'db> {
    pub id: su::DefId,
}

/// An interned scoped term, for use as a salsa query key.
#[salsa::interned]
pub struct InternedTerm<'db> {
    pub id: su::TermId,
}

/// The typed judgment of an intrinsic `Internal` term that needs no
/// environment: a fresh `VType` or `CType` kind node, produced by salsa rather
/// than by the checker's in-context allocator.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn internal_kind_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
) -> Option<(ss::KindId, ss::Kind)> {
    let internal = match data.scoped(db).terms.get(&term.id(db))? {
        | su::Term::Internal(internal) => internal,
        | _ => return None,
    };
    let kind = match internal {
        | su::Internal::VType => ss::Kind::VType(ss::VType),
        | su::Internal::CType => ss::Kind::CType(ss::CType),
        | _ => return None,
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let id = derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, 0), 0);
    Some((id, kind))
}

/// An interned hole-filling site, for use as a salsa query key.
#[salsa::interned]
pub struct InternedFill<'db> {
    pub id: ss::FillId,
}

/// Take the pending resolved program out of the slot and intern it as a
/// tracked struct, inside the query graph where tracked-struct creation is
/// legal.
#[salsa::tracked]
pub fn intern_pending<'db>(db: &'db dyn TyckDb) -> ScopedData<'db> {
    let parts = db
        .pending_parts()
        .lock()
        .expect("pending check slot poisoned")
        .take()
        .expect("pending check slot is empty");
    let parts = match std::sync::Arc::try_unwrap(parts) {
        | Ok(parts) => parts,
        | Err(_) => panic!("pending parts are still shared"),
    };
    ScopedData::new(db, parts.spans, parts.prim, parts.scoped, parts.root)
}

/// The complete result of checking one source snapshot.
#[derive(Clone, Debug)]
pub struct TyckOutput {
    /// The name-resolved arena after checking. The checker may allocate generated
    /// definitions into it during elaboration.
    pub scoped: su::ScopedArena,
    /// The recoverable checking outcome.
    pub outcome: SourceCheckOutcome,
}

// The outcome owns its arenas and reports and contains no database-tied references.
// The non-Update escape hatch stays until the judgment layer gains structural equality.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn check_source<'db>(db: &'db dyn TyckDb, data: ScopedData<'db>) -> TyckOutput {
    let mut scoped = data.scoped(db).clone();
    let outcome = Tycker::new(db, data, data.spans(db), data.prim(db), &mut scoped)
        .check_source_outcome(data.root(db));
    TyckOutput { scoped, outcome }
}
