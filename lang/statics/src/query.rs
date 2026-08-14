//! Salsa-backed query entry points for type checking.
//!
//! [`TyckDb`] extends the session's salsa graph with type-checking queries. The
//! name-resolved program is held as a salsa tracked struct ([`ScopedData`]) so that
//! queries can key on it within the same database; finer-grained judgment queries
//! replace the wholesale [`check_source`] piece by piece (see
//! `docs/logs/query-based-tyck.md`).

use crate::TyEnv;
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

/// The type-checking environment at a judgment site, salsa-visible so that
/// environment-dependent judgments can key on it.
#[salsa::tracked]
pub struct EnvData<'db> {
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub env: TyEnv,
}

/// The nodes produced by checking an intrinsic `Internal` term.
#[derive(Clone, Debug)]
pub enum InternalJudgment {
    Kind { id: ss::KindId, kind: ss::Kind },
    Type { kinds: Vec<(ss::KindId, ss::Kind)>, ty: (ss::TypeId, ss::Type), ann: ss::KindId },
}

/// The typed judgment of an intrinsic `Internal` term.
///
/// Produces the fresh nodes for an intrinsic cache miss; the checker owns the
/// intrinsic singletons (`IntrinsicStatics`) and materializes the returned
/// nodes, recording the caller's environment exactly as in-context allocation
/// did. `OS`, `Monad`, and `Algebra` are not query-produced yet: `OS` resolves
/// against the environment through the builtin signature, and the latter two
/// are ordinary library bindings.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn internal_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>, env: EnvData<'db>,
) -> Option<InternalJudgment> {
    let _ = env;
    let internal = match data.scoped(db).terms.get(&term.id(db))? {
        | su::Term::Internal(internal) => internal,
        | _ => return None,
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let kind_id = |slot: u32| {
        derived_id::<ss::KindId>(
            KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, 0),
            slot,
        )
    };
    let type_id = |slot: u32| {
        derived_id::<ss::TypeId>(
            KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, 0),
            slot,
        )
    };
    match internal {
        | su::Internal::VType => {
            Some(InternalJudgment::Kind { id: kind_id(0), kind: ss::Kind::VType(ss::VType) })
        }
        | su::Internal::CType => {
            Some(InternalJudgment::Kind { id: kind_id(0), kind: ss::Kind::CType(ss::CType) })
        }
        | su::Internal::Unit => {
            let vtype = kind_id(0);
            Some(InternalJudgment::Type {
                kinds: vec![(vtype, ss::Kind::VType(ss::VType))],
                ty: (type_id(1), ss::Type::Unit(ss::UnitTy)),
                ann: vtype,
            })
        }
        | su::Internal::Thk => {
            let ctype = kind_id(0);
            let vtype = kind_id(1);
            let arrow = kind_id(2);
            Some(InternalJudgment::Type {
                kinds: vec![
                    (ctype, ss::Kind::CType(ss::CType)),
                    (vtype, ss::Kind::VType(ss::VType)),
                    (arrow, ss::Kind::Arrow(ss::Arrow(ctype, vtype))),
                ],
                ty: (type_id(3), ss::Type::Thk(ss::ThkTy)),
                ann: arrow,
            })
        }
        | su::Internal::Ret => {
            let vtype = kind_id(0);
            let ctype = kind_id(1);
            let arrow = kind_id(2);
            Some(InternalJudgment::Type {
                kinds: vec![
                    (vtype, ss::Kind::VType(ss::VType)),
                    (ctype, ss::Kind::CType(ss::CType)),
                    (arrow, ss::Kind::Arrow(ss::Arrow(vtype, ctype))),
                ],
                ty: (type_id(3), ss::Type::Ret(ss::RetTy)),
                ann: arrow,
            })
        }
        | su::Internal::Primitive(primitive) => {
            let vtype = kind_id(0);
            Some(InternalJudgment::Type {
                kinds: vec![(vtype, ss::Kind::VType(ss::VType))],
                ty: (type_id(1), ss::Type::Primitive(ss::PrimitiveTy(*primitive))),
                ann: vtype,
            })
        }
        | su::Internal::OS | su::Internal::Monad | su::Internal::Algebra => None,
    }
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
