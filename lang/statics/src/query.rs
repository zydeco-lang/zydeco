//! Salsa-backed query entry points for type checking.
//!
//! [`TyckDb`] extends the session's salsa graph with type-checking queries. The
//! name-resolved program is held as a salsa tracked struct ([`ScopedData`]) so that
//! queries can key on it within the same database; finer-grained judgment queries
//! replace the wholesale [`check_source`] piece by piece (see
//! `docs/logs/query-based-tyck.md`).

use crate::TyEnv;
use crate::alloc::QUERY_DERIVATION_TAG;
use crate::arena::StaticsArena;
use crate::check::{CheckedSource, KontFailure, RejectedSource, SourceCheckOutcome, Tycker};
use crate::surface_syntax as su;
use crate::syntax as ss;
use zydeco_surface::arena::ArenaId;
use zydeco_utils::arena::ArenaAccess;
use zydeco_utils::arena::{KeySpaceId, derived_id};

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

/// The singleton key of an intrinsic kind or type.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum IntrinsicKey {
    VType,
    CType,
    Thk,
    Ret,
    Unit,
    Primitive(zydeco_syntax::PrimitiveType),
}

impl IntrinsicKey {
    fn discriminant(self) -> u32 {
        match self {
            | Self::VType => 0,
            | Self::CType => 1,
            | Self::Thk => 2,
            | Self::Ret => 3,
            | Self::Unit => 4,
            | Self::Primitive(primitive) => {
                5 + zydeco_syntax::PrimitiveType::ALL
                    .iter()
                    .position(|candidate| *candidate == primitive)
                    .expect("every primitive participates in the intrinsic singletons")
                    as u32
            }
        }
    }
}

/// An interned intrinsic key, for use as a salsa query key.
#[salsa::interned]
pub struct InternedIntrinsic<'db> {
    pub key: IntrinsicKey,
}

/// The singleton nodes of one intrinsic kind or type, produced by a query and
/// materialized by the checker before any judgment reads the `IntrinsicStatics`
/// cache. See `docs/ideas/query-owned-statics.md` for the fill-before-read
/// invariant.
#[derive(Clone, Debug)]
pub enum IntrinsicSingleton {
    Kind { id: ss::KindId, kind: ss::Kind },
    Type { kinds: Vec<(ss::KindId, ss::Kind)>, ty: (ss::TypeId, ss::Type), ann: ss::KindId },
}

/// The singleton judgment of one intrinsic kind or type.
///
/// The derived site is synthetic (not tied to any scoped term): the intrinsic
/// belongs to the check, not to the term that first spells it. The key's
/// discriminant separates the singletons so their identifiers never collide.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn intrinsic_singleton<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, key: InternedIntrinsic<'db>,
) -> IntrinsicSingleton {
    let _ = data;
    let occurrence = key.key(db).discriminant();
    let kind_id = |slot: u32| {
        derived_id::<ss::KindId>(
            KeySpaceId::derive(QUERY_DERIVATION_TAG, 0, u32::MAX, occurrence),
            slot,
        )
    };
    let type_id = |slot: u32| {
        derived_id::<ss::TypeId>(
            KeySpaceId::derive(QUERY_DERIVATION_TAG, 0, u32::MAX, occurrence),
            slot,
        )
    };
    match key.key(db) {
        | IntrinsicKey::VType => {
            IntrinsicSingleton::Kind { id: kind_id(0), kind: ss::Kind::VType(ss::VType) }
        }
        | IntrinsicKey::CType => {
            IntrinsicSingleton::Kind { id: kind_id(0), kind: ss::Kind::CType(ss::CType) }
        }
        | IntrinsicKey::Unit => {
            let vtype = kind_id(0);
            IntrinsicSingleton::Type {
                kinds: vec![(vtype, ss::Kind::VType(ss::VType))],
                ty: (type_id(1), ss::Type::Unit(ss::UnitTy)),
                ann: vtype,
            }
        }
        | IntrinsicKey::Thk => {
            let ctype = kind_id(0);
            let vtype = kind_id(1);
            let arrow = kind_id(2);
            IntrinsicSingleton::Type {
                kinds: vec![
                    (ctype, ss::Kind::CType(ss::CType)),
                    (vtype, ss::Kind::VType(ss::VType)),
                    (arrow, ss::Kind::Arrow(ss::Arrow(ctype, vtype))),
                ],
                ty: (type_id(3), ss::Type::Thk(ss::ThkTy)),
                ann: arrow,
            }
        }
        | IntrinsicKey::Ret => {
            let vtype = kind_id(0);
            let ctype = kind_id(1);
            let arrow = kind_id(2);
            IntrinsicSingleton::Type {
                kinds: vec![
                    (vtype, ss::Kind::VType(ss::VType)),
                    (ctype, ss::Kind::CType(ss::CType)),
                    (arrow, ss::Kind::Arrow(ss::Arrow(vtype, ctype))),
                ],
                ty: (type_id(3), ss::Type::Ret(ss::RetTy)),
                ann: arrow,
            }
        }
        | IntrinsicKey::Primitive(primitive) => {
            let vtype = kind_id(0);
            IntrinsicSingleton::Type {
                kinds: vec![(vtype, ss::Kind::VType(ss::VType))],
                ty: (type_id(1), ss::Type::Primitive(ss::PrimitiveTy(primitive))),
                ann: vtype,
            }
        }
    }
}

/// The synthesized judgment of a literal term: its primitive singleton type
/// and the range-checked literal value, produced without touching the arena.
#[derive(Clone, Debug)]
pub enum LiteralSynOutcome {
    Value { id: ss::ValueId, value: ss::Value, ty: ss::TypeId },
    Error(crate::check::TyckError),
}

/// The synthesized judgment of a literal term.
///
/// The primitive type comes from the query-owned intrinsic singleton; the
/// range check and the literal value are pure functions of the source literal.
/// The checker materializes the returned value node with the caller's
/// environment, exactly as in-context allocation did.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn literal_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
) -> Option<LiteralSynOutcome> {
    use zydeco_syntax::{FloatType, IntegerType, Literal, PrimitiveType};
    let lit = match data.scoped(db).terms.get(&term.id(db))? {
        | su::Term::Lit(lit) => lit,
        | _ => return None,
    };
    let primitive_ty = |primitive| {
        let key = crate::query::InternedIntrinsic::new(db, IntrinsicKey::Primitive(primitive));
        let crate::query::IntrinsicSingleton::Type { ty: (ty, _), .. } =
            crate::query::intrinsic_singleton(db, data, key)
        else {
            unreachable!("primitive singletons are type-producing")
        };
        ty
    };
    let (lit, ty) = match lit {
        | Literal::Integer(i) => {
            let integer_type = IntegerType::Int64;
            let value = i.value();
            let Some(i) = i.with_type(integer_type) else {
                return Some(LiteralSynOutcome::Error(
                    crate::check::TyckError::IntegerLiteralOutOfRange { value, integer_type },
                ));
            };
            (Literal::Integer(i), primitive_ty(PrimitiveType::Integer(integer_type)))
        }
        | Literal::Float(value) => {
            let float_type = FloatType::Float64;
            let original = value;
            let Some(value) = value.with_type(float_type) else {
                return Some(LiteralSynOutcome::Error(
                    crate::check::TyckError::FloatLiteralOutOfRange {
                        value: original.value(),
                        float_type,
                    },
                ));
            };
            (Literal::Float(value), primitive_ty(PrimitiveType::Float(float_type)))
        }
        | Literal::String(s) => (Literal::String(s.clone()), primitive_ty(PrimitiveType::String)),
        | Literal::Char(c) => (Literal::Char(*c), primitive_ty(PrimitiveType::Char)),
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let id: ss::ValueId =
        derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, 0), 0);
    Some(LiteralSynOutcome::Value { id, value: ss::Value::Lit(lit), ty })
}

/// The rejection of an intrinsic `Internal` term, carried as a query value so
/// the checker routes decisions through queries and keeps the writer as a sink.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn internal_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>, env: EnvData<'db>,
) -> Option<crate::check::TyckError> {
    let _ = env;
    match data.scoped(db).terms.get(&term.id(db))? {
        | su::Term::Internal(su::Internal::Monad | su::Internal::Algebra) => {
            Some(crate::check::TyckError::Expressivity(
                "`Monad` and `Algebra` are ordinary library bindings, not intrinsic terms",
            ))
        }
        | _ => None,
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

/// The intermediate state between the judgment and finish phases of a check,
/// carried as a tracked struct so both phases are separate queries.
#[salsa::tracked]
pub struct Judgments<'db> {
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub scoped: su::ScopedArena,
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub statics: StaticsArena,
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub errors: Vec<crate::check::TyckErrorEntry>,
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub observations: Vec<crate::check::TyckObservation>,
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub root: Option<ss::TermAnnId>,
    #[tracked]
    pub root_slot: u32,
}

/// The intermediate state between the hole-resolution and the normalization
/// phases, carried as a tracked struct so each finish step is its own query.
#[salsa::tracked]
pub struct Resolved<'db> {
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub scoped: su::ScopedArena,
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub statics: StaticsArena,
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub errors: Vec<crate::check::TyckErrorEntry>,
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub observations: Vec<crate::check::TyckObservation>,
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub root: Option<ss::TermAnnId>,
    #[tracked]
    pub root_slot: u32,
}

/// The judgment phase: check the whole scoped program, producing the typed
/// arena, the accumulated errors, and the root annotation.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn tyck_judgments<'db>(db: &'db dyn TyckDb, data: ScopedData<'db>) -> Judgments<'db> {
    let mut scoped = data.scoped(db).clone();
    let mut tycker = Tycker::new(db, data, data.spans(db), data.prim(db), &mut scoped);
    crate::check::InternalTerm::fill_intrinsics(&mut tycker);
    let root = tycker.run_judgments_k(data.root(db)).ok();
    let root_slot = tycker.root_slot();
    let statics = std::mem::take(&mut tycker.statics);
    let errors = std::mem::take(&mut tycker.errors);
    let observations = std::mem::take(&mut tycker.observations);
    drop(tycker);
    Judgments::new(db, scoped, statics, errors, observations, root, root_slot)
}

/// The hole-resolution phase: resolve every fillable site and collect the
/// solutions, given the judgment phase's arena.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn resolve_holes_phase<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, judgments: Judgments<'db>,
) -> Resolved<'db> {
    let mut scoped = judgments.scoped(db).clone();
    let mut tycker = Tycker::resume(
        db,
        data,
        data.spans(db),
        data.prim(db),
        &mut scoped,
        judgments.statics(db).clone(),
        judgments.errors(db).clone(),
        judgments.observations(db).clone(),
        judgments.root_slot(db),
    );
    tycker.resolve_holes_and_collect();
    let root_slot = tycker.root_slot();
    let statics = std::mem::take(&mut tycker.statics);
    let errors = std::mem::take(&mut tycker.errors);
    let observations = std::mem::take(&mut tycker.observations);
    let root = *judgments.root(db);
    drop(tycker);
    Resolved::new(db, scoped, statics, errors, observations, root, root_slot)
}

/// The normalization phase: normalize and validate the checked arena, given
/// the hole-resolution phase's arena. Reconstructs the checker around the
/// previous phase's state so error reporting and the writer monad behave
/// exactly as the combined pass did.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn finish_checked<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, resolved: Resolved<'db>,
) -> SourceCheckOutcome {
    let mut scoped = resolved.scoped(db).clone();
    let mut tycker = Tycker::resume(
        db,
        data,
        data.spans(db),
        data.prim(db),
        &mut scoped,
        resolved.statics(db).clone(),
        resolved.errors(db).clone(),
        resolved.observations(db).clone(),
        resolved.root_slot(db),
    );
    match resolved.root(db) {
        | None => {
            let reports = tycker.error_reports();
            SourceCheckOutcome::Rejected(RejectedSource {
                statics: tycker.statics,
                reports,
                observations: tycker.observations,
            })
        }
        | Some(root) => match tycker.normalize_and_validate_k() {
            | Ok(()) => SourceCheckOutcome::Checked(CheckedSource {
                statics: tycker.statics,
                root: *root,
                observations: tycker.observations,
            }),
            | Err(KontFailure) => {
                let reports = tycker.error_reports();
                SourceCheckOutcome::Rejected(RejectedSource {
                    statics: tycker.statics,
                    reports,
                    observations: tycker.observations,
                })
            }
        },
    }
}

// The outcome owns its arenas and reports and contains no database-tied references.
// The non-Update escape hatch stays until the judgment layer gains structural equality.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn check_source<'db>(db: &'db dyn TyckDb, data: ScopedData<'db>) -> TyckOutput {
    let judgments = tyck_judgments(db, data);
    let resolved = resolve_holes_phase(db, data, judgments);
    let outcome = finish_checked(db, data, resolved);
    TyckOutput { scoped: resolved.scoped(db).clone(), outcome }
}
