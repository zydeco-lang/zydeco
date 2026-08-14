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
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>, occurrence: u32,
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
        derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence), 0);
    Some(LiteralSynOutcome::Value { id, value: ss::Value::Lit(lit), ty })
}

/// The synthesized judgment of a hole term: the fill identifier standing for
/// the missing node, derived at the term's site without touching the arena.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn term_hole_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>, occurrence: u32,
) -> Option<ss::FillId> {
    let su::Term::Hole(su::Hole) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    Some(derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence), 0))
}

/// The synthesized judgment of a trivial term: the unit value whose type is
/// the query-owned unit singleton.
#[derive(Clone, Debug)]
pub struct TrivSynOutcome {
    pub id: ss::ValueId,
    pub value: ss::Value,
    pub ty: ss::TypeId,
}

/// The synthesized judgment of a trivial term.
///
/// Every `()` checks to the unit singleton type, so the judgment shares the
/// query-owned intrinsic unit node instead of building a fresh one per site;
/// the nodes are structurally identical and closed.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn triv_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>, occurrence: u32,
) -> Option<TrivSynOutcome> {
    let su::Term::Triv(su::Triv) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let key = InternedIntrinsic::new(db, IntrinsicKey::Unit);
    let IntrinsicSingleton::Type { ty: (ty, _), .. } = intrinsic_singleton(db, data, key) else {
        unreachable!("the unit singleton is type-producing")
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let id: ss::ValueId =
        derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence), 0);
    Some(TrivSynOutcome { id, value: ss::Value::Triv(ss::Triv), ty })
}

/// The synthesized judgment of a variable term.
///
/// The variable's annotation — the merge-fold cell `annotations_var[def]` — is
/// still computed by the checker's pattern pass, so it enters the query as an
/// input; the fold itself becomes query-owned once the pattern DAG migrates.
/// The set arm is a pure environment lookup, and the type arm derives the
/// `Value::Var` node at the term's site. The kind arm reads the arena through
/// the recursive-type alias chain, so it stays checker-side for now and the
/// query reports `None`.
#[derive(Clone, Debug)]
pub enum VarSynOutcome {
    Kind { id: ss::KindId },
    Value { id: ss::ValueId, value: ss::Value, ty: ss::TypeId },
}

/// An interned type annotation, for use as a salsa query key.
#[salsa::interned]
pub struct InternedAnn<'db> {
    pub id: ss::AnnId,
}

/// The synthesized judgment of a variable term, keyed on its merge-fold
/// annotation cell.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn var_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, env: EnvData<'db>, term: InternedTerm<'db>,
    annotation: InternedAnn<'db>, occurrence: u32,
) -> Option<VarSynOutcome> {
    let su::Term::Var(def) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let def = *def;
    match annotation.id(db) {
        | ss::AnnId::Set => {
            let ss::AnnId::Kind(kd) = env.env(db)[&def] else {
                unreachable!("kind-bound variables carry kind annotations")
            };
            Some(VarSynOutcome::Kind { id: kd })
        }
        | ss::AnnId::Type(ty) => {
            let site_space = term.id(db).key_space().as_u64();
            let site_raw = term.id(db).raw().into_u32();
            let id: ss::ValueId = derived_id(
                KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence),
                0,
            );
            Some(VarSynOutcome::Value { id, value: ss::Value::Var(def), ty })
        }
        | ss::AnnId::Kind(_) => None,
    }
}

/// An interned scoped pattern, for use as a salsa query key.
#[salsa::interned]
pub struct InternedPat<'db> {
    pub id: su::PatId,
}

/// The synthesized judgment of a hole pattern: it always fails with a missing
/// annotation, produced without touching the arena.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn pat_hole_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, pat: InternedPat<'db>,
) -> Option<crate::check::TyckError> {
    let su::Pattern::Hole(su::Hole) = data.scoped(db).pats.get(&pat.id(db))? else {
        return None;
    };
    Some(crate::check::TyckError::MissingAnnotation)
}

/// The synthesized judgment of a trivial pattern: the unit value pattern whose
/// type is the query-owned unit singleton.
#[derive(Clone, Debug)]
pub struct PatTrivSynOutcome {
    pub id: ss::VPatId,
    pub value: ss::ValuePattern,
    pub ty: ss::TypeId,
}

/// The synthesized judgment of a trivial pattern, mirroring the trivial term.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn pat_triv_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, pat: InternedPat<'db>, occurrence: u32,
) -> Option<PatTrivSynOutcome> {
    let su::Pattern::Triv(su::Triv) = data.scoped(db).pats.get(&pat.id(db))? else {
        return None;
    };
    let key = InternedIntrinsic::new(db, IntrinsicKey::Unit);
    let IntrinsicSingleton::Type { ty: (ty, _), .. } = intrinsic_singleton(db, data, key) else {
        unreachable!("the unit singleton is type-producing")
    };
    let site_space = pat.id(db).key_space().as_u64();
    let site_raw = pat.id(db).raw().into_u32();
    let id: ss::VPatId =
        derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence), 0);
    Some(PatTrivSynOutcome { id, value: ss::ValuePattern::Triv(ss::Triv), ty })
}

/// An interned term annotation, for use as a salsa query key.
#[salsa::interned]
pub struct InternedTermAnn<'db> {
    pub id: ss::TermAnnId,
}

/// The synthesized judgment of a named term, keyed on its inner term's
/// judgment. The type arm (a named type whose payload is itself a type)
/// allocates the label kind and the named type node; the rejection arms
/// surface as errors. The value arm reads the arena through `lub`, so it
/// stays checker-side and the query reports `None`.
#[derive(Clone, Debug)]
pub enum NamedSynOutcome {
    Type { kind_id: ss::KindId, kind: ss::Kind, named_id: ss::TypeId, named: ss::Type },
    Error(crate::check::TyckError),
}

/// The synthesized judgment of a named term.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn named_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    inner: InternedTermAnn<'db>, occurrence: u32,
) -> Option<NamedSynOutcome> {
    let su::Term::Named(su::Named(name, _inner_term)) = data.scoped(db).terms.get(&term.id(db))?
    else {
        return None;
    };
    let name = name.clone();
    match inner.id(db) {
        | ss::TermAnnId::Type(inner, kd) => {
            let site_space = term.id(db).key_space().as_u64();
            let site_raw = term.id(db).raw().into_u32();
            let key_space =
                KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
            let kind_id: ss::KindId = derived_id(key_space, 0);
            let named_id: ss::TypeId = derived_id(key_space, 1);
            Some(NamedSynOutcome::Type {
                kind_id,
                kind: ss::Kind::Label(ss::Label(name.clone(), kd)),
                named_id,
                named: ss::Type::Named(ss::Named(name, inner)),
            })
        }
        | ss::TermAnnId::Hole(_) => {
            Some(NamedSynOutcome::Error(crate::check::TyckError::MissingAnnotation))
        }
        | ss::TermAnnId::Kind(_) | ss::TermAnnId::Compu(_, _) => {
            Some(NamedSynOutcome::Error(crate::check::TyckError::SortMismatch))
        }
        | ss::TermAnnId::Value(_, _) => None,
    }
}

/// The synthesized judgment of a label term, keyed on its inner term's
/// judgment. The kind arm allocates the label kind node; the rejection arms
/// surface as errors. The type arm reads the arena through `lub`, so it stays
/// checker-side and the query reports `None`.
#[derive(Clone, Debug)]
pub enum LabelSynOutcome {
    Kind { id: ss::KindId, kind: ss::Kind },
    Error(crate::check::TyckError),
}

/// The synthesized judgment of a label term.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn label_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    inner: InternedTermAnn<'db>, occurrence: u32,
) -> Option<LabelSynOutcome> {
    let su::Term::Label(su::Label(name, _inner_term)) = data.scoped(db).terms.get(&term.id(db))?
    else {
        return None;
    };
    let name = name.clone();
    match inner.id(db) {
        | ss::TermAnnId::Kind(inner) => {
            let site_space = term.id(db).key_space().as_u64();
            let site_raw = term.id(db).raw().into_u32();
            let id: ss::KindId = derived_id(
                KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence),
                0,
            );
            Some(LabelSynOutcome::Kind { id, kind: ss::Kind::Label(ss::Label(name, inner)) })
        }
        | ss::TermAnnId::Hole(_) => {
            Some(LabelSynOutcome::Error(crate::check::TyckError::MissingAnnotation))
        }
        | ss::TermAnnId::Value(_, _) | ss::TermAnnId::Compu(_, _) => {
            Some(LabelSynOutcome::Error(crate::check::TyckError::SortMismatch))
        }
        | ss::TermAnnId::Type(_, _) => None,
    }
}

/// An interned pattern annotation, for use as a salsa query key.
#[salsa::interned]
pub struct InternedPatAnn<'db> {
    pub id: ss::PatAnnId,
}

/// An interned value pattern identifier, for use as a salsa query key.
#[salsa::interned]
pub struct InternedVPat<'db> {
    pub id: ss::VPatId,
}

/// An interned value identifier, for use as a salsa query key.
#[salsa::interned]
pub struct InternedValue<'db> {
    pub id: ss::ValueId,
}

/// The synthesized judgment of a named pattern, keyed on its inner pattern's
/// judgment. The type arm allocates the label kind and the named type-pattern
/// node; the kind arm surfaces as an expressivity rejection. The value arm
/// reads the arena through `lub`, so it stays checker-side and the query
/// reports `None`.
#[derive(Clone, Debug)]
pub enum PatNamedSynOutcome {
    Type { kind_id: ss::KindId, kind: ss::Kind, named_id: ss::TPatId, named: ss::TypePattern },
    Error(crate::check::TyckError),
}

/// The synthesized judgment of a named pattern.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn pat_named_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, pat: InternedPat<'db>, inner: InternedPatAnn<'db>,
    occurrence: u32,
) -> Option<PatNamedSynOutcome> {
    let su::Pattern::Named(su::Named(name, _inner_pat)) = data.scoped(db).pats.get(&pat.id(db))?
    else {
        return None;
    };
    let name = name.clone();
    match inner.id(db) {
        | ss::PatAnnId::Type(inner, inner_kind) => {
            let site_space = pat.id(db).key_space().as_u64();
            let site_raw = pat.id(db).raw().into_u32();
            let key_space =
                KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
            let kind_id: ss::KindId = derived_id(key_space, 0);
            let named_id: ss::TPatId = derived_id(key_space, 1);
            Some(PatNamedSynOutcome::Type {
                kind_id,
                kind: ss::Kind::Label(ss::Label(name.clone(), inner_kind)),
                named_id,
                named: ss::TypePattern::Named(ss::Named(name, inner)),
            })
        }
        | ss::PatAnnId::Kind(_) => Some(PatNamedSynOutcome::Error(
            crate::check::TyckError::Expressivity("named kind components are not supported"),
        )),
        | ss::PatAnnId::Value(_, _) => None,
    }
}

/// An interned list of term annotations, for use as a salsa query key.
#[salsa::interned]
pub struct InternedConsItems<'db> {
    pub items: Vec<ss::TermAnnId>,
}

/// The synthesized judgment of a consumed term, keyed on its items' and
/// tail's judgments. Every arm is allocation: the right-nested product type
/// chain over the shared vtype singleton, and the consumed value node. The
/// per-item sort rejections stay at their checker-side abort points (they
/// happen mid-fold), so this query only ever sees value outcomes.
#[derive(Clone, Debug)]
pub struct ConsSynOutcome {
    pub vtype: ss::KindId,
    /// The product type nodes in build order (innermost first).
    pub prods: Vec<(ss::TypeId, ss::Type)>,
    pub cons_id: ss::ValueId,
    pub cons: ss::Value,
    pub ann: ss::TypeId,
}

/// The synthesized judgment of a consumed term.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn cons_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    items: InternedConsItems<'db>, tail: InternedTermAnn<'db>, occurrence: u32,
) -> Option<ConsSynOutcome> {
    let su::Term::Cons(su::ConsN(_, _)) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let ss::TermAnnId::Value(tail_value, tail_ty) = tail.id(db) else {
        return None;
    };
    let item_values = items
        .items(db)
        .iter()
        .map(|outcome| match outcome {
            | ss::TermAnnId::Value(value, ty) => (*value, *ty),
            | _ => unreachable!("consumed items are value judgments"),
        })
        .collect::<Vec<_>>();
    let vtype = {
        let key = InternedIntrinsic::new(db, IntrinsicKey::VType);
        let IntrinsicSingleton::Kind { id, .. } = intrinsic_singleton(db, data, key) else {
            unreachable!("the vtype singleton is kind-producing")
        };
        id
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    let mut ann = tail_ty;
    let mut prods = Vec::with_capacity(item_values.len());
    for (idx, (_, head_ty)) in item_values.iter().rev().enumerate() {
        let id: ss::TypeId = derived_id(key_space, idx as u32);
        prods.push((id, ss::Type::Prod(ss::Prod(*head_ty, ann))));
        ann = id;
    }
    let cons_id: ss::ValueId = derived_id(key_space, item_values.len() as u32);
    let cons = ss::Value::VCons(ss::ConsN(
        item_values.into_iter().map(|(value, _)| value).collect(),
        tail_value,
    ));
    Some(ConsSynOutcome { vtype, prods, cons_id, cons, ann })
}

/// An interned list of pattern annotations, for use as a salsa query key.
#[salsa::interned]
pub struct InternedPatItems<'db> {
    pub items: Vec<ss::PatAnnId>,
}

/// The synthesized judgment of a consumed pattern, keyed on its items' and
/// tail's judgments, mirroring the consumed term: the right-nested product
/// chain over the shared vtype singleton and the consumed value-pattern node.
#[derive(Clone, Debug)]
pub struct PatConsSynOutcome {
    pub vtype: ss::KindId,
    /// The product type nodes in build order (innermost first).
    pub prods: Vec<(ss::TypeId, ss::Type)>,
    pub pat_id: ss::VPatId,
    pub pat: ss::ValuePattern,
    pub ann: ss::TypeId,
}

/// The synthesized judgment of a consumed pattern.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn pat_cons_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, pat: InternedPat<'db>,
    items: InternedPatItems<'db>, tail: InternedPatAnn<'db>, occurrence: u32,
) -> Option<PatConsSynOutcome> {
    let su::Pattern::Cons(su::ConsN(_, _)) = data.scoped(db).pats.get(&pat.id(db))? else {
        return None;
    };
    let ss::PatAnnId::Value(tail_value, tail_ty) = tail.id(db) else {
        return None;
    };
    let item_values = items
        .items(db)
        .iter()
        .map(|outcome| match outcome {
            | ss::PatAnnId::Value(vpat, ty) => (*vpat, *ty),
            | _ => unreachable!("consumed pattern items are value judgments"),
        })
        .collect::<Vec<_>>();
    let vtype = {
        let key = InternedIntrinsic::new(db, IntrinsicKey::VType);
        let IntrinsicSingleton::Kind { id, .. } = intrinsic_singleton(db, data, key) else {
            unreachable!("the vtype singleton is kind-producing")
        };
        id
    };
    let site_space = pat.id(db).key_space().as_u64();
    let site_raw = pat.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    let mut ann = tail_ty;
    let mut prods = Vec::with_capacity(item_values.len());
    for (idx, (_, head_ty)) in item_values.iter().rev().enumerate() {
        let id: ss::TypeId = derived_id(key_space, idx as u32);
        prods.push((id, ss::Type::Prod(ss::Prod(*head_ty, ann))));
        ann = id;
    }
    let pat_id: ss::VPatId = derived_id(key_space, item_values.len() as u32);
    let pat = ss::ValuePattern::VCons(ss::ConsN(
        item_values.into_iter().map(|(vpat, _)| vpat).collect(),
        tail_value,
    ));
    Some(PatConsSynOutcome { vtype, prods, pat_id, pat, ann })
}

/// The allocation tail of a thunk judgment, shared by both modes: the thunk
/// type node (an application of the query-owned thunk singleton to the body
/// type) and the thunk value node, keyed on the checked body only.
#[derive(Clone, Debug)]
pub struct ThunkSynOutcome {
    pub thk_ty_id: ss::TypeId,
    pub thk_ty: ss::Type,
    pub vtype: ss::KindId,
    pub thunk_id: ss::ValueId,
    pub thunk: ss::Value,
}

/// The synthesized judgment of a thunk term.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn thunk_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    body: InternedTermAnn<'db>, occurrence: u32,
) -> Option<ThunkSynOutcome> {
    let su::Term::Thunk(su::Thunk(_)) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let ss::TermAnnId::Compu(body_out, body_ty) = body.id(db) else {
        return None;
    };
    let thk = {
        let key = InternedIntrinsic::new(db, IntrinsicKey::Thk);
        let IntrinsicSingleton::Type { ty: (id, _), .. } = intrinsic_singleton(db, data, key)
        else {
            unreachable!("the thunk singleton is type-producing")
        };
        id
    };
    let vtype = {
        let key = InternedIntrinsic::new(db, IntrinsicKey::VType);
        let IntrinsicSingleton::Kind { id, .. } = intrinsic_singleton(db, data, key) else {
            unreachable!("the vtype singleton is kind-producing")
        };
        id
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    let thk_ty_id: ss::TypeId = derived_id(key_space, 0);
    let thunk_id: ss::ValueId = derived_id(key_space, 1);
    Some(ThunkSynOutcome {
        thk_ty_id,
        thk_ty: ss::Type::App(ss::App(thk, body_ty)),
        vtype,
        thunk_id,
        thunk: ss::Value::Thunk(ss::Thunk(body_out)),
    })
}

/// The allocation tail of a return judgment, shared by both modes: the return
/// type node (an application of the query-owned return singleton to the body
/// type) and the return computation node.
#[derive(Clone, Debug)]
pub struct RetSynOutcome {
    pub ret_ty_id: ss::TypeId,
    pub ret_ty: ss::Type,
    pub vtype: ss::KindId,
    pub ret_id: ss::CompuId,
    pub ret: ss::Computation,
}

/// The synthesized judgment of a return term.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn ret_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    body: InternedTermAnn<'db>, occurrence: u32,
) -> Option<RetSynOutcome> {
    let su::Term::Ret(su::Return(_)) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let ss::TermAnnId::Value(body_out, body_ty) = body.id(db) else {
        return None;
    };
    let ret = {
        let key = InternedIntrinsic::new(db, IntrinsicKey::Ret);
        let IntrinsicSingleton::Type { ty: (id, _), .. } = intrinsic_singleton(db, data, key)
        else {
            unreachable!("the return singleton is type-producing")
        };
        id
    };
    let vtype = {
        let key = InternedIntrinsic::new(db, IntrinsicKey::VType);
        let IntrinsicSingleton::Kind { id, .. } = intrinsic_singleton(db, data, key) else {
            unreachable!("the vtype singleton is kind-producing")
        };
        id
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    let ret_ty_id: ss::TypeId = derived_id(key_space, 0);
    let ret_id: ss::CompuId = derived_id(key_space, 1);
    Some(RetSynOutcome {
        ret_ty_id,
        ret_ty: ss::Type::App(ss::App(ret, body_ty)),
        vtype,
        ret_id,
        ret: ss::Computation::Ret(ss::Return(body_out)),
    })
}

/// The allocation tail of a force judgment: the force computation node, keyed
/// on the checked body and the force type the checker destructured from the
/// body's thunk type.
#[derive(Clone, Debug)]
pub struct ForceSynOutcome {
    pub id: ss::CompuId,
    pub compu: ss::Computation,
    pub ann: ss::TypeId,
}

/// An interned force judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedForceInput<'db> {
    pub body: ss::ValueId,
    pub force_ty: ss::TypeId,
}

/// The synthesized judgment of a force term.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn force_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    input: InternedForceInput<'db>, occurrence: u32,
) -> Option<ForceSynOutcome> {
    let su::Term::Force(su::Force(_)) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let id: ss::CompuId =
        derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence), 0);
    Some(ForceSynOutcome {
        id,
        compu: ss::Computation::Force(ss::Force(input.body(db))),
        ann: input.force_ty(db),
    })
}

/// An interned bind judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedDoInput<'db> {
    pub binder: ss::VPatId,
    pub bindee: ss::CompuId,
    pub tail: ss::CompuId,
    pub ann: ss::TypeId,
}

/// The allocation tail of a bind judgment: the bind computation node.
#[derive(Clone, Debug)]
pub struct DoSynOutcome {
    pub id: ss::CompuId,
    pub compu: ss::Computation,
    pub ann: ss::TypeId,
}

/// The synthesized judgment of a bind term.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn do_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    input: InternedDoInput<'db>, occurrence: u32,
) -> Option<DoSynOutcome> {
    let su::Term::Do(su::Bind { .. }) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let id: ss::CompuId =
        derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence), 0);
    Some(DoSynOutcome {
        id,
        compu: ss::Computation::Do(ss::Bind {
            binder: input.binder(db),
            bindee: input.bindee(db),
            tail: input.tail(db),
        }),
        ann: input.ann(db),
    })
}

/// The allocation tail of a let judgment, split by the tail's sort: a let
/// value node or a let computation node.
#[derive(Clone, Debug)]
pub enum LetSynOutcome {
    Value { id: ss::ValueId, value: ss::Value, ann: ss::TypeId },
    Compu { id: ss::CompuId, compu: ss::Computation, ann: ss::TypeId },
}

/// The synthesized judgment of a let term, keyed on its binder, bindee, and
/// checked tail.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn let_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>, binder: InternedVPat<'db>,
    bindee: InternedValue<'db>, tail: InternedTermAnn<'db>, occurrence: u32,
) -> Option<LetSynOutcome> {
    let su::Term::Let(su::Let { .. }) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    match tail.id(db) {
        | ss::TermAnnId::Value(tail, ann) => {
            let id: ss::ValueId = derived_id(key_space, 0);
            Some(LetSynOutcome::Value {
                id,
                value: ss::Value::Let(ss::Let {
                    binder: binder.id(db),
                    bindee: bindee.id(db),
                    tail,
                }),
                ann,
            })
        }
        | ss::TermAnnId::Compu(tail, ann) => {
            let id: ss::CompuId = derived_id(key_space, 0);
            Some(LetSynOutcome::Compu {
                id,
                compu: ss::Computation::Let(ss::Let {
                    binder: binder.id(db),
                    bindee: bindee.id(db),
                    tail,
                }),
                ann,
            })
        }
        | ss::TermAnnId::Hole(_) | ss::TermAnnId::Kind(_) | ss::TermAnnId::Type(_, _) => None,
    }
}

/// The shape of an application judgment: which sorts the function and
/// argument were checked as.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum AppKind {
    ValueValue { function: ss::ValueId, argument: ss::ValueId },
    ValueType { function: ss::ValueId, argument: ss::TypeId },
    CompuValue { function: ss::CompuId, argument: ss::ValueId },
    CompuType { function: ss::CompuId, argument: ss::TypeId },
}

/// An interned application judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedAppInput<'db> {
    pub kind: AppKind,
    /// The annotation recorded on the application node.
    pub ann: ss::TypeId,
    /// The type reported by the judgment; usually the annotation, but the
    /// polymorphic computation application reports the substituted body type.
    pub reported: ss::TypeId,
}

/// The allocation tail of an application judgment, split by the application's
/// sort.
#[derive(Clone, Debug)]
pub enum AppSynOutcome {
    Value { id: ss::ValueId, value: ss::Value, ann: ss::TypeId, reported: ss::TypeId },
    Compu { id: ss::CompuId, compu: ss::Computation, ann: ss::TypeId, reported: ss::TypeId },
}

/// The synthesized judgment of an application term, keyed on the checked
/// function and argument plus the result types the checker destructured from
/// the function's arrow or forall.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn app_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    input: InternedAppInput<'db>, occurrence: u32,
) -> Option<AppSynOutcome> {
    let su::Term::App(su::App(_, _)) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    match input.kind(db) {
        | AppKind::ValueValue { function, argument } => {
            let id: ss::ValueId = derived_id(key_space, 0);
            Some(AppSynOutcome::Value {
                id,
                value: ss::Value::VApp(ss::App(function, argument)),
                ann: input.ann(db),
                reported: input.reported(db),
            })
        }
        | AppKind::ValueType { function, argument } => {
            let id: ss::ValueId = derived_id(key_space, 0);
            Some(AppSynOutcome::Value {
                id,
                value: ss::Value::TApp(ss::App(function, argument)),
                ann: input.ann(db),
                reported: input.reported(db),
            })
        }
        | AppKind::CompuValue { function, argument } => {
            let id: ss::CompuId = derived_id(key_space, 0);
            Some(AppSynOutcome::Compu {
                id,
                compu: ss::Computation::VApp(ss::App(function, argument)),
                ann: input.ann(db),
                reported: input.reported(db),
            })
        }
        | AppKind::CompuType { function, argument } => {
            let id: ss::CompuId = derived_id(key_space, 0);
            Some(AppSynOutcome::Compu {
                id,
                compu: ss::Computation::TApp(ss::App(function, argument)),
                ann: input.ann(db),
                reported: input.reported(db),
            })
        }
    }
}

/// An interned fixpoint judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedFixInput<'db> {
    pub binder: ss::VPatId,
    pub body: ss::CompuId,
    pub ann: ss::TypeId,
}

/// The allocation tail of a fixpoint judgment: the fix computation node.
#[derive(Clone, Debug)]
pub struct FixSynOutcome {
    pub id: ss::CompuId,
    pub compu: ss::Computation,
    pub ann: ss::TypeId,
}

/// The synthesized judgment of a fixpoint term.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn fix_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    input: InternedFixInput<'db>, occurrence: u32,
) -> Option<FixSynOutcome> {
    let su::Term::Fix(su::Fix(_, _)) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let id: ss::CompuId =
        derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence), 0);
    Some(FixSynOutcome {
        id,
        compu: ss::Computation::Fix(ss::Fix(input.binder(db), input.body(db))),
        ann: input.ann(db),
    })
}

/// The shape of a hole's analyzed annotation: a kind (a type hole), a value
/// type (a value hole), or a computation type (a computation hole).
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum HoleAnaKind {
    Type { kd: ss::KindId },
    Value { ty: ss::TypeId },
    Compu { ty: ss::TypeId },
}

/// An interned hole analysis input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedHoleAna<'db> {
    pub kind: HoleAnaKind,
}

/// The allocation result of an analyzed hole: the stand-in fill, and either a
/// type pre-node holding `Fillable::Fill` or the hole value/computation node.
#[derive(Clone, Debug)]
pub enum HoleAnaOutcome {
    Type { fill: ss::FillId, ty: ss::TypeId, kd: ss::KindId },
    Value { fill: ss::FillId, id: ss::ValueId, value: ss::Value, ann: ss::TypeId },
    Compu { fill: ss::FillId, id: ss::CompuId, compu: ss::Computation, ann: ss::TypeId },
}

/// The analyzed judgment of a hole term.
///
/// The first query to produce fill-state content: the type arm's pre-node is
/// `Fillable::Fill`, derived at the term's site. The checker keeps the
/// resolution side effects (`fill_k`'s solution write, `fill_hints`, and the
/// `fill_scopes` bookkeeping).
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn hole_ana_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    input: InternedHoleAna<'db>, occurrence: u32,
) -> Option<HoleAnaOutcome> {
    let su::Term::Hole(su::Hole) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    let fill: ss::FillId = derived_id(key_space, 0);
    match input.kind(db) {
        | HoleAnaKind::Type { kd } => {
            let ty: ss::TypeId = derived_id(key_space, 1);
            Some(HoleAnaOutcome::Type { fill, ty, kd })
        }
        | HoleAnaKind::Value { ty } => {
            let id: ss::ValueId = derived_id(key_space, 1);
            Some(HoleAnaOutcome::Value { fill, id, value: ss::Value::Hole(ss::Hole), ann: ty })
        }
        | HoleAnaKind::Compu { ty } => {
            let id: ss::CompuId = derived_id(key_space, 1);
            Some(HoleAnaOutcome::Compu {
                fill,
                id,
                compu: ss::Computation::Hole(ss::Hole),
                ann: ty,
            })
        }
    }
}

/// The synthesized judgment of a constructor pattern: it always fails with a
/// missing annotation.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn pat_ctor_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, pat: InternedPat<'db>,
) -> Option<crate::check::TyckError> {
    let su::Pattern::Ctor(_) = data.scoped(db).pats.get(&pat.id(db))? else {
        return None;
    };
    Some(crate::check::TyckError::MissingAnnotation)
}

/// The synthesized judgment of an alias pattern: it always fails with a
/// missing annotation.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn pat_alias_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, pat: InternedPat<'db>,
) -> Option<crate::check::TyckError> {
    let su::Pattern::Alias(_) = data.scoped(db).pats.get(&pat.id(db))? else {
        return None;
    };
    Some(crate::check::TyckError::MissingAnnotation)
}

/// The arm of a pi judgment after its binder checks: which shape the body's
/// judgment took.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum PiSynArm {
    KindArrow { kd_1: ss::KindId, kd_2: ss::KindId },
    ValueForall { ty_2: ss::TypeId, kd_2: ss::KindId },
    Forall { ty_2: ss::TypeId, kd_2: ss::KindId },
    KindMismatch,
    MissingAnnotation,
    SortMismatch,
}

/// An interned pi judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedPiSyn<'db> {
    pub arm: PiSynArm,
    pub tpat: ss::TPatId,
    pub abst: ss::AbstId,
}

/// The allocation tail of a pi judgment: the kind arrow, the value forall, or
/// the computation forall node; the rejections surface as errors.
#[derive(Clone, Debug)]
pub enum PiSynOutcome {
    Kind { id: ss::KindId, kind: ss::Kind },
    Type { id: ss::TypeId, ty: ss::Type, kd: ss::KindId },
    Error(crate::check::TyckError),
}

/// The synthesized judgment of a pi term, keyed on the checked binder and
/// body arm.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn pi_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>, input: InternedPiSyn<'db>,
    occurrence: u32,
) -> Option<PiSynOutcome> {
    let su::Term::Pi(su::Pi(_, _)) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    match input.arm(db) {
        | PiSynArm::KindArrow { kd_1, kd_2 } => {
            let id: ss::KindId = derived_id(key_space, 0);
            Some(PiSynOutcome::Kind { id, kind: ss::Kind::Arrow(ss::Arrow(kd_1, kd_2)) })
        }
        | PiSynArm::ValueForall { ty_2, kd_2 } => {
            let id: ss::TypeId = derived_id(key_space, 0);
            Some(PiSynOutcome::Type {
                id,
                ty: ss::Type::VForall(ss::ValueForall(
                    ss::TypeBinder { pattern: input.tpat(db), witness: input.abst(db) },
                    ty_2,
                )),
                kd: kd_2,
            })
        }
        | PiSynArm::Forall { ty_2, kd_2 } => {
            let id: ss::TypeId = derived_id(key_space, 0);
            Some(PiSynOutcome::Type {
                id,
                ty: ss::Type::Forall(ss::Forall(
                    ss::TypeBinder { pattern: input.tpat(db), witness: input.abst(db) },
                    ty_2,
                )),
                kd: kd_2,
            })
        }
        | PiSynArm::KindMismatch => {
            Some(PiSynOutcome::Error(crate::check::TyckError::KindMismatch))
        }
        | PiSynArm::MissingAnnotation => {
            Some(PiSynOutcome::Error(crate::check::TyckError::MissingAnnotation))
        }
        | PiSynArm::SortMismatch => {
            Some(PiSynOutcome::Error(crate::check::TyckError::SortMismatch))
        }
    }
}

/// The arm of a sigma judgment after its binder checks: an existential, a
/// product, or the kind-level rejection.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum SigmaSynArm {
    Exists { tpat: ss::TPatId, abst: ss::AbstId, body_ty: ss::TypeId },
    Prod { ty_1: ss::TypeId, ty_2: ss::TypeId },
    Expressivity,
}

/// An interned sigma judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedSigmaSyn<'db> {
    pub arm: SigmaSynArm,
}

/// The allocation tail of a sigma judgment: the existential or product type
/// node; the kind-level rejection surfaces as an error.
#[derive(Clone, Debug)]
pub enum SigmaSynOutcome {
    Type { id: ss::TypeId, ty: ss::Type, kd: ss::KindId },
    Error(crate::check::TyckError),
}

/// The synthesized judgment of a sigma term, keyed on the checked binder and
/// body arm.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn sigma_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    input: InternedSigmaSyn<'db>, occurrence: u32,
) -> Option<SigmaSynOutcome> {
    let su::Term::Sigma(su::Sigma(_, _)) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    let vtype = {
        let key = InternedIntrinsic::new(db, IntrinsicKey::VType);
        let IntrinsicSingleton::Kind { id, .. } = intrinsic_singleton(db, data, key) else {
            unreachable!("the vtype singleton is kind-producing")
        };
        id
    };
    match input.arm(db) {
        | SigmaSynArm::Exists { tpat, abst, body_ty } => {
            let id: ss::TypeId = derived_id(key_space, 0);
            Some(SigmaSynOutcome::Type {
                id,
                ty: ss::Type::Exists(ss::Exists::new(
                    ss::TypeBinder { pattern: tpat, witness: abst },
                    body_ty,
                )),
                kd: vtype,
            })
        }
        | SigmaSynArm::Prod { ty_1, ty_2 } => {
            let id: ss::TypeId = derived_id(key_space, 0);
            Some(SigmaSynOutcome::Type { id, ty: ss::Type::Prod(ss::Prod(ty_1, ty_2)), kd: vtype })
        }
        | SigmaSynArm::Expressivity => Some(SigmaSynOutcome::Error(
            crate::check::TyckError::Expressivity("abstract existential kinds are not supported"),
        )),
    }
}

/// The arm of an abstraction judgment after its binder and body checks:
/// which shapes the pattern and body took.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum AbsSynArm {
    TypeFunction {
        tpat: ss::TPatId,
        kd: ss::KindId,
        body_kd: ss::KindId,
        ty: ss::TypeId,
    },
    PolymorphicCompu {
        tpat: ss::TPatId,
        abst: ss::AbstId,
        compu: ss::CompuId,
        body_ty: ss::TypeId,
    },
    PolymorphicValue {
        tpat: ss::TPatId,
        abst: ss::AbstId,
        value: ss::ValueId,
        body_ty: ss::TypeId,
    },
    ValueArrow {
        vpat: ss::VPatId,
        ty: ss::TypeId,
        value: ss::ValueId,
        body_ty: ss::TypeId,
    },
    ValuePackPi {
        vpat: ss::VPatId,
        domain: ss::TypeId,
        first: ss::AbstId,
        rest: Vec<ss::AbstId>,
        codomain: ss::TypeId,
        value: ss::ValueId,
    },
    CompuArrow {
        vpat: ss::VPatId,
        ty: ss::TypeId,
        compu: ss::CompuId,
        body_ty: ss::TypeId,
    },
    CompuPackPi {
        vpat: ss::VPatId,
        domain: ss::TypeId,
        first: ss::AbstId,
        rest: Vec<ss::AbstId>,
        codomain: ss::TypeId,
        compu: ss::CompuId,
    },
    Expressivity,
    SortMismatch,
}

/// An interned abstraction judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedAbsSyn<'db> {
    pub arm: AbsSynArm,
}

/// The allocation tail of an abstraction judgment: the arrow or forall
/// annotation node and the abstraction node; the rejections surface as errors.
#[derive(Clone, Debug)]
pub enum AbsSynOutcome {
    TypeFunction {
        arrow_id: ss::KindId,
        arrow: ss::Kind,
        abs_id: ss::TypeId,
        abs: ss::Type,
    },
    TAbsCompu {
        ann_id: ss::TypeId,
        ann: ss::Type,
        kd: ss::KindId,
        abs_id: ss::CompuId,
        abs: ss::Computation,
    },
    TAbsValue {
        ann_id: ss::TypeId,
        ann: ss::Type,
        kd: ss::KindId,
        abs_id: ss::ValueId,
        abs: ss::Value,
    },
    VAbsValue {
        ann_id: ss::TypeId,
        ann: ss::Type,
        kd: ss::KindId,
        abs_id: ss::ValueId,
        abs: ss::Value,
    },
    VAbsCompu {
        ann_id: ss::TypeId,
        ann: ss::Type,
        kd: ss::KindId,
        abs_id: ss::CompuId,
        abs: ss::Computation,
    },
    Error(crate::check::TyckError),
}

/// The synthesized judgment of an abstraction term, keyed on the checked
/// pattern and body arms.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn abs_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    input: InternedAbsSyn<'db>, occurrence: u32,
) -> Option<AbsSynOutcome> {
    let su::Term::Abs(su::Abs(_, _)) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    let vtype = |db: &dyn TyckDb, data: ScopedData<'_>| {
        let key = InternedIntrinsic::new(db, IntrinsicKey::VType);
        let IntrinsicSingleton::Kind { id, .. } = intrinsic_singleton(db, data, key) else {
            unreachable!("the vtype singleton is kind-producing")
        };
        id
    };
    let ctype = |db: &dyn TyckDb, data: ScopedData<'_>| {
        let key = InternedIntrinsic::new(db, IntrinsicKey::CType);
        let IntrinsicSingleton::Kind { id, .. } = intrinsic_singleton(db, data, key) else {
            unreachable!("the ctype singleton is kind-producing")
        };
        id
    };
    match input.arm(db) {
        | AbsSynArm::TypeFunction { tpat, kd, body_kd, ty } => {
            let arrow_id: ss::KindId = derived_id(key_space, 0);
            let abs_id: ss::TypeId = derived_id(key_space, 1);
            Some(AbsSynOutcome::TypeFunction {
                arrow_id,
                arrow: ss::Kind::Arrow(ss::Arrow(kd, body_kd)),
                abs_id,
                abs: ss::Type::Abs(ss::Abs(tpat, ty)),
            })
        }
        | AbsSynArm::PolymorphicCompu { tpat, abst, compu, body_ty } => {
            let ann_id: ss::TypeId = derived_id(key_space, 0);
            let abs_id: ss::CompuId = derived_id(key_space, 1);
            Some(AbsSynOutcome::TAbsCompu {
                ann_id,
                ann: ss::Type::Forall(ss::Forall(
                    ss::TypeBinder { pattern: tpat, witness: abst },
                    body_ty,
                )),
                kd: ctype(db, data),
                abs_id,
                abs: ss::Computation::TAbs(ss::Abs(tpat, compu)),
            })
        }
        | AbsSynArm::PolymorphicValue { tpat, abst, value, body_ty } => {
            let ann_id: ss::TypeId = derived_id(key_space, 0);
            let abs_id: ss::ValueId = derived_id(key_space, 1);
            Some(AbsSynOutcome::TAbsValue {
                ann_id,
                ann: ss::Type::VForall(ss::ValueForall(
                    ss::TypeBinder { pattern: tpat, witness: abst },
                    body_ty,
                )),
                kd: vtype(db, data),
                abs_id,
                abs: ss::Value::TAbs(ss::Abs(tpat, value)),
            })
        }
        | AbsSynArm::ValueArrow { vpat, ty, value, body_ty } => {
            let ann_id: ss::TypeId = derived_id(key_space, 0);
            let abs_id: ss::ValueId = derived_id(key_space, 1);
            Some(AbsSynOutcome::VAbsValue {
                ann_id,
                ann: ss::Type::VArrow(ss::ValueArrow(ty, body_ty)),
                kd: vtype(db, data),
                abs_id,
                abs: ss::Value::VAbs(ss::Abs(vpat, value)),
            })
        }
        | AbsSynArm::ValuePackPi { vpat, domain, first, rest, codomain, value } => {
            let ann_id: ss::TypeId = derived_id(key_space, 0);
            let abs_id: ss::ValueId = derived_id(key_space, 1);
            Some(AbsSynOutcome::VAbsValue {
                ann_id,
                ann: ss::Type::VPackPi(ss::ValuePackPi {
                    domain,
                    witnesses: ss::PackTelescope::new(first, rest),
                    codomain,
                }),
                kd: vtype(db, data),
                abs_id,
                abs: ss::Value::VAbs(ss::Abs(vpat, value)),
            })
        }
        | AbsSynArm::CompuArrow { vpat, ty, compu, body_ty } => {
            let ann_id: ss::TypeId = derived_id(key_space, 0);
            let abs_id: ss::CompuId = derived_id(key_space, 1);
            Some(AbsSynOutcome::VAbsCompu {
                ann_id,
                ann: ss::Type::Arrow(ss::Arrow(ty, body_ty)),
                kd: ctype(db, data),
                abs_id,
                abs: ss::Computation::VAbs(ss::Abs(vpat, compu)),
            })
        }
        | AbsSynArm::CompuPackPi { vpat, domain, first, rest, codomain, compu } => {
            let ann_id: ss::TypeId = derived_id(key_space, 0);
            let abs_id: ss::CompuId = derived_id(key_space, 1);
            Some(AbsSynOutcome::VAbsCompu {
                ann_id,
                ann: ss::Type::PackPi(ss::PackPi {
                    domain,
                    witnesses: ss::PackTelescope::new(first, rest),
                    codomain,
                }),
                kd: ctype(db, data),
                abs_id,
                abs: ss::Computation::VAbs(ss::Abs(vpat, compu)),
            })
        }
        | AbsSynArm::Expressivity => {
            Some(AbsSynOutcome::Error(crate::check::TyckError::Expressivity(
                "functions cannot abstract over the meta-level `Set`",
            )))
        }
        | AbsSynArm::SortMismatch => {
            Some(AbsSynOutcome::Error(crate::check::TyckError::SortMismatch))
        }
    }
}

/// The arm of a manifest-exists judgment after its definition and binder
/// checks.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ManifestSynArm {
    Kind { pattern: ss::KPatId, definition: ss::KindId, body: ss::TypeId },
    Type { pattern: ss::TPatId, witness: ss::AbstId, definition: ss::TypeId, body: ss::TypeId },
    SortMismatch,
}

/// An interned manifest-exists judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedManifestSyn<'db> {
    pub arm: ManifestSynArm,
}

/// The allocation tail of a manifest-exists judgment: the manifest kind or
/// the manifest existential node; the rejection surfaces as an error.
#[derive(Clone, Debug)]
pub enum ManifestSynOutcome {
    Type { id: ss::TypeId, ty: ss::Type, kd: ss::KindId },
    Error(crate::check::TyckError),
}

/// The synthesized judgment of a manifest-exists term, keyed on the checked
/// definition, binder, and body.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn manifest_exists_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    input: InternedManifestSyn<'db>, occurrence: u32,
) -> Option<ManifestSynOutcome> {
    let su::Term::ManifestExists(su::ManifestExists { .. }) =
        data.scoped(db).terms.get(&term.id(db))?
    else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    let vtype = {
        let key = InternedIntrinsic::new(db, IntrinsicKey::VType);
        let IntrinsicSingleton::Kind { id, .. } = intrinsic_singleton(db, data, key) else {
            unreachable!("the vtype singleton is kind-producing")
        };
        id
    };
    match input.arm(db) {
        | ManifestSynArm::Kind { pattern, definition, body } => {
            let id: ss::TypeId = derived_id(key_space, 0);
            Some(ManifestSynOutcome::Type {
                id,
                ty: ss::Type::ManifestKind(ss::ManifestKind { binder: pattern, definition, body }),
                kd: vtype,
            })
        }
        | ManifestSynArm::Type { pattern, witness, definition, body } => {
            let id: ss::TypeId = derived_id(key_space, 0);
            Some(ManifestSynOutcome::Type {
                id,
                ty: ss::Type::Exists(ss::Exists::with_manifest(
                    ss::TypeBinder { pattern, witness },
                    definition,
                    body,
                )),
                kd: vtype,
            })
        }
        | ManifestSynArm::SortMismatch => {
            Some(ManifestSynOutcome::Error(crate::check::TyckError::SortMismatch))
        }
    }
}

/// An interned data-arms table, for use as a salsa query key.
#[salsa::interned]
pub struct InternedDataArms<'db> {
    pub arms: Vec<(ss::CtorName, ss::TypeId)>,
}

/// An interned codata-arms table, for use as a salsa query key.
#[salsa::interned]
pub struct InternedCoDataArms<'db> {
    pub arms: Vec<(ss::DtorName, ss::TypeId)>,
}

/// The allocation tail of a data or codata declaration: the definition node
/// and the type node referencing it.
#[derive(Clone, Debug)]
pub struct DataSynOutcome {
    pub data_id: ss::DataId,
    pub data: ss::Data,
    pub ty_id: ss::TypeId,
    pub ty: ss::Type,
    pub kd: ss::KindId,
}

/// The allocation tail of a codata declaration.
#[derive(Clone, Debug)]
pub struct CoDataSynOutcome {
    pub codata_id: ss::CoDataId,
    pub codata: ss::CoData,
    pub ty_id: ss::TypeId,
    pub ty: ss::Type,
    pub kd: ss::KindId,
}

/// The synthesized judgment of a data declaration, keyed on the checked arms.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn data_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    arms: InternedDataArms<'db>, kd: InternedKind<'db>, occurrence: u32,
) -> Option<DataSynOutcome> {
    let su::Term::Data(su::Data { .. }) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    let data_id: ss::DataId = derived_id(key_space, 0);
    let ty_id: ss::TypeId = derived_id(key_space, 1);
    Some(DataSynOutcome {
        data_id,
        data: ss::Data::new(arms.arms(db).iter().cloned()),
        ty_id,
        ty: ss::Type::Data(data_id),
        kd: kd.id(db),
    })
}

/// The synthesized judgment of a codata declaration, keyed on the checked
/// arms.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn codata_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    arms: InternedCoDataArms<'db>, kd: InternedKind<'db>, occurrence: u32,
) -> Option<CoDataSynOutcome> {
    let su::Term::CoData(su::CoData { .. }) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    let codata_id: ss::CoDataId = derived_id(key_space, 0);
    let ty_id: ss::TypeId = derived_id(key_space, 1);
    Some(CoDataSynOutcome {
        codata_id,
        codata: ss::CoData::new(arms.arms(db).iter().cloned()),
        ty_id,
        ty: ss::Type::CoData(codata_id),
        kd: kd.id(db),
    })
}

/// An interned match judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedMatchInput<'db> {
    pub scrut: ss::ValueId,
    pub arms: Vec<(ss::VPatId, ss::CompuId)>,
    pub ann: ss::TypeId,
}

/// The allocation tail of a match judgment: the match computation node.
#[derive(Clone, Debug)]
pub struct MatchSynOutcome {
    pub id: ss::CompuId,
    pub compu: ss::Computation,
    pub ann: ss::TypeId,
}

/// The synthesized judgment of a match term, keyed on the checked scrutinee
/// and arms.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn match_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    input: InternedMatchInput<'db>, occurrence: u32,
) -> Option<MatchSynOutcome> {
    let su::Term::Match(su::Match { .. }) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let id: ss::CompuId =
        derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence), 0);
    Some(MatchSynOutcome {
        id,
        compu: ss::Computation::Match(ss::Match {
            scrut: input.scrut(db),
            arms: input
                .arms(db)
                .iter()
                .map(|(binder, tail)| ss::Matcher { binder: *binder, tail: *tail })
                .collect(),
        }),
        ann: input.ann(db),
    })
}

/// An interned constructor judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedCtorInput<'db> {
    pub name: ss::CtorName,
    pub arg: ss::ValueId,
    pub ann: ss::TypeId,
    pub data_id: ss::DataId,
}

/// The allocation tail of a constructor judgment: the constructor value node.
#[derive(Clone, Debug)]
pub struct CtorSynOutcome {
    pub id: ss::ValueId,
    pub value: ss::Value,
    pub ann: ss::TypeId,
}

/// The synthesized judgment of a constructor term, keyed on the checked
/// argument and the destructured data definition.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn ctor_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    input: InternedCtorInput<'db>, occurrence: u32,
) -> Option<CtorSynOutcome> {
    let su::Term::Ctor(su::Ctor(_, _)) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let id: ss::ValueId =
        derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence), 0);
    Some(CtorSynOutcome {
        id,
        value: ss::Value::Ctor(ss::Ctor(input.name(db), input.arg(db))),
        ann: input.ann(db),
    })
}

/// An interned comatch judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedCoMatchInput<'db> {
    pub arms: Vec<(ss::DtorName, ss::CompuId)>,
    pub ann: ss::TypeId,
}

/// The allocation tail of a comatch judgment: the comatch computation node.
#[derive(Clone, Debug)]
pub struct CoMatchSynOutcome {
    pub id: ss::CompuId,
    pub compu: ss::Computation,
    pub ann: ss::TypeId,
}

/// The synthesized judgment of a comatch term, keyed on the checked arms.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn comatch_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    input: InternedCoMatchInput<'db>, occurrence: u32,
) -> Option<CoMatchSynOutcome> {
    let su::Term::CoMatch(su::CoMatch { .. }) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let id: ss::CompuId =
        derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence), 0);
    Some(CoMatchSynOutcome {
        id,
        compu: ss::Computation::CoMatch(ss::CoMatch {
            arms: input
                .arms(db)
                .iter()
                .map(|(dtor, tail)| ss::CoMatcher { dtor: dtor.clone(), tail: *tail })
                .collect(),
        }),
        ann: input.ann(db),
    })
}

/// An interned destructor judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedDtorInput<'db> {
    pub body: ss::CompuId,
    pub dtor: ss::DtorName,
    pub ann: ss::TypeId,
}

/// The allocation tail of a destructor judgment: the destructor computation
/// node.
#[derive(Clone, Debug)]
pub struct DtorSynOutcome {
    pub id: ss::CompuId,
    pub compu: ss::Computation,
    pub ann: ss::TypeId,
}

/// The synthesized judgment of a destructor term, keyed on the checked body.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn dtor_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    input: InternedDtorInput<'db>, occurrence: u32,
) -> Option<DtorSynOutcome> {
    let su::Term::Dtor(su::Dtor(_, _)) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let id: ss::CompuId =
        derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence), 0);
    Some(DtorSynOutcome {
        id,
        compu: ss::Computation::Dtor(ss::Dtor(input.body(db), input.dtor(db))),
        ann: input.ann(db),
    })
}

/// An interned projection judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedProjInput<'db> {
    pub head: ss::ValueId,
    pub name: ss::FieldName,
    pub products: Vec<(ss::TypeId, usize)>,
    pub ann: ss::TypeId,
}

/// The allocation tail of a projection judgment: the projection value node.
#[derive(Clone, Debug)]
pub struct ProjSynOutcome {
    pub id: ss::ValueId,
    pub value: ss::Value,
    pub ann: ss::TypeId,
}

/// The synthesized judgment of a projection term, keyed on the checked head
/// and the resolved field.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn proj_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    input: InternedProjInput<'db>, occurrence: u32,
) -> Option<ProjSynOutcome> {
    let su::Term::Proj(su::Proj(_, _)) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let id: ss::ValueId =
        derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence), 0);
    let field = ss::ResolvedField {
        name: input.name(db),
        target: ss::ProjTarget {
            products: input
                .products(db)
                .iter()
                .map(|(product, position)| ss::ProductProjection {
                    product: *product,
                    position: *position,
                })
                .collect(),
        },
    };
    Some(ProjSynOutcome {
        id,
        value: ss::Value::Proj(ss::Proj(input.head(db), field)),
        ann: input.ann(db),
    })
}

/// The stand-in hole pair of an unannotated variable pattern: the fill and
/// the vtype-annotated type pre-node holding it, derived at the pattern's
/// site.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn pat_var_hole_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, pat: InternedPat<'db>, occurrence: u32,
) -> Option<(ss::FillId, ss::TypeId, ss::KindId)> {
    let su::Pattern::Var(_) = data.scoped(db).pats.get(&pat.id(db))? else {
        return None;
    };
    let vtype = {
        let key = InternedIntrinsic::new(db, IntrinsicKey::VType);
        let IntrinsicSingleton::Kind { id, .. } = intrinsic_singleton(db, data, key) else {
            unreachable!("the vtype singleton is kind-producing")
        };
        id
    };
    let site_space = pat.id(db).key_space().as_u64();
    let site_raw = pat.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    let fill: ss::FillId = derived_id(key_space, 0);
    let ty: ss::TypeId = derived_id(key_space, 1);
    Some((fill, ty, vtype))
}

/// A leaf pattern node: a hole or a variable reference.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum PatLeaf {
    Hole,
    Var(su::DefId),
}

/// An interned leaf-pattern node input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedPatLeafNode<'db> {
    pub leaf: PatLeaf,
    pub ann: ss::AnnId,
}

/// The allocation tail of a leaf pattern judgment, split by the annotation's
/// sort: a kind pattern, a type pattern, or a value pattern node.
#[derive(Clone, Debug)]
pub enum PatLeafOutcome {
    Kind { id: ss::KPatId, pat: ss::KindPattern },
    Type { id: ss::TPatId, pat: ss::TypePattern, kd: ss::KindId },
    Value { id: ss::VPatId, pat: ss::ValuePattern, ty: ss::TypeId },
}

/// The synthesized judgment of a leaf pattern node.
///
/// The node derives at slot 2 of the pattern's site, leaving slots 0 and 1
/// for the variable pattern's stand-in hole pair.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn pat_leaf_node_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, pat: InternedPat<'db>,
    node: InternedPatLeafNode<'db>, occurrence: u32,
) -> Option<PatLeafOutcome> {
    let site_space = pat.id(db).key_space().as_u64();
    let site_raw = pat.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    let leaf = node.leaf(db);
    let _ = data;
    match (leaf, node.ann(db)) {
        | (PatLeaf::Hole, ss::AnnId::Set) => {
            let id: ss::KPatId = derived_id(key_space, 2);
            Some(PatLeafOutcome::Kind { id, pat: ss::KindPattern::Hole(ss::Hole) })
        }
        | (PatLeaf::Var(def), ss::AnnId::Set) => {
            let id: ss::KPatId = derived_id(key_space, 2);
            Some(PatLeafOutcome::Kind { id, pat: ss::KindPattern::Var(def) })
        }
        | (PatLeaf::Hole, ss::AnnId::Kind(kd)) => {
            let id: ss::TPatId = derived_id(key_space, 2);
            Some(PatLeafOutcome::Type { id, pat: ss::TypePattern::Hole(ss::Hole), kd })
        }
        | (PatLeaf::Var(def), ss::AnnId::Kind(kd)) => {
            let id: ss::TPatId = derived_id(key_space, 2);
            Some(PatLeafOutcome::Type { id, pat: ss::TypePattern::Var(def), kd })
        }
        | (PatLeaf::Hole, ss::AnnId::Type(ty)) => {
            let id: ss::VPatId = derived_id(key_space, 2);
            Some(PatLeafOutcome::Value { id, pat: ss::ValuePattern::Hole(ss::Hole), ty })
        }
        | (PatLeaf::Var(def), ss::AnnId::Type(ty)) => {
            let id: ss::VPatId = derived_id(key_space, 2);
            Some(PatLeafOutcome::Value { id, pat: ss::ValuePattern::Var(def), ty })
        }
    }
}

/// An interned constructor-pattern judgment input, for use as a salsa query
/// key.
#[salsa::interned]
pub struct InternedPatCtorInput<'db> {
    pub name: ss::CtorName,
    pub args: ss::VPatId,
    pub ann: ss::TypeId,
    pub data_id: ss::DataId,
}

/// The allocation tail of a constructor-pattern judgment: the constructor
/// value-pattern node.
#[derive(Clone, Debug)]
pub struct PatCtorOutcome {
    pub id: ss::VPatId,
    pub pat: ss::ValuePattern,
    pub ann: ss::TypeId,
}

/// The synthesized judgment of a constructor pattern, keyed on the checked
/// argument pattern and the destructured data definition.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn pat_ctor_ana_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, pat: InternedPat<'db>,
    input: InternedPatCtorInput<'db>, occurrence: u32,
) -> Option<PatCtorOutcome> {
    let su::Pattern::Ctor(_) = data.scoped(db).pats.get(&pat.id(db))? else {
        return None;
    };
    let site_space = pat.id(db).key_space().as_u64();
    let site_raw = pat.id(db).raw().into_u32();
    let id: ss::VPatId =
        derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence), 0);
    Some(PatCtorOutcome {
        id,
        pat: ss::ValuePattern::Ctor(ss::Ctor(input.name(db), input.args(db))),
        ann: input.ann(db),
    })
}

/// An interned alias-pattern judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedPatAliasInput<'db> {
    pub patterns: Vec<ss::VPatId>,
    pub ann: ss::TypeId,
}

/// The allocation tail of an alias-pattern judgment: the alias value-pattern
/// node.
#[derive(Clone, Debug)]
pub struct PatAliasOutcome {
    pub id: ss::VPatId,
    pub pat: ss::ValuePattern,
    pub ann: ss::TypeId,
}

/// The synthesized judgment of an alias pattern, keyed on the checked member
/// patterns.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn pat_alias_ana_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, pat: InternedPat<'db>,
    input: InternedPatAliasInput<'db>, occurrence: u32,
) -> Option<PatAliasOutcome> {
    let su::Pattern::Alias(_) = data.scoped(db).pats.get(&pat.id(db))? else {
        return None;
    };
    let site_space = pat.id(db).key_space().as_u64();
    let site_raw = pat.id(db).raw().into_u32();
    let id: ss::VPatId =
        derived_id(KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence), 0);
    let patterns = ss::ConsN::from_vec(input.patterns(db))?;
    Some(PatAliasOutcome {
        id,
        pat: ss::ValuePattern::Alias(ss::Alias(patterns)),
        ann: input.ann(db),
    })
}

/// The arm of a named pattern's analyzed judgment.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum PatNamedAnaArm {
    Kind { name: ss::FieldName, inner: ss::TPatId, expected: ss::KindId },
    Type { name: ss::FieldName, inner: ss::VPatId, expected: ss::TypeId },
    SortMismatch,
}

/// An interned named-pattern analysis input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedPatNamedAna<'db> {
    pub arm: PatNamedAnaArm,
}

/// The allocation tail of a named pattern's analyzed judgment.
#[derive(Clone, Debug)]
pub enum PatNamedAnaOutcome {
    Type { id: ss::TPatId, pat: ss::TypePattern, kd: ss::KindId },
    Value { id: ss::VPatId, pat: ss::ValuePattern, ty: ss::TypeId },
    Error(crate::check::TyckError),
}

/// The synthesized judgment of a named pattern's analyzed arm, keyed on the
/// checked inner pattern and the destructured expected label.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn pat_named_ana_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, pat: InternedPat<'db>,
    input: InternedPatNamedAna<'db>, occurrence: u32,
) -> Option<PatNamedAnaOutcome> {
    let su::Pattern::Named(_) = data.scoped(db).pats.get(&pat.id(db))? else {
        return None;
    };
    let site_space = pat.id(db).key_space().as_u64();
    let site_raw = pat.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    match input.arm(db) {
        | PatNamedAnaArm::Kind { name, inner, expected } => {
            let id: ss::TPatId = derived_id(key_space, 0);
            Some(PatNamedAnaOutcome::Type {
                id,
                pat: ss::TypePattern::Named(ss::Named(name, inner)),
                kd: expected,
            })
        }
        | PatNamedAnaArm::Type { name, inner, expected } => {
            let id: ss::VPatId = derived_id(key_space, 0);
            Some(PatNamedAnaOutcome::Value {
                id,
                pat: ss::ValuePattern::Named(ss::Named(name, inner)),
                ty: expected,
            })
        }
        | PatNamedAnaArm::SortMismatch => {
            Some(PatNamedAnaOutcome::Error(crate::check::TyckError::SortMismatch))
        }
    }
}

/// The synthesized judgment of a projection pattern: it always fails with a
/// missing annotation.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
pub fn pat_project_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, pat: InternedPat<'db>,
) -> Option<crate::check::TyckError> {
    let su::Pattern::Project(_) = data.scoped(db).pats.get(&pat.id(db))? else {
        return None;
    };
    Some(crate::check::TyckError::MissingAnnotation)
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
