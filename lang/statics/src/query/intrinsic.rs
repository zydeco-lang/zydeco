//! Canonical intrinsic nodes and compiler-generated internal terms.

use super::*;

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
                5 + zydeco_syntax::PrimitiveType::all()
                    .position(|candidate| candidate == primitive)
                    .expect("every primitive participates in the intrinsic singletons")
                    as u32
            }
        }
    }
}

/// An interned intrinsic key, for use as a salsa query key.
#[salsa::interned]
pub struct InternedIntrinsic<'db> {
    #[returns(clone)]
    pub key: IntrinsicKey,
}

/// The singleton nodes of one intrinsic kind or type, produced by a query and
/// materialized by the checker before any judgment reads the `IntrinsicStatics`
/// cache. See `docs/proposals/query-owned-statics.md` for the fill-before-read
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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

/// The rejection of an intrinsic `Internal` term, carried as a query value so
/// the checker routes decisions through queries and keeps the writer as a sink.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
