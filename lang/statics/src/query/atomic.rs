//! Synthesis and analysis queries for literals, units, variables, and holes.

use super::*;

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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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

/// The synthesized judgment of a variable term, keyed on its merge-fold
/// annotation cell.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
