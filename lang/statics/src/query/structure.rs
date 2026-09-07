//! Site-keyed producers for named terms, labels, products, and projections.

use super::*;

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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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

/// An interned list of term annotations, for use as a salsa query key.
#[salsa::interned]
pub struct InternedConsItems<'db> {
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
pub fn cons_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    items: InternedConsItems<'db>, tail: InternedTermAnn<'db>, occurrence: u32,
) -> Option<ConsSynOutcome> {
    let su::Term::Cons(_) = data.scoped(db).terms.get(&term.id(db))? else {
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
    let mut components = Vec::with_capacity(item_values.len() + 1);
    let mut component_tys = Vec::with_capacity(item_values.len() + 1);
    for (value, head_ty) in item_values {
        components.push(value);
        component_tys.push(head_ty);
    }
    components.push(tail_value);
    component_tys.push(tail_ty);
    let prod_id: ss::TypeId = derived_id(key_space, 0);
    let prods = vec![(prod_id, ss::Type::Prod(ss::Prod(component_tys)))];
    let cons_id: ss::ValueId = derived_id(key_space, 1);
    let cons = ss::Value::VCons(components);
    Some(ConsSynOutcome { vtype, prods, cons_id, cons, ann: prod_id })
}

/// An interned projection judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedProjInput<'db> {
    #[returns(clone)]
    pub head: ss::ValueId,
    #[returns(clone)]
    pub name: ss::FieldName,
    #[returns(clone)]
    pub products: Vec<(ss::TypeId, usize)>,
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
