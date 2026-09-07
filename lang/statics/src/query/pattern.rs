//! Site-keyed producers for checked patterns.

use super::*;

/// The synthesized judgment of a trivial pattern: the unit value pattern whose
/// type is the query-owned unit singleton.
#[derive(Clone, Debug)]
pub struct PatTrivSynOutcome {
    pub id: ss::VPatId,
    pub value: ss::ValuePattern,
    pub ty: ss::TypeId,
}

/// The synthesized judgment of a trivial pattern, mirroring the trivial term.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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

/// An interned list of pattern annotations, for use as a salsa query key.
#[salsa::interned]
pub struct InternedPatItems<'db> {
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
pub fn pat_cons_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, pat: InternedPat<'db>,
    items: InternedPatItems<'db>, tail: InternedPatAnn<'db>, occurrence: u32,
) -> Option<PatConsSynOutcome> {
    let su::Pattern::Cons(_) = data.scoped(db).pats.get(&pat.id(db))? else {
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
    let mut components = Vec::with_capacity(item_values.len() + 1);
    let mut component_tys = Vec::with_capacity(item_values.len() + 1);
    for (vpat, head_ty) in item_values {
        components.push(vpat);
        component_tys.push(head_ty);
    }
    components.push(tail_value);
    component_tys.push(tail_ty);
    let prod_id: ss::TypeId = derived_id(key_space, 0);
    let prods = vec![(prod_id, ss::Type::Prod(ss::Prod(component_tys)))];
    let pat_id: ss::VPatId = derived_id(key_space, 1);
    let pat = ss::ValuePattern::VCons(components);
    Some(PatConsSynOutcome { vtype, prods, pat_id, pat, ann: prod_id })
}

/// The synthesized judgment of a constructor pattern: it always fails with a
/// missing annotation.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
pub fn pat_alias_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, pat: InternedPat<'db>,
) -> Option<crate::check::TyckError> {
    let su::Pattern::Alias(_) = data.scoped(db).pats.get(&pat.id(db))? else {
        return None;
    };
    Some(crate::check::TyckError::MissingAnnotation)
}

/// An interned constructor-pattern judgment input, for use as a salsa query
/// key.
#[salsa::interned]
pub struct InternedPatCtorInput<'db> {
    #[returns(clone)]
    pub name: ss::CtorName,
    #[returns(clone)]
    pub args: ss::VPatId,
    #[returns(clone)]
    pub ann: ss::TypeId,
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
    #[returns(clone)]
    pub patterns: Vec<ss::VPatId>,
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
pub fn pat_project_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, pat: InternedPat<'db>,
) -> Option<crate::check::TyckError> {
    let su::Pattern::Project(_) = data.scoped(db).pats.get(&pat.id(db))? else {
        return None;
    };
    Some(crate::check::TyckError::MissingAnnotation)
}
