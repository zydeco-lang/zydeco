//! Data and codata declaration, construction, matching, and observation queries.

use super::*;

/// An interned data-arms table, for use as a salsa query key.
#[salsa::interned]
pub struct InternedDataArms<'db> {
    #[returns(clone)]
    pub arms: Vec<(ss::CtorName, ss::TypeId)>,
}

/// An interned codata-arms table, for use as a salsa query key.
#[salsa::interned]
pub struct InternedCoDataArms<'db> {
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
    #[returns(clone)]
    pub scrut: ss::ValueId,
    #[returns(clone)]
    pub arms: Vec<(ss::VPatId, ss::CompuId)>,
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
    #[returns(clone)]
    pub name: ss::CtorName,
    #[returns(clone)]
    pub arg: ss::ValueId,
    #[returns(clone)]
    pub ann: ss::TypeId,
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
    #[returns(clone)]
    pub arms: Vec<(ss::DtorName, ss::CompuId)>,
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
    #[returns(clone)]
    pub body: ss::CompuId,
    #[returns(clone)]
    pub dtor: ss::DtorName,
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
