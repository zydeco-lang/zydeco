//! Site-keyed producers for CBPV computations and function application.

use super::*;

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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
    pub ctype: ss::KindId,
    pub ret_id: ss::CompuId,
    pub ret: ss::Computation,
}

/// The synthesized judgment of a return term.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
    let ctype = {
        let key = InternedIntrinsic::new(db, IntrinsicKey::CType);
        let IntrinsicSingleton::Kind { id, .. } = intrinsic_singleton(db, data, key) else {
            unreachable!("the ctype singleton is kind-producing")
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
        ctype,
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
    #[returns(clone)]
    pub body: ss::ValueId,
    #[returns(clone)]
    pub force_ty: ss::TypeId,
}

/// The synthesized judgment of a force term.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
    #[returns(clone)]
    pub binder: ss::VPatId,
    #[returns(clone)]
    pub bindee: ss::CompuId,
    #[returns(clone)]
    pub tail: ss::CompuId,
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
pub fn do_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    input: InternedDoInput<'db>, occurrence: u32,
) -> Option<DoSynOutcome> {
    let su::Term::Do(_) = data.scoped(db).terms.get(&term.id(db))? else {
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
pub fn let_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>, binder: InternedVPat<'db>,
    bindee: InternedValue<'db>, tail: InternedTermAnn<'db>, occurrence: u32,
) -> Option<LetSynOutcome> {
    let su::Term::Let(_) = data.scoped(db).terms.get(&term.id(db))? else {
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
    Value { function: ss::ValueId, argument: ss::ValArgument },
    CompuValue { function: ss::CompuId, argument: ss::ValueId },
    CompuType { function: ss::CompuId, argument: ss::TypeId },
}

/// An interned application judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedAppInput<'db> {
    #[returns(clone)]
    pub kind: AppKind,
    /// The annotation recorded on the application node.
    #[returns(clone)]
    pub ann: ss::TypeId,
    /// The type reported by the judgment; usually the annotation, but the
    /// polymorphic computation application reports the substituted body type.
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
    derive_app_outcome(db, key_space, input)
}

/// The application judgment keyed on an explicit allocation site, for
/// auxiliary entities that allocate at the enclosing term's site.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
pub fn app_judgment_at<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, site: InternedSite<'db>,
    input: InternedAppInput<'db>,
) -> Option<AppSynOutcome> {
    let _ = data;
    let key_space =
        KeySpaceId::derive(QUERY_DERIVATION_TAG, site.space(db), site.raw(db), site.occurrence(db));
    derive_app_outcome(db, key_space, input)
}

/// The application tail shared by the term-keyed and site-keyed judgments.
fn derive_app_outcome<'db>(
    db: &'db dyn TyckDb, key_space: KeySpaceId, input: InternedAppInput<'db>,
) -> Option<AppSynOutcome> {
    match input.kind(db) {
        | AppKind::Value { function, argument } => {
            let id: ss::ValueId = derived_id(key_space, 0);
            Some(AppSynOutcome::Value {
                id,
                value: ss::Value::ValApp(ss::App(function, argument)),
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
    #[returns(clone)]
    pub binder: ss::VPatId,
    #[returns(clone)]
    pub body: ss::CompuId,
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
