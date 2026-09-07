//! Function classifier, abstraction, and package-dependent introduction queries.

use super::*;

/// The arm of a pi judgment after its binder checks: which shape the body's
/// judgment took.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum PiSynArm {
    KindArrow { kd_1: ss::KindId, kd_2: ss::KindId },
    Forall { ty_2: ss::TypeId, kd_2: ss::KindId },
    KindMismatch,
    MissingAnnotation,
    SortMismatch,
}

/// An interned pi judgment input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedPiSyn<'db> {
    #[returns(clone)]
    pub arm: PiSynArm,
    #[returns(clone)]
    pub tpat: ss::TPatId,
    #[returns(clone)]
    pub abst: ss::AbstId,
}

/// The allocation tail of a pi judgment: either a kind arrow or a universal
/// type node; the rejections surface as errors.
#[derive(Clone, Debug)]
pub enum PiSynOutcome {
    Kind { id: ss::KindId, kind: ss::Kind },
    Type { id: ss::TypeId, ty: ss::Type, kd: ss::KindId },
    Error(crate::check::TyckError),
}

/// The synthesized judgment of a pi term, keyed on the checked binder and
/// body arm.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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

/// The arm of an abstraction judgment after its binder and body checks:
/// which shapes the pattern and body took.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum AbsSynArm {
    TypeFunction {
        tpat: ss::TPatId,
        witness: ss::AbstId,
        kd: ss::KindId,
        body_kd: ss::KindId,
        body: ss::TypeId,
    },
    PolymorphicCompu {
        tpat: ss::TPatId,
        abst: ss::AbstId,
        compu: ss::CompuId,
        body_ty: ss::TypeId,
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
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
    let ctype = |db: &dyn TyckDb, data: ScopedData<'_>| {
        let key = InternedIntrinsic::new(db, IntrinsicKey::CType);
        let IntrinsicSingleton::Kind { id, .. } = intrinsic_singleton(db, data, key) else {
            unreachable!("the ctype singleton is kind-producing")
        };
        id
    };
    match input.arm(db) {
        | AbsSynArm::TypeFunction { tpat, witness, kd, body_kd, body } => {
            let arrow_id: ss::KindId = derived_id(key_space, 0);
            let abs_id: ss::TypeId = derived_id(key_space, 1);
            Some(AbsSynOutcome::TypeFunction {
                arrow_id,
                arrow: ss::Kind::Arrow(ss::Arrow(kd, body_kd)),
                abs_id,
                abs: ss::Type::Abs(ss::TypeAbstraction {
                    binder: ss::TypeBinder { pattern: tpat, witness },
                    body,
                }),
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
                ann: ss::PackPi {
                    domain,
                    witnesses: ss::PackTelescope::new(first, rest),
                    codomain,
                }
                .into(),
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

/// An interned pack-pi introduction input, for use as a salsa query key.
#[salsa::interned]
pub struct InternedPackPiIntro<'db> {
    #[returns(clone)]
    pub pattern: ss::VPatId,
    #[returns(clone)]
    pub body: ss::CompuId,
    #[returns(clone)]
    pub domain: ss::TypeId,
    #[returns(clone)]
    pub first: ss::AbstId,
    #[returns(clone)]
    pub rest: Vec<ss::AbstId>,
    #[returns(clone)]
    pub codomain: ss::TypeId,
}

/// The allocation tail of a package-pi introduction: the pack-pi signature
/// and the abstraction node.
#[derive(Clone, Debug)]
pub struct PackPiIntroOutcome {
    pub sig_id: ss::TypeId,
    pub sig: ss::Type,
    pub kd: ss::KindId,
    pub abs_id: ss::CompuId,
    pub abs: ss::Computation,
}

/// The synthesized judgment of a package-pi introduction, keyed on the
/// checked pattern, body, and witness telescope.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
pub fn pack_pi_intro_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, site: InternedSite<'db>,
    input: InternedPackPiIntro<'db>,
) -> Option<PackPiIntroOutcome> {
    let _ = data;
    let ctype = {
        let key = InternedIntrinsic::new(db, IntrinsicKey::CType);
        let IntrinsicSingleton::Kind { id, .. } = intrinsic_singleton(db, data, key) else {
            unreachable!("the ctype singleton is kind-producing")
        };
        id
    };
    let key_space =
        KeySpaceId::derive(QUERY_DERIVATION_TAG, site.space(db), site.raw(db), site.occurrence(db));
    let sig_id: ss::TypeId = derived_id(key_space, 0);
    let abs_id: ss::CompuId = derived_id(key_space, 1);
    Some(PackPiIntroOutcome {
        sig_id,
        sig: ss::PackPi {
            domain: input.domain(db),
            witnesses: ss::PackTelescope::new(input.first(db), input.rest(db).iter().copied()),
            codomain: input.codomain(db),
        }
        .into(),
        kd: ctype,
        abs_id,
        abs: ss::Computation::VAbs(ss::Abs(input.pattern(db), input.body(db))),
    })
}
