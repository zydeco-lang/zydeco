//! Existential classifier, manifest equation, and package introduction queries.

use super::*;

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
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
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
                ty: ss::Exists::new(ss::TypeBinder { pattern: tpat, witness: abst }, body_ty)
                    .into(),
                kd: vtype,
            })
        }
        | SigmaSynArm::Prod { ty_1, ty_2 } => {
            let id: ss::TypeId = derived_id(key_space, 0);
            Some(SigmaSynOutcome::Type {
                id,
                ty: ss::Type::Prod(ss::Prod(vec![ty_1, ty_2])),
                kd: vtype,
            })
        }
        | SigmaSynArm::Expressivity => Some(SigmaSynOutcome::Error(
            crate::check::TyckError::Expressivity("abstract existential kinds are not supported"),
        )),
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
    #[returns(clone)]
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
pub fn manifest_exists_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    input: InternedManifestSyn<'db>, occurrence: u32,
) -> Option<ManifestSynOutcome> {
    let su::Term::ManifestExists(_) = data.scoped(db).terms.get(&term.id(db))? else {
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
                ty: ss::Exists::with_manifest(
                    ss::TypeBinder { pattern, witness },
                    definition,
                    body,
                )
                .into(),
                kd: vtype,
            })
        }
        | ManifestSynArm::SortMismatch => {
            Some(ManifestSynOutcome::Error(crate::check::TyckError::SortMismatch))
        }
    }
}

/// The arm of a package-introduction judgment after its definition, binder,
/// and payload checks.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum PackSynArm {
    Package {
        pattern: ss::TPatId,
        witness: ss::AbstId,
        definition: ss::TypeId,
        body: ss::ValueId,
        body_ty: ss::TypeId,
    },
    Sealed {
        pattern: ss::TPatId,
        witness: ss::AbstId,
        definition: ss::TypeId,
        body: ss::ValueId,
        body_ty: ss::TypeId,
    },
    PayloadNotValue,
}

/// An interned package-introduction judgment input, for use as a salsa query
/// key.
#[salsa::interned]
pub struct InternedPackSyn<'db> {
    #[returns(clone)]
    pub arm: PackSynArm,
}

/// The allocation tail of a package-introduction judgment: the existential
/// package type and its sealed value; the rejection surfaces as an error.
#[derive(Clone, Debug)]
pub enum PackSynOutcome {
    Package { exists_id: ss::TypeId, exists: ss::Type, cons_id: ss::ValueId, cons: ss::Value },
    Error(crate::check::TyckError),
}

/// The synthesized judgment of a `pack` term, keyed on the checked definition,
/// binder, and payload.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
pub fn pack_syn_judgment<'db>(
    db: &'db dyn TyckDb, data: ScopedData<'db>, term: InternedTerm<'db>,
    input: InternedPackSyn<'db>, occurrence: u32,
) -> Option<PackSynOutcome> {
    let su::Term::Pack(_) = data.scoped(db).terms.get(&term.id(db))? else {
        return None;
    };
    let site_space = term.id(db).key_space().as_u64();
    let site_raw = term.id(db).raw().into_u32();
    let key_space = KeySpaceId::derive(QUERY_DERIVATION_TAG, site_space, site_raw, occurrence);
    match input.arm(db) {
        | PackSynArm::Package { pattern, witness, definition, body, body_ty } => {
            let exists_id: ss::TypeId = derived_id(key_space, 0);
            let cons_id: ss::ValueId = derived_id(key_space, 1);
            Some(PackSynOutcome::Package {
                exists_id,
                exists: ss::Exists::with_manifest(
                    ss::TypeBinder { pattern, witness },
                    definition,
                    body_ty,
                )
                .into(),
                cons_id,
                cons: ss::Value::SCons(ss::ConsN(vec![definition.into()], body)),
            })
        }
        | PackSynArm::Sealed { pattern, witness, definition, body, body_ty } => {
            let exists_id: ss::TypeId = derived_id(key_space, 0);
            let cons_id: ss::ValueId = derived_id(key_space, 1);
            Some(PackSynOutcome::Package {
                exists_id,
                exists: ss::Exists::new(ss::TypeBinder { pattern, witness }, body_ty).into(),
                cons_id,
                cons: ss::Value::SCons(ss::ConsN(vec![definition.into()], body)),
            })
        }
        | PackSynArm::PayloadNotValue => Some(PackSynOutcome::Error(
            crate::check::TyckError::Expressivity("pack payloads must synthesize as values"),
        )),
    }
}
