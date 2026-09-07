//! Checker-dependent operations on typed annotation identities.

use crate::*;

impl PatAnnId {
    pub fn try_as_kind(
        self, tycker: &mut Tycker<'_>, error: TyckError,
        blame: &'static std::panic::Location<'static>,
    ) -> ResultKont<KPatId> {
        match self {
            | Self::Kind(pattern) => Ok(pattern),
            | Self::Type(_, _) | Self::Value(_, _) => tycker.err_k(error, blame),
        }
    }

    pub fn try_as_type(
        self, tycker: &mut Tycker<'_>, error: TyckError,
        blame: &'static std::panic::Location<'static>,
    ) -> ResultKont<(TPatId, KindId)> {
        match self {
            | Self::Type(pattern, kind) => Ok((pattern, kind)),
            | Self::Kind(_) | Self::Value(_, _) => tycker.err_k(error, blame),
        }
    }

    pub fn try_as_value(
        self, tycker: &mut Tycker<'_>, error: TyckError,
        blame: &'static std::panic::Location<'static>,
    ) -> ResultKont<(VPatId, TypeId)> {
        match self {
            | Self::Value(pattern, ty) => Ok((pattern, ty)),
            | Self::Kind(_) | Self::Type(_, _) => tycker.err_k(error, blame),
        }
    }
}

impl TermAnnId {
    /// Promote an already synthesized classifier to an ordinary static term.
    pub(super) fn classifier_k(self, tycker: &mut Tycker<'_>) -> ResultKont<Self> {
        match self {
            | Self::Type(_, kind) => Ok(Self::Kind(kind)),
            | Self::Value(_, ty) | Self::Compu(_, ty) => {
                Ok(Self::Type(ty, tycker.statics.type_kind(ty)))
            }
            | Self::Kind(_) => tycker.err_k(TyckError::TypeOfKind, std::panic::Location::caller()),
            | Self::Hole(_) => {
                tycker.err_k(TyckError::MissingAnnotation, std::panic::Location::caller())
            }
        }
    }

    pub fn try_as_kind(
        self, tycker: &mut Tycker<'_>, error: TyckError,
        blame: &'static std::panic::Location<'static>,
    ) -> ResultKont<KindId> {
        match self {
            | Self::Kind(kind) => Ok(kind),
            | Self::Hole(_) | Self::Type(_, _) | Self::Value(_, _) | Self::Compu(_, _) => {
                tycker.err_k(error, blame)
            }
        }
    }

    pub fn try_as_type(
        self, tycker: &mut Tycker<'_>, error: TyckError,
        blame: &'static std::panic::Location<'static>,
    ) -> ResultKont<(TypeId, KindId)> {
        match self {
            | Self::Type(ty, kind) => Ok((ty, kind)),
            | Self::Hole(_) | Self::Kind(_) | Self::Value(_, _) | Self::Compu(_, _) => {
                tycker.err_k(error, blame)
            }
        }
    }

    pub fn try_as_value(
        self, tycker: &mut Tycker<'_>, error: TyckError,
        blame: &'static std::panic::Location<'static>,
    ) -> ResultKont<(ValueId, TypeId)> {
        match self {
            | Self::Value(value, ty) => Ok((value, ty)),
            | Self::Hole(_) | Self::Kind(_) | Self::Type(_, _) | Self::Compu(_, _) => {
                tycker.err_k(error, blame)
            }
        }
    }

    pub fn try_as_compu(
        self, tycker: &mut Tycker<'_>, error: TyckError,
        blame: &'static std::panic::Location<'static>,
    ) -> ResultKont<(CompuId, TypeId)> {
        match self {
            | Self::Compu(computation, ty) => Ok((computation, ty)),
            | Self::Hole(_) | Self::Kind(_) | Self::Type(_, _) | Self::Value(_, _) => {
                tycker.err_k(error, blame)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::tests::{anonymous_type_binder, with_empty_tycker};

    #[test]
    fn annotation_probe_uses_lub_for_rigid_equality_and_mismatch() {
        with_empty_tycker(|tycker| {
            let environment = TyEnv::new();
            let vtype = ss::VType.build(tycker, &environment);
            let unit = ss::UnitTy.build(tycker, &environment);
            let other_unit: ss::TypeId = Alloc::alloc(tycker, ss::UnitTy, vtype, &environment);
            let integer = ss::PrimitiveTy(ss::PrimitiveType::Integer(ss::IntegerType::Int64))
                .build(tycker, &environment);

            assert_ne!(unit, other_unit);
            assert_eq!(
                tycker.annotation_compatibility(unit.into(), other_unit.into()),
                AnnotationCompatibility::Equal
            );
            assert_eq!(
                tycker.annotation_compatibility(unit.into(), integer.into()),
                AnnotationCompatibility::Mismatch
            );
            assert_eq!(
                tycker.annotation_compatibility(vtype.into(), unit.into()),
                AnnotationCompatibility::Mismatch
            );
            assert!(tycker.errors.is_empty());
        });
    }

    #[test]
    fn annotation_probe_defers_unsolved_fills_without_changing_inference() {
        with_empty_tycker(|tycker| {
            let environment = TyEnv::new();
            let vtype = ss::VType.build(tycker, &environment);
            let unit = ss::UnitTy.build(tycker, &environment);
            let root = tycker.data.root(tycker.db);
            let fill = Alloc::alloc(tycker, root, (), &());
            let pending: ss::TypeId = Alloc::alloc(tycker, fill, vtype, &environment);
            let scopes = tycker.statics.fill_scopes.iter().count();
            let scope = tycker.statics.fill_scopes.get(&fill).cloned();
            let errors = tycker.errors.len();

            assert_eq!(
                tycker.annotation_compatibility(pending.into(), unit.into()),
                AnnotationCompatibility::Unknown
            );
            assert_eq!(
                tycker.annotation_compatibility(pending.into(), pending.into()),
                AnnotationCompatibility::Unknown
            );
            assert!(tycker.statics.solus.get(&fill).is_none());
            assert_eq!(tycker.statics.fill_scopes.iter().count(), scopes);
            assert_eq!(tycker.statics.fill_scopes.get(&fill).cloned(), scope);
            assert_eq!(tycker.errors.len(), errors);

            assert_eq!(pending.lub(unit, tycker).ok(), Some(unit));
            assert_eq!(tycker.statics.solus.get(&fill), Some(&AnnId::Type(unit)));
        });
    }

    #[test]
    fn annotation_probe_keeps_a_rigid_mismatch_beside_deferred_fills() {
        with_empty_tycker(|tycker| {
            let environment = TyEnv::new();
            let vtype = ss::VType.build(tycker, &environment);
            let unit = ss::UnitTy.build(tycker, &environment);
            let integer = ss::PrimitiveTy(ss::PrimitiveType::Integer(ss::IntegerType::Int64))
                .build(tycker, &environment);
            let root = tycker.data.root(tycker.db);
            let fill = Alloc::alloc(tycker, root, (), &());
            let pending: ss::TypeId = Alloc::alloc(tycker, fill, vtype, &environment);
            let partly_unknown: ss::TypeId =
                Alloc::alloc(tycker, ss::Prod(vec![pending, unit]), vtype, &environment);
            let compatible: ss::TypeId =
                Alloc::alloc(tycker, ss::Prod(vec![integer, unit]), vtype, &environment);
            let mismatched: ss::TypeId =
                Alloc::alloc(tycker, ss::Prod(vec![integer, integer]), vtype, &environment);

            assert_eq!(
                tycker.annotation_compatibility(partly_unknown.into(), compatible.into()),
                AnnotationCompatibility::Unknown
            );
            assert_eq!(
                tycker.annotation_compatibility(partly_unknown.into(), mismatched.into()),
                AnnotationCompatibility::Mismatch
            );
            assert!(tycker.statics.solus.get(&fill).is_none());
            assert!(tycker.errors.is_empty());
        });
    }

    #[test]
    fn annotation_probe_follows_existing_solutions_without_rewriting_them() {
        with_empty_tycker(|tycker| {
            let environment = TyEnv::new();
            let vtype = ss::VType.build(tycker, &environment);
            let unit = ss::UnitTy.build(tycker, &environment);
            let root = tycker.data.root(tycker.db);
            let fill = Alloc::alloc(tycker, root, (), &());
            let pending: ss::TypeId = Alloc::alloc(tycker, fill, vtype, &environment);
            tycker.statics.solus.insert_new(fill, unit.into());

            assert_eq!(
                tycker.annotation_compatibility(pending.into(), unit.into()),
                AnnotationCompatibility::Equal
            );
            assert_eq!(tycker.statics.solus.get(&fill), Some(&AnnId::Type(unit)));
            assert!(tycker.errors.is_empty());
        });
    }

    #[test]
    fn annotation_probe_preserves_lub_alpha_equivalence() {
        with_empty_tycker(|tycker| {
            let environment = TyEnv::new();
            let vtype = ss::VType.build(tycker, &environment);
            let ctype = ss::CType.build(tycker, &environment);
            let ret = ss::RetTy.build(tycker, &environment);
            let quantified = |tycker: &mut Tycker<'_>| {
                let binder = anonymous_type_binder(tycker, vtype, &environment);
                let witness = Alloc::alloc(tycker, binder.witness, vtype, &environment);
                let body = Alloc::alloc(tycker, ss::App(ret, witness), ctype, &environment);
                Alloc::alloc(tycker, ss::Forall(binder, body), ctype, &environment)
            };
            let left = quantified(tycker);
            let right = quantified(tycker);

            assert_ne!(left, right);
            assert_eq!(
                tycker.annotation_compatibility(left.into(), right.into()),
                AnnotationCompatibility::Equal
            );
            let unit = ss::UnitTy.build(tycker, &environment);
            let monomorphic: ss::TypeId =
                Alloc::alloc(tycker, ss::App(ret, unit), ctype, &environment);
            assert_eq!(
                tycker.annotation_compatibility(left.into(), monomorphic.into()),
                AnnotationCompatibility::Mismatch
            );
            assert!(tycker.errors.is_empty());
        });
    }

    #[test]
    fn annotation_probe_normalizes_applications_but_preserves_sealed_identity() {
        with_empty_tycker(|tycker| {
            let environment = TyEnv::new();
            let vtype = ss::VType.build(tycker, &environment);
            let unit = ss::UnitTy.build(tycker, &environment);
            let binder = anonymous_type_binder(tycker, vtype, &environment);
            let body = Alloc::alloc(tycker, binder.witness, vtype, &environment);
            let kind = Alloc::alloc(tycker, ss::Arrow(vtype, vtype), (), &());
            let identity =
                Alloc::alloc(tycker, ss::TypeAbstraction { binder, body }, kind, &environment);
            let applied: ss::TypeId =
                Alloc::alloc(tycker, ss::App(identity, unit), vtype, &environment);
            assert_eq!(
                tycker.annotation_compatibility(applied.into(), unit.into()),
                AnnotationCompatibility::Equal
            );

            let sealed = |tycker: &mut Tycker<'_>| {
                let witness = Alloc::alloc(tycker, None::<ss::DefId>, vtype, &());
                tycker.record_seal(witness, unit);
                Alloc::alloc(tycker, witness, vtype, &environment)
            };
            let left: ss::TypeId = sealed(tycker);
            let right: ss::TypeId = sealed(tycker);
            assert_eq!(
                tycker.annotation_compatibility(left.into(), left.into()),
                AnnotationCompatibility::Equal
            );
            assert_eq!(
                tycker.annotation_compatibility(left.into(), right.into()),
                AnnotationCompatibility::Mismatch
            );
            assert!(tycker.errors.is_empty());
        });
    }
}
