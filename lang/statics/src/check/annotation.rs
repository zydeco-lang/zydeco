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
