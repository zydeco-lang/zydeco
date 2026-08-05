//! Checker-dependent operations on typed annotation identities.

use crate::{syntax::*, *};

impl PatAnnId {
    pub fn mk_hole(tycker: &mut Tycker<'_>, env: &TyEnv, ann: AnnId) -> Self {
        match ann {
            | AnnId::Set => {
                let term = Alloc::alloc(tycker, Hole, (), env);
                Self::Kind(term)
            }
            | AnnId::Kind(kind) => {
                let term = Alloc::alloc(tycker, Hole, kind, env);
                Self::Type(term, kind)
            }
            | AnnId::Type(ty) => {
                let term = Alloc::alloc(tycker, Hole, ty, env);
                Self::Value(term, ty)
            }
        }
    }

    pub fn mk_var(tycker: &mut Tycker<'_>, env: &TyEnv, definition: DefId, ann: AnnId) -> Self {
        match ann {
            | AnnId::Set => {
                let term = Alloc::alloc(tycker, definition, (), env);
                Self::Kind(term)
            }
            | AnnId::Kind(kind) => {
                let term = Alloc::alloc(tycker, definition, kind, env);
                Self::Type(term, kind)
            }
            | AnnId::Type(ty) => {
                let term = Alloc::alloc(tycker, definition, ty, env);
                Self::Value(term, ty)
            }
        }
    }

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
