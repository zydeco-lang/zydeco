use std::convert::Infallible;
use thiserror::Error;
use zydeco_syntax::{BuiltinRole, IntrinsicRole, Meta, SpecializeMeta};

/// The typed meaning of a `@[doc]` or `@[doc(...)]` annotation.
///
/// Arguments remain ordinary metadata values so documentation renderers can
/// define presentation policies without extending the surface parser.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct DocMeta {
    pub arguments: Vec<Meta>,
}

impl SpecializeMeta for DocMeta {
    const NAME: &'static str = "doc";
    type Error = Infallible;

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        Ok(Self { arguments: arguments.to_vec() })
    }
}

/// A decoded `intrinsic(role)` splice annotation.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntrinsicMeta {
    pub role: IntrinsicRole,
}

impl SpecializeMeta for IntrinsicMeta {
    const NAME: &'static str = "intrinsic";
    type Error = IntrinsicMetaError;

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        match arguments {
            | [Meta::Ident(role)] => IntrinsicRole::from_source_name(role)
                .map(|role| Self { role })
                .ok_or_else(|| IntrinsicMetaError::UnknownRole(role.clone())),
            | [_] => Err(IntrinsicMetaError::RoleNotIdentifier),
            | arguments => Err(IntrinsicMetaError::RoleArity { found: arguments.len() }),
        }
    }
}

#[derive(Clone, Debug, Error, Hash, PartialEq, Eq)]
pub enum IntrinsicMetaError {
    #[error("intrinsic expects one role identifier, but found {found} arguments")]
    RoleArity { found: usize },
    #[error("intrinsic role must be an identifier")]
    RoleNotIdentifier,
    #[error("unknown intrinsic role `{0}`")]
    UnknownRole(String),
}

/// A decoded `builtin(role)` annotation.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct BuiltinMeta {
    pub role: BuiltinRole,
}

impl SpecializeMeta for BuiltinMeta {
    const NAME: &'static str = "builtin";
    type Error = BuiltinMetaError;

    fn from_arguments(arguments: &[Meta]) -> Result<Self, Self::Error> {
        match arguments {
            | [Meta::Ident(role)] => BuiltinRole::from_source_name(role)
                .map(|role| Self { role })
                .ok_or_else(|| BuiltinMetaError::UnknownRole(role.clone())),
            | [_] => Err(BuiltinMetaError::RoleNotIdentifier),
            | arguments => Err(BuiltinMetaError::RoleArity { found: arguments.len() }),
        }
    }
}

#[derive(Clone, Debug, Error, Hash, PartialEq, Eq)]
pub enum BuiltinMetaError {
    #[error("builtin expects one role identifier, but found {found} arguments")]
    RoleArity { found: usize },
    #[error("builtin role must be an identifier")]
    RoleNotIdentifier,
    #[error("unknown builtin role `{0}`")]
    UnknownRole(String),
}
