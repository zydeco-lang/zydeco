use crate::bitter::syntax::*;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ResolveError {
    #[error("Unbound variable: {0}")]
    UnboundVar(Sp<NameRef<VarName>>),
    #[error("Duplicate definition: {0} and {1}")]
    DuplicateDefinition(Sp<VarName>, Sp<VarName>),
    #[error("Undefined primitive: {0}")]
    UndefinedPrimitive(Sp<VarName>),
    #[error("Duplicate primitive: {0} and {1}")]
    DuplicatePrimitive(Sp<VarName>, Sp<VarName>),
    #[error("Missing primitive: {0}")]
    MissingPrim(&'static str),
    #[error("No such module found: {0}")]
    ModuleNotFound(Sp<NameRef<VarName>>),
}

pub type Result<T> = std::result::Result<T, Box<ResolveError>>;
