use crate::bitter::syntax::*;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ResolveError {
    #[error("Unbound variable: {0}")]
    UnboundVar(Sp<NameRef<VarName>>),
    #[error("Ambiguous annotation on binder: {0}")]
    AmbiguousBinderAnnotation(Sp<VarName>),
    #[error("Duplicate definition: {0} and {1}")]
    DuplicateDefinition(Sp<VarName>, Sp<VarName>),
    #[error("No such module found: {0}")]
    ModuleNotFound(Sp<NameRef<VarName>>),
}

pub type Result<T> = std::result::Result<T, ResolveError>;
