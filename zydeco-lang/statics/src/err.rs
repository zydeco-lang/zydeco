use crate::syntax::*;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum TyckError {
    #[error("Missing annotation")]
    MissingAnnotation,
    #[error("Sort mismatch")]
    SortMismatch,
    #[error("Kind mismatch")]
    KindMismatch,
    #[error("Type mismatch")]
    TypeMismatch,
    #[error("Missing data arm: {0:?}")]
    MissingDataArm(CtorName),
}

pub type Result<T> = std::result::Result<T, TyckError>;
