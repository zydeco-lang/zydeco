// use crate::syntax::*;
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
}

pub type Result<T> = std::result::Result<T, TyckError>;
