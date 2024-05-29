use crate::syntax::*;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum TyckError {
    #[error("Missing annotation: {0}")]
    MissingAnnotation(Span),
    #[error("Kind mismatch")]
    KindMismatch,
}

pub type Result<T> = std::result::Result<T, TyckError>;
