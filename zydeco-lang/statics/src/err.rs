use crate::syntax::*;
use thiserror::Error;

// Todo: writer monad instead of error monad

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
    #[error("Missing codata arm: {0:?}")]
    MissingCoDataArm(DtorName),
    #[error("Non-exhaustive data arms: {0:?}")]
    NonExhaustiveCoDataArms(im::HashMap<DtorName, TypeId>),
    #[error("{0}")]
    Expressivity(&'static str),
    #[error("Multiple monad implementations")]
    MultipleMonads,
    #[error("Multiple algebra implementations")]
    MultipleAlgebras,
    #[error("Neither monad nor algebra")]
    NeitherMonadNorAlgebra,
    #[error("Missing monad")]
    MissingMonad,
    #[error("Missing algebra")]
    MissingAlgebra,
}

pub type Result<T> = std::result::Result<T, TyckError>;
