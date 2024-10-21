use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("Resolve error:\n\t{0}")]
    ResolveError(String),
    #[error("Tyck error:\n{0}")]
    TyckErrors(String)
}

pub type Result<T> = std::result::Result<T, CompileError>;
