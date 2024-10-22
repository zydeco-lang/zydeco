use thiserror::Error;

#[derive(Error, Debug)]
pub enum BuildError {
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("{0}")]
    LocalError(#[from] crate::local::err::LocalError),
    #[error("{0}")]
    CompileError(#[from] crate::check::err::CompileError),
    #[error("{0}")]
    InterpError(#[from] crate::interp::err::InterpError),
    #[error("Duplicate package marked name: {0}")]
    DuplicateMark(String),
    #[error("Can't determine a suitable marked binary to run from: {0:#?}")]
    AmbiguousMark(Vec<String>),
}

pub type Result<T> = std::result::Result<T, BuildError>;
