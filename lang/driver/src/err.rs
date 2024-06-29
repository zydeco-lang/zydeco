use thiserror::Error;

#[derive(Error, Debug)]
pub enum BuildError {
    #[error("Package error: {0}")]
    PackageError(#[from] crate::local::err::PackageError),
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, BuildError>;
