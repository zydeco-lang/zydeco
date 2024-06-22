use thiserror::Error;

#[derive(Error, Debug)]
pub enum BuildError {
    #[error("Package error: {0}")]
    PackageError(#[from] crate::package::err::ZydecoError),
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, BuildError>;
