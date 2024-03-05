use std::path::PathBuf;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum SurfaceError {
    #[error("Package file not found: `{}`", .0.display())]
    PackageFileNotFound(PathBuf),
    #[error("Package file at `{}` invalid: {}", .0.display(), .1)]
    PackageFileInvalid(PathBuf, std::io::Error),
    #[error("Source file not found: `{}`", .0.display())]
    SrcFileNotFound(PathBuf),
    #[error("Error while canonicalizing src file path: `{}`", .0)]
    CanonicalizationError(String),
    #[error("Lexer error: failed to run lexer")]
    LexerError,
    #[error("Parse error:\n{0}")]
    ParseError(String),
    #[error("Resolve errors:\n{0}")]
    ResolveErrors(String),
}

pub type Result<T> = std::result::Result<T, SurfaceError>;
