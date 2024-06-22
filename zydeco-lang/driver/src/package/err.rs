use std::path::PathBuf;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ZydecoError {
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
    #[error("Parse error:\n\t{0}")]
    ParseError(String),
    #[error("Resolve error:\n\t{0}")]
    ResolveError(String),
    #[error("Tyck error:\n\t{0}\n{1}")]
    TyckError(String, String),
}

pub type Result<T> = std::result::Result<T, ZydecoError>;
