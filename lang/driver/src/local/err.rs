use std::path::PathBuf;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum PackageError {
    #[error("Package file not found: `{}`", .0.display())]
    PackageFileNotFound(PathBuf),
    #[error("Package file at `{}` invalid: {}", .0.display(), .1)]
    PackageFileInvalid(PathBuf, std::io::Error),
    #[error("Source file not found: `{}`", .0.display())]
    SrcFileNotFound(PathBuf),
    #[error("Error while canonicalizing src file path: `{0}`")]
    CanonicalizationError(String),
    #[error("Lock poisoned.")]
    LockPoisoned,
    #[error("Lexer error:\n\t{0}")]
    LexerError(String),
    #[error("Parse error:\n\t{0}")]
    ParseError(String),
    #[error("Desugar error:\n\t{0}")]
    DesugarError(String),
    #[error("Resolve error:\n\t{0}")]
    ResolveError(String),
    #[error("Tyck error:\n{0}")]
    TyckErrors(String),
    #[error("Test failed:\n\t{0}")]
    TestFailed(String),
}

pub type Result<T> = std::result::Result<T, PackageError>;
