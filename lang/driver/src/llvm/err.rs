#[derive(Debug, derive_more::Display)]
pub enum LinkError {
    #[display("unsupported target arch: {_0}")]
    UnsupportedTargetArch(String),
    #[display("unsupported target os: {_0}")]
    UnsupportedTargetOs(String),
    #[display("llvm not found")]
    LlvmNotFound,
    #[display("failed to prepare build artifacts: {_0}")]
    BuildPreparationError(std::io::Error),
    #[display("llvm compile error: {_0}")]
    LlvmCompileError(String),
    #[display("executable run error: {_0}")]
    ExecutableRunError(std::io::Error),
}

impl std::error::Error for LinkError {}

pub type Result<T> = std::result::Result<T, LinkError>;
