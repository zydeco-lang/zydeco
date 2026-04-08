use std::fmt;

#[derive(Debug)]
pub enum LinkError {
    UnsupportedTargetArch(String),
    UnsupportedTargetOs(String),
    LlvmNotFound,
    BuildPreparationError(std::io::Error),
    LlvmCompileError(String),
    ExecutableRunError(std::io::Error),
}

impl fmt::Display for LinkError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            | LinkError::UnsupportedTargetArch(s) => write!(f, "unsupported target arch: {}", s),
            | LinkError::UnsupportedTargetOs(s) => write!(f, "unsupported target os: {}", s),
            | LinkError::LlvmNotFound => write!(f, "llvm not found"),
            | LinkError::BuildPreparationError(e) => {
                write!(f, "failed to prepare build artifacts: {}", e)
            }
            | LinkError::LlvmCompileError(s) => write!(f, "llvm compile error: {}", s),
            | LinkError::ExecutableRunError(e) => write!(f, "executable run error: {}", e),
        }
    }
}

impl std::error::Error for LinkError {}

pub type Result<T> = std::result::Result<T, LinkError>;
