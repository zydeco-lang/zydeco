use thiserror::Error;
use std::io;

#[derive(Error, Debug)]
pub enum LinkError {
    #[error("Failed to write to assembly file: {0}")]
    AssemblyWriteError(io::Error),
    #[error("Failed to run nasm: {0}")]
    NasmRunError(io::Error),
    #[error("Failure in nasm call: {0}")]
    NasmOutputError(String),
    #[error("Failed to run ar: {0}")]
    ArRunError(io::Error),
    #[error("Failure in ar call: {0}")]
    ArOutputError(String),
    #[error("Failed to run cargo: {0}")]
    CargoRunError(io::Error),
    #[error("Failure in cargo call: {0}")]
    CargoOutputError(String),
    #[error("Failed to copy executable file: {0}")]
    ExecutableCopyError(std::io::Error),
    #[error("Failed to run executable: {0}")]
    ExecutableRunError(std::io::Error),
    #[error("Program exited with status: {0}")]
    ProgramCallError(std::process::ExitStatus),
}

pub type Result<T> = std::result::Result<T, LinkError>;
