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
    #[error("{0}")]
    X86LinkError(#[from] crate::x86::err::LinkError),
    #[error("Program terminated with {0}")]
    X86RunError(std::process::ExitStatus),
    #[error("{0}")]
    AssemblyInterpError(#[from] crate::zasm::err::AssemblyInterpError),
    #[error("Duplicate package marked name: {0}")]
    DuplicateMark(String),
    #[error("No suitable marked binary to run: wanted: {0}, available: {1:#?}")]
    NoSuitableMark(String, Vec<String>),
    #[error("Can't determine a suitable marked binary to run from: {0:#?}")]
    AmbiguousMark(Vec<String>),
    #[error("Missing build config for package: {0}")]
    MissingBuildConfig(String),
    #[error("Unsupported target: {0}")]
    UnsupportedTarget(String),
}

pub type Result<T> = std::result::Result<T, BuildError>;
