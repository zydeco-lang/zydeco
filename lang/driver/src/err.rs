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
    Amd64LinkError(#[from] crate::amd64::err::LinkError),
    #[error("Program terminated with {0}")]
    Amd64RunError(std::process::ExitStatus),
    #[error("{0}")]
    AssemblyInterpError(#[from] crate::zasm::err::AssemblyInterpError),
    #[error("Duplicate package marked name: {0}")]
    DuplicateMark(String),
    #[error("No suitable marked binary to run: wanted: {0}, available: {1:#?}")]
    NoSuitableMark(String, Vec<String>),
    #[error("Can't determine a suitable marked binary to run from: {0:#?}")]
    AmbiguousMark(Vec<String>),
    #[error("Missing build configuration for package: {0}")]
    MissingBuildConf(String),
    #[error("Unsupported target: {0}")]
    UnsupportedTarget(String),
    #[error("Unsupported target OS: {0}")]
    UnsupportedTargetOs(String),
    #[error("Infallible error: {0}")]
    Infallible(#[from] std::convert::Infallible),
}

impl BuildError {
    /// Print this error using Ariadne reports if available, otherwise use standard Display.
    pub fn print_ariadne(&self) {
        match self {
            | BuildError::CompileError(crate::check::err::CompileError::TyckErrorReports {
                reports,
                sources,
            }) => {
                use crate::diagnostics::create_source_cache;
                for report in reports.iter() {
                    let cache = create_source_cache(sources);
                    let _ = report.eprint(cache);
                }
            }
            | BuildError::CompileError(crate::check::err::CompileError::ResolveErrorReport {
                report,
                sources,
            }) => {
                use crate::diagnostics::create_source_cache;
                let cache = create_source_cache(sources);
                let _ = report.eprint(cache);
            }
            | _ => {
                // Fall back to standard error display
                eprintln!("{}", self);
            }
        }
    }
}

pub type Result<T> = std::result::Result<T, BuildError>;
