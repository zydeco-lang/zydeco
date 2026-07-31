use thiserror::Error;

#[derive(Error, Debug)]
pub enum BuildError {
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("{0}")]
    CompileError(#[from] crate::check::err::CompileError),
    #[error("{0}")]
    Amd64LinkError(#[from] crate::amd64::err::LinkError),
    #[error("Program terminated with {0}")]
    Amd64RunError(std::process::ExitStatus),
    #[error("{0}")]
    LlvmLinkError(#[from] crate::llvm::err::LinkError),
    #[error("LLVM emitter cannot represent local {variable:?} at assembly program {program:?}")]
    LlvmUnsupportedLocal {
        program: zydeco_assembly::syntax::ProgId,
        variable: zydeco_assembly::syntax::VarId,
    },
    #[error("Program terminated with {0}")]
    LlvmRunError(std::process::ExitStatus),
    #[error("{0}")]
    AssemblyInterpError(#[from] crate::zasm::err::AssemblyInterpError),
    #[error("{0}")]
    SourceLoadError(#[from] crate::source::SourceLoadError),
    #[error("{0}")]
    ProgramAssemblyError(#[from] crate::source::ProgramAssemblyError),
    #[error("{0}")]
    SourceDesugarError(#[from] zydeco_surface::bitter::DesugarError),
    #[error("{0}")]
    SourceLowerError(#[from] crate::source::SourceLowerError),
    #[error("source execution produced {0} roots; expected exactly one")]
    SourceEntryCount(usize),
    #[error("source test expected exit code 0, got {0:?}")]
    SourceTestFailure(zydeco_dynamics::ProgKont),
    #[error("source root path `{0}` does not have a valid UTF-8 artifact name")]
    InvalidSourceArtifactName(std::path::PathBuf),
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
