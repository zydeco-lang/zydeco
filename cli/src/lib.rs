#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod cli;
pub mod compile;
pub mod diagnostics;
pub mod documentation;
pub mod format;
pub mod native;
pub use zydeco_assembly::representation::RepresentationStrategy;

pub use crate::{
    cli::{
        BuildTarget, Cli, Commands, DocumentationCommand, RepresentationChoice, TargetArchitecture,
        TargetOs,
    },
    compile::{AssemblyOutcome, BackendProgram, CommandCompiler, CompileError, TestInteraction},
    diagnostics::DiagnosticRenderer,
    format::{SourceFormatError, SourceFormatOutcome, SourceFormatter},
    native::{BuildOptions, Executable, NativeError, WasmArtifact, WasmBackendKind},
};

impl From<RepresentationChoice> for RepresentationStrategy {
    fn from(choice: RepresentationChoice) -> Self {
        match choice {
            | RepresentationChoice::Boxed => Self::Boxed,
            | RepresentationChoice::Direct => Self::Direct,
            | RepresentationChoice::Local => Self::Local,
            | RepresentationChoice::Shared => Self::Shared,
        }
    }
}
