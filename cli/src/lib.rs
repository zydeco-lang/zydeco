#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod cli;
pub mod compile;
pub mod diagnostics;
pub mod documentation;
pub mod execution;
pub mod format;
pub mod native;
pub use zydeco_assembly::representation::RepresentationStrategy;
pub use zydeco_stackir::passes::{HighSpsInspection, HighSpsPass, HighSpsPlan, HighSpsPlanError};

pub use crate::{
    cli::{
        BuildTarget, Cli, Commands, DocumentationCommand, ExecutionOptions, ExecutionTarget,
        RepresentationChoice, TargetArchitecture, TargetOs, TestTarget,
    },
    compile::{
        Amd64Artifact, AssemblyOutcome, BackendProgram, CommandCompiler, CompileError,
        TestInteraction,
    },
    diagnostics::DiagnosticRenderer,
    execution::{ExecutionError, ExecutionRunner, PreparedExecution},
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
