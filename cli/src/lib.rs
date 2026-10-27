#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod cli;
pub mod compile;
pub mod diagnostics;
pub mod execution;
pub mod format;
pub mod native;
pub mod library;
pub mod unit;
pub use zydeco_assembly::representation::RepresentationStrategy;
pub use zydeco_stackir::{
    SpsScheme,
    passes::{SpsInspection, SpsPass, SpsPlan, SpsPlanError, SpsSelection},
};

pub use crate::{
    cli::{
        BuildTarget, Cli, Commands, ExecutionOptions, ExecutionTarget, RepresentationChoice,
        SourceSelection, SpsSchemeChoice, TargetArchitecture, TargetOs, TestTarget,
    },
    compile::{Amd64Artifact, BackendProgram, CommandCompiler, CompileError, TestInteraction},
    diagnostics::DiagnosticRenderer,
    execution::{ExecutionError, ExecutionRunner, PreparedExecution},
    format::{SourceFormatError, SourceFormatOutcome, SourceFormatter},
    native::{BuildOptions, Executable, NativeError, WasmArtifact},
};

impl From<SpsSchemeChoice> for SpsScheme {
    fn from(choice: SpsSchemeChoice) -> Self {
        match choice {
            | SpsSchemeChoice::Duplicating => Self::Duplicating,
            | SpsSchemeChoice::BranchJoin => Self::BranchJoin,
            | SpsSchemeChoice::Full => Self::Full,
        }
    }
}

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
