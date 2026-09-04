#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod cli;
pub mod compile;
pub mod diagnostics;
pub mod documentation;
pub mod format;
pub mod native;

pub use crate::{
    cli::{BuildTarget, Cli, Commands, DocumentationCommand, TargetArchitecture, TargetOs},
    compile::{AssemblyOutcome, BackendProgram, CommandCompiler, CompileError, TestInteraction},
    diagnostics::DiagnosticRenderer,
    format::{SourceFormatError, SourceFormatOutcome, SourceFormatter},
    native::{BuildOptions, Executable, NativeError, WasmArtifact, WasmBackendKind},
};
