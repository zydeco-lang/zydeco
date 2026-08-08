#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod cli;
pub mod compile;
pub mod diagnostics;
pub mod format;
pub mod native;
// pub mod repl;

pub use crate::{
    cli::{BuildTarget, Cli, Commands, TargetArchitecture, TargetOs},
    compile::{AssemblyOutcome, BackendProgram, CommandCompiler, CompileError},
    diagnostics::DiagnosticRenderer,
    format::{SourceFormatError, SourceFormatOutcome, SourceFormatter},
    native::{BuildOptions, Executable, NativeError},
    // repl::Repl,
};
