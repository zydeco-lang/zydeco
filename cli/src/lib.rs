#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod cli;
pub mod compile;
pub mod diagnostics;
pub mod native;
// pub mod repl;

pub use crate::{
    cli::{BuildTarget, Cli, Commands, TargetArchitecture, TargetOs},
    compile::{AssemblyOutcome, BackendProgram, CommandCompiler, CompileError},
    diagnostics::DiagnosticRenderer,
    native::{BuildOptions, Executable, NativeError},
    // repl::Repl,
};
