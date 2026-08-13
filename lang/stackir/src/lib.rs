#![doc = include_str!("README.md")]

use zydeco_statics::syntax as static_syntax;

/// Stack IR syntax and identifiers.
pub mod syntax;
/// Arenas and builders for stack IR nodes.
pub mod arena;
/// Builtin definitions for externs in stack IR.
pub mod builtin;
pub use builtin::*;

mod pipeline;
pub use pipeline::{CpsMode, StackirPipeline};

/// Passes and utilities specific to stack IR.
pub mod sps {
    /// Specific syntax and identifiers for stack IR
    pub mod syntax;
    /// Arenas and builders for stack IR nodes.
    pub mod arena;
    /// Pretty/ugly formatters for stack IR.
    pub mod fmt;
    /// Lowering from typed syntax into stack IR.
    pub mod lower;
    /// Double check the stack IR is well-formed. For debugging purposes only.
    pub mod check;
    /// CPS translation over stack IR.
    pub mod cps;
    /// Closure conversion over stack IR.
    pub mod convert;
    /// Free-variable analysis for stack IR.
    pub mod variables;
}

pub use sps::{
    arena::*,
    check::{BranchJoinError, BranchJoinProgram},
    convert::ClosureConverter,
    cps::CpsTranslator,
    lower::{BuiltinRootLowerer, Lowerer, RootLowerer},
};
