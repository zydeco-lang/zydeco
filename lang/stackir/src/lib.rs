#![doc = include_str!("README.md")]

use zydeco_statics::tyck::syntax as static_syntax;

/// Stack IR syntax and identifiers.
pub mod syntax;
/// Arenas and builders for stack IR nodes.
pub mod arena;
/// Builtin definitions for externs in stack IR.
pub mod builtin;
pub use builtin::*;

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
    /// Closure conversion over stack IR.
    pub mod convert;
    /// In-place substitution helpers for stack IR.
    pub mod substitute;
    /// Free-variable analysis for stack IR.
    pub mod variables;
}

pub use sps::{arena::*, convert::ClosureConverter, lower::Lowerer};

pub mod norm {
    /// Extra syntax for normalization.
    pub mod syntax;
    /// Arenas and builders for normalized stack IR.
    pub mod arena;
    /// Pretty/ugly formatters for normalized stack IR.
    pub mod fmt;
    /// Elaboration pass from stack IR into normalized stack IR.
    pub mod elaborate;
    /// Reconstructing substitution from normalized stack IR into stack IR.
    pub mod substitute;
    /// Variable introduction analysis for normalized stack IR.
    pub mod variables;
    /// Deep cloning of nodes in normalized stack IR.
    pub mod clone;
}

pub use norm::{arena::*, elaborate::Elaborator, substitute::Substitutor};
