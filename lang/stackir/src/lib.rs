#![doc = include_str!("README.md")]

pub mod sps {
    /// Stack IR syntax and identifiers.
    pub mod syntax;
    /// Arenas and builders for stack IR nodes.
    pub mod arena;
    /// Pretty/ugly formatters for stack IR.
    pub mod fmt;
    /// Lowering from typed syntax into stack IR.
    pub mod lower;
    /// Closure conversion over stack IR.
    pub mod convert;
    /// In-place substitution helpers for stack IR.
    pub mod substitution;
    /// Free-variable analysis for stack IR.
    pub mod free;
    /// Builtin definitions for externs in stack IR.
    pub mod builtin;

    use zydeco_statics::tyck::syntax as ss;
}

pub use sps::{arena::*, builtin::*, convert::ClosureConverter, lower::Lowerer};

pub mod norm {}
