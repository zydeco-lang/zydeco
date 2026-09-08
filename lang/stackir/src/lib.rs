#![doc = include_str!("README.md")]

use zydeco_statics::syntax as static_syntax;

/// Syntax shared by high and low Stack IR.
pub mod syntax;
/// Shared builders and definition-name lookup for Stack IR.
pub mod arena;
/// Builtin definitions for externs in stack IR.
pub mod builtin;
pub use builtin::*;

mod pipeline;
pub use pipeline::SpsLowPipeline;

/// First-order stack-passing IR with blocks, jumps, and explicit packages.
pub mod low;
pub use low::{SpsLowConverter, SpsLowError, SpsLowProgram};

/// Lexical stack-passing IR with closures and continuations.
pub mod high;

pub use high::{
    arena::*,
    check::{BranchJoinError, BranchJoinProgram},
    lower::{BuiltinRootLowerError, BuiltinRootLowerer, Lowerer, RootLowerer, SpsLowerError},
};
