#![doc = include_str!("README.md")]

use zydeco_statics::syntax as static_syntax;

/// Syntax shared by SPS and SPSLow.
pub mod syntax;
/// Structural traversal shared by the phases.
pub mod traverse;
/// Bound- and free-variable summaries shared by the phases.
pub mod variables;
/// Printing shared by the phases.
pub mod fmt;
/// Partial source value and stack protocols, independent of physical frame extent.
pub mod protocol;
/// Shared builders and definition-name lookup for Stack IR.
pub mod arena;
/// Builtin definitions for externs in stack IR.
pub mod builtin;
pub use builtin::*;

/// Stack-passing compilation schemes and their fragments' placement rules.
pub mod scheme;
pub use scheme::{PlacementError, SpsScheme};

mod pipeline;
pub use pipeline::SpsLowPipeline;

/// Built-in SPS pass selection and typed inspection hooks.
pub mod passes;

/// First-order stack-passing IR with blocks, jumps, and explicit packages.
pub mod low;
pub use low::{SpsLowConverter, SpsLowProgram, SpsLowValidationError};

/// Lexical stack-passing IR with closures and continuations.
pub mod high;

pub use high::{
    arena::*,
    check::{FragmentError, FragmentErrors, FragmentProgram},
    lower::{BuiltinRootLowerError, BuiltinRootLowerer, Lowerer, RootLowerer, SpsLoweringError},
};
