#![doc = include_str!("README.md")]

pub use zydeco_ir as ir;

/// Persistent radix-trie lexical environments keyed by [`zydeco_ir::VarId`].
pub mod env;
/// Dynamic runtime values, host continuations, and state.
pub mod syntax;
pub use syntax::{ProgKont, Runtime};
/// Host-owned values and capabilities used by runtime primitives.
pub mod host;
/// Registry for runtime builtin primitives.
pub mod builtin;
/// Checked foreign calls and the interpreter's dynamic-library runtime.
pub mod foreign;
pub use foreign::ForeignRuntimeError;
/// Implementations of runtime builtin primitives.
mod impls;
mod memory;
mod representation;
/// Small-step evaluator for [`zydeco_ir::CoreProgram`].
pub mod eval;
/// Formatting for runtime semantic values.
pub mod fmt;
