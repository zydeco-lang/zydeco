#![doc = include_str!("README.md")]

pub mod statics_syntax {
    pub use zydeco_statics::syntax::*;
}

/// Dynamic syntax and runtime state for the evaluator.
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
/// Linking from statics syntax to dynamic runtime syntax.
pub mod link;
pub use link::*;
/// Small-step evaluator for dynamic computations.
pub mod eval;
pub use eval::*;
/// Formatting for dynamic syntax and runtime values.
pub mod fmt;

pub(crate) use statics_syntax as ss;
pub(crate) use syntax as ds;

// Todo: generalize Rc/Arc using family trait
// Reference: https://jedsek.xyz/posts/rust-typed-magic/gats/#family-trait
