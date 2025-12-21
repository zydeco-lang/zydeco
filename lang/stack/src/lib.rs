//! This crate implements the Zydeco-Intermediate-Representation (ZIR),
//! a high-level IR abstraction in stack-passing style.
//!
//! The ZIR has a one-to-one mapping to Zydeco itself.

pub mod syntax;
pub mod arena;
pub mod fmt;
pub mod lower;
pub mod conversion;
pub mod substitution;
pub mod free;

use zydeco_statics::tyck::syntax as ss;

pub use arena::*;
pub use conversion::ClosureConverter;
pub use lower::Lowerer;
