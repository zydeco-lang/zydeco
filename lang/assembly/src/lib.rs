//! This crate implements the Zydeco-Intermediate-Representation (ZIR),
//! a low-level IR abstraction over stack mechanisms.

pub mod syntax;
pub mod arena;
pub mod fmt;
pub mod lower;
mod norm;

pub use lower::Lowerer;
pub use norm::Normalizer;
