//! This crate implements the Zydeco-Intermediate-Representation (ZIR),
//! a low-level IR abstraction over stack mechanisms.

pub mod syntax;
pub mod arena;
pub mod fmt;
pub mod lower;

pub use lower::Lowerer;
