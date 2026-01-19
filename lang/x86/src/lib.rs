//! The x86-64 backend for the Zydeco-Intermediate-Representation (ZIR).
//!
//! The pointer to control stack is stored in Rsp.
//! The pointer to environment stack is stored in Rbp.
//! The pointer to the heap is managed by the runtime, not the assembly side.

pub mod syntax;
pub mod emit;

pub use emit::{Emitter, TargetFormat};
