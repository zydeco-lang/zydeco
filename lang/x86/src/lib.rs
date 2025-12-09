//! The pointer to control stack is stored in Rsp.
//! The pointer to environment stack is stored in R10.
//! The pointer to the heap is stored in R11.

pub mod syntax;
pub mod emit;

pub use emit::Emitter;
