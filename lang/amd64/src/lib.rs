//! The amd64 backend for the Zydeco-Intermediate-Representation (ZIR).
//!
//! The pointer to control stack is stored in Rsp.
//! The pointer to environment stack is stored in Rbp.
//! The fixed two-space heap and collection cursor are managed by the runtime.
//! Runtime words use an OCaml-style low-bit tag: odd words are immediates and
//! aligned even words are pointer-shaped. Full-width scalars use opaque blocks.

pub mod syntax;
pub mod emit;

pub use emit::{Emitter, TargetFormat};
