//! The LLVM IR backend for the Zydeco-Intermediate-Representation (ZIR).
//!
//! This module provides code generation from the Zydeco assembly IR to LLVM IR.
//! The resulting LLVM IR can be compiled to native code via LLVM.

pub mod syntax;
pub mod emit;

pub use emit::Emitter;
pub use syntax::TargetTriple;
