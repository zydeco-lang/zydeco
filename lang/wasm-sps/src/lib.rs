//! Structured WebAssembly backend for Zydeco's first-order `SPS_l` IR.
//!
//! The backend preserves the lexical structure of [`SpsLowProgram`]. The root
//! and every first-order [`Block`] become one WebAssembly function; ordinary
//! SPS computations are emitted into that function as structured code. Dynamic
//! jumps cross block boundaries through a private function table and a small
//! trampoline, so source recursion does not consume the host call stack.
//!
//! Runtime values are 64-bit tagged words, including block handles encoded as
//! immediate table indices. Products, closures, continuations, and persistent
//! stack frames live in linear memory. Unlike the ZASM backend, there is no
//! mutable environment array or operand/control stack, and there is no
//! WebAssembly function for each assembly instruction.
//!
//! [`SpsLowProgram`]: zydeco_stackir::SpsLowProgram
//! [`Block`]: zydeco_stackir::sps_low::syntax::Block

mod emit;

pub use emit::{EmitError, Emitter, HOST_MODULE, WasmModule};
