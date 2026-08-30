//! Abstract-machine WebAssembly backend for Zydeco Assembly (ZASM).
//!
//! ZASM continuations contain code addresses. WebAssembly does not expose raw
//! function addresses, so this backend represents them as indices into a
//! private function table. Every ZASM program point becomes a table entry and
//! the exported `entry` function drives them through a tail-dispatch loop.
//!
//! Runtime data words retain their 64-bit tagged representation. ZASM code
//! addresses are separate backend-private table indices. Products, boxed
//! scalars, the control stack, and the reusable environment live in the
//! module's linear memory. Builtin operations are imported from the `zydeco`
//! module through the ABI described by [`HOST_MODULE`].

mod emit;

pub use emit::{EmitError, Emitter, HOST_MODULE, WasmModule};
