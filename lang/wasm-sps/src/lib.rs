//! Structured WebAssembly backend for Zydeco's first-order `SPS_l` IR.
//!
//! The backend preserves the lexical structure of [`SpsLowProgram`]. The root
//! and every first-order [`Block`] become one WebAssembly function; ordinary
//! SPS computations are emitted into that function as structured code. Dynamic
//! jumps cross block boundaries through a private function table and a small
//! trampoline, so source recursion does not consume the host call stack.
//!
//! Generated `.o` objects follow one runtime contract: an OCaml-style
//! 64-bit tagged-word representation with one-word boxes, `zydeco_wasm_alloc`
//! over WASI linear memory, host imports under `env`, and `.rodata.strings`
//! static literals linked with `zydeco-host`. Block handles are encoded as
//! immediate table indices; products, closures, continuations, and persistent
//! stack frames live in linear memory.
//!
//! [`SpsLowProgram`]: zydeco_stackir::SpsLowProgram
//! [`Block`]: zydeco_stackir::low::syntax::Block

mod emit;
mod host;
mod module;
mod object;
mod word;

pub use emit::{EmitError, Emitter};
pub use module::WasmModule;

pub(crate) use host::{
    ENTRY_SYMBOL, HostCallKind, HostImport, HostSections, RuntimeFailure, StaticString,
    StringTable, WasmRuntimeFailure,
};
pub(crate) use object::{DataSymbol, FuncRef, RelocFunction, TableSymbol, WasmObjectBuilder};
pub(crate) use word::{
    EncodedScalar, PointerLocal, ProductFields, RuntimeWord, WordEmitter, WordError,
};

use thiserror::Error;
use wasm_encoder::MemArg;

/// Import module namespace used by relocatable Wasm object files.
pub const HOST_MODULE: &str = "env";

/// Size of one runtime word in bytes.
pub(crate) const WORD_BYTES: u32 = 8;

/// Eight-byte-aligned access to linear memory 0.
pub(crate) const WORD_MEMORY: MemArg = MemArg { offset: 0, align: 3, memory_index: 0 };

/// Errors raised by WebAssembly object encoding and scalar-word lowering.
#[derive(Debug, Error)]
pub enum WasmEmitError {
    #[error("integer exceeds the tagged payload range")]
    IntegerRange,
    #[error("unresolved integer literal reached WebAssembly emission")]
    UnresolvedInteger,
    #[error("{what} ({value}) exceeds the wasm32 backend limit")]
    Limit { what: &'static str, value: usize },
}

impl From<WordError> for WasmEmitError {
    fn from(error: WordError) -> Self {
        match error {
            | WordError::IntegerRange => Self::IntegerRange,
            | WordError::UnresolvedInteger => Self::UnresolvedInteger,
            | WordError::TagIndex(value) => Self::Limit { what: "runtime tag index", value },
        }
    }
}

/// Checked conversions for wasm32-sized values.
pub(crate) struct Limits;

impl Limits {
    pub(crate) fn u32(value: usize, what: &'static str) -> Result<u32, WasmEmitError> {
        u32::try_from(value).map_err(|_| WasmEmitError::Limit { what, value })
    }

    pub(crate) fn align(value: u32, alignment: u32) -> Result<u32, WasmEmitError> {
        debug_assert!(alignment.is_power_of_two());
        value
            .checked_add(alignment - 1)
            .map(|value| value & !(alignment - 1))
            .ok_or(WasmEmitError::Limit { what: "aligned memory address", value: value as usize })
    }
}
