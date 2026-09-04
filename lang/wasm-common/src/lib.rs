//! Support shared by the Zydeco WebAssembly backends.
//!
//! Both wasm backends compile to the same runtime contract: an OCaml-style tagged-word
//! representation with one-word boxes, a bump allocator over linear memory, host imports
//! under one namespace, and a `string_literal` import for static string data. This crate
//! owns that contract once — the word encoding, the host-call ABI, the allocator body,
//! and the module scaffolding — so the backends only decide how their IR drives it.

pub mod host;
pub mod module;
pub mod word;

pub use host::{HostCallKind, HostImport, HostSections, StaticString, StringTable};
pub use module::{AllocFunction, WasmModule, WasmSections};
pub use word::{EncodedScalar, Intrinsics, PointerLocal, ProductFields, RuntimeWord, WordEmitter};

use thiserror::Error;
use wasm_encoder::MemArg;

/// Import namespace used by generated modules.
///
/// Returning builtins accept their arguments as `i64` words and return one `i64`.
/// Control builtins return four `i64` words: argument count, closure, first argument,
/// and second argument. Operations that can produce a boxed 64-bit scalar receive one
/// trailing `i32` spare-box address.
pub const HOST_MODULE: &str = "zydeco";

/// WebAssembly page size in bytes.
pub const WASM_PAGE_BYTES: u32 = 64 * 1024;

/// Size of one runtime word in bytes.
pub const WORD_BYTES: u32 = 8;

/// Unaligned eight-byte access to linear memory 0.
pub const WORD_MEMORY: MemArg = MemArg { offset: 0, align: 3, memory_index: 0 };

/// Errors raised by the shared WebAssembly backend support.
#[derive(Debug, Error)]
pub enum WasmEmitError {
    #[error("unresolved integer literal reached WebAssembly emission")]
    UnresolvedInteger,
    #[error("{what} ({value}) exceeds the wasm32 backend limit")]
    Limit { what: &'static str, value: usize },
}

/// Checked conversions for wasm32-sized values.
pub struct Limits;

impl Limits {
    pub fn u32(value: usize, what: &'static str) -> Result<u32, WasmEmitError> {
        u32::try_from(value).map_err(|_| WasmEmitError::Limit { what, value })
    }

    pub fn align(value: u32, alignment: u32) -> Result<u32, WasmEmitError> {
        debug_assert!(alignment.is_power_of_two());
        value
            .checked_add(alignment - 1)
            .map(|value| value & !(alignment - 1))
            .ok_or(WasmEmitError::Limit { what: "aligned memory address", value: value as usize })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tagged_words_match_the_native_immediate_boundary() {
        assert_eq!(RuntimeWord::index(0).unwrap(), 1);
        assert_eq!(RuntimeWord::code(0), 1);
        assert_eq!(RuntimeWord::code(1), 3);
        assert_eq!(RuntimeWord::signed(RuntimeWord::SIGNED_MIN), Some(0x8000_0000_0000_0001));
        assert_eq!(RuntimeWord::signed(RuntimeWord::SIGNED_MAX), Some(0x7fff_ffff_ffff_ffff));
        assert_eq!(RuntimeWord::signed(RuntimeWord::SIGNED_MIN - 1), None);
        assert_eq!(RuntimeWord::unsigned(RuntimeWord::UNSIGNED_MAX), Some(u64::MAX));
    }
}
