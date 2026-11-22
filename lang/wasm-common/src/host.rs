//! The host-call ABI: imports, type signatures, static strings, and naming.

use std::collections::HashMap;

use wasm_encoder::{Instruction, ValType};
use zydeco_syntax::{SpareBox, Utf8String};

pub use zydeco_abi::RuntimeFailure;

use crate::{
    Limits, WasmEmitError,
    object::{FuncRef, RelocFunction, WasmObjectBuilder},
};

pub const RUNTIME_ERROR_SYMBOL: &str = "zydeco_runtime_error";
pub const ALLOC_SYMBOL: &str = "zydeco_wasm_alloc";
pub const ENTRY_SYMBOL: &str = "zydeco_wasm_entry";

/// WebAssembly lowering helpers for the shared [`RuntimeFailure`] trap contract.
pub struct WasmRuntimeFailure;

impl WasmRuntimeFailure {
    /// Mandatory host imports precede optional builtins in every relocatable object.
    pub const FUNCTION: FuncRef = FuncRef(0);
    pub const ALLOC_FUNCTION: FuncRef = FuncRef(1);
    pub const IMPORT_COUNT: u32 = 2;

    pub fn emit(failure: RuntimeFailure, function: &mut RelocFunction) {
        function.instruction(&Instruction::I32Const(failure.code()));
        function.call(Self::FUNCTION);
        // Preserve the no-return contract even if a linker stub ignores unreachable traps.
        function.instruction(&Instruction::Unreachable);
    }
}

/// Whether a host call returns its result or transfers control.
#[derive(Clone, Copy)]
pub enum HostCallKind {
    /// The call returns one word that replaces an argument of the continuation.
    Returning,
    /// The call returns a pointer (`i32`) to a `WasmHostTransfer` record.
    Control,
}

/// One planned host import.
#[derive(Clone)]
pub struct HostImport {
    pub function: FuncRef,
    pub name: String,
    pub arity: usize,
    pub mode: HostCallKind,
    pub spare: Option<SpareBox>,
}

/// The static-data placement of one string inside `.rodata.strings`.
#[derive(Clone, Copy)]
pub struct StaticString {
    pub offset: u32,
    pub length: u32,
}

/// Layout of static string data in `.rodata.strings`.
///
/// Each string is placed at an 8-byte-aligned offset with an 8-byte little-endian byte
/// length followed immediately by its UTF-8 bytes, matching [`zydeco_host::wasm`].
pub struct StringTable;

impl StringTable {
    const LENGTH_BYTES: usize = core::mem::size_of::<u64>();
    const ALIGNMENT: usize = 8;

    /// Build `.rodata.strings` bytes and the placement map for `entries`.
    ///
    /// Entries are laid out in iteration order, which callers keep deterministic by
    /// sorting their ids first.
    pub fn build<Id: Eq + std::hash::Hash>(
        entries: impl IntoIterator<Item = (Id, Utf8String)>,
    ) -> Result<(Vec<u8>, HashMap<Id, StaticString>), WasmEmitError> {
        let mut static_data = Vec::new();
        let mut strings = HashMap::new();
        for (id, string) in entries {
            let aligned = Limits::align(
                Limits::u32(static_data.len(), "static string offset")?,
                Self::ALIGNMENT as u32,
            )? as usize;
            static_data.resize(aligned, 0);
            let offset = Limits::u32(static_data.len(), "static string offset")?;
            let length = Limits::u32(string.byte_len(), "static string length")?;
            static_data.extend_from_slice(&(string.byte_len() as u64).to_le_bytes());
            static_data.extend_from_slice(string.as_bytes());
            debug_assert!(
                static_data.len() - offset as usize == Self::LENGTH_BYTES + string.byte_len()
            );
            strings.insert(id, StaticString { offset, length });
        }
        Ok((static_data, strings))
    }
}

/// The shared host-import sections of a relocatable Wasm object.
pub struct HostSections;

impl HostSections {
    /// Append `zydeco_runtime_error`, `zydeco_wasm_alloc`, and host import types/imports,
    /// returning the next free type index.
    pub fn append_imports(
        builder: &mut WasmObjectBuilder, first_type: u32, host_imports: &[HostImport],
    ) -> u32 {
        let mut next_type = first_type;
        builder.types_mut().ty().function([ValType::I32], []);
        let err_ref = builder.import_function(RUNTIME_ERROR_SYMBOL, next_type);
        debug_assert_eq!(err_ref, WasmRuntimeFailure::FUNCTION);
        next_type += 1;

        builder.types_mut().ty().function([ValType::I32], [ValType::I32]);
        let alloc_ref = builder.import_function(ALLOC_SYMBOL, next_type);
        debug_assert_eq!(alloc_ref, WasmRuntimeFailure::ALLOC_FUNCTION);
        next_type += 1;

        for import in host_imports {
            let mut parameters = vec![ValType::I64; import.arity];
            if import.spare.is_some() {
                parameters.push(ValType::I32);
            }
            let results = match import.mode {
                | HostCallKind::Returning => ValType::I64,
                | HostCallKind::Control => ValType::I32,
            };
            builder.types_mut().ty().function(parameters, [results]);
            let assigned = builder.import_function(&import.name, next_type);
            debug_assert_eq!(assigned, import.function);
            next_type += 1;
        }
        next_type
    }
}
