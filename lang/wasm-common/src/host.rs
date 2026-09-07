//! The host-call ABI: imports, type signatures, static strings, and naming.

use std::collections::HashMap;

use wasm_encoder::{
    EntityType, Function, ImportSection, Instruction, NameMap, TypeSection, ValType,
};
use zydeco_syntax::{SpareBox, Utf8String};

use crate::{HOST_MODULE, Limits, WORD_BYTES, WasmEmitError};

/// Fatal language errors reported through `zydeco.runtime_error(i32)`.
/// Hosts must stop execution and report the corresponding error to the user.
#[derive(Clone, Copy)]
#[repr(i32)]
pub enum RuntimeFailure {
    PatternMatch = 1,
    StackOverflow = 2,
    StackUnderflow = 3,
}

impl RuntimeFailure {
    /// The mandatory runtime-error import precedes optional imports in every module.
    pub const FUNCTION: u32 = 0;
    pub const IMPORT_COUNT: u32 = 1;

    pub fn emit(self, function: &mut Function) {
        function.instruction(&Instruction::I32Const(self as i32));
        function.instruction(&Instruction::Call(Self::FUNCTION));
        // Preserve the no-return contract even if an embedding returns from the import.
        function.instruction(&Instruction::Unreachable);
    }
}

/// Whether a host call returns its result or transfers control.
#[derive(Clone, Copy)]
pub enum HostCallKind {
    /// The call returns one word that replaces an argument of the continuation.
    Returning,
    /// The call returns a transfer: count, closure, and up to two arguments.
    Control,
}

/// One planned host import.
#[derive(Clone)]
pub struct HostImport {
    pub function: u32,
    pub name: String,
    pub arity: usize,
    pub mode: HostCallKind,
    pub spare: Option<SpareBox>,
}

/// The static-data placement of one string.
#[derive(Clone, Copy)]
pub struct StaticString {
    pub offset: u32,
    pub length: u32,
}

/// Layout of string data in the module's static region.
///
/// The region opens with one padding word so that no string is ever confused with a
/// valid heap object during garbage collection; the first real word of memory is never
/// a box.
pub struct StringTable;

impl StringTable {
    /// Build the static data segment and the placement map for `entries`.
    ///
    /// Entries are laid out in iteration order, which callers keep deterministic by
    /// sorting their ids first.
    pub fn build<Id: Eq + std::hash::Hash>(
        entries: impl IntoIterator<Item = (Id, Utf8String)>,
    ) -> Result<(Vec<u8>, HashMap<Id, StaticString>), WasmEmitError> {
        let mut static_data = vec![0; WORD_BYTES as usize];
        let mut strings = HashMap::new();
        for (id, string) in entries {
            let offset = Limits::u32(static_data.len(), "static string offset")?;
            let length = Limits::u32(string.byte_len(), "static string length")?;
            static_data.extend_from_slice(string.as_bytes());
            strings.insert(id, StaticString { offset, length });
        }
        Ok((static_data, strings))
    }
}

/// The shared host-import sections of a module.
pub struct HostSections;

impl HostSections {
    /// Append the runtime-error, `string_literal`, and host import types and imports, returning the
    /// next free type index.
    pub fn append_imports(
        types: &mut TypeSection, imports: &mut ImportSection, first_type: u32,
        string_literal: bool, host_imports: &[HostImport],
    ) -> u32 {
        let mut next_type = first_type;
        types.ty().function([ValType::I32], []);
        imports.import(HOST_MODULE, "runtime_error", EntityType::Function(next_type));
        next_type += 1;
        if string_literal {
            types.ty().function([ValType::I32, ValType::I32], [ValType::I64]);
            imports.import(HOST_MODULE, "string_literal", EntityType::Function(next_type));
            next_type += 1;
        }
        for import in host_imports {
            let mut parameters = vec![ValType::I64; import.arity];
            if import.spare.is_some() {
                parameters.push(ValType::I32);
            }
            let results = match import.mode {
                | HostCallKind::Returning => vec![ValType::I64],
                | HostCallKind::Control => vec![ValType::I64; 4],
            };
            types.ty().function(parameters, results);
            imports.import(HOST_MODULE, &import.name, EntityType::Function(next_type));
            next_type += 1;
        }
        next_type
    }

    /// Append the shared function names: the string-literal import and every host
    /// import under its `zydeco.`-prefixed host name.
    pub fn append_names(
        functions: &mut NameMap, string_literal: Option<u32>, host_imports: &[HostImport],
    ) {
        functions.append(RuntimeFailure::FUNCTION, "zydeco.runtime_error");
        if let Some(function) = string_literal {
            functions.append(function, "zydeco.string_literal");
        }
        for import in host_imports {
            functions.append(import.function, &format!("{HOST_MODULE}.{}", import.name));
        }
    }
}
