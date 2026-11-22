//! Relocatable WebAssembly object emission (Linking Specification v2) for `wasm-ld` / `rust-lld`.

use wasm_encoder::{
    CustomSection, Encode, EntityType, FunctionSection, ImportSection,
    Instruction as WasmInstruction, MemoryType, Module, RawSection, RefType, SectionId, TableType,
    TypeSection, ValType,
};

use crate::{HOST_MODULE, Limits, WasmEmitError};

const LINKING_VERSION: u32 = 2;
const WASM_SEGMENT_INFO: u8 = 5;
const WASM_SYMBOL_TABLE: u8 = 8;

const SYMTAB_FUNCTION: u8 = 0;
const SYMTAB_DATA: u8 = 1;
const SYMTAB_TABLE: u8 = 5;

const WASM_SYM_BINDING_LOCAL: u32 = 0x02;
const WASM_SYM_UNDEFINED: u32 = 0x10;

const R_WASM_FUNCTION_INDEX_LEB: u8 = 0;
const R_WASM_TABLE_INDEX_I32: u8 = 2;
const R_WASM_MEMORY_ADDR_SLEB: u8 = 4;
const R_WASM_TYPE_INDEX_LEB: u8 = 6;
const R_WASM_TABLE_NUMBER_LEB: u8 = 20;

const CODE_SECTION_INDEX: u32 = 3;
const DATA_SECTION_INDEX: u32 = 4;

const OPCODE_CALL: u8 = 0x10;
const OPCODE_RETURN_CALL: u8 = 0x12;
const OPCODE_RETURN_CALL_INDIRECT: u8 = 0x13;
const OPCODE_I32_CONST: u8 = 0x41;

const FEATURE_PREFIX_USED: u8 = 0x2b;

/// Function reference sharing the same index as both its Wasm function index and its
/// `linking` symbol-table index.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FuncRef(pub u32);

/// Token identifying the imported `__indirect_function_table` symbol.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TableSymbol;

/// Zero-based segment index of one defined data segment.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DataSymbol(pub u32);

#[derive(Clone, Copy, Debug)]
enum RelocTarget {
    Function(FuncRef),
    Type(u32),
    Table,
    Data(DataSymbol),
}

#[derive(Clone, Copy, Debug)]
struct CodeReloc {
    kind: u8,
    offset_in_body: u32,
    target: RelocTarget,
    addend: Option<i32>,
}

/// Function body buffer supporting standard `wasm_encoder` instructions alongside
/// 5-byte relocatable call and data-address operands.
pub struct RelocFunction {
    bytes: Vec<u8>,
    relocs: Vec<CodeReloc>,
}

impl RelocFunction {
    pub fn new<L>(locals: L) -> Self
    where
        L: IntoIterator<Item = (u32, ValType)>,
    {
        let filtered: Vec<(u32, ValType)> =
            locals.into_iter().filter(|(count, _)| *count > 0).collect();
        let mut bytes = Vec::new();
        write_uleb(&mut bytes, filtered.len() as u32);
        for (count, ty) in filtered {
            write_uleb(&mut bytes, count);
            ty.encode(&mut bytes);
        }
        Self { bytes, relocs: Vec::new() }
    }

    pub fn instruction(&mut self, instruction: &WasmInstruction<'_>) {
        instruction.encode(&mut self.bytes);
    }

    pub fn call(&mut self, func: FuncRef) {
        self.emit_func_reloc(OPCODE_CALL, func);
    }

    pub fn return_call(&mut self, func: FuncRef) {
        self.emit_func_reloc(OPCODE_RETURN_CALL, func);
    }

    pub fn return_call_indirect(&mut self, type_index: u32, _table: TableSymbol) {
        self.bytes.push(OPCODE_RETURN_CALL_INDIRECT);
        let type_offset = self.bytes.len() as u32;
        self.relocs.push(CodeReloc {
            kind: R_WASM_TYPE_INDEX_LEB,
            offset_in_body: type_offset,
            target: RelocTarget::Type(type_index),
            addend: None,
        });
        write_uleb5(&mut self.bytes, type_index);

        let table_offset = self.bytes.len() as u32;
        self.relocs.push(CodeReloc {
            kind: R_WASM_TABLE_NUMBER_LEB,
            offset_in_body: table_offset,
            target: RelocTarget::Table,
            addend: None,
        });
        write_uleb5(&mut self.bytes, 0);
    }

    pub fn i32_const_memory(&mut self, symbol: DataSymbol, addend: i32) {
        self.bytes.push(OPCODE_I32_CONST);
        let offset_in_body = self.bytes.len() as u32;
        self.relocs.push(CodeReloc {
            kind: R_WASM_MEMORY_ADDR_SLEB,
            offset_in_body,
            target: RelocTarget::Data(symbol),
            addend: Some(addend),
        });
        write_sleb5(&mut self.bytes, 0);
    }

    fn emit_func_reloc(&mut self, opcode: u8, func: FuncRef) {
        self.bytes.push(opcode);
        let offset_in_body = self.bytes.len() as u32;
        self.relocs.push(CodeReloc {
            kind: R_WASM_FUNCTION_INDEX_LEB,
            offset_in_body,
            target: RelocTarget::Function(func),
            addend: None,
        });
        write_uleb5(&mut self.bytes, func.0);
    }
}

struct ImportedFunc {
    name: String,
    type_index: u32,
}

struct DefinedFunc {
    symbol_name: String,
    exported: bool,
    type_index: u32,
    body: RelocFunction,
}

#[derive(Clone, Copy)]
struct TableDataReloc {
    offset_in_segment: u32,
    func: FuncRef,
}

struct DataSegment {
    name: String,
    alignment_log2: u32,
    bytes: Vec<u8>,
    relocs: Vec<TableDataReloc>,
}

/// Assembles a relocatable `.o` WebAssembly module compatible with `rust-lld` / `wasm-ld`.
///
/// Function symbols are registered strictly in Wasm function-index order (imports followed by
/// defined functions), preserving the invariant that `FuncRef(k)` denotes both Wasm function
/// `k` and linking symbol `k`.
pub struct WasmObjectBuilder {
    types: TypeSection,
    imported_funcs: Vec<ImportedFunc>,
    defined_funcs: Vec<DefinedFunc>,
    data_segments: Vec<DataSegment>,
}

impl Default for WasmObjectBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl WasmObjectBuilder {
    pub fn new() -> Self {
        Self {
            types: TypeSection::new(),
            imported_funcs: Vec::new(),
            defined_funcs: Vec::new(),
            data_segments: Vec::new(),
        }
    }

    pub fn types_mut(&mut self) -> &mut TypeSection {
        &mut self.types
    }

    pub fn import_function(&mut self, name: impl Into<String>, type_index: u32) -> FuncRef {
        assert!(
            self.defined_funcs.is_empty(),
            "imported functions must be registered before defined functions"
        );
        let index = self.imported_funcs.len() as u32;
        self.imported_funcs.push(ImportedFunc { name: name.into(), type_index });
        FuncRef(index)
    }

    pub fn add_function(
        &mut self, symbol_name: impl Into<String>, exported: bool, type_index: u32,
        body: RelocFunction,
    ) -> FuncRef {
        let index = (self.imported_funcs.len() + self.defined_funcs.len()) as u32;
        self.defined_funcs.push(DefinedFunc {
            symbol_name: symbol_name.into(),
            exported,
            type_index,
            body,
        });
        FuncRef(index)
    }

    pub fn table_symbol(&self) -> TableSymbol {
        TableSymbol
    }

    pub fn add_data_segment(
        &mut self, name: impl Into<String>, alignment_log2: u32, bytes: Vec<u8>,
    ) -> DataSymbol {
        let segment = self.data_segments.len() as u32;
        self.data_segments.push(DataSegment {
            name: name.into(),
            alignment_log2,
            bytes,
            relocs: Vec::new(),
        });
        DataSymbol(segment)
    }

    pub fn add_table_reloc(&mut self, segment: DataSymbol, offset_in_segment: u32, func: FuncRef) {
        self.data_segments[segment.0 as usize]
            .relocs
            .push(TableDataReloc { offset_in_segment, func });
    }

    pub fn finish(self) -> Result<Vec<u8>, WasmEmitError> {
        let table_symbol_index = (self.imported_funcs.len() + self.defined_funcs.len()) as u32;
        let first_data_symbol = table_symbol_index + 1;

        let mut module = Module::new();

        // Section 0: TypeSection
        module.section(&self.types);

        // Section 1: ImportSection (__linear_memory, __indirect_function_table, imported functions)
        let mut imports = ImportSection::new();
        imports.import(
            HOST_MODULE,
            "__linear_memory",
            MemoryType {
                minimum: 0,
                maximum: None,
                memory64: false,
                shared: false,
                page_size_log2: None,
            },
        );
        imports.import(
            HOST_MODULE,
            "__indirect_function_table",
            TableType {
                element_type: RefType::FUNCREF,
                table64: false,
                minimum: 0,
                maximum: None,
                shared: false,
            },
        );
        for imported in &self.imported_funcs {
            imports.import(HOST_MODULE, &imported.name, EntityType::Function(imported.type_index));
        }
        module.section(&imports);

        // Section 2: FunctionSection
        let mut functions = FunctionSection::new();
        for defined in &self.defined_funcs {
            functions.function(defined.type_index);
        }
        module.section(&functions);

        // Section 3: CodeSection (raw payload so reloc offsets stay exact)
        let (code_payload, code_relocs) =
            Self::encode_code_section(&self.defined_funcs, table_symbol_index, first_data_symbol)?;
        module.section(&RawSection { id: SectionId::Code as u8, data: &code_payload });

        // Section 4 (optional): DataSection
        let data_relocs = if self.data_segments.is_empty() {
            Vec::new()
        } else {
            let (data_payload, relocs) = Self::encode_data_section(&self.data_segments)?;
            module.section(&RawSection { id: SectionId::Data as u8, data: &data_payload });
            relocs
        };

        // Custom "linking" section (v2)
        let linking_payload = Self::encode_linking_section(
            &self.imported_funcs,
            &self.defined_funcs,
            &self.data_segments,
        );
        module.section(&CustomSection { name: "linking".into(), data: linking_payload.into() });

        // Custom "reloc.CODE"
        if !code_relocs.is_empty() {
            let reloc_payload = Self::encode_code_relocs(&code_relocs);
            module
                .section(&CustomSection { name: "reloc.CODE".into(), data: reloc_payload.into() });
        }

        // Custom "reloc.DATA"
        if !data_relocs.is_empty() {
            let reloc_payload = Self::encode_data_relocs(&data_relocs);
            module
                .section(&CustomSection { name: "reloc.DATA".into(), data: reloc_payload.into() });
        }

        // Custom "target_features"
        let features_payload = Self::encode_target_features();
        module.section(&CustomSection {
            name: "target_features".into(),
            data: features_payload.into(),
        });

        Ok(module.finish())
    }

    fn encode_code_section(
        defined_funcs: &[DefinedFunc], table_symbol_index: u32, first_data_symbol: u32,
    ) -> Result<(Vec<u8>, Vec<(u8, u32, u32, Option<i32>)>), WasmEmitError> {
        let mut payload = Vec::new();
        write_uleb(&mut payload, Limits::u32(defined_funcs.len(), "function count")?);
        let mut relocs = Vec::new();
        for func in defined_funcs {
            write_uleb(&mut payload, Limits::u32(func.body.bytes.len(), "function body size")?);
            let body_start = Limits::u32(payload.len(), "code section size")?;
            payload.extend_from_slice(&func.body.bytes);
            for reloc in &func.body.relocs {
                let index = match reloc.target {
                    | RelocTarget::Function(func_ref) => func_ref.0,
                    | RelocTarget::Type(type_index) => type_index,
                    | RelocTarget::Table => table_symbol_index,
                    | RelocTarget::Data(seg) => first_data_symbol + seg.0,
                };
                relocs.push((reloc.kind, body_start + reloc.offset_in_body, index, reloc.addend));
            }
        }
        Ok((payload, relocs))
    }

    fn encode_data_section(
        segments: &[DataSegment],
    ) -> Result<(Vec<u8>, Vec<(u32, u32)>), WasmEmitError> {
        let mut payload = Vec::new();
        write_uleb(&mut payload, Limits::u32(segments.len(), "data segment count")?);
        let mut relocs = Vec::new();
        let mut virtual_cursor = 0_u32;
        for seg in segments {
            let align = 1_u32 << seg.alignment_log2;
            let virtual_offset = Limits::align(virtual_cursor, align)?;
            // Active memory 0 segment: flags=0, expr=[i32.const offset, end], size, bytes
            payload.push(0x00);
            payload.push(OPCODE_I32_CONST);
            write_sleb(&mut payload, virtual_offset as i32);
            payload.push(0x0b);
            write_uleb(&mut payload, Limits::u32(seg.bytes.len(), "data segment size")?);
            let data_start = Limits::u32(payload.len(), "data section size")?;
            payload.extend_from_slice(&seg.bytes);
            for reloc in &seg.relocs {
                relocs.push((data_start + reloc.offset_in_segment, reloc.func.0));
            }
            virtual_cursor = virtual_offset
                .checked_add(Limits::u32(seg.bytes.len(), "data segment size")?)
                .ok_or(WasmEmitError::Limit {
                    what: "data section virtual size",
                    value: seg.bytes.len(),
                })?;
        }
        Ok((payload, relocs))
    }

    fn encode_linking_section(
        imported: &[ImportedFunc], defined: &[DefinedFunc], segments: &[DataSegment],
    ) -> Vec<u8> {
        let mut payload = Vec::new();
        write_uleb(&mut payload, LINKING_VERSION);

        if !segments.is_empty() {
            let mut seg_info = Vec::new();
            write_uleb(&mut seg_info, segments.len() as u32);
            for seg in segments {
                write_str(&mut seg_info, &seg.name);
                write_uleb(&mut seg_info, seg.alignment_log2);
                write_uleb(&mut seg_info, 0);
            }
            payload.push(WASM_SEGMENT_INFO);
            write_uleb(&mut payload, seg_info.len() as u32);
            payload.extend_from_slice(&seg_info);
        }

        let mut symtab = Vec::new();
        let total_symbols = imported.len() + defined.len() + 1 + segments.len();
        write_uleb(&mut symtab, total_symbols as u32);

        for (index, _) in imported.iter().enumerate() {
            symtab.push(SYMTAB_FUNCTION);
            write_uleb(&mut symtab, WASM_SYM_UNDEFINED);
            write_uleb(&mut symtab, index as u32);
        }
        for (offset, func) in defined.iter().enumerate() {
            symtab.push(SYMTAB_FUNCTION);
            write_uleb(&mut symtab, if func.exported { 0 } else { WASM_SYM_BINDING_LOCAL });
            write_uleb(&mut symtab, (imported.len() + offset) as u32);
            write_str(&mut symtab, &func.symbol_name);
        }
        // Imported __indirect_function_table at table index 0
        symtab.push(SYMTAB_TABLE);
        write_uleb(&mut symtab, WASM_SYM_UNDEFINED);
        write_uleb(&mut symtab, 0);

        for (seg_idx, seg) in segments.iter().enumerate() {
            symtab.push(SYMTAB_DATA);
            write_uleb(&mut symtab, WASM_SYM_BINDING_LOCAL);
            write_str(&mut symtab, &seg.name);
            write_uleb(&mut symtab, seg_idx as u32);
            write_uleb(&mut symtab, 0);
            write_uleb(&mut symtab, seg.bytes.len() as u32);
        }

        payload.push(WASM_SYMBOL_TABLE);
        write_uleb(&mut payload, symtab.len() as u32);
        payload.extend_from_slice(&symtab);
        payload
    }

    fn encode_code_relocs(relocs: &[(u8, u32, u32, Option<i32>)]) -> Vec<u8> {
        let mut payload = Vec::new();
        write_uleb(&mut payload, CODE_SECTION_INDEX);
        write_uleb(&mut payload, relocs.len() as u32);
        for &(kind, offset, index, addend) in relocs {
            payload.push(kind);
            write_uleb(&mut payload, offset);
            write_uleb(&mut payload, index);
            if let Some(addend) = addend {
                write_sleb(&mut payload, addend);
            }
        }
        payload
    }

    fn encode_data_relocs(relocs: &[(u32, u32)]) -> Vec<u8> {
        let mut payload = Vec::new();
        write_uleb(&mut payload, DATA_SECTION_INDEX);
        write_uleb(&mut payload, relocs.len() as u32);
        for &(offset, symbol) in relocs {
            payload.push(R_WASM_TABLE_INDEX_I32);
            write_uleb(&mut payload, offset);
            write_uleb(&mut payload, symbol);
        }
        payload
    }

    fn encode_target_features() -> Vec<u8> {
        let mut payload = Vec::new();
        write_uleb(&mut payload, 1);
        payload.push(FEATURE_PREFIX_USED);
        write_str(&mut payload, "tail-call");
        payload
    }
}

fn write_str(out: &mut Vec<u8>, value: &str) {
    write_uleb(out, value.len() as u32);
    out.extend_from_slice(value.as_bytes());
}

fn write_uleb(out: &mut Vec<u8>, mut value: u32) {
    loop {
        let byte = (value & 0x7f) as u8;
        value >>= 7;
        if value == 0 {
            out.push(byte);
            break;
        }
        out.push(byte | 0x80);
    }
}

fn write_sleb(out: &mut Vec<u8>, value: i32) {
    let mut value = i64::from(value);
    loop {
        let byte = (value & 0x7f) as u8;
        value >>= 7;
        let done = (value == 0 && (byte & 0x40) == 0) || (value == -1 && (byte & 0x40) != 0);
        if done {
            out.push(byte);
            break;
        }
        out.push(byte | 0x80);
    }
}

fn write_uleb5(out: &mut Vec<u8>, mut value: u32) {
    for index in 0..5 {
        let byte = (value & 0x7f) as u8;
        value >>= 7;
        out.push(if index < 4 { byte | 0x80 } else { byte });
    }
}

fn write_sleb5(out: &mut Vec<u8>, value: i32) {
    let mut value = i64::from(value);
    for index in 0..5 {
        let byte = (value & 0x7f) as u8;
        value >>= 7;
        out.push(if index < 4 { byte | 0x80 } else { byte });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_encoder::MemArg;
    use wasmparser::{Parser, Payload, Validator};

    #[test]
    fn relocatable_object_with_tail_calls_and_table_relocs_validates() {
        let mut builder = WasmObjectBuilder::new();
        // type 0: (i64) -> ()
        builder.types_mut().ty().function([ValType::I64], []);
        // type 1: (i32) -> ()
        builder.types_mut().ty().function([ValType::I32], []);
        // type 2: () -> ()
        builder.types_mut().ty().function([], []);

        let err_fn = builder.import_function("zydeco_runtime_error", 1);
        let table = builder.table_symbol();

        let mut case0 = RelocFunction::new([]);
        case0.instruction(&WasmInstruction::I32Const(1));
        case0.call(err_fn);
        case0.instruction(&WasmInstruction::End);
        let case0_ref = builder.add_function("sps.case.0", false, 0, case0);

        let cases_data = builder.add_data_segment(".rodata.cases", 2, vec![0_u8; 4]);
        let strings_data = builder.add_data_segment(
            ".rodata.strings",
            3,
            vec![2, 0, 0, 0, 0, 0, 0, 0, b'o', b'k'],
        );

        let mut entry = RelocFunction::new([]);
        entry.i32_const_memory(strings_data, 0);
        entry.instruction(&WasmInstruction::Drop);
        entry.instruction(&WasmInstruction::I64Const(0));
        entry.i32_const_memory(cases_data, 0);
        entry.instruction(&WasmInstruction::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        entry.return_call_indirect(0, table);
        entry.instruction(&WasmInstruction::End);
        builder.add_function("zydeco_wasm_entry", true, 2, entry);

        builder.add_table_reloc(cases_data, 0, case0_ref);

        let bytes = builder.finish().unwrap();
        Validator::new().validate_all(&bytes).unwrap();

        let custom_names: Vec<String> = Parser::new(0)
            .parse_all(&bytes)
            .filter_map(|payload| match payload.unwrap() {
                | Payload::CustomSection(reader) => Some(reader.name().to_owned()),
                | _ => None,
            })
            .collect();
        assert_eq!(custom_names, ["linking", "reloc.CODE", "reloc.DATA", "target_features"]);
    }
}
