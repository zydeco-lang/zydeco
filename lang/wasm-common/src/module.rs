//! Encoded modules, core section scaffolding, and the allocator helper.

use std::borrow::Cow;

use wasm_encoder::{
    BlockType, ConstExpr, DataSection, ElementSection, Elements, ExportKind, ExportSection,
    Function, GlobalSection, GlobalType, Instruction as WasmInstruction, MemorySection, MemoryType,
    Module, RefType, TableSection, TableType, ValType,
};

use crate::WASM_PAGE_BYTES;

/// An encoded core WebAssembly module.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WasmModule {
    bytes: Vec<u8>,
}

impl WasmModule {
    pub fn as_bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.bytes
    }

    pub fn from_module(module: Module) -> Self {
        Self { bytes: module.finish() }
    }
}

/// The core sections every backend module shares.
///
/// The section order is fixed by the emitters: types and imports, functions, table,
/// memory, globals, exports, elements, code, data, and the name section. These helpers
/// cover the sections whose shape does not depend on the backend's IR.
pub struct WasmSections;

impl WasmSections {
    /// The case-dispatch table sized to the case count.
    pub fn table(module: &mut Module, case_count: u32) {
        let mut tables = TableSection::new();
        tables.table(TableType {
            element_type: RefType::FUNCREF,
            table64: false,
            minimum: u64::from(case_count),
            maximum: Some(u64::from(case_count)),
            shared: false,
        });
        module.section(&tables);
    }

    /// Linear memory sized for one growable heap page beyond the initial end.
    pub fn memory(module: &mut Module, initial_pages: u64) {
        let mut memories = MemorySection::new();
        memories.memory(MemoryType {
            minimum: initial_pages,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        module.section(&memories);
    }

    /// Mutable globals initialized to their constants, in order.
    pub fn globals(module: &mut Module, entries: Vec<(GlobalType, ConstExpr)>) {
        let mut globals = GlobalSection::new();
        for (global_type, initializer) in entries {
            globals.global(global_type, &initializer);
        }
        module.section(&globals);
    }

    /// The `memory`, `entry`, and `_start` exports.
    pub fn exports(module: &mut Module, entry: u32) {
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        exports.export("entry", ExportKind::Func, entry);
        exports.export("_start", ExportKind::Func, entry);
        module.section(&exports);
    }

    /// The active element segment installing the case functions at table index zero.
    pub fn elements(module: &mut Module, case_functions: Vec<u32>) {
        let mut elements = ElementSection::new();
        elements.active(
            None,
            &ConstExpr::i32_const(0),
            Elements::Functions(Cow::Owned(case_functions)),
        );
        module.section(&elements);
    }

    /// The active data segment holding the static region, when nonempty.
    pub fn data(module: &mut Module, static_data: &[u8]) {
        if !static_data.is_empty() {
            let mut data = DataSection::new();
            data.active(0, &ConstExpr::i32_const(0), static_data.iter().copied());
            module.section(&data);
        }
    }
}

/// The one-word bump allocator helper shared by the backends.
///
/// Parameter 0 is the requested word count; locals 1 and 2 are the old and new heap
/// cursors. The returned pointer is the old cursor; growth beyond the current memory
/// size traps.
pub struct AllocFunction {
    heap_global: u32,
}

impl AllocFunction {
    /// The allocator reading and writing the heap-cursor global at `heap_global`.
    pub fn new(heap_global: u32) -> Self {
        Self { heap_global }
    }

    pub fn emit(&self) -> Function {
        let mut function = Function::new([(2, ValType::I32)]);
        function.instruction(&WasmInstruction::GlobalGet(self.heap_global));
        function.instruction(&WasmInstruction::LocalTee(1));
        function.instruction(&WasmInstruction::LocalGet(0));
        function.instruction(&WasmInstruction::I32Const(3));
        function.instruction(&WasmInstruction::I32Shl);
        function.instruction(&WasmInstruction::I32Add);
        function.instruction(&WasmInstruction::LocalTee(2));
        function.instruction(&WasmInstruction::MemorySize(0));
        function.instruction(&WasmInstruction::I32Const(16));
        function.instruction(&WasmInstruction::I32Shl);
        function.instruction(&WasmInstruction::I32GtU);
        function.instruction(&WasmInstruction::If(BlockType::Empty));
        function.instruction(&WasmInstruction::LocalGet(2));
        function.instruction(&WasmInstruction::I32Const((WASM_PAGE_BYTES - 1) as i32));
        function.instruction(&WasmInstruction::I32Add);
        function.instruction(&WasmInstruction::I32Const(16));
        function.instruction(&WasmInstruction::I32ShrU);
        function.instruction(&WasmInstruction::MemorySize(0));
        function.instruction(&WasmInstruction::I32Sub);
        function.instruction(&WasmInstruction::MemoryGrow(0));
        function.instruction(&WasmInstruction::I32Const(-1));
        function.instruction(&WasmInstruction::I32Eq);
        function.instruction(&WasmInstruction::If(BlockType::Empty));
        function.instruction(&WasmInstruction::Unreachable);
        function.instruction(&WasmInstruction::End);
        function.instruction(&WasmInstruction::End);
        function.instruction(&WasmInstruction::LocalGet(2));
        function.instruction(&WasmInstruction::GlobalSet(self.heap_global));
        function.instruction(&WasmInstruction::LocalGet(1));
        function.instruction(&WasmInstruction::End);
        function
    }
}
