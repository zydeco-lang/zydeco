use std::{borrow::Cow, collections::HashMap};

use thiserror::Error;
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, DataSection, ElementSection, Elements, EntityType,
    ExportKind, ExportSection, Function, FunctionSection, GlobalSection, GlobalType, ImportSection,
    Instruction as WasmInstruction, MemArg, MemorySection, MemoryType, Module, NameMap,
    NameSection, RefType, TableSection, TableType, TypeSection, ValType,
};
use zydeco_stackir::{
    SpsLowProgram,
    sps_low::syntax::{
        self as sps, Block, CompuId, Computation, DefId, HostCallMode, ProductLayout, StackId,
        VPatId, ValueId, ValuePattern,
    },
};
use zydeco_syntax::{
    BuiltinValueRole, FloatOperation, FloatType, IntegerLiteral, IntegerOperation, IntegerType,
    Literal,
};

/// Import namespace shared with the abstract-machine WebAssembly backend.
///
/// Returning functions accept their SPS arguments as `i64` words and return
/// one `i64`. Control functions return four `i64` words: argument count,
/// closure, first argument, and second argument. Operations that can produce a
/// boxed 64-bit scalar receive a trailing `i32` spare-box address.
pub const HOST_MODULE: &str = "zydeco";

const CASE_TYPE: u32 = 0;
const ALLOC_TYPE: u32 = 1;
const PAIR_TYPE: u32 = 2;
const ENTRY_TYPE: u32 = 3;
const FIRST_IMPORT_TYPE: u32 = 4;

const WASM_PAGE_BYTES: u32 = 64 * 1024;
const WORD_BYTES: u32 = 8;

const HEAP_POINTER_GLOBAL: u32 = 0;
const PROGRAM_COUNTER_GLOBAL: u32 = 1;
const AMBIENT_STACK_GLOBAL: u32 = 2;

const WORD_MEMORY: MemArg = MemArg { offset: 0, align: 3, memory_index: 0 };

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
}

/// Direct structured `SPS_l`-to-WebAssembly emitter.
pub struct Emitter<'a> {
    program: &'a SpsLowProgram,
}

impl<'a> Emitter<'a> {
    pub fn new(program: &'a SpsLowProgram) -> Self {
        Self { program }
    }

    pub fn run(self) -> Result<WasmModule, EmitError> {
        let plan = ModulePlan::new(self.program)?;
        ModuleEncoder::new(self.program, plan).encode()
    }
}

#[derive(Debug, Error)]
pub enum EmitError {
    #[error("SPS WebAssembly backend cannot find block label {0:?}")]
    MissingBlock(DefId),
    #[error("SPS WebAssembly backend cannot find local for variable {0:?}")]
    MissingVariable(DefId),
    #[error("SPS WebAssembly backend cannot find local for value {0:?}")]
    MissingValue(ValueId),
    #[error("SPS WebAssembly backend cannot find local for stack {0:?}")]
    MissingStack(StackId),
    #[error("SPS WebAssembly backend cannot find local for pattern {0:?}")]
    MissingPattern(VPatId),
    #[error("SPS WebAssembly backend cannot find static string data for value {0:?}")]
    MissingString(ValueId),
    #[error("SPS WebAssembly backend cannot find host import `{0}`")]
    MissingHostImport(String),
    #[error("SPS WebAssembly backend found an operator `{0}` in function position")]
    OperatorCalledAsFunction(String),
    #[error("unresolved integer literal reached SPS WebAssembly emission")]
    UnresolvedInteger,
    #[error("unsupported SPS intrinsic `{name}/{arity}` in the WebAssembly backend")]
    UnsupportedIntrinsic { name: String, arity: usize },
    #[error("invalid SPS coproduct match: constructor arms cannot be mixed with a catch-all arm")]
    InvalidCoprodMatch,
    #[error("{what} ({value}) exceeds the wasm32 backend limit")]
    Limit { what: &'static str, value: usize },
}

#[derive(Clone, Copy)]
enum SpareBox {
    Opaque,
    Unused,
}

impl SpareBox {
    fn for_role(role: BuiltinValueRole) -> Option<Self> {
        match role {
            | BuiltinValueRole::Integer(
                integer,
                IntegerOperation::Add
                | IntegerOperation::Sub
                | IntegerOperation::Mul
                | IntegerOperation::Div
                | IntegerOperation::Mod,
            ) => Some(if matches!(integer, IntegerType::Int64 | IntegerType::UInt64) {
                Self::Opaque
            } else {
                Self::Unused
            }),
            | BuiltinValueRole::Float(
                float,
                FloatOperation::Add
                | FloatOperation::Sub
                | FloatOperation::Mul
                | FloatOperation::Div,
            ) => Some(if float == FloatType::Float64 { Self::Opaque } else { Self::Unused }),
            | BuiltinValueRole::StrParseInt
            | BuiltinValueRole::ReadLineAsInt
            | BuiltinValueRole::RandomInt => Some(Self::Opaque),
            | _ => None,
        }
    }
}

#[derive(Clone)]
struct HostImport {
    function: u32,
    name: String,
    arity: usize,
    mode: HostCallMode,
    spare: Option<SpareBox>,
}

#[derive(Clone, Copy)]
struct StaticString {
    offset: u32,
    length: u32,
}

struct MemoryLayout {
    heap_base: u32,
    initial_pages: u64,
}

impl MemoryLayout {
    fn new(static_bytes: usize) -> Result<Self, EmitError> {
        let static_bytes = Limits::u32(static_bytes, "static data size")?;
        let heap_base = Limits::align(
            static_bytes.checked_add(WORD_BYTES).ok_or(EmitError::Limit {
                what: "static data end",
                value: static_bytes as usize,
            })?,
            WORD_BYTES,
        )?;
        let initial_end = heap_base
            .checked_add(WASM_PAGE_BYTES)
            .ok_or(EmitError::Limit { what: "initial memory size", value: heap_base as usize })?;
        Ok(Self { heap_base, initial_pages: u64::from(initial_end.div_ceil(WASM_PAGE_BYTES)) })
    }
}

struct Limits;

impl Limits {
    fn u32(value: usize, what: &'static str) -> Result<u32, EmitError> {
        u32::try_from(value).map_err(|_| EmitError::Limit { what, value })
    }

    fn align(value: u32, alignment: u32) -> Result<u32, EmitError> {
        debug_assert!(alignment.is_power_of_two());
        value
            .checked_add(alignment - 1)
            .map(|value| value & !(alignment - 1))
            .ok_or(EmitError::Limit { what: "aligned memory address", value: value as usize })
    }
}

#[derive(Clone, Copy)]
struct Case {
    index: u32,
    label: Option<DefId>,
    body: CompuId,
}

struct LocalPlan {
    variables: HashMap<DefId, u32>,
    values: HashMap<ValueId, u32>,
    stacks: HashMap<StackId, u32>,
    patterns: HashMap<VPatId, u32>,
    scratch_stack: u32,
    scratch_word: u32,
    result: u32,
    transfer_count: u32,
    transfer_closure: u32,
    transfer_first: u32,
    transfer_second: u32,
    decoded_first: u32,
    decoded_second: u32,
    local_count: u32,
}

impl LocalPlan {
    fn new(arena: &sps::SpsLowArena) -> Result<Self, EmitError> {
        let mut next = 1_u32;

        let mut defs = arena
            .inner
            .vpats
            .iter()
            .filter_map(|(_, pattern)| match pattern {
                | ValuePattern::Var(def) => Some(*def),
                | _ => None,
            })
            .collect::<Vec<_>>();
        defs.sort_unstable();
        defs.dedup();
        let variables = Self::assign(defs, &mut next, "SPS variable count")?;

        let mut values = arena.inner.values.iter().map(|(id, _)| *id).collect::<Vec<_>>();
        values.sort_unstable();
        let values = Self::assign(values, &mut next, "SPS value count")?;

        let mut stacks = arena.inner.stacks.iter().map(|(id, _)| *id).collect::<Vec<_>>();
        stacks.sort_unstable();
        let stacks = Self::assign(stacks, &mut next, "SPS stack count")?;

        let mut patterns = arena.inner.vpats.iter().map(|(id, _)| *id).collect::<Vec<_>>();
        patterns.sort_unstable();
        let patterns = Self::assign(patterns, &mut next, "SPS pattern count")?;

        let scratch_stack = Self::take(&mut next, "SPS local count")?;
        let scratch_word = Self::take(&mut next, "SPS local count")?;
        let result = Self::take(&mut next, "SPS local count")?;
        let transfer_count = Self::take(&mut next, "SPS local count")?;
        let transfer_closure = Self::take(&mut next, "SPS local count")?;
        let transfer_first = Self::take(&mut next, "SPS local count")?;
        let transfer_second = Self::take(&mut next, "SPS local count")?;
        let decoded_first = Self::take(&mut next, "SPS local count")?;
        let decoded_second = Self::take(&mut next, "SPS local count")?;

        Ok(Self {
            variables,
            values,
            stacks,
            patterns,
            scratch_stack,
            scratch_word,
            result,
            transfer_count,
            transfer_closure,
            transfer_first,
            transfer_second,
            decoded_first,
            decoded_second,
            local_count: next - 1,
        })
    }

    fn assign<Id: Eq + std::hash::Hash>(
        ids: Vec<Id>, next: &mut u32, what: &'static str,
    ) -> Result<HashMap<Id, u32>, EmitError> {
        ids.into_iter().map(|id| Self::take(next, what).map(|local| (id, local))).collect()
    }

    fn take(next: &mut u32, what: &'static str) -> Result<u32, EmitError> {
        let local = *next;
        *next = next.checked_add(1).ok_or(EmitError::Limit { what, value: usize::MAX })?;
        Ok(local)
    }

    fn variable(&self, def: DefId) -> Result<u32, EmitError> {
        self.variables.get(&def).copied().ok_or(EmitError::MissingVariable(def))
    }

    fn value(&self, value: ValueId) -> Result<u32, EmitError> {
        self.values.get(&value).copied().ok_or(EmitError::MissingValue(value))
    }

    fn stack(&self, stack: StackId) -> Result<u32, EmitError> {
        self.stacks.get(&stack).copied().ok_or(EmitError::MissingStack(stack))
    }

    fn pattern(&self, pattern: VPatId) -> Result<u32, EmitError> {
        self.patterns.get(&pattern).copied().ok_or(EmitError::MissingPattern(pattern))
    }
}

struct ModulePlan {
    cases: Vec<Case>,
    labels: HashMap<DefId, u32>,
    strings: HashMap<ValueId, StaticString>,
    static_data: Vec<u8>,
    string_literal_function: Option<u32>,
    host_imports: Vec<HostImport>,
    host_functions: HashMap<String, usize>,
    import_count: u32,
    layout: MemoryLayout,
    locals: LocalPlan,
}

impl ModulePlan {
    fn new(program: &SpsLowProgram) -> Result<Self, EmitError> {
        let arena = program.arena();
        let mut blocks = arena
            .inner
            .values
            .iter()
            .filter_map(|(_, value)| match value {
                | sps::Value::Block(block) => Some(block.clone()),
                | _ => None,
            })
            .collect::<Vec<_>>();
        blocks.sort_by_key(|block| block.label);

        let root = Case { index: 0, label: None, body: program.root() };
        let block_cases = blocks
            .into_iter()
            .enumerate()
            .map(|(offset, Block { label, body })| {
                let index = Limits::u32(offset + 1, "SPS block count")?;
                Ok(Case { index, label: Some(label), body })
            })
            .collect::<Result<Vec<_>, EmitError>>()?;
        let cases = std::iter::once(root).chain(block_cases).collect::<Vec<_>>();
        let labels =
            cases.iter().filter_map(|case| case.label.map(|label| (label, case.index))).collect();

        let mut string_values = arena
            .inner
            .values
            .iter()
            .filter_map(|(id, value)| {
                matches!(value, sps::Value::Literal(Literal::String(_))).then_some(*id)
            })
            .collect::<Vec<_>>();
        string_values.sort_unstable();
        let mut static_data = vec![0; WORD_BYTES as usize];
        let mut strings = HashMap::new();
        for value in string_values {
            let sps::Value::Literal(Literal::String(string)) = &arena.inner.values[&value] else {
                unreachable!("string value collection changed during planning")
            };
            let offset = Limits::u32(static_data.len(), "static string offset")?;
            let length = Limits::u32(string.byte_len(), "static string length")?;
            static_data.extend_from_slice(string.as_bytes());
            strings.insert(value, StaticString { offset, length });
        }

        let layout = MemoryLayout::new(static_data.len())?;
        let string_literal_function = (!strings.is_empty()).then_some(0);
        let first_host_function = u32::from(string_literal_function.is_some());

        let mut builtins = arena
            .admin
            .builtins
            .values()
            .filter_map(|builtin| match builtin.sort {
                | sps::BuiltinSort::Function(mode) => Some((builtin, mode)),
                | sps::BuiltinSort::Operator => None,
            })
            .collect::<Vec<_>>();
        builtins.sort_by(|(left, _), (right, _)| left.name.cmp(&right.name));
        let host_imports = builtins
            .into_iter()
            .enumerate()
            .map(|(offset, (builtin, mode))| {
                let offset = Limits::u32(offset, "host import count")?;
                Ok(HostImport {
                    function: first_host_function + offset,
                    name: builtin.name.clone(),
                    arity: builtin.arity,
                    mode,
                    spare: SpareBox::for_role(builtin.role),
                })
            })
            .collect::<Result<Vec<_>, EmitError>>()?;
        let host_functions = host_imports
            .iter()
            .enumerate()
            .map(|(index, import)| (import.name.clone(), index))
            .collect();
        let import_count = first_host_function
            .checked_add(Limits::u32(host_imports.len(), "host import count")?)
            .ok_or(EmitError::Limit { what: "host import count", value: host_imports.len() })?;
        let locals = LocalPlan::new(arena)?;

        Ok(Self {
            cases,
            labels,
            strings,
            static_data,
            string_literal_function,
            host_imports,
            host_functions,
            import_count,
            layout,
            locals,
        })
    }

    fn alloc_function(&self) -> u32 {
        self.import_count
    }

    fn pair_function(&self) -> u32 {
        self.import_count + 1
    }

    fn case_function(&self, index: u32) -> u32 {
        self.import_count + 2 + index
    }

    fn entry_function(&self) -> Result<u32, EmitError> {
        Ok(self.import_count + 2 + Limits::u32(self.cases.len(), "SPS block count")?)
    }

    fn label(&self, label: DefId) -> Result<u32, EmitError> {
        self.labels.get(&label).copied().ok_or(EmitError::MissingBlock(label))
    }

    fn string(&self, value: ValueId) -> Result<StaticString, EmitError> {
        self.strings.get(&value).copied().ok_or(EmitError::MissingString(value))
    }

    fn host(&self, name: &str) -> Result<&HostImport, EmitError> {
        self.host_functions
            .get(name)
            .map(|index| &self.host_imports[*index])
            .ok_or_else(|| EmitError::MissingHostImport(name.to_owned()))
    }
}

struct ModuleEncoder<'a> {
    program: &'a SpsLowProgram,
    plan: ModulePlan,
}

impl<'a> ModuleEncoder<'a> {
    fn new(program: &'a SpsLowProgram, plan: ModulePlan) -> Self {
        Self { program, plan }
    }

    fn encode(self) -> Result<WasmModule, EmitError> {
        let mut module = Module::new();
        let (types, imports) = self.type_and_import_sections();
        module.section(&types);
        if !imports.is_empty() {
            module.section(&imports);
        }

        let mut functions = FunctionSection::new();
        functions.function(ALLOC_TYPE);
        functions.function(PAIR_TYPE);
        self.plan.cases.iter().for_each(|_| {
            functions.function(CASE_TYPE);
        });
        functions.function(ENTRY_TYPE);
        module.section(&functions);

        let case_count = Limits::u32(self.plan.cases.len(), "SPS block count")?;
        let mut tables = TableSection::new();
        tables.table(TableType {
            element_type: RefType::FUNCREF,
            table64: false,
            minimum: u64::from(case_count),
            maximum: Some(u64::from(case_count)),
            shared: false,
        });
        module.section(&tables);

        let mut memories = MemorySection::new();
        memories.memory(MemoryType {
            minimum: self.plan.layout.initial_pages,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        module.section(&memories);

        let mut globals = GlobalSection::new();
        globals.global(
            GlobalType { val_type: ValType::I32, mutable: true, shared: false },
            &ConstExpr::i32_const(self.plan.layout.heap_base as i32),
        );
        globals.global(
            GlobalType { val_type: ValType::I32, mutable: true, shared: false },
            &ConstExpr::i32_const(0),
        );
        globals.global(
            GlobalType { val_type: ValType::I64, mutable: true, shared: false },
            &ConstExpr::i64_const(0),
        );
        module.section(&globals);

        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        let entry = self.plan.entry_function()?;
        exports.export("entry", ExportKind::Func, entry);
        exports.export("_start", ExportKind::Func, entry);
        module.section(&exports);

        let case_functions =
            (0..case_count).map(|index| self.plan.case_function(index)).collect::<Vec<_>>();
        let mut elements = ElementSection::new();
        elements.active(
            None,
            &ConstExpr::i32_const(0),
            Elements::Functions(Cow::Owned(case_functions)),
        );
        module.section(&elements);

        let mut code = CodeSection::new();
        code.function(&self.alloc_function());
        code.function(&self.pair_function());
        for case in &self.plan.cases {
            code.function(&CaseEncoder::new(self.program, &self.plan).encode(case.body)?);
        }
        code.function(&self.entry_function());
        module.section(&code);

        if !self.plan.static_data.is_empty() {
            let mut data = DataSection::new();
            data.active(0, &ConstExpr::i32_const(0), self.plan.static_data.iter().copied());
            module.section(&data);
        }
        module.section(&self.name_section()?);

        Ok(WasmModule { bytes: module.finish() })
    }

    fn type_and_import_sections(&self) -> (TypeSection, ImportSection) {
        let mut types = TypeSection::new();
        types.ty().function([ValType::I64], []);
        types.ty().function([ValType::I32], [ValType::I32]);
        types.ty().function([ValType::I64, ValType::I64], [ValType::I64]);
        types.ty().function([], []);

        let mut imports = ImportSection::new();
        let mut next_type = FIRST_IMPORT_TYPE;
        if self.plan.string_literal_function.is_some() {
            types.ty().function([ValType::I32, ValType::I32], [ValType::I64]);
            imports.import(HOST_MODULE, "string_literal", EntityType::Function(next_type));
            next_type += 1;
        }
        for import in &self.plan.host_imports {
            let mut parameters = vec![ValType::I64; import.arity];
            if import.spare.is_some() {
                parameters.push(ValType::I32);
            }
            let results = match import.mode {
                | HostCallMode::Returning => vec![ValType::I64],
                | HostCallMode::Control => vec![ValType::I64; 4],
            };
            types.ty().function(parameters, results);
            imports.import(HOST_MODULE, &import.name, EntityType::Function(next_type));
            next_type += 1;
        }
        (types, imports)
    }

    fn alloc_function(&self) -> Function {
        let mut function = Function::new([(2, ValType::I32)]);
        function.instruction(&WasmInstruction::GlobalGet(HEAP_POINTER_GLOBAL));
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
        function.instruction(&WasmInstruction::GlobalSet(HEAP_POINTER_GLOBAL));
        function.instruction(&WasmInstruction::LocalGet(1));
        function.instruction(&WasmInstruction::End);
        function
    }

    fn pair_function(&self) -> Function {
        let mut function = Function::new([(1, ValType::I32)]);
        function.instruction(&WasmInstruction::I32Const(2));
        function.instruction(&WasmInstruction::Call(self.plan.alloc_function()));
        function.instruction(&WasmInstruction::LocalTee(2));
        function.instruction(&WasmInstruction::LocalGet(0));
        function.instruction(&WasmInstruction::I64Store(WORD_MEMORY));
        function.instruction(&WasmInstruction::LocalGet(2));
        function.instruction(&WasmInstruction::LocalGet(1));
        function.instruction(&WasmInstruction::I64Store(Self::word_at_const(1)));
        function.instruction(&WasmInstruction::LocalGet(2));
        function.instruction(&WasmInstruction::I64ExtendI32U);
        function.instruction(&WasmInstruction::End);
        function
    }

    fn entry_function(&self) -> Function {
        let mut function = Function::new([]);
        function.instruction(&WasmInstruction::I32Const(self.plan.layout.heap_base as i32));
        function.instruction(&WasmInstruction::GlobalSet(HEAP_POINTER_GLOBAL));
        function.instruction(&WasmInstruction::I32Const(0));
        function.instruction(&WasmInstruction::GlobalSet(PROGRAM_COUNTER_GLOBAL));
        function.instruction(&WasmInstruction::I64Const(0));
        function.instruction(&WasmInstruction::GlobalSet(AMBIENT_STACK_GLOBAL));
        function.instruction(&WasmInstruction::Loop(BlockType::Empty));
        function.instruction(&WasmInstruction::GlobalGet(AMBIENT_STACK_GLOBAL));
        function.instruction(&WasmInstruction::GlobalGet(PROGRAM_COUNTER_GLOBAL));
        function
            .instruction(&WasmInstruction::CallIndirect { type_index: CASE_TYPE, table_index: 0 });
        function.instruction(&WasmInstruction::Br(0));
        function.instruction(&WasmInstruction::End);
        function.instruction(&WasmInstruction::End);
        function
    }

    fn name_section(&self) -> Result<NameSection, EmitError> {
        let mut names = NameSection::new();
        names.module("zydeco-sps");
        let mut functions = NameMap::new();
        if let Some(function) = self.plan.string_literal_function {
            functions.append(function, "zydeco.string_literal");
        }
        for import in &self.plan.host_imports {
            functions.append(import.function, &format!("zydeco.{}", import.name));
        }
        functions.append(self.plan.alloc_function(), "sps.alloc");
        functions.append(self.plan.pair_function(), "sps.pair");
        for case in &self.plan.cases {
            let name = case
                .label
                .map_or_else(|| "sps.root".to_owned(), |_| format!("sps.block.{}", case.index));
            functions.append(self.plan.case_function(case.index), &name);
        }
        functions.append(self.plan.entry_function()?, "entry");
        names.functions(&functions);
        Ok(names)
    }

    fn word_at_const(index: u32) -> MemArg {
        MemArg { offset: u64::from(index * WORD_BYTES), ..WORD_MEMORY }
    }
}

struct CaseEncoder<'a> {
    arena: &'a sps::SpsLowArena,
    plan: &'a ModulePlan,
    function: Function,
}

impl<'a> CaseEncoder<'a> {
    fn new(program: &'a SpsLowProgram, plan: &'a ModulePlan) -> Self {
        let function = Function::new([(plan.locals.local_count, ValType::I64)]);
        Self { arena: program.arena(), plan, function }
    }

    fn encode(mut self, body: CompuId) -> Result<Function, EmitError> {
        self.emit_compu(body)?;
        self.function.instruction(&WasmInstruction::End);
        Ok(self.function)
    }

    fn emit_compu(&mut self, mut id: CompuId) -> Result<(), EmitError> {
        loop {
            match self.arena.inner.compus[&id].clone() {
                | Computation::Hole(sps::SHole(stack)) => {
                    self.emit_stack(stack)?;
                    self.function.instruction(&WasmInstruction::Drop);
                    self.function.instruction(&WasmInstruction::Unreachable);
                    break;
                }
                | Computation::Jump(sps::Jump { target, stack }) => {
                    self.emit_stack(stack)?;
                    self.function.instruction(&WasmInstruction::GlobalSet(AMBIENT_STACK_GLOBAL));
                    self.emit_value(target)?;
                    self.set_program_counter_from_code();
                    self.function.instruction(&WasmInstruction::Return);
                    break;
                }
                | Computation::ProductMatch(sps::SProductMatch { scrut, binder, body }) => {
                    self.emit_value(scrut)?;
                    self.emit_pattern(binder)?;
                    id = body;
                }
                | Computation::CoprodMatch(sps::SCoprodMatch { scrut, arms }) => {
                    self.emit_coprod_match(scrut, arms)?;
                    break;
                }
                | Computation::LetValue(sps::LetValue { binder, bindee, body }) => {
                    self.emit_value(bindee)?;
                    self.emit_pattern(binder)?;
                    id = body;
                }
                | Computation::LetStack(sps::LetStack { bindee, body }) => {
                    self.emit_stack(bindee)?;
                    self.function.instruction(&WasmInstruction::LocalSet(0));
                    id = body;
                }
                | Computation::LetArg(sps::LetArg { binder, bindee, body }) => {
                    self.emit_stack(bindee)?;
                    self.function
                        .instruction(&WasmInstruction::LocalSet(self.plan.locals.scratch_stack));
                    self.load_local_word(self.plan.locals.scratch_stack, 0);
                    self.emit_pattern(binder)?;
                    self.load_local_word(self.plan.locals.scratch_stack, 1);
                    self.function.instruction(&WasmInstruction::LocalSet(0));
                    id = body;
                }
                | Computation::CoCase(sps::SCoMatch { scrut, arms }) => {
                    self.emit_stack(scrut)?;
                    self.function
                        .instruction(&WasmInstruction::LocalSet(self.plan.locals.scratch_stack));
                    self.load_local_word(self.plan.locals.scratch_stack, 0);
                    self.function
                        .instruction(&WasmInstruction::LocalSet(self.plan.locals.scratch_word));
                    self.load_local_word(self.plan.locals.scratch_stack, 1);
                    self.function.instruction(&WasmInstruction::LocalSet(0));
                    for sps::CoMatcher { dtor: sps::Cons(dtor, sps::Bullet), tail } in arms {
                        self.function
                            .instruction(&WasmInstruction::LocalGet(self.plan.locals.scratch_word));
                        self.function
                            .instruction(&WasmInstruction::I64Const(RuntimeWord::index(dtor.idx)?));
                        self.function.instruction(&WasmInstruction::I64Eq);
                        self.function.instruction(&WasmInstruction::If(BlockType::Empty));
                        self.emit_compu(tail)?;
                        self.function.instruction(&WasmInstruction::End);
                    }
                    self.function.instruction(&WasmInstruction::Unreachable);
                    break;
                }
                | Computation::OpenClosure(sps::OpenClosure {
                    package,
                    environment,
                    code,
                    body,
                }) => {
                    self.emit_value(package)?;
                    self.function
                        .instruction(&WasmInstruction::LocalSet(self.plan.locals.scratch_word));
                    self.load_local_word(self.plan.locals.scratch_word, 0);
                    self.emit_pattern(environment)?;
                    self.load_local_word(self.plan.locals.scratch_word, 1);
                    self.emit_pattern(code)?;
                    id = body;
                }
                | Computation::OpenContinuation(sps::OpenContinuation { package, code, body }) => {
                    self.emit_stack(package)?;
                    self.function
                        .instruction(&WasmInstruction::LocalSet(self.plan.locals.scratch_stack));
                    self.load_local_word(self.plan.locals.scratch_stack, 0);
                    self.emit_pattern(code)?;
                    self.load_local_word(self.plan.locals.scratch_stack, 1);
                    self.function.instruction(&WasmInstruction::LocalSet(0));
                    id = body;
                }
                | Computation::ExternCall(sps::ExternCall { function, stack }) => {
                    self.emit_extern(&function, stack)?;
                    break;
                }
            }
        }
        Ok(())
    }

    fn emit_coprod_match(
        &mut self, scrut: ValueId, arms: Vec<sps::Matcher<VPatId, CompuId>>,
    ) -> Result<(), EmitError> {
        self.emit_value(scrut)?;
        self.function.instruction(&WasmInstruction::Drop);
        let scrut_local = self.plan.locals.value(scrut)?;
        let constructors = arms
            .iter()
            .all(|arm| matches!(self.arena.inner.vpats[&arm.binder], ValuePattern::Ctor(_)));
        if constructors {
            for sps::Matcher { binder, tail } in arms {
                let ValuePattern::Ctor(sps::Ctor(ctor, body)) =
                    self.arena.inner.vpats[&binder].clone()
                else {
                    unreachable!("constructor-arm check changed during emission")
                };
                self.load_local_word(scrut_local, 0);
                self.function
                    .instruction(&WasmInstruction::I64Const(RuntimeWord::index(ctor.idx)?));
                self.function.instruction(&WasmInstruction::I64Eq);
                self.function.instruction(&WasmInstruction::If(BlockType::Empty));
                self.load_local_word(scrut_local, 1);
                self.emit_pattern(body)?;
                self.emit_compu(tail)?;
                self.function.instruction(&WasmInstruction::End);
            }
            self.function.instruction(&WasmInstruction::Unreachable);
        } else {
            let [sps::Matcher { binder, tail }] = arms.as_slice() else {
                return Err(EmitError::InvalidCoprodMatch);
            };
            self.function.instruction(&WasmInstruction::LocalGet(scrut_local));
            self.emit_pattern(*binder)?;
            self.emit_compu(*tail)?;
        }
        Ok(())
    }

    fn emit_value(&mut self, id: ValueId) -> Result<(), EmitError> {
        let target = self.plan.locals.value(id)?;
        match self.arena.inner.values[&id].clone() {
            | sps::Value::Hole(_) => {
                self.function.instruction(&WasmInstruction::Unreachable);
            }
            | sps::Value::Var(def) => {
                if let Some(index) = self.plan.labels.get(&def) {
                    self.function
                        .instruction(&WasmInstruction::I64Const(RuntimeWord::code(*index)));
                } else {
                    self.function
                        .instruction(&WasmInstruction::LocalGet(self.plan.locals.variable(def)?));
                }
            }
            | sps::Value::Block(Block { label, body: _ }) => {
                self.function.instruction(&WasmInstruction::I64Const(RuntimeWord::code(
                    self.plan.label(label)?,
                )));
            }
            | sps::Value::ClosurePackage(sps::ClosurePackage { environment, code }) => {
                self.emit_value(environment)?;
                self.emit_value(code)?;
                self.function.instruction(&WasmInstruction::Call(self.plan.pair_function()));
            }
            | sps::Value::Ctor(sps::Ctor(ctor, body)) => {
                self.function
                    .instruction(&WasmInstruction::I64Const(RuntimeWord::index(ctor.idx)?));
                self.emit_value(body)?;
                self.function.instruction(&WasmInstruction::Call(self.plan.pair_function()));
            }
            | sps::Value::Triv(_) => {
                self.function.instruction(&WasmInstruction::I64Const(RuntimeWord::index(0)?));
            }
            | sps::Value::VCons(sps::VCons { items, layout }) => {
                self.emit_product(target, items, layout)?;
            }
            | sps::Value::Literal(literal) => self.emit_literal(id, literal)?,
            | sps::Value::Complex(sps::Complex { operator, operands }) => {
                self.emit_intrinsic(&operator, &operands)?;
            }
        }
        self.function.instruction(&WasmInstruction::LocalTee(target));
        Ok(())
    }

    fn emit_product(
        &mut self, target: u32, items: Vec<ValueId>, layout: ProductLayout,
    ) -> Result<(), EmitError> {
        let arity = Limits::u32(layout.arity, "product arity")?;
        self.function.instruction(&WasmInstruction::I32Const(arity as i32));
        self.function.instruction(&WasmInstruction::Call(self.plan.alloc_function()));
        self.function.instruction(&WasmInstruction::I64ExtendI32U);
        self.function.instruction(&WasmInstruction::LocalSet(target));

        let explicit = items.len();
        for (index, value) in items.into_iter().enumerate() {
            if index + 1 == explicit && explicit < layout.arity {
                self.emit_value(value)?;
                self.function
                    .instruction(&WasmInstruction::LocalSet(self.plan.locals.scratch_word));
                for destination in index..layout.arity {
                    self.function.instruction(&WasmInstruction::LocalGet(target));
                    self.function.instruction(&WasmInstruction::I32WrapI64);
                    self.load_local_word(self.plan.locals.scratch_word, destination - index);
                    self.function
                        .instruction(&WasmInstruction::I64Store(Self::word_at(destination)?));
                }
            } else {
                self.function.instruction(&WasmInstruction::LocalGet(target));
                self.function.instruction(&WasmInstruction::I32WrapI64);
                self.emit_value(value)?;
                self.function.instruction(&WasmInstruction::I64Store(Self::word_at(index)?));
            }
        }
        self.function.instruction(&WasmInstruction::LocalGet(target));
        Ok(())
    }

    fn emit_literal(&mut self, id: ValueId, literal: Literal) -> Result<(), EmitError> {
        match literal {
            | Literal::Integer(integer) => match RuntimeWord::integer(integer)? {
                | EncodedScalar::Immediate(word) => {
                    self.function.instruction(&WasmInstruction::I64Const(word as i64));
                }
                | EncodedScalar::Boxed(bits) => self.emit_boxed(bits),
            },
            | Literal::Float(float) => match RuntimeWord::float(float) {
                | EncodedScalar::Immediate(word) => {
                    self.function.instruction(&WasmInstruction::I64Const(word as i64));
                }
                | EncodedScalar::Boxed(bits) => self.emit_boxed(bits),
            },
            | Literal::String(_) => {
                let string = self.plan.string(id)?;
                let function =
                    self.plan.string_literal_function.ok_or(EmitError::MissingString(id))?;
                self.function.instruction(&WasmInstruction::I32Const(string.offset as i32));
                self.function.instruction(&WasmInstruction::I32Const(string.length as i32));
                self.function.instruction(&WasmInstruction::Call(function));
            }
            | Literal::Char(character) => {
                self.function.instruction(&WasmInstruction::I64Const(RuntimeWord::index(
                    character as usize,
                )?));
            }
        }
        Ok(())
    }

    fn emit_boxed(&mut self, bits: u64) {
        self.function.instruction(&WasmInstruction::I32Const(1));
        self.function.instruction(&WasmInstruction::Call(self.plan.alloc_function()));
        self.function.instruction(&WasmInstruction::I64ExtendI32U);
        self.function.instruction(&WasmInstruction::LocalTee(self.plan.locals.scratch_word));
        self.function.instruction(&WasmInstruction::I32WrapI64);
        self.function.instruction(&WasmInstruction::I64Const(bits as i64));
        self.function.instruction(&WasmInstruction::I64Store(WORD_MEMORY));
        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.scratch_word));
    }

    fn emit_stack(&mut self, id: StackId) -> Result<(), EmitError> {
        let target = self.plan.locals.stack(id)?;
        match self.arena.inner.stacks[&id].clone() {
            | sps::Stack::Var(sps::Bullet) => {
                self.function.instruction(&WasmInstruction::LocalGet(0));
            }
            | sps::Stack::Arg(sps::Cons(value, stack)) => {
                self.emit_value(value)?;
                self.emit_stack(stack)?;
                self.function.instruction(&WasmInstruction::Call(self.plan.pair_function()));
            }
            | sps::Stack::Tag(sps::Cons(dtor, stack)) => {
                self.function
                    .instruction(&WasmInstruction::I64Const(RuntimeWord::index(dtor.idx)?));
                self.emit_stack(stack)?;
                self.function.instruction(&WasmInstruction::Call(self.plan.pair_function()));
            }
            | sps::Stack::ContinuationPackage(sps::ContinuationPackage { code, residual }) => {
                self.emit_value(code)?;
                self.emit_stack(residual)?;
                self.function.instruction(&WasmInstruction::Call(self.plan.pair_function()));
            }
        }
        self.function.instruction(&WasmInstruction::LocalTee(target));
        Ok(())
    }

    fn emit_pattern(&mut self, id: VPatId) -> Result<(), EmitError> {
        let bindee = self.plan.locals.pattern(id)?;
        self.function.instruction(&WasmInstruction::LocalSet(bindee));
        match self.arena.inner.vpats[&id].clone() {
            | ValuePattern::Hole(_) | ValuePattern::Triv(_) => {}
            | ValuePattern::Var(def) => {
                self.function.instruction(&WasmInstruction::LocalGet(bindee));
                self.function
                    .instruction(&WasmInstruction::LocalSet(self.plan.locals.variable(def)?));
            }
            | ValuePattern::Ctor(sps::Ctor(ctor, body)) => {
                self.load_local_word(bindee, 0);
                self.function
                    .instruction(&WasmInstruction::I64Const(RuntimeWord::index(ctor.idx)?));
                self.function.instruction(&WasmInstruction::I64Ne);
                self.function.instruction(&WasmInstruction::If(BlockType::Empty));
                self.function.instruction(&WasmInstruction::Unreachable);
                self.function.instruction(&WasmInstruction::End);
                self.load_local_word(bindee, 1);
                self.emit_pattern(body)?;
            }
            | ValuePattern::Alias(sps::Alias(patterns)) => {
                for pattern in patterns {
                    self.function.instruction(&WasmInstruction::LocalGet(bindee));
                    self.emit_pattern(pattern)?;
                }
            }
            | ValuePattern::VCons(sps::VCons { items, layout }) => {
                let explicit = items.len();
                for (index, pattern) in items.into_iter().enumerate() {
                    if index + 1 == explicit && explicit < layout.arity {
                        self.function.instruction(&WasmInstruction::LocalGet(bindee));
                        self.function.instruction(&WasmInstruction::I64Const(i64::from(
                            Self::byte_offset(index)?,
                        )));
                        self.function.instruction(&WasmInstruction::I64Add);
                    } else {
                        self.load_local_word(bindee, index);
                    }
                    self.emit_pattern(pattern)?;
                }
            }
        }
        Ok(())
    }

    fn emit_extern(&mut self, name: &str, stack: StackId) -> Result<(), EmitError> {
        let import = self.plan.host(name)?.clone();
        if !matches!(self.arena.admin.builtins[name].sort, sps::BuiltinSort::Function(_)) {
            return Err(EmitError::OperatorCalledAsFunction(name.to_owned()));
        }
        self.emit_stack(stack)?;
        self.function.instruction(&WasmInstruction::LocalSet(self.plan.locals.scratch_stack));
        for _ in 0..import.arity {
            self.load_local_word(self.plan.locals.scratch_stack, 0);
            self.load_local_word(self.plan.locals.scratch_stack, 1);
            self.function.instruction(&WasmInstruction::LocalSet(self.plan.locals.scratch_stack));
        }
        if let Some(spare) = import.spare {
            match spare {
                | SpareBox::Opaque => {
                    self.function.instruction(&WasmInstruction::I32Const(1));
                    self.function.instruction(&WasmInstruction::Call(self.plan.alloc_function()));
                }
                | SpareBox::Unused => {
                    self.function.instruction(&WasmInstruction::I32Const(0));
                }
            }
        }
        self.function.instruction(&WasmInstruction::Call(import.function));
        match import.mode {
            | HostCallMode::Returning => self.resume_returning_call(),
            | HostCallMode::Control => self.resume_control_call(),
        }
        Ok(())
    }

    fn resume_returning_call(&mut self) {
        self.function.instruction(&WasmInstruction::LocalSet(self.plan.locals.result));
        self.load_local_word(self.plan.locals.scratch_stack, 0);
        self.set_program_counter_from_code();
        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.result));
        self.load_local_word(self.plan.locals.scratch_stack, 1);
        self.function.instruction(&WasmInstruction::Call(self.plan.pair_function()));
        self.function.instruction(&WasmInstruction::GlobalSet(AMBIENT_STACK_GLOBAL));
        self.function.instruction(&WasmInstruction::Return);
    }

    fn resume_control_call(&mut self) {
        self.function.instruction(&WasmInstruction::LocalSet(self.plan.locals.transfer_second));
        self.function.instruction(&WasmInstruction::LocalSet(self.plan.locals.transfer_first));
        self.function.instruction(&WasmInstruction::LocalSet(self.plan.locals.transfer_closure));
        self.function.instruction(&WasmInstruction::LocalSet(self.plan.locals.transfer_count));

        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.transfer_count));
        self.function.instruction(&WasmInstruction::I64Const(2));
        self.function.instruction(&WasmInstruction::I64GtU);
        self.function.instruction(&WasmInstruction::If(BlockType::Empty));
        self.function.instruction(&WasmInstruction::Unreachable);
        self.function.instruction(&WasmInstruction::End);

        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.transfer_count));
        self.function.instruction(&WasmInstruction::I64Const(2));
        self.function.instruction(&WasmInstruction::I64Eq);
        self.function.instruction(&WasmInstruction::If(BlockType::Empty));
        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.transfer_second));
        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.scratch_stack));
        self.function.instruction(&WasmInstruction::Call(self.plan.pair_function()));
        self.function.instruction(&WasmInstruction::LocalSet(self.plan.locals.scratch_stack));
        self.function.instruction(&WasmInstruction::End);

        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.transfer_count));
        self.function.instruction(&WasmInstruction::I64Eqz);
        self.function.instruction(&WasmInstruction::I32Eqz);
        self.function.instruction(&WasmInstruction::If(BlockType::Empty));
        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.transfer_first));
        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.scratch_stack));
        self.function.instruction(&WasmInstruction::Call(self.plan.pair_function()));
        self.function.instruction(&WasmInstruction::LocalSet(self.plan.locals.scratch_stack));
        self.function.instruction(&WasmInstruction::End);

        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.transfer_closure));
        self.function.instruction(&WasmInstruction::LocalSet(self.plan.locals.scratch_word));
        self.load_local_word(self.plan.locals.scratch_word, 0);
        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.scratch_stack));
        self.function.instruction(&WasmInstruction::Call(self.plan.pair_function()));
        self.function.instruction(&WasmInstruction::GlobalSet(AMBIENT_STACK_GLOBAL));
        self.load_local_word(self.plan.locals.scratch_word, 1);
        self.set_program_counter_from_code();
        self.function.instruction(&WasmInstruction::Return);
    }

    fn emit_intrinsic(&mut self, name: &str, operands: &[ValueId]) -> Result<(), EmitError> {
        if operands.len() != 2 {
            return Err(EmitError::UnsupportedIntrinsic {
                name: name.to_owned(),
                arity: operands.len(),
            });
        }
        self.emit_value(operands[0])?;
        self.function.instruction(&WasmInstruction::Drop);
        self.emit_value(operands[1])?;
        self.function.instruction(&WasmInstruction::Drop);
        self.decode_signed_local(
            self.plan.locals.value(operands[0])?,
            self.plan.locals.decoded_first,
        );
        self.decode_signed_local(
            self.plan.locals.value(operands[1])?,
            self.plan.locals.decoded_second,
        );

        match name {
            | "int_eq" | "int_lt" | "int_gt" => {
                self.function
                    .instruction(&WasmInstruction::LocalGet(self.plan.locals.decoded_first));
                self.function
                    .instruction(&WasmInstruction::LocalGet(self.plan.locals.decoded_second));
                self.function.instruction(&match name {
                    | "int_eq" => WasmInstruction::I64Eq,
                    | "int_lt" => WasmInstruction::I64LtS,
                    | "int_gt" => WasmInstruction::I64GtS,
                    | _ => unreachable!(),
                });
                self.function.instruction(&WasmInstruction::I64ExtendI32U);
                self.function.instruction(&WasmInstruction::I64Const(1));
                self.function.instruction(&WasmInstruction::I64Shl);
                self.function.instruction(&WasmInstruction::I64Const(1));
                self.function.instruction(&WasmInstruction::I64Or);
                self.function.instruction(&WasmInstruction::LocalSet(self.plan.locals.result));
                // Intrinsic comparisons return the runtime pair `(encoded_bool, ())`.
                self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.result));
                self.function.instruction(&WasmInstruction::I64Const(RuntimeWord::index(0)?));
                self.function.instruction(&WasmInstruction::Call(self.plan.pair_function()));
            }
            | "add" | "sub" | "mul" | "and" | "or" | "xor" => {
                self.function
                    .instruction(&WasmInstruction::LocalGet(self.plan.locals.decoded_first));
                self.function
                    .instruction(&WasmInstruction::LocalGet(self.plan.locals.decoded_second));
                self.function.instruction(&match name {
                    | "add" => WasmInstruction::I64Add,
                    | "sub" => WasmInstruction::I64Sub,
                    | "mul" => WasmInstruction::I64Mul,
                    | "and" => WasmInstruction::I64And,
                    | "or" => WasmInstruction::I64Or,
                    | "xor" => WasmInstruction::I64Xor,
                    | _ => unreachable!(),
                });
                self.function.instruction(&WasmInstruction::LocalSet(self.plan.locals.result));
                self.encode_signed_local(self.plan.locals.result);
            }
            | "div" | "mod" => {
                self.emit_wrapping_division(name == "mod");
                self.encode_signed_local(self.plan.locals.result);
            }
            | _ => {
                return Err(EmitError::UnsupportedIntrinsic {
                    name: name.to_owned(),
                    arity: operands.len(),
                });
            }
        }
        Ok(())
    }

    fn decode_signed_local(&mut self, input: u32, output: u32) {
        self.function.instruction(&WasmInstruction::LocalGet(input));
        self.function.instruction(&WasmInstruction::I64Const(1));
        self.function.instruction(&WasmInstruction::I64And);
        self.function.instruction(&WasmInstruction::I64Eqz);
        self.function.instruction(&WasmInstruction::If(BlockType::Result(ValType::I64)));
        self.function.instruction(&WasmInstruction::LocalGet(input));
        self.function.instruction(&WasmInstruction::I32WrapI64);
        self.function.instruction(&WasmInstruction::I64Load(WORD_MEMORY));
        self.function.instruction(&WasmInstruction::Else);
        self.function.instruction(&WasmInstruction::LocalGet(input));
        self.function.instruction(&WasmInstruction::I64Const(1));
        self.function.instruction(&WasmInstruction::I64ShrS);
        self.function.instruction(&WasmInstruction::End);
        self.function.instruction(&WasmInstruction::LocalSet(output));
    }

    fn encode_signed_local(&mut self, input: u32) {
        self.function.instruction(&WasmInstruction::LocalGet(input));
        self.function.instruction(&WasmInstruction::I64Const(RuntimeWord::SIGNED_MIN));
        self.function.instruction(&WasmInstruction::I64GeS);
        self.function.instruction(&WasmInstruction::LocalGet(input));
        self.function.instruction(&WasmInstruction::I64Const(RuntimeWord::SIGNED_MAX));
        self.function.instruction(&WasmInstruction::I64LeS);
        self.function.instruction(&WasmInstruction::I32And);
        self.function.instruction(&WasmInstruction::If(BlockType::Result(ValType::I64)));
        self.function.instruction(&WasmInstruction::LocalGet(input));
        self.function.instruction(&WasmInstruction::I64Const(1));
        self.function.instruction(&WasmInstruction::I64Shl);
        self.function.instruction(&WasmInstruction::I64Const(1));
        self.function.instruction(&WasmInstruction::I64Or);
        self.function.instruction(&WasmInstruction::Else);
        self.function.instruction(&WasmInstruction::I32Const(1));
        self.function.instruction(&WasmInstruction::Call(self.plan.alloc_function()));
        self.function.instruction(&WasmInstruction::I64ExtendI32U);
        self.function.instruction(&WasmInstruction::LocalTee(self.plan.locals.scratch_word));
        self.function.instruction(&WasmInstruction::I32WrapI64);
        self.function.instruction(&WasmInstruction::LocalGet(input));
        self.function.instruction(&WasmInstruction::I64Store(WORD_MEMORY));
        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.scratch_word));
        self.function.instruction(&WasmInstruction::End);
    }

    fn emit_wrapping_division(&mut self, remainder: bool) {
        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.decoded_second));
        self.function.instruction(&WasmInstruction::I64Eqz);
        self.function.instruction(&WasmInstruction::If(BlockType::Empty));
        self.function.instruction(&WasmInstruction::Unreachable);
        self.function.instruction(&WasmInstruction::End);
        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.decoded_first));
        self.function.instruction(&WasmInstruction::I64Const(i64::MIN));
        self.function.instruction(&WasmInstruction::I64Eq);
        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.decoded_second));
        self.function.instruction(&WasmInstruction::I64Const(-1));
        self.function.instruction(&WasmInstruction::I64Eq);
        self.function.instruction(&WasmInstruction::I32And);
        self.function.instruction(&WasmInstruction::If(BlockType::Result(ValType::I64)));
        self.function.instruction(&WasmInstruction::I64Const(if remainder { 0 } else { i64::MIN }));
        self.function.instruction(&WasmInstruction::Else);
        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.decoded_first));
        self.function.instruction(&WasmInstruction::LocalGet(self.plan.locals.decoded_second));
        self.function.instruction(&if remainder {
            WasmInstruction::I64RemS
        } else {
            WasmInstruction::I64DivS
        });
        self.function.instruction(&WasmInstruction::End);
        self.function.instruction(&WasmInstruction::LocalSet(self.plan.locals.result));
    }

    fn load_local_word(&mut self, local: u32, index: usize) {
        self.function.instruction(&WasmInstruction::LocalGet(local));
        self.function.instruction(&WasmInstruction::I32WrapI64);
        self.function.instruction(&WasmInstruction::I64Load(Self::word_at_const(index as u32)));
    }

    fn set_program_counter_from_code(&mut self) {
        self.function.instruction(&WasmInstruction::I64Const(1));
        self.function.instruction(&WasmInstruction::I64ShrU);
        self.function.instruction(&WasmInstruction::I32WrapI64);
        self.function.instruction(&WasmInstruction::GlobalSet(PROGRAM_COUNTER_GLOBAL));
    }

    fn byte_offset(index: usize) -> Result<u32, EmitError> {
        Limits::u32(index, "product field index")?
            .checked_mul(WORD_BYTES)
            .ok_or(EmitError::Limit { what: "product field offset", value: index })
    }

    fn word_at(index: usize) -> Result<MemArg, EmitError> {
        Ok(MemArg { offset: u64::from(Self::byte_offset(index)?), ..WORD_MEMORY })
    }

    fn word_at_const(index: u32) -> MemArg {
        MemArg { offset: u64::from(index * WORD_BYTES), ..WORD_MEMORY }
    }
}

enum EncodedScalar {
    Immediate(u64),
    Boxed(u64),
}

struct RuntimeWord;

impl RuntimeWord {
    const SIGNED_MIN: i64 = -(1_i64 << 62);
    const SIGNED_MAX: i64 = (1_i64 << 62) - 1;
    const UNSIGNED_MAX: u64 = u64::MAX >> 1;

    fn unsigned(value: u64) -> Option<u64> {
        (value <= Self::UNSIGNED_MAX).then_some((value << 1) | 1)
    }

    fn signed(value: i64) -> Option<u64> {
        (Self::SIGNED_MIN..=Self::SIGNED_MAX).contains(&value).then_some(((value as u64) << 1) | 1)
    }

    fn integer(value: IntegerLiteral) -> Result<EncodedScalar, EmitError> {
        use IntegerLiteral::*;
        let immediate = match value {
            | Int8(value) => Self::signed(value.into()),
            | Int16(value) => Self::signed(value.into()),
            | Int32(value) => Self::signed(value.into()),
            | Int64(value) => Self::signed(value),
            | UInt8(value) => Self::unsigned(value.into()),
            | UInt16(value) => Self::unsigned(value.into()),
            | UInt32(value) => Self::unsigned(value.into()),
            | UInt64(value) => Self::unsigned(value),
            | Unresolved(_) => return Err(EmitError::UnresolvedInteger),
        };
        Ok(immediate
            .map_or_else(|| EncodedScalar::Boxed(value.to_word_bits()), EncodedScalar::Immediate))
    }

    fn float(value: zydeco_syntax::FloatLiteral) -> EncodedScalar {
        match value {
            | zydeco_syntax::FloatLiteral::Float32(bits) => EncodedScalar::Immediate(
                Self::unsigned(bits.into()).expect("Float32 payload fits an immediate"),
            ),
            | zydeco_syntax::FloatLiteral::Float64(bits) => EncodedScalar::Boxed(bits),
        }
    }

    fn code(index: u32) -> i64 {
        (i64::from(index) << 1) | 1
    }

    fn index(value: usize) -> Result<i64, EmitError> {
        let value = u64::try_from(value)
            .map_err(|_| EmitError::Limit { what: "runtime tag index", value })?;
        Self::unsigned(value)
            .map(|word| word as i64)
            .ok_or(EmitError::Limit { what: "runtime tag index", value: value as usize })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasmparser::{Parser, Payload, Validator};
    use zydeco_stackir::sps_low::{SpsLowArena, arena::Construct as _};

    struct Fixture {
        arena: SpsLowArena,
    }

    impl Fixture {
        fn new() -> Self {
            Self { arena: SpsLowArena::default() }
        }

        fn hole(&mut self) -> CompuId {
            let stack = sps::Bullet.build(&mut self.arena, None);
            sps::SHole(stack).build(&mut self.arena, None)
        }

        fn closed_let_chain(mut self, bindings: usize) -> SpsLowProgram {
            let mut body = self.hole();
            for _ in 0..bindings {
                let bindee = sps::Triv.build(&mut self.arena, None);
                let binder = sps::Hole.build(&mut self.arena, None);
                body = sps::LetValue { binder, bindee, body }.build(&mut self.arena, None);
            }
            SpsLowProgram::try_new(self.arena, body).unwrap()
        }
    }

    #[test]
    fn structured_module_validates() {
        let program = Fixture::new().closed_let_chain(4);
        let module = Emitter::new(&program).run().unwrap();
        Validator::new().validate_all(module.as_bytes()).unwrap();
    }

    #[test]
    fn boxed_literal_module_validates() {
        let mut fixture = Fixture::new();
        let body = fixture.hole();
        let bindee =
            Literal::Integer(IntegerLiteral::Int64(i64::MAX)).build(&mut fixture.arena, None);
        let binder = sps::Hole.build(&mut fixture.arena, None);
        let root = sps::LetValue { binder, bindee, body }.build(&mut fixture.arena, None);
        let program = SpsLowProgram::try_new(fixture.arena, root).unwrap();
        let module = Emitter::new(&program).run().unwrap();

        Validator::new().validate_all(module.as_bytes()).unwrap();
    }

    #[test]
    fn lexical_computations_do_not_become_wasm_functions() {
        let program = Fixture::new().closed_let_chain(16);
        let module = Emitter::new(&program).run().unwrap();
        let bodies = Parser::new(0)
            .parse_all(module.as_bytes())
            .filter(|payload| matches!(payload, Ok(Payload::CodeSectionEntry(_))))
            .count();

        // alloc, pair, one root case, and entry. The sixteen LetValue nodes are
        // structured instructions inside the root case rather than functions.
        assert_eq!(bodies, 4);
    }

    #[test]
    fn emission_is_deterministic() {
        let program = Fixture::new().closed_let_chain(8);
        let first = Emitter::new(&program).run().unwrap();
        let second = Emitter::new(&program).run().unwrap();

        assert_eq!(first, second);
    }

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
