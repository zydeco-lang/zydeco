use std::{borrow::Cow, collections::HashMap};

use thiserror::Error;
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, DataSection, ElementSection, Elements, EntityType,
    ExportKind, ExportSection, Function, FunctionSection, GlobalSection, GlobalType, ImportSection,
    Instruction as WasmInstruction, MemArg, MemorySection, MemoryType, Module, NameMap,
    NameSection, RefType, TableSection, TableType, TypeSection, ValType,
};
use zydeco_assembly::{
    arena::{AssemblyArena, AssemblyArenaRefLike, AssemblyProgram},
    syntax::{
        self as zasm, Atom, ExternMode, Imm, Instruction, ProgId, Program, SymId, Symbol,
        Terminator, VarId,
    },
};
use zydeco_syntax::{
    BuiltinValueRole, FloatOperation, FloatType, IntegerLiteral, IntegerOperation, IntegerType,
};

/// Import namespace used by generated modules.
///
/// Returning builtins accept their ZASM arguments as `i64` values and return
/// one `i64`. Control builtins return four `i64` values: argument count,
/// closure, first argument, and second argument. Operations that can produce a
/// boxed 64-bit scalar receive one trailing `i32` spare-box address.
pub const HOST_MODULE: &str = "zydeco";

const CASE_TYPE: u32 = 0;
const PUSH_TYPE: u32 = 1;
const POP_TYPE: u32 = 2;
const ALLOC_TYPE: u32 = 3;
const FIRST_IMPORT_TYPE: u32 = 4;

const STACK_BYTES: u32 = 1024 * 1024;
const WASM_PAGE_BYTES: u32 = 64 * 1024;
const WORD_BYTES: u32 = 8;

const STACK_POINTER_GLOBAL: u32 = 0;
const HEAP_POINTER_GLOBAL: u32 = 1;
const PROGRAM_COUNTER_GLOBAL: u32 = 2;

const POINTER_LOCAL: u32 = 0;
const WORD_LOCAL: u32 = 2;
const TAG_LOCAL: u32 = WORD_LOCAL;
const FIRST_ARGUMENT_LOCAL: u32 = 3;
const RESULT_LOCAL: u32 = 7;
const TRANSFER_COUNT_LOCAL: u32 = 8;
const TRANSFER_CLOSURE_LOCAL: u32 = 9;
const TRANSFER_FIRST_LOCAL: u32 = 10;
const TRANSFER_SECOND_LOCAL: u32 = 11;
const DECODED_FIRST_LOCAL: u32 = 12;
const DECODED_SECOND_LOCAL: u32 = 13;

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

/// ZASM abstract-machine WebAssembly emitter.
pub struct Emitter<'a> {
    assembly: &'a AssemblyProgram,
}

impl<'a> Emitter<'a> {
    pub fn new(assembly: &'a AssemblyProgram) -> Self {
        Self { assembly }
    }

    pub fn run(self) -> Result<WasmModule, EmitError> {
        let plan = ModulePlan::new(self.assembly.arena(), self.assembly.root())?;
        ModuleEncoder::new(self.assembly.arena(), plan).encode()
    }
}

#[derive(Debug, Error)]
pub enum EmitError {
    #[error("WebAssembly abstract-machine backend received an empty ZASM program")]
    EmptyProgram,
    #[error("WebAssembly abstract-machine backend cannot find program {0:?}")]
    MissingProgram(ProgId),
    #[error("WebAssembly abstract-machine backend cannot find environment slot for variable {0:?}")]
    MissingVariable(VarId),
    #[error("WebAssembly abstract-machine backend cannot find static string data for symbol {0:?}")]
    MissingString(SymId),
    #[error("WebAssembly abstract-machine backend cannot find host import `{0}`")]
    MissingHostImport(String),
    #[error("duplicate ZASM extern `{0}`")]
    DuplicateExtern(String),
    #[error("unresolved integer literal reached WebAssembly emission")]
    UnresolvedInteger,
    #[error(
        "unsupported ZASM intrinsic `{name}/{arity}` in the WebAssembly abstract-machine backend"
    )]
    UnsupportedIntrinsic { name: String, arity: usize },
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
    mode: ExternMode,
    spare: Option<SpareBox>,
}

#[derive(Clone, Copy)]
struct StaticString {
    offset: u32,
    length: u32,
}

struct MemoryLayout {
    environment_base: u32,
    stack_base: u32,
    heap_base: u32,
    initial_pages: u64,
}

impl MemoryLayout {
    fn new(static_bytes: usize, variable_count: usize) -> Result<Self, EmitError> {
        let static_bytes = Limits::u32(static_bytes, "static data size")?;
        let variable_count = Limits::u32(variable_count, "environment slot count")?;
        let environment_base = Limits::align(static_bytes.saturating_add(WORD_BYTES), WORD_BYTES)?;
        let environment_bytes = variable_count.checked_mul(WORD_BYTES).ok_or(EmitError::Limit {
            what: "environment byte size",
            value: variable_count as usize,
        })?;
        let stack_base = Limits::align(
            environment_base
                .checked_add(environment_bytes)
                .and_then(|end| end.checked_add(WORD_BYTES))
                .ok_or(EmitError::Limit {
                    what: "environment end",
                    value: environment_bytes as usize,
                })?,
            WORD_BYTES,
        )?;
        let heap_base = stack_base
            .checked_add(STACK_BYTES)
            .ok_or(EmitError::Limit { what: "control stack end", value: stack_base as usize })?;
        let initial_end = heap_base
            .checked_add(WASM_PAGE_BYTES)
            .ok_or(EmitError::Limit { what: "initial memory size", value: heap_base as usize })?;
        let initial_pages = u64::from(initial_end.div_ceil(WASM_PAGE_BYTES));
        Ok(Self { environment_base, stack_base, heap_base, initial_pages })
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

struct ModulePlan {
    programs: Vec<ProgId>,
    program_indices: HashMap<ProgId, u32>,
    variable_addresses: HashMap<VarId, u32>,
    strings: HashMap<SymId, StaticString>,
    static_data: Vec<u8>,
    string_literal_function: Option<u32>,
    host_imports: Vec<HostImport>,
    host_functions: HashMap<String, u32>,
    import_count: u32,
    layout: MemoryLayout,
    root: u32,
}

impl ModulePlan {
    fn new(assembly: &AssemblyArena, root: ProgId) -> Result<Self, EmitError> {
        let mut programs =
            assembly.programs.iter().map(|(program, _)| *program).collect::<Vec<_>>();
        programs.sort_unstable();
        if programs.is_empty() {
            return Err(EmitError::EmptyProgram);
        }
        let program_indices = programs
            .iter()
            .copied()
            .enumerate()
            .map(|(index, program)| {
                Limits::u32(index, "program count").map(|index| (program, index))
            })
            .collect::<Result<HashMap<_, _>, _>>()?;
        let root = program_indices.get(&root).copied().ok_or(EmitError::MissingProgram(root))?;

        let mut variables =
            assembly.variables.iter().map(|(variable, _)| *variable).collect::<Vec<_>>();
        variables.sort_unstable();

        let mut string_symbols = assembly
            .symbols
            .iter()
            .filter_map(|(symbol, named)| {
                matches!(named.inner, Symbol::StringLiteral(_)).then_some(*symbol)
            })
            .collect::<Vec<_>>();
        string_symbols.sort_unstable();
        let mut static_data = vec![0; WORD_BYTES as usize];
        let mut strings = HashMap::new();
        for symbol in string_symbols {
            let Symbol::StringLiteral(string) = &assembly.symbols[&symbol].inner else {
                unreachable!("string symbol collection changed during planning")
            };
            let offset = Limits::u32(static_data.len(), "static string offset")?;
            let length = Limits::u32(string.byte_len(), "static string length")?;
            static_data.extend_from_slice(string.as_bytes());
            strings.insert(symbol, StaticString { offset, length });
        }

        let layout = MemoryLayout::new(static_data.len(), variables.len())?;
        let variable_addresses = variables
            .into_iter()
            .enumerate()
            .map(|(index, variable)| {
                let index = Limits::u32(index, "environment slot count")?;
                let offset = index.checked_mul(WORD_BYTES).ok_or(EmitError::Limit {
                    what: "environment address",
                    value: index as usize,
                })?;
                let address =
                    layout.environment_base.checked_add(offset).ok_or(EmitError::Limit {
                        what: "environment address",
                        value: index as usize,
                    })?;
                Ok((variable, address))
            })
            .collect::<Result<HashMap<_, _>, EmitError>>()?;

        let mut externs = assembly.externs.iter().collect::<Vec<_>>();
        externs.sort_by(|left, right| left.name.cmp(&right.name));
        if let Some(duplicate) = externs
            .windows(2)
            .find(|pair| pair[0].name == pair[1].name)
            .map(|pair| pair[0].name.clone())
        {
            return Err(EmitError::DuplicateExtern(duplicate));
        }

        let string_literal_function = (!strings.is_empty()).then_some(0);
        let first_host_function = u32::from(string_literal_function.is_some());
        let host_imports = externs
            .into_iter()
            .enumerate()
            .map(|(index, external)| {
                let index = Limits::u32(index, "host import count")?;
                Ok(HostImport {
                    function: first_host_function + index,
                    name: external.name.clone(),
                    arity: external.arity,
                    mode: external.mode,
                    spare: SpareBox::for_role(external.role),
                })
            })
            .collect::<Result<Vec<_>, EmitError>>()?;
        let host_functions =
            host_imports.iter().map(|import| (import.name.clone(), import.function)).collect();
        let import_count = first_host_function
            .checked_add(Limits::u32(host_imports.len(), "host import count")?)
            .ok_or(EmitError::Limit { what: "host import count", value: host_imports.len() })?;

        Ok(Self {
            programs,
            program_indices,
            variable_addresses,
            strings,
            static_data,
            string_literal_function,
            host_imports,
            host_functions,
            import_count,
            layout,
            root,
        })
    }

    fn push_function(&self) -> u32 {
        self.import_count
    }

    fn pop_function(&self) -> u32 {
        self.import_count + 1
    }

    fn alloc_function(&self) -> u32 {
        self.import_count + 2
    }

    fn case_function(&self, index: u32) -> u32 {
        self.import_count + 3 + index
    }

    fn entry_function(&self) -> Result<u32, EmitError> {
        Ok(self.import_count + 3 + Limits::u32(self.programs.len(), "program count")?)
    }

    fn program_index(&self, program: ProgId) -> Result<u32, EmitError> {
        self.program_indices.get(&program).copied().ok_or(EmitError::MissingProgram(program))
    }

    fn variable_address(&self, variable: VarId) -> Result<u32, EmitError> {
        self.variable_addresses.get(&variable).copied().ok_or(EmitError::MissingVariable(variable))
    }

    fn string(&self, symbol: SymId) -> Result<StaticString, EmitError> {
        self.strings.get(&symbol).copied().ok_or(EmitError::MissingString(symbol))
    }

    fn host_function(&self, name: &str) -> Result<u32, EmitError> {
        self.host_functions
            .get(name)
            .copied()
            .ok_or_else(|| EmitError::MissingHostImport(name.to_owned()))
    }
}

struct ModuleEncoder<'a> {
    assembly: &'a AssemblyArena,
    plan: ModulePlan,
}

impl<'a> ModuleEncoder<'a> {
    fn new(assembly: &'a AssemblyArena, plan: ModulePlan) -> Self {
        Self { assembly, plan }
    }

    fn encode(self) -> Result<WasmModule, EmitError> {
        let mut module = Module::new();
        let (types, imports) = self.type_and_import_sections();
        module.section(&types);
        if !imports.is_empty() {
            module.section(&imports);
        }

        let mut functions = FunctionSection::new();
        functions.function(PUSH_TYPE);
        functions.function(POP_TYPE);
        functions.function(ALLOC_TYPE);
        self.plan.programs.iter().for_each(|_| {
            functions.function(CASE_TYPE);
        });
        functions.function(CASE_TYPE);
        module.section(&functions);

        let program_count = Limits::u32(self.plan.programs.len(), "program count")?;
        let mut tables = TableSection::new();
        tables.table(TableType {
            element_type: RefType::FUNCREF,
            table64: false,
            minimum: u64::from(program_count),
            maximum: Some(u64::from(program_count)),
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
        let mutable_i32 = GlobalType { val_type: ValType::I32, mutable: true, shared: false };
        globals.global(mutable_i32, &ConstExpr::i32_const(self.plan.layout.stack_base as i32));
        globals.global(mutable_i32, &ConstExpr::i32_const(self.plan.layout.heap_base as i32));
        globals.global(mutable_i32, &ConstExpr::i32_const(self.plan.root as i32));
        module.section(&globals);

        let entry = self.plan.entry_function()?;
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        exports.export("entry", ExportKind::Func, entry);
        exports.export("_start", ExportKind::Func, entry);
        module.section(&exports);

        let case_functions =
            (0..program_count).map(|index| self.plan.case_function(index)).collect::<Vec<_>>();
        let mut elements = ElementSection::new();
        elements.active(
            None,
            &ConstExpr::i32_const(0),
            Elements::Functions(Cow::Owned(case_functions)),
        );
        module.section(&elements);

        let mut code = CodeSection::new();
        code.function(&self.push_function());
        code.function(&self.pop_function());
        code.function(&self.alloc_function());
        for program in &self.plan.programs {
            let body = &self.assembly.programs[program];
            code.function(&CaseEncoder::new(self.assembly, &self.plan).encode(body)?);
        }
        code.function(&self.entry_body());
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
        types.ty().function([], []);
        types.ty().function([ValType::I64], []);
        types.ty().function([], [ValType::I64]);
        types.ty().function([ValType::I32], [ValType::I32]);

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
                | ExternMode::Returning => vec![ValType::I64],
                | ExternMode::Control => vec![ValType::I64; 4],
            };
            types.ty().function(parameters, results);
            imports.import(HOST_MODULE, &import.name, EntityType::Function(next_type));
            next_type += 1;
        }
        (types, imports)
    }

    fn push_function(&self) -> Function {
        let mut function = Function::new([]);
        function.instruction(&WasmInstruction::GlobalGet(STACK_POINTER_GLOBAL));
        function.instruction(&WasmInstruction::I32Const(self.plan.layout.heap_base as i32));
        function.instruction(&WasmInstruction::I32GeU);
        function.instruction(&WasmInstruction::If(BlockType::Empty));
        function.instruction(&WasmInstruction::Unreachable);
        function.instruction(&WasmInstruction::End);
        function.instruction(&WasmInstruction::GlobalGet(STACK_POINTER_GLOBAL));
        function.instruction(&WasmInstruction::LocalGet(0));
        function.instruction(&WasmInstruction::I64Store(WORD_MEMORY));
        function.instruction(&WasmInstruction::GlobalGet(STACK_POINTER_GLOBAL));
        function.instruction(&WasmInstruction::I32Const(WORD_BYTES as i32));
        function.instruction(&WasmInstruction::I32Add);
        function.instruction(&WasmInstruction::GlobalSet(STACK_POINTER_GLOBAL));
        function.instruction(&WasmInstruction::End);
        function
    }

    fn pop_function(&self) -> Function {
        let mut function = Function::new([]);
        function.instruction(&WasmInstruction::GlobalGet(STACK_POINTER_GLOBAL));
        function.instruction(&WasmInstruction::I32Const(self.plan.layout.stack_base as i32));
        function.instruction(&WasmInstruction::I32LeU);
        function.instruction(&WasmInstruction::If(BlockType::Empty));
        function.instruction(&WasmInstruction::Unreachable);
        function.instruction(&WasmInstruction::End);
        function.instruction(&WasmInstruction::GlobalGet(STACK_POINTER_GLOBAL));
        function.instruction(&WasmInstruction::I32Const(WORD_BYTES as i32));
        function.instruction(&WasmInstruction::I32Sub);
        function.instruction(&WasmInstruction::GlobalSet(STACK_POINTER_GLOBAL));
        function.instruction(&WasmInstruction::GlobalGet(STACK_POINTER_GLOBAL));
        function.instruction(&WasmInstruction::I64Load(WORD_MEMORY));
        function.instruction(&WasmInstruction::End);
        function
    }

    fn alloc_function(&self) -> Function {
        // Parameter 0 is the requested word count. Locals 1 and 2 are the old
        // and new heap cursors.
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

    fn entry_body(&self) -> Function {
        let mut function = Function::new([]);
        function.instruction(&WasmInstruction::I32Const(self.plan.layout.stack_base as i32));
        function.instruction(&WasmInstruction::GlobalSet(STACK_POINTER_GLOBAL));
        function.instruction(&WasmInstruction::I32Const(self.plan.layout.heap_base as i32));
        function.instruction(&WasmInstruction::GlobalSet(HEAP_POINTER_GLOBAL));
        function.instruction(&WasmInstruction::I32Const(self.plan.root as i32));
        function.instruction(&WasmInstruction::GlobalSet(PROGRAM_COUNTER_GLOBAL));
        function.instruction(&WasmInstruction::Loop(BlockType::Empty));
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
        names.module("zydeco");
        let mut functions = NameMap::new();
        if let Some(function) = self.plan.string_literal_function {
            functions.append(function, "zydeco.string_literal");
        }
        for import in &self.plan.host_imports {
            functions.append(import.function, &format!("zydeco.{}", import.name));
        }
        functions.append(self.plan.push_function(), "zasm.push");
        functions.append(self.plan.pop_function(), "zasm.pop");
        functions.append(self.plan.alloc_function(), "zasm.alloc");
        for (index, program) in self.plan.programs.iter().enumerate() {
            let index = Limits::u32(index, "program count")?;
            let label = self.assembly.prog_label(program).unwrap_or_else(|| {
                format!(
                    "case_{}_{}",
                    program.concise_inner().replace('#', "_"),
                    Self::program_kind(&self.assembly.programs[program])
                )
            });
            functions.append(self.plan.case_function(index), &label);
        }
        functions.append(self.plan.entry_function()?, "entry");
        names.functions(&functions);
        Ok(names)
    }

    fn program_kind(program: &Program) -> &'static str {
        match program {
            | Program::Instruction(instruction, _) => match instruction {
                | Instruction::PackProduct(_) => "pack",
                | Instruction::UnpackProduct(_) => "unpack",
                | Instruction::AllocContext(_) => "alloc_context",
                | Instruction::PushArg(_) => "push",
                | Instruction::PopArg(_) => "pop",
                | Instruction::PushTag(_) => "tag",
                | Instruction::Intrinsic(_) => "intrinsic",
                | Instruction::Clear(_) => "clear",
            },
            | Program::Terminator(terminator) => match terminator {
                | Terminator::Jump(_) => "jump",
                | Terminator::PopJump(_) => "pop_jump",
                | Terminator::PopBranch(_) => "branch",
                | Terminator::Abort(_) => "abort",
                | Terminator::Extern(_) => "extern",
            },
        }
    }
}

struct CaseEncoder<'a> {
    assembly: &'a AssemblyArena,
    plan: &'a ModulePlan,
    function: Function,
}

impl<'a> CaseEncoder<'a> {
    fn new(assembly: &'a AssemblyArena, plan: &'a ModulePlan) -> Self {
        // Two i32 temporaries followed by twelve i64 temporaries.
        let function = Function::new([(2, ValType::I32), (12, ValType::I64)]);
        Self { assembly, plan, function }
    }

    fn encode(mut self, body: &Program) -> Result<Function, EmitError> {
        match body {
            | Program::Instruction(instruction, next) => {
                self.emit_instruction(instruction)?;
                self.set_program_counter(*next)?;
            }
            | Program::Terminator(terminator) => self.emit_terminator(terminator)?,
        }
        self.function.instruction(&WasmInstruction::End);
        Ok(self.function)
    }

    fn emit_instruction(&mut self, instruction: &Instruction) -> Result<(), EmitError> {
        match instruction {
            | Instruction::PackProduct(zasm::Pack(layout)) => self.emit_pack(*layout)?,
            | Instruction::UnpackProduct(zasm::Unpack(layout)) => self.emit_unpack(*layout)?,
            | Instruction::AllocContext(_) => {}
            | Instruction::PushArg(zasm::Push(atom)) => self.emit_atom(atom)?,
            | Instruction::PopArg(zasm::Pop(variable)) => {
                let address = self.plan.variable_address(*variable)?;
                self.function.instruction(&WasmInstruction::I32Const(address as i32));
                self.function.instruction(&WasmInstruction::Call(self.plan.pop_function()));
                self.function.instruction(&WasmInstruction::I64Store(WORD_MEMORY));
            }
            | Instruction::PushTag(zasm::Push(tag)) => {
                self.push_constant(RuntimeWord::index(tag.idx)? as i64);
            }
            | Instruction::Intrinsic(intrinsic) => self.emit_intrinsic(intrinsic)?,
            | Instruction::Clear(context) => {
                for variable in context {
                    let address = self.plan.variable_address(*variable)?;
                    self.function.instruction(&WasmInstruction::I32Const(address as i32));
                    self.function.instruction(&WasmInstruction::I64Const(0));
                    self.function.instruction(&WasmInstruction::I64Store(WORD_MEMORY));
                }
            }
        }
        Ok(())
    }

    fn emit_terminator(&mut self, terminator: &Terminator) -> Result<(), EmitError> {
        match terminator {
            | Terminator::Jump(zasm::Jump(target)) => self.set_program_counter(*target)?,
            | Terminator::PopJump(_) => {
                self.function.instruction(&WasmInstruction::Call(self.plan.pop_function()));
                self.function.instruction(&WasmInstruction::I32WrapI64);
                self.function.instruction(&WasmInstruction::GlobalSet(PROGRAM_COUNTER_GLOBAL));
            }
            | Terminator::PopBranch(zasm::PopBranch(arms)) => {
                self.pop_to(TAG_LOCAL);
                for (tag, target) in arms {
                    self.function.instruction(&WasmInstruction::LocalGet(TAG_LOCAL));
                    self.function
                        .instruction(&WasmInstruction::I64Const(
                            RuntimeWord::index(tag.idx)? as i64
                        ));
                    self.function.instruction(&WasmInstruction::I64Eq);
                    self.function.instruction(&WasmInstruction::If(BlockType::Empty));
                    self.set_program_counter(*target)?;
                    self.function.instruction(&WasmInstruction::Return);
                    self.function.instruction(&WasmInstruction::End);
                }
                self.function.instruction(&WasmInstruction::Unreachable);
            }
            | Terminator::Extern(external) => self.emit_extern(external)?,
            | Terminator::Abort(_) => {
                self.function.instruction(&WasmInstruction::Unreachable);
            }
        }
        Ok(())
    }

    fn emit_pack(&mut self, layout: zasm::ProductLayout) -> Result<(), EmitError> {
        let arity = Limits::u32(layout.arity, "product arity")?;
        self.function.instruction(&WasmInstruction::I32Const(arity as i32));
        self.function.instruction(&WasmInstruction::Call(self.plan.alloc_function()));
        self.function.instruction(&WasmInstruction::LocalSet(POINTER_LOCAL));
        for index in 0..layout.elements {
            if index + 1 == layout.elements && layout.elements < layout.arity {
                self.pop_to(WORD_LOCAL);
                for field in index..layout.arity {
                    self.function.instruction(&WasmInstruction::LocalGet(POINTER_LOCAL));
                    self.function.instruction(&WasmInstruction::LocalGet(WORD_LOCAL));
                    self.function.instruction(&WasmInstruction::I32WrapI64);
                    self.function
                        .instruction(&WasmInstruction::I64Load(Self::word_at(field - index)?));
                    self.function.instruction(&WasmInstruction::I64Store(Self::word_at(field)?));
                }
            } else {
                self.function.instruction(&WasmInstruction::LocalGet(POINTER_LOCAL));
                self.function.instruction(&WasmInstruction::Call(self.plan.pop_function()));
                self.function.instruction(&WasmInstruction::I64Store(Self::word_at(index)?));
            }
        }
        self.push_pointer(POINTER_LOCAL);
        Ok(())
    }

    fn emit_unpack(&mut self, layout: zasm::ProductLayout) -> Result<(), EmitError> {
        self.function.instruction(&WasmInstruction::Call(self.plan.pop_function()));
        self.function.instruction(&WasmInstruction::I32WrapI64);
        self.function.instruction(&WasmInstruction::LocalSet(POINTER_LOCAL));
        let last = layout.elements - 1;
        if layout.elements < layout.arity {
            self.function.instruction(&WasmInstruction::LocalGet(POINTER_LOCAL));
            self.function.instruction(&WasmInstruction::I32Const(Self::byte_offset(last)? as i32));
            self.function.instruction(&WasmInstruction::I32Add);
            self.function.instruction(&WasmInstruction::I64ExtendI32U);
            self.function.instruction(&WasmInstruction::Call(self.plan.push_function()));
        } else {
            self.push_loaded_word(POINTER_LOCAL, last)?;
        }
        for index in (0..last).rev() {
            self.push_loaded_word(POINTER_LOCAL, index)?;
        }
        Ok(())
    }

    fn emit_atom(&mut self, atom: &Atom) -> Result<(), EmitError> {
        match atom {
            | Atom::Var(variable) => {
                let address = self.plan.variable_address(*variable)?;
                self.function.instruction(&WasmInstruction::I32Const(address as i32));
                self.function.instruction(&WasmInstruction::I64Load(WORD_MEMORY));
                self.function.instruction(&WasmInstruction::Call(self.plan.push_function()));
            }
            | Atom::Sym(symbol) => match &self.assembly.symbols[symbol].inner {
                | Symbol::Prog(program) => {
                    self.push_constant(i64::from(self.plan.program_index(*program)?));
                }
                | Symbol::StringLiteral(_) => {
                    let string = self.plan.string(*symbol)?;
                    let function = self
                        .plan
                        .string_literal_function
                        .ok_or(EmitError::MissingString(*symbol))?;
                    self.function.instruction(&WasmInstruction::I32Const(string.offset as i32));
                    self.function.instruction(&WasmInstruction::I32Const(string.length as i32));
                    self.function.instruction(&WasmInstruction::Call(function));
                    self.function.instruction(&WasmInstruction::Call(self.plan.push_function()));
                }
                | Symbol::Undefined(_) => unreachable!("undefined symbol reached emission"),
            },
            | Atom::Imm(immediate) => self.emit_immediate(immediate)?,
        }
        Ok(())
    }

    fn emit_immediate(&mut self, immediate: &Imm) -> Result<(), EmitError> {
        match immediate {
            | Imm::Triv(_) => self.push_constant(RuntimeWord::index(0)? as i64),
            | Imm::Integer(integer) => match RuntimeWord::integer(*integer)? {
                | EncodedScalar::Immediate(word) => self.push_constant(word as i64),
                | EncodedScalar::Boxed(bits) => self.push_boxed(bits),
            },
            | Imm::Float(float) => match RuntimeWord::float(*float) {
                | EncodedScalar::Immediate(word) => self.push_constant(word as i64),
                | EncodedScalar::Boxed(bits) => self.push_boxed(bits),
            },
            | Imm::Char(character) => {
                self.push_constant(RuntimeWord::index(*character as usize)? as i64)
            }
        }
        Ok(())
    }

    fn emit_extern(&mut self, external: &zasm::Extern) -> Result<(), EmitError> {
        let function = self.plan.host_function(&external.name)?;
        for index in 0..external.arity {
            self.pop_to(FIRST_ARGUMENT_LOCAL + Limits::u32(index, "host argument count")?);
        }
        for index in 0..external.arity {
            self.function.instruction(&WasmInstruction::LocalGet(
                FIRST_ARGUMENT_LOCAL + Limits::u32(index, "host argument count")?,
            ));
        }
        if let Some(spare) = SpareBox::for_role(external.role) {
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
        self.function.instruction(&WasmInstruction::Call(function));
        match external.mode {
            | ExternMode::Returning => {
                self.function.instruction(&WasmInstruction::LocalSet(RESULT_LOCAL));
                self.function.instruction(&WasmInstruction::Call(self.plan.pop_function()));
                self.function.instruction(&WasmInstruction::I32WrapI64);
                self.function.instruction(&WasmInstruction::GlobalSet(PROGRAM_COUNTER_GLOBAL));
                self.push_local(RESULT_LOCAL);
            }
            | ExternMode::Control => {
                self.function.instruction(&WasmInstruction::LocalSet(TRANSFER_SECOND_LOCAL));
                self.function.instruction(&WasmInstruction::LocalSet(TRANSFER_FIRST_LOCAL));
                self.function.instruction(&WasmInstruction::LocalSet(TRANSFER_CLOSURE_LOCAL));
                self.function.instruction(&WasmInstruction::LocalSet(TRANSFER_COUNT_LOCAL));
                self.resume_control_transfer();
            }
        }
        Ok(())
    }

    fn resume_control_transfer(&mut self) {
        self.function.instruction(&WasmInstruction::LocalGet(TRANSFER_COUNT_LOCAL));
        self.function.instruction(&WasmInstruction::I64Const(2));
        self.function.instruction(&WasmInstruction::I64GtU);
        self.function.instruction(&WasmInstruction::If(BlockType::Empty));
        self.function.instruction(&WasmInstruction::Unreachable);
        self.function.instruction(&WasmInstruction::End);

        self.function.instruction(&WasmInstruction::LocalGet(TRANSFER_COUNT_LOCAL));
        self.function.instruction(&WasmInstruction::I64Const(2));
        self.function.instruction(&WasmInstruction::I64Eq);
        self.function.instruction(&WasmInstruction::If(BlockType::Empty));
        self.push_local(TRANSFER_SECOND_LOCAL);
        self.function.instruction(&WasmInstruction::End);

        self.function.instruction(&WasmInstruction::LocalGet(TRANSFER_COUNT_LOCAL));
        self.function.instruction(&WasmInstruction::I64Eqz);
        self.function.instruction(&WasmInstruction::I32Eqz);
        self.function.instruction(&WasmInstruction::If(BlockType::Empty));
        self.push_local(TRANSFER_FIRST_LOCAL);
        self.function.instruction(&WasmInstruction::End);

        self.function.instruction(&WasmInstruction::LocalGet(TRANSFER_CLOSURE_LOCAL));
        self.function.instruction(&WasmInstruction::I32WrapI64);
        self.function.instruction(&WasmInstruction::LocalTee(POINTER_LOCAL));
        self.function.instruction(&WasmInstruction::I64Load(Self::word_at_const(0)));
        self.function.instruction(&WasmInstruction::Call(self.plan.push_function()));
        self.function.instruction(&WasmInstruction::LocalGet(POINTER_LOCAL));
        self.function.instruction(&WasmInstruction::I64Load(Self::word_at_const(1)));
        self.function.instruction(&WasmInstruction::I32WrapI64);
        self.function.instruction(&WasmInstruction::GlobalSet(PROGRAM_COUNTER_GLOBAL));
    }

    fn emit_intrinsic(&mut self, intrinsic: &zasm::Intrinsic) -> Result<(), EmitError> {
        if intrinsic.arity != 2 {
            return Err(EmitError::UnsupportedIntrinsic {
                name: intrinsic.name.clone(),
                arity: intrinsic.arity,
            });
        }
        self.pop_to(FIRST_ARGUMENT_LOCAL);
        self.pop_to(FIRST_ARGUMENT_LOCAL + 1);
        self.decode_signed_word(FIRST_ARGUMENT_LOCAL, DECODED_FIRST_LOCAL);
        self.decode_signed_word(FIRST_ARGUMENT_LOCAL + 1, DECODED_SECOND_LOCAL);

        match intrinsic.name.as_str() {
            | "int_eq" | "int_lt" | "int_gt" => {
                self.function.instruction(&WasmInstruction::LocalGet(DECODED_FIRST_LOCAL));
                self.function.instruction(&WasmInstruction::LocalGet(DECODED_SECOND_LOCAL));
                self.function.instruction(&match intrinsic.name.as_str() {
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
                self.function.instruction(&WasmInstruction::LocalSet(RESULT_LOCAL));
                self.function.instruction(&WasmInstruction::I32Const(2));
                self.function.instruction(&WasmInstruction::Call(self.plan.alloc_function()));
                self.function.instruction(&WasmInstruction::LocalTee(POINTER_LOCAL));
                self.function.instruction(&WasmInstruction::LocalGet(RESULT_LOCAL));
                self.function.instruction(&WasmInstruction::I64Store(Self::word_at_const(0)));
                self.function.instruction(&WasmInstruction::LocalGet(POINTER_LOCAL));
                self.function.instruction(&WasmInstruction::I64Const(1));
                self.function.instruction(&WasmInstruction::I64Store(Self::word_at_const(1)));
                self.push_pointer(POINTER_LOCAL);
            }
            | "add" | "sub" | "mul" | "and" | "or" | "xor" => {
                self.function.instruction(&WasmInstruction::LocalGet(DECODED_FIRST_LOCAL));
                self.function.instruction(&WasmInstruction::LocalGet(DECODED_SECOND_LOCAL));
                self.function.instruction(&match intrinsic.name.as_str() {
                    | "add" => WasmInstruction::I64Add,
                    | "sub" => WasmInstruction::I64Sub,
                    | "mul" => WasmInstruction::I64Mul,
                    | "and" => WasmInstruction::I64And,
                    | "or" => WasmInstruction::I64Or,
                    | "xor" => WasmInstruction::I64Xor,
                    | _ => unreachable!(),
                });
                self.function.instruction(&WasmInstruction::LocalSet(RESULT_LOCAL));
                self.encode_signed_local(RESULT_LOCAL);
                self.function.instruction(&WasmInstruction::Call(self.plan.push_function()));
            }
            | "div" | "mod" => {
                self.emit_wrapping_division(intrinsic.name == "mod");
                self.encode_signed_local(RESULT_LOCAL);
                self.function.instruction(&WasmInstruction::Call(self.plan.push_function()));
            }
            | _ => {
                return Err(EmitError::UnsupportedIntrinsic {
                    name: intrinsic.name.clone(),
                    arity: intrinsic.arity,
                });
            }
        }
        Ok(())
    }

    fn decode_signed_word(&mut self, input: u32, output: u32) {
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
        self.function.instruction(&WasmInstruction::LocalTee(POINTER_LOCAL));
        self.function.instruction(&WasmInstruction::LocalGet(input));
        self.function.instruction(&WasmInstruction::I64Store(WORD_MEMORY));
        self.function.instruction(&WasmInstruction::LocalGet(POINTER_LOCAL));
        self.function.instruction(&WasmInstruction::I64ExtendI32U);
        self.function.instruction(&WasmInstruction::End);
    }

    fn emit_wrapping_division(&mut self, remainder: bool) {
        self.function.instruction(&WasmInstruction::LocalGet(DECODED_SECOND_LOCAL));
        self.function.instruction(&WasmInstruction::I64Eqz);
        self.function.instruction(&WasmInstruction::If(BlockType::Empty));
        self.function.instruction(&WasmInstruction::Unreachable);
        self.function.instruction(&WasmInstruction::End);
        self.function.instruction(&WasmInstruction::LocalGet(DECODED_FIRST_LOCAL));
        self.function.instruction(&WasmInstruction::I64Const(i64::MIN));
        self.function.instruction(&WasmInstruction::I64Eq);
        self.function.instruction(&WasmInstruction::LocalGet(DECODED_SECOND_LOCAL));
        self.function.instruction(&WasmInstruction::I64Const(-1));
        self.function.instruction(&WasmInstruction::I64Eq);
        self.function.instruction(&WasmInstruction::I32And);
        self.function.instruction(&WasmInstruction::If(BlockType::Result(ValType::I64)));
        self.function.instruction(&WasmInstruction::I64Const(if remainder { 0 } else { i64::MIN }));
        self.function.instruction(&WasmInstruction::Else);
        self.function.instruction(&WasmInstruction::LocalGet(DECODED_FIRST_LOCAL));
        self.function.instruction(&WasmInstruction::LocalGet(DECODED_SECOND_LOCAL));
        self.function.instruction(&if remainder {
            WasmInstruction::I64RemS
        } else {
            WasmInstruction::I64DivS
        });
        self.function.instruction(&WasmInstruction::End);
        self.function.instruction(&WasmInstruction::LocalSet(RESULT_LOCAL));
    }

    fn set_program_counter(&mut self, program: ProgId) -> Result<(), EmitError> {
        let index = self.plan.program_index(program)?;
        self.function.instruction(&WasmInstruction::I32Const(index as i32));
        self.function.instruction(&WasmInstruction::GlobalSet(PROGRAM_COUNTER_GLOBAL));
        Ok(())
    }

    fn pop_to(&mut self, local: u32) {
        self.function.instruction(&WasmInstruction::Call(self.plan.pop_function()));
        self.function.instruction(&WasmInstruction::LocalSet(local));
    }

    fn push_constant(&mut self, value: i64) {
        self.function.instruction(&WasmInstruction::I64Const(value));
        self.function.instruction(&WasmInstruction::Call(self.plan.push_function()));
    }

    fn push_local(&mut self, local: u32) {
        self.function.instruction(&WasmInstruction::LocalGet(local));
        self.function.instruction(&WasmInstruction::Call(self.plan.push_function()));
    }

    fn push_pointer(&mut self, local: u32) {
        self.function.instruction(&WasmInstruction::LocalGet(local));
        self.function.instruction(&WasmInstruction::I64ExtendI32U);
        self.function.instruction(&WasmInstruction::Call(self.plan.push_function()));
    }

    fn push_boxed(&mut self, bits: u64) {
        self.function.instruction(&WasmInstruction::I32Const(1));
        self.function.instruction(&WasmInstruction::Call(self.plan.alloc_function()));
        self.function.instruction(&WasmInstruction::LocalTee(POINTER_LOCAL));
        self.function.instruction(&WasmInstruction::I64Const(bits as i64));
        self.function.instruction(&WasmInstruction::I64Store(WORD_MEMORY));
        self.push_pointer(POINTER_LOCAL);
    }

    fn push_loaded_word(&mut self, pointer: u32, index: usize) -> Result<(), EmitError> {
        self.function.instruction(&WasmInstruction::LocalGet(pointer));
        self.function.instruction(&WasmInstruction::I64Load(Self::word_at(index)?));
        self.function.instruction(&WasmInstruction::Call(self.plan.push_function()));
        Ok(())
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

    fn index(value: usize) -> Result<u64, EmitError> {
        let value = u64::try_from(value)
            .map_err(|_| EmitError::Limit { what: "runtime tag index", value })?;
        Self::unsigned(value)
            .ok_or(EmitError::Limit { what: "runtime tag index", value: value as usize })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tagged_words_match_the_native_immediate_boundary() {
        assert_eq!(RuntimeWord::signed(RuntimeWord::SIGNED_MIN), Some(0x8000_0000_0000_0001));
        assert_eq!(RuntimeWord::signed(RuntimeWord::SIGNED_MAX), Some(0x7fff_ffff_ffff_ffff));
        assert_eq!(RuntimeWord::signed(RuntimeWord::SIGNED_MIN - 1), None);
        assert_eq!(RuntimeWord::unsigned(RuntimeWord::UNSIGNED_MAX), Some(u64::MAX));
    }
}
