use std::collections::HashMap;

use thiserror::Error;
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, Function, FunctionSection, GlobalType, ImportSection,
    Instruction as WasmInstruction, Module, NameMap, NameSection, TypeSection, ValType,
};
use zydeco_assembly::{
    arena::{AssemblyArena, AssemblyArenaRefLike, AssemblyProgram},
    syntax::{
        self as zasm, Atom, ExternMode, Imm, Instruction, ProgId, Program, SymId, Symbol,
        Terminator, VarId,
    },
};
use zydeco_wasm_common::{
    AllocFunction, EncodedScalar, HostCallKind, HostImport, HostSections, Intrinsics, Limits,
    PointerLocal, ProductFields, RuntimeWord, StaticString, StringTable, WASM_PAGE_BYTES,
    WORD_BYTES, WORD_MEMORY, WasmEmitError, WasmSections, WordEmitter,
};

pub use zydeco_wasm_common::{HOST_MODULE, WasmModule};

const CASE_TYPE: u32 = 0;
const PUSH_TYPE: u32 = 1;
const POP_TYPE: u32 = 2;
const ALLOC_TYPE: u32 = 3;
const FIRST_IMPORT_TYPE: u32 = 4;

const STACK_BYTES: u32 = 1024 * 1024;

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

/// The pointer temporary shared by the word-emission sequences.
const POINTER: PointerLocal = PointerLocal::I32(POINTER_LOCAL);

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
    #[error("WebAssembly abstract-machine backend cannot import native foreign symbol `{0}`")]
    UnsupportedForeignImport(String),
    #[error(
        "unsupported ZASM intrinsic `{name}/{arity}` in the WebAssembly abstract-machine backend"
    )]
    UnsupportedIntrinsic { name: String, arity: usize },
    #[error(transparent)]
    Common(#[from] WasmEmitError),
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
        let environment_bytes =
            variable_count.checked_mul(WORD_BYTES).ok_or(WasmEmitError::Limit {
                what: "environment byte size",
                value: variable_count as usize,
            })?;
        let stack_base = Limits::align(
            environment_base
                .checked_add(environment_bytes)
                .and_then(|end| end.checked_add(WORD_BYTES))
                .ok_or(WasmEmitError::Limit {
                    what: "environment end",
                    value: environment_bytes as usize,
                })?,
            WORD_BYTES,
        )?;
        let heap_base = stack_base.checked_add(STACK_BYTES).ok_or(WasmEmitError::Limit {
            what: "control stack end",
            value: stack_base as usize,
        })?;
        let initial_end = heap_base.checked_add(WASM_PAGE_BYTES).ok_or(WasmEmitError::Limit {
            what: "initial memory size",
            value: heap_base as usize,
        })?;
        let initial_pages = u64::from(initial_end.div_ceil(WASM_PAGE_BYTES));
        Ok(Self { environment_base, stack_base, heap_base, initial_pages })
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
        let string_entries = string_symbols.into_iter().map(|symbol| {
            let Symbol::StringLiteral(string) = &assembly.symbols[&symbol].inner else {
                unreachable!("string symbol collection changed during planning")
            };
            (symbol, string.clone())
        });
        let (static_data, strings) = StringTable::build(string_entries)?;

        let layout = MemoryLayout::new(static_data.len(), variables.len())?;
        let variable_addresses =
            variables
                .into_iter()
                .enumerate()
                .map(|(index, variable)| {
                    let index = Limits::u32(index, "environment slot count")?;
                    let offset = index.checked_mul(WORD_BYTES).ok_or(WasmEmitError::Limit {
                        what: "environment address",
                        value: index as usize,
                    })?;
                    let address = layout.environment_base.checked_add(offset).ok_or(
                        WasmEmitError::Limit { what: "environment address", value: index as usize },
                    )?;
                    Ok((variable, address))
                })
                .collect::<Result<HashMap<_, _>, EmitError>>()?;

        if let Some(symbol) = assembly.externs.iter().find_map(|external| match external {
            | zasm::Extern::Foreign(import) => Some(import.target.symbol.to_string()),
            | zasm::Extern::Host { .. } => None,
        }) {
            return Err(EmitError::UnsupportedForeignImport(symbol));
        }
        let mut externs = assembly
            .externs
            .iter()
            .filter(|external| matches!(external, zasm::Extern::Host { .. }))
            .collect::<Vec<_>>();
        externs.sort_by(|left, right| match (left, right) {
            | (zasm::Extern::Host { name: left, .. }, zasm::Extern::Host { name: right, .. }) => {
                left.cmp(right)
            }
            | _ => unreachable!("foreign imports were rejected above"),
        });
        if let Some(duplicate) = externs.windows(2).find_map(|pair| match (pair[0], pair[1]) {
            | (zasm::Extern::Host { name: left, .. }, zasm::Extern::Host { name: right, .. })
                if left == right =>
            {
                Some(left.clone())
            }
            | _ => None,
        }) {
            return Err(EmitError::DuplicateExtern(duplicate));
        }

        let string_literal_function = (!strings.is_empty()).then_some(0);
        let first_host_function = u32::from(string_literal_function.is_some());
        let host_imports = externs
            .into_iter()
            .enumerate()
            .map(|(index, external)| {
                let zasm::Extern::Host { role, name, arity, mode } = external else {
                    unreachable!("foreign imports were rejected above")
                };
                let index = Limits::u32(index, "host import count")?;
                Ok(HostImport {
                    function: first_host_function + index,
                    name: name.clone(),
                    arity: *arity,
                    mode: match mode {
                        | ExternMode::Returning => HostCallKind::Returning,
                        | ExternMode::Control => HostCallKind::Control,
                    },
                    spare: role.spare_box(),
                })
            })
            .collect::<Result<Vec<_>, EmitError>>()?;
        let host_functions =
            host_imports.iter().map(|import| (import.name.clone(), import.function)).collect();
        let import_count = first_host_function
            .checked_add(Limits::u32(host_imports.len(), "host import count")?)
            .ok_or(WasmEmitError::Limit { what: "host import count", value: host_imports.len() })?;

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
        WasmSections::table(&mut module, program_count);
        WasmSections::memory(&mut module, self.plan.layout.initial_pages);
        let mutable_i32 = GlobalType { val_type: ValType::I32, mutable: true, shared: false };
        WasmSections::globals(
            &mut module,
            vec![
                (mutable_i32, ConstExpr::i32_const(self.plan.layout.stack_base as i32)),
                (mutable_i32, ConstExpr::i32_const(self.plan.layout.heap_base as i32)),
                (mutable_i32, ConstExpr::i32_const(self.plan.root as i32)),
            ],
        );

        let entry = self.plan.entry_function()?;
        WasmSections::exports(&mut module, entry);

        let case_functions =
            (0..program_count).map(|index| self.plan.case_function(index)).collect::<Vec<_>>();
        WasmSections::elements(&mut module, case_functions);

        let mut code = CodeSection::new();
        code.function(&self.push_function());
        code.function(&self.pop_function());
        code.function(&AllocFunction::new(HEAP_POINTER_GLOBAL).emit());
        for program in &self.plan.programs {
            let body = &self.assembly.programs[program];
            code.function(&CaseEncoder::new(self.assembly, &self.plan).encode(body)?);
        }
        code.function(&self.entry_body());
        module.section(&code);

        WasmSections::data(&mut module, &self.plan.static_data);
        module.section(&self.name_section()?);

        Ok(WasmModule::from_module(module))
    }

    fn type_and_import_sections(&self) -> (TypeSection, ImportSection) {
        let mut types = TypeSection::new();
        types.ty().function([], []);
        types.ty().function([ValType::I64], []);
        types.ty().function([], [ValType::I64]);
        types.ty().function([ValType::I32], [ValType::I32]);

        let mut imports = ImportSection::new();
        HostSections::append_imports(
            &mut types,
            &mut imports,
            FIRST_IMPORT_TYPE,
            self.plan.string_literal_function.is_some(),
            &self.plan.host_imports,
        );
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
        HostSections::append_names(
            &mut functions,
            self.plan.string_literal_function,
            &self.plan.host_imports,
        );
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
                self.push_constant(RuntimeWord::index(tag.idx)?);
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
                        .instruction(&WasmInstruction::I64Const(RuntimeWord::index(tag.idx)?));
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
                    self.function.instruction(&WasmInstruction::I64Load(ProductFields::word_at(
                        field - index,
                    )?));
                    self.function
                        .instruction(&WasmInstruction::I64Store(ProductFields::word_at(field)?));
                }
            } else {
                self.function.instruction(&WasmInstruction::LocalGet(POINTER_LOCAL));
                self.function.instruction(&WasmInstruction::Call(self.plan.pop_function()));
                self.function
                    .instruction(&WasmInstruction::I64Store(ProductFields::word_at(index)?));
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
            self.function
                .instruction(&WasmInstruction::I32Const(ProductFields::byte_offset(last)? as i32));
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
            | Imm::Triv(_) => self.push_constant(RuntimeWord::index(0)?),
            | Imm::Integer(integer) => match RuntimeWord::integer(*integer)? {
                | EncodedScalar::Immediate(word) => self.push_constant(word as i64),
                | EncodedScalar::Boxed(bits) => self.push_boxed(bits),
            },
            | Imm::Float(float) => match RuntimeWord::float(*float) {
                | EncodedScalar::Immediate(word) => self.push_constant(word as i64),
                | EncodedScalar::Boxed(bits) => self.push_boxed(bits),
            },
            | Imm::Char(character) => self.push_constant(RuntimeWord::index(*character as usize)?),
        }
        Ok(())
    }

    fn emit_extern(&mut self, external: &zasm::Extern) -> Result<(), EmitError> {
        let zasm::Extern::Host { role, name, arity, mode } = external else {
            let zasm::Extern::Foreign(import) = external else { unreachable!() };
            return Err(EmitError::UnsupportedForeignImport(import.target.symbol.to_string()));
        };
        let function = self.plan.host_function(name)?;
        for index in 0..*arity {
            self.pop_to(FIRST_ARGUMENT_LOCAL + Limits::u32(index, "host argument count")?);
        }
        for index in 0..*arity {
            self.function.instruction(&WasmInstruction::LocalGet(
                FIRST_ARGUMENT_LOCAL + Limits::u32(index, "host argument count")?,
            ));
        }
        WordEmitter::new(&mut self.function, self.plan.alloc_function())
            .spare_box(role.spare_box());
        self.function.instruction(&WasmInstruction::Call(function));
        match mode {
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
        self.function.instruction(&WasmInstruction::I64Load(ProductFields::word_at_const(0)));
        self.function.instruction(&WasmInstruction::Call(self.plan.push_function()));
        self.function.instruction(&WasmInstruction::LocalGet(POINTER_LOCAL));
        self.function.instruction(&WasmInstruction::I64Load(ProductFields::word_at_const(1)));
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
        let name = intrinsic.name.as_str();
        self.pop_to(FIRST_ARGUMENT_LOCAL);
        self.pop_to(FIRST_ARGUMENT_LOCAL + 1);
        WordEmitter::new(&mut self.function, self.plan.alloc_function())
            .decode_signed_local(FIRST_ARGUMENT_LOCAL, DECODED_FIRST_LOCAL);
        WordEmitter::new(&mut self.function, self.plan.alloc_function())
            .decode_signed_local(FIRST_ARGUMENT_LOCAL + 1, DECODED_SECOND_LOCAL);

        if let Some(operation) = Intrinsics::comparison(name) {
            self.function.instruction(&WasmInstruction::LocalGet(DECODED_FIRST_LOCAL));
            self.function.instruction(&WasmInstruction::LocalGet(DECODED_SECOND_LOCAL));
            self.function.instruction(&operation);
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
            self.function.instruction(&WasmInstruction::I64Store(ProductFields::word_at_const(0)));
            self.function.instruction(&WasmInstruction::LocalGet(POINTER_LOCAL));
            self.function.instruction(&WasmInstruction::I64Const(1));
            self.function.instruction(&WasmInstruction::I64Store(ProductFields::word_at_const(1)));
            self.push_pointer(POINTER_LOCAL);
        } else if let Some(operation) = Intrinsics::arithmetic(name) {
            self.function.instruction(&WasmInstruction::LocalGet(DECODED_FIRST_LOCAL));
            self.function.instruction(&WasmInstruction::LocalGet(DECODED_SECOND_LOCAL));
            self.function.instruction(&operation);
            self.function.instruction(&WasmInstruction::LocalSet(RESULT_LOCAL));
            WordEmitter::new(&mut self.function, self.plan.alloc_function())
                .encode_signed_local(RESULT_LOCAL, POINTER);
            self.function.instruction(&WasmInstruction::Call(self.plan.push_function()));
        } else if let Some(remainder) = Intrinsics::division(name) {
            WordEmitter::new(&mut self.function, self.plan.alloc_function()).wrapping_division(
                DECODED_FIRST_LOCAL,
                DECODED_SECOND_LOCAL,
                RESULT_LOCAL,
                remainder,
            );
            WordEmitter::new(&mut self.function, self.plan.alloc_function())
                .encode_signed_local(RESULT_LOCAL, POINTER);
            self.function.instruction(&WasmInstruction::Call(self.plan.push_function()));
        } else {
            return Err(EmitError::UnsupportedIntrinsic {
                name: intrinsic.name.clone(),
                arity: intrinsic.arity,
            });
        }
        Ok(())
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
        WordEmitter::new(&mut self.function, self.plan.alloc_function()).boxed(bits, POINTER);
        self.function.instruction(&WasmInstruction::Call(self.plan.push_function()));
    }

    fn push_loaded_word(&mut self, pointer: u32, index: usize) -> Result<(), EmitError> {
        self.function.instruction(&WasmInstruction::LocalGet(pointer));
        self.function.instruction(&WasmInstruction::I64Load(ProductFields::word_at(index)?));
        self.function.instruction(&WasmInstruction::Call(self.plan.push_function()));
        Ok(())
    }
}
