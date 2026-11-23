use std::collections::{HashMap, HashSet};

use thiserror::Error;
use wasm_encoder::{BlockType, Instruction as WasmInstruction, MemArg, ValType};
use zydeco_abi::HostCallMode;
use zydeco_stackir::low::scalar::ScalarPlans;
use zydeco_stackir::{
    SpsLowProgram,
    low::syntax::{
        self as sps, Block, CompuId, Computation, DefId, ProductLayout, StackId, VPatId, ValueId,
        ValuePattern,
    },
};
use zydeco_syntax::scalar::ScalarBoxing;
use zydeco_syntax::{BuiltinValueRole, Literal};
use zydeco_wasm_common::{
    DataSymbol, ENTRY_SYMBOL, EncodedScalar, FuncRef, HostCallKind, HostImport, HostSections,
    Limits, PointerLocal, ProductFields, RelocFunction, RuntimeFailure, RuntimeWord, StaticString,
    StringTable, TableSymbol, WORD_MEMORY, WasmEmitError, WasmObjectBuilder, WasmRuntimeFailure,
    WordEmitter, WordError,
};

pub use zydeco_wasm_common::{HOST_MODULE, WasmModule};

const CASE_TYPE: u32 = 0;
const PAIR_TYPE: u32 = 1;
const ENTRY_TYPE: u32 = 2;
const FIRST_IMPORT_TYPE: u32 = 3;

const CASES_SYMBOL: DataSymbol = DataSymbol(0);
const CASES_ALIGNMENT: MemArg = MemArg { offset: 0, align: 2, memory_index: 0 };

/// Direct structured `SPS_l`-to-WebAssembly emitter.
pub struct Emitter<'a> {
    program: &'a SpsLowProgram,
    scalar_boxing: ScalarBoxing,
}

impl<'a> Emitter<'a> {
    pub fn new(program: &'a SpsLowProgram) -> Self {
        Self { program, scalar_boxing: ScalarBoxing::default() }
    }

    pub fn with_scalar_boxing(mut self, scalar_boxing: ScalarBoxing) -> Self {
        self.scalar_boxing = scalar_boxing;
        self
    }

    pub fn run(self) -> Result<WasmModule, EmitError> {
        let plan = ModulePlan::new(self.program, self.scalar_boxing)?;
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
    MissingHostImport(BuiltinValueRole),
    #[error("SPS WebAssembly backend cannot import native foreign symbol `{0}`")]
    UnsupportedForeignImport(String),
    #[error("invalid SPS coproduct match: constructor arms cannot be mixed with a catch-all arm")]
    InvalidCoprodMatch,
    #[error(transparent)]
    Common(#[from] WasmEmitError),
}

impl From<WordError> for EmitError {
    fn from(error: WordError) -> Self {
        Self::Common(WasmEmitError::from(error))
    }
}

#[derive(Clone, Copy)]
struct Case {
    index: u32,
    label: Option<DefId>,
    entry: Option<sps::EntryParameters>,
    body: CompuId,
}

struct LocalPlan {
    variables: HashMap<DefId, u32>,
    values: HashMap<ValueId, u32>,
    stacks: HashMap<StackId, u32>,
    patterns: HashMap<VPatId, u32>,
    scratch_stack: u32,
    scratch_word: u32,
    scratch_payload: u32,
    result: u32,
    transfer_ptr: u32,
    scalar_base: u32,
    local_count: u32,
}

impl LocalPlan {
    fn for_case(
        arena: &sps::SpsLowArena, labels: &HashMap<DefId, u32>, scalars: &ScalarPlans, case: Case,
    ) -> Result<Self, EmitError> {
        let mut collector = CaseNodeCollector {
            arena,
            labels,
            scalars,
            defs: HashSet::new(),
            values: HashSet::new(),
            stacks: HashSet::new(),
            patterns: HashSet::new(),
            visited_compus: HashSet::new(),
            scalar_count: 0,
        };
        if let Some(entry) = case.entry {
            for (_, pattern) in entry.words() {
                collector.visit_pattern(pattern);
            }
        }
        collector.visit_compu(case.body);

        let mut next = 1_u32;

        let mut defs = collector.defs.into_iter().collect::<Vec<_>>();
        defs.sort_unstable();
        let variables = Self::assign(defs, &mut next, "SPS variable count")?;

        let mut values = collector.values.into_iter().collect::<Vec<_>>();
        values.sort_unstable();
        let values = Self::assign(values, &mut next, "SPS value count")?;

        let mut stacks = collector.stacks.into_iter().collect::<Vec<_>>();
        stacks.sort_unstable();
        let stacks = Self::assign(stacks, &mut next, "SPS stack count")?;

        let mut patterns = collector.patterns.into_iter().collect::<Vec<_>>();
        patterns.sort_unstable();
        let patterns = Self::assign(patterns, &mut next, "SPS pattern count")?;

        let scratch_stack = Self::take(&mut next, "SPS local count")?;
        let scratch_word = Self::take(&mut next, "SPS local count")?;
        let scratch_payload = Self::take(&mut next, "SPS local count")?;
        let result = Self::take(&mut next, "SPS local count")?;
        let scalar_base = next;
        for _ in 0..collector.scalar_count {
            Self::take(&mut next, "SPS scalar local count")?;
        }
        let local_count = next - 1;
        // One trailing i32 local follows the i64 locals to hold `*const WasmHostTransfer`.
        let transfer_ptr = next;

        Ok(Self {
            variables,
            values,
            stacks,
            patterns,
            scratch_stack,
            scratch_word,
            scratch_payload,
            result,
            transfer_ptr,
            scalar_base,
            local_count,
        })
    }

    fn assign<Id: Eq + std::hash::Hash>(
        ids: Vec<Id>, next: &mut u32, what: &'static str,
    ) -> Result<HashMap<Id, u32>, EmitError> {
        ids.into_iter().map(|id| Self::take(next, what).map(|local| (id, local))).collect()
    }

    fn take(next: &mut u32, what: &'static str) -> Result<u32, EmitError> {
        let local = *next;
        *next = next.checked_add(1).ok_or(WasmEmitError::Limit { what, value: usize::MAX })?;
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

    /// The pointer temporary shared by the word-emission sequences.
    fn pointer(&self) -> PointerLocal {
        PointerLocal(self.scratch_word)
    }
}

struct CaseNodeCollector<'a> {
    arena: &'a sps::SpsLowArena,
    labels: &'a HashMap<DefId, u32>,
    scalars: &'a ScalarPlans,
    defs: HashSet<DefId>,
    values: HashSet<ValueId>,
    stacks: HashSet<StackId>,
    patterns: HashSet<VPatId>,
    visited_compus: HashSet<CompuId>,
    scalar_count: usize,
}

impl CaseNodeCollector<'_> {
    fn visit_compu(&mut self, root: CompuId) {
        let mut work = vec![root];
        while let Some(id) = work.pop() {
            if !self.visited_compus.insert(id) {
                continue;
            }
            if let Some(call) = self.scalars.memory(id) {
                self.scalar_count = self.scalar_count.max(call.kernel.definitions());
                for binding in &call.address_bindings {
                    self.visit_value(binding.value);
                    self.visit_pattern(binding.binder);
                }
                for &input in &call.inputs {
                    self.visit_value(input);
                }
                self.visit_value(call.store_address);
                self.visit_value(call.load_address);
                work.push(call.next);
                continue;
            }
            match &self.arena.inner.compus[&id] {
                | Computation::Memory(sps::MemoryStep::Load { address, result, next, .. }) => {
                    self.visit_value(*address);
                    self.visit_pattern(*result);
                    work.push(*next);
                }
                | Computation::Memory(sps::MemoryStep::Store { address, value, next, .. }) => {
                    self.visit_value(*value);
                    self.visit_value(*address);
                    work.push(*next);
                }
                | Computation::Hole(sps::SHole(stack)) => {
                    self.visit_stack(*stack);
                }
                | Computation::Ext(sps::CompuExt::LetLabel(sps::LetLabel {
                    kind,
                    body,
                    tail,
                    ..
                })) => match kind {
                    | sps::LabelKind::Local => {
                        work.push(*body);
                        work.push(*tail);
                    }
                    | sps::LabelKind::Closed => work.push(*tail),
                },
                | Computation::Ext(sps::CompuExt::JumpLabel(sps::JumpLabel { stack, .. })) => {
                    self.visit_stack(*stack);
                }
                | Computation::Ext(sps::CompuExt::Jump(sps::Jump { target, argument, stack })) => {
                    self.visit_value(argument.word().1);
                    self.visit_stack(*stack);
                    self.visit_value(*target);
                }
                | Computation::ProductMatch(sps::SProductMatch { scrut, binder, body }) => {
                    self.visit_value(*scrut);
                    self.visit_pattern(*binder);
                    work.push(*body);
                }
                | Computation::Compare(sps::CompareBranch {
                    operands,
                    when_true,
                    when_false,
                    ..
                }) => {
                    for &operand in operands {
                        self.visit_value(operand);
                    }
                    work.push(*when_true);
                    work.push(*when_false);
                }
                | Computation::CoprodMatch(sps::SCoprodMatch { scrut, arms }) => {
                    self.visit_value(*scrut);
                    for arm in arms {
                        self.visit_pattern(arm.binder);
                        work.push(arm.tail);
                    }
                }
                | Computation::LetValue(sps::LetValue { binder, bindee, tail }) => {
                    if !self.scalars.elided(id) {
                        self.visit_value(*bindee);
                        self.visit_pattern(*binder);
                    }
                    work.push(*tail);
                }
                | Computation::LetStack(sps::LetStack { bindee, tail, .. })
                | Computation::LetArg(sps::LetArg { bindee, tail, .. }) => {
                    self.visit_stack(*bindee);
                    if let Computation::LetArg(sps::LetArg {
                        binder: sps::Cons(binder, _), ..
                    }) = &self.arena.inner.compus[&id]
                    {
                        self.visit_pattern(*binder);
                    }
                    work.push(*tail);
                }
                | Computation::CoCase(sps::SCoMatch { scrut, arms }) => {
                    self.visit_stack(*scrut);
                    for arm in arms {
                        work.push(arm.tail);
                    }
                }
                | Computation::Ext(sps::CompuExt::OpenClosure(sps::OpenClosure {
                    package,
                    environment,
                    code,
                    body,
                })) => {
                    self.visit_value(*package);
                    self.visit_pattern(*environment);
                    self.visit_pattern(*code);
                    work.push(*body);
                }
                | Computation::Ext(sps::CompuExt::OpenContinuation(sps::OpenContinuation {
                    package,
                    code,
                    body,
                })) => {
                    self.visit_stack(*package);
                    self.visit_pattern(*code);
                    work.push(*body);
                }
                | Computation::ExternCall(sps::ExternCall { stack, .. }) => {
                    self.visit_stack(*stack);
                }
            }
        }
    }

    fn visit_value(&mut self, root: ValueId) {
        let mut work = vec![root];
        while let Some(id) = work.pop() {
            if !self.values.insert(id) {
                continue;
            }
            match &self.arena.inner.values[&id] {
                | sps::Value::Var(def) => {
                    if !self.labels.contains_key(def) {
                        self.defs.insert(*def);
                    }
                }
                | sps::Value::Ext(sps::ValueExt::ClosurePackage(sps::ClosurePackage {
                    environment,
                    code,
                })) => {
                    work.push(*environment);
                    work.push(*code);
                }
                | sps::Value::Ctor(sps::Ctor(_, body)) => {
                    if self.arena.inner.nullary_ctor_tag(id).is_none() {
                        work.push(*body);
                    }
                }
                | sps::Value::VCons(sps::VCons { items, .. }) => {
                    work.extend(items.iter().copied());
                }
                | sps::Value::AddrOffset(sps::AddrOffset { base, displacement }) => {
                    work.push(*base);
                    work.push(*displacement);
                }
                | sps::Value::Primitive(_) => {
                    let call = self.scalars.call(id);
                    self.scalar_count = self.scalar_count.max(call.program.representations().len());
                    work.extend(call.inputs.iter().copied());
                }
                | sps::Value::Hole(_)
                | sps::Value::Triv(_)
                | sps::Value::Literal(_)
                | sps::Value::Ext(sps::ValueExt::Block(_)) => {}
            }
        }
    }

    fn visit_stack(&mut self, root: StackId) {
        let mut work = vec![root];
        while let Some(id) = work.pop() {
            if !self.stacks.insert(id) {
                continue;
            }
            match &self.arena.inner.stacks[&id] {
                | sps::Stack::Var(_) => {}
                | sps::Stack::Arg(sps::Cons(value, rest)) => {
                    self.visit_value(*value);
                    work.push(*rest);
                }
                | sps::Stack::Tag(sps::Cons(_, rest)) => {
                    work.push(*rest);
                }
                | sps::Stack::Ext(sps::StackExt::ContinuationPackage(
                    sps::ContinuationPackage { code, residual },
                )) => {
                    self.visit_value(*code);
                    work.push(*residual);
                }
            }
        }
    }

    fn visit_pattern(&mut self, root: VPatId) {
        let mut work = vec![root];
        while let Some(id) = work.pop() {
            if !self.patterns.insert(id) {
                continue;
            }
            match &self.arena.inner.vpats[&id] {
                | ValuePattern::Hole(_) | ValuePattern::Triv(_) => {}
                | ValuePattern::Var(def) => {
                    self.defs.insert(*def);
                }
                | ValuePattern::Ctor(sps::Ctor(_, body)) => {
                    work.push(*body);
                }
                | ValuePattern::Alias(sps::Alias(patterns)) => {
                    work.extend(patterns.iter().copied());
                }
                | ValuePattern::VCons(sps::VCons { items, .. }) => {
                    work.extend(items.iter().copied());
                }
            }
        }
    }
}

struct ModulePlan {
    cases: Vec<Case>,
    labels: HashMap<DefId, u32>,
    strings: HashMap<ValueId, StaticString>,
    static_data: Vec<u8>,
    strings_symbol: Option<DataSymbol>,
    host_imports: Vec<HostImport>,
    host_functions: HashMap<BuiltinValueRole, usize>,
    import_count: u32,
    scalars: ScalarPlans,
}

impl ModulePlan {
    fn new(program: &SpsLowProgram, boxing: ScalarBoxing) -> Result<Self, EmitError> {
        let arena = program.arena();
        let mut blocks = arena
            .inner
            .values
            .iter()
            .filter_map(|(_, value)| match value {
                | sps::Value::Ext(sps::ValueExt::Block(block)) => Some(block.clone()),
                | _ => None,
            })
            .collect::<Vec<_>>();
        blocks.sort_by_key(|block| block.label);

        // A closed label reads no variable, so it runs as a case of its own like a block.
        let mut closed_labels = arena
            .inner
            .compus
            .iter()
            .filter_map(|(_, compu)| match compu {
                | Computation::Ext(sps::CompuExt::LetLabel(sps::LetLabel {
                    label,
                    kind: sps::LabelKind::Closed,
                    body,
                    ..
                })) => Some((*label, *body)),
                | _ => None,
            })
            .collect::<Vec<_>>();
        closed_labels.sort_by_key(|(label, _)| *label);

        let root = Case { index: 0, label: None, entry: None, body: program.root() };
        let block_cases = blocks
            .into_iter()
            .map(|Block { label, entry, body }| (label, Some(entry), body))
            .chain(closed_labels.into_iter().map(|(label, body)| (label, None, body)))
            .enumerate()
            .map(|(offset, (label, entry, body))| {
                let index = Limits::u32(offset + 1, "SPS block count")?;
                Ok(Case { index, label: Some(label), entry, body })
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
                matches!(value, sps::Value::Literal(Literal::String(_))).then_some(id)
            })
            .collect::<Vec<_>>();
        string_values.sort_unstable();
        let string_entries = string_values.into_iter().map(|value| {
            let sps::Value::Literal(Literal::String(string)) = &arena.inner.values[&value] else {
                unreachable!("string value collection changed during planning")
            };
            (value, string.clone())
        });
        let (static_data, strings) = StringTable::build(string_entries)?;
        let strings_symbol = (!static_data.is_empty()).then_some(DataSymbol(1));

        let first_host_function = WasmRuntimeFailure::IMPORT_COUNT;
        let builtins = arena
            .inner
            .compus
            .iter()
            .filter_map(|(_, compu)| match compu {
                | sps::Computation::ExternCall(sps::ExternCall {
                    function: sps::ExternalFunction::Host(role),
                    ..
                }) => Some(*role),
                | _ => None,
            })
            .collect::<std::collections::BTreeSet<_>>();
        let host_imports = builtins
            .iter()
            .copied()
            .enumerate()
            .map(|(offset, role)| {
                let offset = Limits::u32(offset, "host import count")?;
                Ok(HostImport {
                    function: FuncRef(first_host_function + offset),
                    name: format!("zydeco_{}", role.host_name()),
                    arity: role.arity(),
                    mode: match role.call_mode() {
                        | HostCallMode::Returning => HostCallKind::Returning,
                        | HostCallMode::Control => HostCallKind::Control,
                    },
                    spare: role.spare_box(),
                })
            })
            .collect::<Result<Vec<_>, EmitError>>()?;
        let host_functions =
            builtins.into_iter().enumerate().map(|(index, role)| (role, index)).collect();
        let import_count = first_host_function
            .checked_add(Limits::u32(host_imports.len(), "host import count")?)
            .ok_or(WasmEmitError::Limit { what: "host import count", value: host_imports.len() })?;
        let scalars = ScalarPlans::new(program, boxing);

        Ok(Self {
            cases,
            labels,
            strings,
            static_data,
            strings_symbol,
            host_imports,
            host_functions,
            import_count,
            scalars,
        })
    }

    fn alloc_function(&self) -> FuncRef {
        WasmRuntimeFailure::ALLOC_FUNCTION
    }

    fn pair_function(&self) -> FuncRef {
        FuncRef(self.import_count)
    }

    fn case_function(&self, index: u32) -> FuncRef {
        FuncRef(self.import_count + 1 + index)
    }

    fn entry_function(&self) -> Result<FuncRef, EmitError> {
        Ok(FuncRef(self.import_count + 1 + Limits::u32(self.cases.len(), "SPS block count")?))
    }

    fn label(&self, label: DefId) -> Result<u32, EmitError> {
        self.labels.get(&label).copied().ok_or(EmitError::MissingBlock(label))
    }

    fn string(&self, value: ValueId) -> Result<StaticString, EmitError> {
        self.strings.get(&value).copied().ok_or(EmitError::MissingString(value))
    }

    fn host(&self, role: BuiltinValueRole) -> Result<&HostImport, EmitError> {
        self.host_functions
            .get(&role)
            .map(|index| &self.host_imports[*index])
            .ok_or(EmitError::MissingHostImport(role))
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
        let mut builder = WasmObjectBuilder::new();
        builder.types_mut().ty().function([ValType::I64], []);
        builder.types_mut().ty().function([ValType::I64, ValType::I64], [ValType::I64]);
        builder.types_mut().ty().function([], []);
        HostSections::append_imports(&mut builder, FIRST_IMPORT_TYPE, &self.plan.host_imports);

        let pair_ref = builder.add_function("sps.pair", false, PAIR_TYPE, self.pair_function());
        debug_assert_eq!(pair_ref, self.plan.pair_function());

        let case_count = Limits::u32(self.plan.cases.len(), "SPS block count")?;
        let cases_segment =
            builder.add_data_segment(".rodata.cases", 2, vec![0_u8; 4 * case_count as usize]);
        debug_assert_eq!(cases_segment, CASES_SYMBOL);
        if let Some(strings_symbol) = self.plan.strings_symbol {
            let assigned =
                builder.add_data_segment(".rodata.strings", 3, self.plan.static_data.clone());
            debug_assert_eq!(assigned, strings_symbol);
        }

        for case in &self.plan.cases {
            let body = CaseEncoder::new(self.program, &self.plan, *case)?.encode(*case)?;
            let name = case
                .label
                .map_or_else(|| "sps.root".to_owned(), |_| format!("sps.case.{}", case.index));
            let case_ref = builder.add_function(name, false, CASE_TYPE, body);
            debug_assert_eq!(case_ref, self.plan.case_function(case.index));
            builder.add_table_reloc(cases_segment, case.index * 4, case_ref);
        }

        let entry_ref = builder.add_function(ENTRY_SYMBOL, true, ENTRY_TYPE, self.entry_function());
        debug_assert_eq!(entry_ref, self.plan.entry_function()?);

        Ok(WasmModule::new(builder.finish()?))
    }

    fn pair_function(&self) -> RelocFunction {
        let mut function = RelocFunction::new([(1, ValType::I32)]);
        function.instruction(&WasmInstruction::I32Const(2));
        function.call(self.plan.alloc_function());
        function.instruction(&WasmInstruction::LocalTee(2));
        function.instruction(&WasmInstruction::LocalGet(0));
        function.instruction(&WasmInstruction::I64Store(WORD_MEMORY));
        function.instruction(&WasmInstruction::LocalGet(2));
        function.instruction(&WasmInstruction::LocalGet(1));
        function.instruction(&WasmInstruction::I64Store(ProductFields::word_at_const(1)));
        function.instruction(&WasmInstruction::LocalGet(2));
        function.instruction(&WasmInstruction::I64ExtendI32U);
        function.instruction(&WasmInstruction::End);
        function
    }

    fn entry_function(&self) -> RelocFunction {
        let mut function = RelocFunction::new([]);
        function.instruction(&WasmInstruction::I64Const(0));
        function.return_call(self.plan.case_function(0));
        function.instruction(&WasmInstruction::End);
        function
    }
}

struct CaseEncoder<'a> {
    arena: &'a sps::SpsLowArena,
    plan: &'a ModulePlan,
    locals: LocalPlan,
    function: RelocFunction,
    /// The number of structured control blocks around the current instruction.
    depth: u32,
    /// The depth inside the `block` of each local label in scope; jumps branch out of it.
    local_labels: HashMap<DefId, u32>,
}

impl<'a> CaseEncoder<'a> {
    fn new(
        program: &'a SpsLowProgram, plan: &'a ModulePlan, case: Case,
    ) -> Result<Self, EmitError> {
        let locals = LocalPlan::for_case(program.arena(), &plan.labels, &plan.scalars, case)?;
        let function = RelocFunction::new([(locals.local_count, ValType::I64), (1, ValType::I32)]);
        Ok(Self {
            arena: program.arena(),
            plan,
            locals,
            function,
            depth: 0,
            local_labels: HashMap::new(),
        })
    }

    /// Emit a computation nested in one more structured control block.
    fn emit_nested(&mut self, id: CompuId) -> Result<(), EmitError> {
        self.depth += 1;
        let result = self.emit_compu(id);
        self.depth -= 1;
        result
    }

    fn encode(mut self, case: Case) -> Result<RelocFunction, EmitError> {
        if let Some(entry) = case.entry {
            for (_, pattern) in entry.words() {
                self.function.instruction(&WasmInstruction::LocalGet(0));
                self.consume_argument(pattern)?;
            }
        }
        self.emit_compu(case.body)?;
        self.function.instruction(&WasmInstruction::End);
        Ok(self.function)
    }

    fn consume_argument(&mut self, binder: VPatId) -> Result<(), EmitError> {
        self.function.instruction(&WasmInstruction::LocalSet(self.locals.scratch_stack));
        self.load_local_word(self.locals.scratch_stack, 0);
        self.emit_pattern(binder)?;
        self.load_local_word(self.locals.scratch_stack, 1);
        self.function.instruction(&WasmInstruction::LocalSet(0));
        Ok(())
    }

    // One arm per computation form is the clearest shape for this emitter.
    #[allow(clippy::too_many_lines)]
    fn emit_compu(&mut self, mut id: CompuId) -> Result<(), EmitError> {
        zydeco_foundation::stack::maybe_grow(|| {
            loop {
                if let Some(call) = self.plan.scalars.memory(id).cloned() {
                    self.emit_memory_kernel(&call)?;
                    id = call.next;
                    continue;
                }

                match self.arena.inner.compus[&id].clone() {
                    | Computation::Memory(sps::MemoryStep::Load {
                        scalar,
                        address,
                        result,
                        next,
                    }) => {
                        self.emit_value(address)?;
                        {
                            let mut words =
                                WordEmitter::new(&mut self.function, self.plan.alloc_function());
                            words.raw_load(scalar);
                        }
                        self.function.instruction(&WasmInstruction::LocalSet(self.locals.result));
                        WordEmitter::new(&mut self.function, self.plan.alloc_function())
                            .memory_encode(scalar, self.locals.result, self.locals.pointer());
                        self.emit_pattern(result)?;
                        id = next;
                    }
                    | Computation::Memory(sps::MemoryStep::Store {
                        scalar,
                        address,
                        value,
                        next,
                    }) => {
                        self.emit_value(value)?;
                        self.function.instruction(&WasmInstruction::Drop);
                        self.emit_value(address)?;
                        let local = self.locals.value(value)?;
                        WordEmitter::new(&mut self.function, self.plan.alloc_function())
                            .memory_decode(scalar, local);
                        self.function.instruction(&WasmInstruction::LocalGet(local));
                        WordEmitter::new(&mut self.function, self.plan.alloc_function())
                            .raw_store(scalar, self.locals.scratch_word);
                        id = next;
                    }
                    | Computation::Hole(sps::SHole(stack)) => {
                        self.emit_stack(stack)?;
                        self.function.instruction(&WasmInstruction::Drop);
                        self.function.instruction(&WasmInstruction::Unreachable);
                        break;
                    }
                    | Computation::Ext(sps::CompuExt::LetLabel(sps::LetLabel {
                        label,
                        kind,
                        recursive,
                        body,
                        tail,
                    })) => match kind {
                        // A local label's tail runs inside a `block`; a jump branches out of it into
                        // the label body that follows, keeping this function's locals as its frame.
                        | sps::LabelKind::Local if !recursive => {
                            self.function.instruction(&WasmInstruction::Block(BlockType::Empty));
                            self.local_labels.insert(label, self.depth + 1);
                            self.emit_nested(tail)?;
                            self.function.instruction(&WasmInstruction::End);
                            self.local_labels.remove(&label);
                            id = body;
                        }
                        // A recursive local label enters a structured `loop`, so self-jumps
                        // branch back to the loop header while keeping enclosing frame locals.
                        | sps::LabelKind::Local => {
                            self.function.instruction(&WasmInstruction::Block(BlockType::Empty));
                            self.local_labels.insert(label, self.depth + 1);
                            self.emit_nested(tail)?;
                            self.function.instruction(&WasmInstruction::End);
                            self.function.instruction(&WasmInstruction::Loop(BlockType::Empty));
                            self.local_labels.insert(label, self.depth + 1);
                            self.emit_nested(body)?;
                            self.function.instruction(&WasmInstruction::End);
                            self.local_labels.remove(&label);
                            break;
                        }
                        | sps::LabelKind::Closed => id = tail,
                    },
                    | Computation::Ext(sps::CompuExt::JumpLabel(sps::JumpLabel {
                        label,
                        stack,
                    })) => {
                        self.emit_stack(stack)?;
                        if let Some(depth) = self.local_labels.get(&label) {
                            self.function.instruction(&WasmInstruction::LocalSet(0));
                            self.function.instruction(&WasmInstruction::Br(self.depth - depth));
                        } else {
                            self.function
                                .return_call(self.plan.case_function(self.plan.label(label)?));
                        }
                        break;
                    }
                    | Computation::Ext(sps::CompuExt::Jump(sps::Jump {
                        target,
                        argument,
                        stack,
                    })) => {
                        self.emit_value(argument.word().1)?;
                        self.emit_stack(stack)?;
                        self.function.call(self.plan.pair_function());
                        self.emit_value(target)?;
                        self.emit_indirect_tail_call();
                        break;
                    }
                    | Computation::ProductMatch(sps::SProductMatch { scrut, binder, body }) => {
                        self.emit_value(scrut)?;
                        self.emit_pattern(binder)?;
                        id = body;
                    }
                    | Computation::Compare(sps::CompareBranch {
                        operation,
                        operands,
                        when_true,
                        when_false,
                    }) => {
                        for operand in operands.into_iter().rev() {
                            self.emit_value(operand)?;
                            self.function.instruction(&WasmInstruction::Drop);
                        }
                        WordEmitter::new(&mut self.function, self.plan.alloc_function())
                            .comparison(
                                operation,
                                self.locals.value(operands[0])?,
                                self.locals.value(operands[1])?,
                            );
                        self.function.instruction(&WasmInstruction::If(BlockType::Empty));
                        self.emit_nested(when_true)?;
                        self.function.instruction(&WasmInstruction::Else);
                        self.emit_nested(when_false)?;
                        self.function.instruction(&WasmInstruction::End);
                        break;
                    }
                    | Computation::CoprodMatch(sps::SCoprodMatch { scrut, arms }) => {
                        self.emit_coprod_match(scrut, arms)?;
                        break;
                    }
                    | Computation::LetValue(sps::LetValue { binder, bindee, tail: body }) => {
                        if !self.plan.scalars.elided(id) {
                            self.emit_value(bindee)?;
                            self.emit_pattern(binder)?;
                        }
                        id = body;
                    }
                    | Computation::LetStack(sps::LetStack {
                        binder: sps::StackVar,
                        bindee,
                        tail: body,
                    }) => {
                        self.emit_stack(bindee)?;
                        self.function.instruction(&WasmInstruction::LocalSet(0));
                        id = body;
                    }
                    | Computation::LetArg(sps::LetArg {
                        binder: sps::Cons(binder, sps::StackVar),
                        bindee,
                        tail: body,
                    }) => {
                        self.emit_stack(bindee)?;
                        self.consume_argument(binder)?;
                        id = body;
                    }
                    | Computation::CoCase(sps::SCoMatch { scrut, arms }) => {
                        self.emit_stack(scrut)?;
                        self.function
                            .instruction(&WasmInstruction::LocalSet(self.locals.scratch_stack));
                        self.load_local_word(self.locals.scratch_stack, 0);
                        self.function
                            .instruction(&WasmInstruction::LocalSet(self.locals.scratch_word));
                        self.load_local_word(self.locals.scratch_stack, 1);
                        self.function.instruction(&WasmInstruction::LocalSet(0));
                        for sps::CoMatcher { dtor: sps::Cons(dtor, sps::StackVar), tail } in arms {
                            self.function
                                .instruction(&WasmInstruction::LocalGet(self.locals.scratch_word));
                            self.function.instruction(&WasmInstruction::I64Const(
                                RuntimeWord::index(dtor.idx)? as i64,
                            ));
                            self.function.instruction(&WasmInstruction::I64Eq);
                            self.function.instruction(&WasmInstruction::If(BlockType::Empty));
                            self.emit_nested(tail)?;
                            self.function.instruction(&WasmInstruction::End);
                        }
                        self.function.instruction(&WasmInstruction::Unreachable);
                        break;
                    }
                    | Computation::Ext(sps::CompuExt::OpenClosure(sps::OpenClosure {
                        package,
                        environment,
                        code,
                        body,
                    })) => {
                        self.emit_value(package)?;
                        self.function
                            .instruction(&WasmInstruction::LocalSet(self.locals.scratch_word));
                        self.load_local_word(self.locals.scratch_word, 0);
                        self.emit_pattern(environment)?;
                        self.load_local_word(self.locals.scratch_word, 1);
                        self.emit_pattern(code)?;
                        id = body;
                    }
                    | Computation::Ext(sps::CompuExt::OpenContinuation(
                        sps::OpenContinuation { package, code, body },
                    )) => {
                        self.emit_stack(package)?;
                        self.function
                            .instruction(&WasmInstruction::LocalSet(self.locals.scratch_stack));
                        self.load_local_word(self.locals.scratch_stack, 0);
                        self.emit_pattern(code)?;
                        self.load_local_word(self.locals.scratch_stack, 1);
                        self.function.instruction(&WasmInstruction::LocalSet(0));
                        id = body;
                    }
                    | Computation::ExternCall(sps::ExternCall { function, stack }) => {
                        match function {
                            | sps::ExternalFunction::Host(function) => {
                                self.emit_extern(function, stack)?
                            }
                            | sps::ExternalFunction::Foreign(import) => {
                                return Err(EmitError::UnsupportedForeignImport(
                                    import.target.symbol.to_string(),
                                ));
                            }
                            | sps::ExternalFunction::Unit(import) => {
                                return Err(EmitError::UnsupportedForeignImport(
                                    import.target.symbol.to_string(),
                                ));
                            }
                        }
                        break;
                    }
                }
            }
            Ok(())
        })
    }

    fn emit_coprod_match(
        &mut self, scrut: ValueId, arms: Vec<sps::Matcher<VPatId, CompuId>>,
    ) -> Result<(), EmitError> {
        self.emit_value(scrut)?;
        self.function.instruction(&WasmInstruction::Drop);
        let scrut_local = self.locals.value(scrut)?;
        let constructors = arms
            .iter()
            .all(|arm| matches!(self.arena.inner.vpats[&arm.binder], ValuePattern::Ctor(_)));
        if constructors {
            // Immediate nullary constructors encode `RuntimeWord::index(tag)` with low bit 1;
            // non-nullary constructors are 8-byte aligned heap pointers with low bit 0.
            let tag_local = self.locals.scratch_word;
            let payload_local = self.locals.scratch_payload;
            self.function.instruction(&WasmInstruction::LocalGet(scrut_local));
            self.function.instruction(&WasmInstruction::I64Const(1));
            self.function.instruction(&WasmInstruction::I64And);
            self.function.instruction(&WasmInstruction::I64Eqz);
            self.function.instruction(&WasmInstruction::If(BlockType::Empty));
            self.load_local_word(scrut_local, 0);
            self.function.instruction(&WasmInstruction::LocalSet(tag_local));
            self.load_local_word(scrut_local, 1);
            self.function.instruction(&WasmInstruction::LocalSet(payload_local));
            self.function.instruction(&WasmInstruction::Else);
            self.function.instruction(&WasmInstruction::LocalGet(scrut_local));
            self.function.instruction(&WasmInstruction::LocalSet(tag_local));
            self.function.instruction(&WasmInstruction::I64Const(RuntimeWord::index(0)? as i64));
            self.function.instruction(&WasmInstruction::LocalSet(payload_local));
            self.function.instruction(&WasmInstruction::End);

            for sps::Matcher { binder, tail } in arms {
                let ValuePattern::Ctor(sps::Ctor(ctor, body)) =
                    self.arena.inner.vpats[&binder].clone()
                else {
                    unreachable!("constructor-arm check changed during emission")
                };
                self.function.instruction(&WasmInstruction::LocalGet(tag_local));
                self.function
                    .instruction(&WasmInstruction::I64Const(RuntimeWord::index(ctor.idx)? as i64));
                self.function.instruction(&WasmInstruction::I64Eq);
                self.function.instruction(&WasmInstruction::If(BlockType::Empty));
                self.function.instruction(&WasmInstruction::LocalGet(payload_local));
                self.emit_pattern(body)?;
                self.emit_nested(tail)?;
                self.function.instruction(&WasmInstruction::End);
            }
            WasmRuntimeFailure::emit(RuntimeFailure::PatternMatch, &mut self.function);
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
        zydeco_foundation::stack::maybe_grow(|| {
            let target = self.locals.value(id)?;
            match self.arena.inner.values[&id].clone() {
                | sps::Value::Hole(_) => {
                    self.function.instruction(&WasmInstruction::Unreachable);
                }
                | sps::Value::Var(def) => {
                    if let Some(index) = self.plan.labels.get(&def) {
                        self.function
                            .instruction(&WasmInstruction::I64Const(
                                RuntimeWord::code(*index) as i64
                            ));
                    } else {
                        self.function
                            .instruction(&WasmInstruction::LocalGet(self.locals.variable(def)?));
                    }
                }
                | sps::Value::Ext(sps::ValueExt::Block(Block { label, .. })) => {
                    self.function
                        .instruction(&WasmInstruction::I64Const(RuntimeWord::code(
                            self.plan.label(label)?,
                        ) as i64));
                }
                | sps::Value::Ext(sps::ValueExt::ClosurePackage(sps::ClosurePackage {
                    environment,
                    code,
                })) => {
                    self.emit_value(environment)?;
                    self.emit_value(code)?;
                    self.function.call(self.plan.pair_function());
                }
                | sps::Value::Ctor(sps::Ctor(ctor, body)) => {
                    if let Some(tag) = self.arena.inner.nullary_ctor_tag(id) {
                        self.function
                            .instruction(&WasmInstruction::I64Const(
                                RuntimeWord::index(tag)? as i64
                            ));
                    } else {
                        self.function
                            .instruction(&WasmInstruction::I64Const(
                                RuntimeWord::index(ctor.idx)? as i64
                            ));
                        self.emit_value(body)?;
                        self.function.call(self.plan.pair_function());
                    }
                }
                | sps::Value::Triv(_) => {
                    self.function
                        .instruction(&WasmInstruction::I64Const(RuntimeWord::index(0)? as i64));
                }
                | sps::Value::VCons(sps::VCons { items, layout }) => {
                    self.emit_product(target, items, layout)?;
                }
                | sps::Value::Literal(literal) => self.emit_literal(id, literal)?,
                | sps::Value::AddrOffset(sps::AddrOffset { base, displacement }) => {
                    self.emit_value(displacement)?;
                    self.function.instruction(&WasmInstruction::I64Const(1));
                    self.function.instruction(&WasmInstruction::I64ShrS);
                    self.emit_value(base)?;
                    self.function.instruction(&WasmInstruction::I64Add);
                }
                | sps::Value::Primitive(_) => {
                    self.emit_scalar(id)?;
                }
            }
            self.function.instruction(&WasmInstruction::LocalTee(target));
            Ok(())
        })
    }

    fn emit_product(
        &mut self, target: u32, items: Vec<ValueId>, layout: ProductLayout,
    ) -> Result<(), EmitError> {
        let arity = Limits::u32(layout.arity, "product arity")?;
        self.function.instruction(&WasmInstruction::I32Const(arity as i32));
        self.function.call(self.plan.alloc_function());
        self.function.instruction(&WasmInstruction::I64ExtendI32U);
        self.function.instruction(&WasmInstruction::LocalSet(target));

        let explicit = items.len();
        for (index, value) in items.into_iter().enumerate() {
            if index + 1 == explicit && explicit < layout.arity {
                self.emit_value(value)?;
                self.function.instruction(&WasmInstruction::LocalSet(self.locals.scratch_word));
                for destination in index..layout.arity {
                    self.function.instruction(&WasmInstruction::LocalGet(target));
                    self.function.instruction(&WasmInstruction::I32WrapI64);
                    self.load_local_word(self.locals.scratch_word, destination - index);
                    self.function.instruction(&WasmInstruction::I64Store(ProductFields::word_at(
                        destination,
                    )?));
                }
            } else {
                self.function.instruction(&WasmInstruction::LocalGet(target));
                self.function.instruction(&WasmInstruction::I32WrapI64);
                self.emit_value(value)?;
                self.function
                    .instruction(&WasmInstruction::I64Store(ProductFields::word_at(index)?));
            }
        }
        self.function.instruction(&WasmInstruction::LocalGet(target));
        Ok(())
    }

    fn emit_literal(&mut self, id: ValueId, literal: Literal) -> Result<(), EmitError> {
        match literal {
            | Literal::Integer(integer) => match integer.encode_runtime()? {
                | EncodedScalar::Immediate(word) => {
                    self.function.instruction(&WasmInstruction::I64Const(word as i64));
                }
                | EncodedScalar::Boxed(bits) => self.emit_boxed(bits),
            },
            | Literal::Float(float) => match float.encode_runtime() {
                | EncodedScalar::Immediate(word) => {
                    self.function.instruction(&WasmInstruction::I64Const(word as i64));
                }
                | EncodedScalar::Boxed(bits) => self.emit_boxed(bits),
            },
            | Literal::String(_) => {
                let string = self.plan.string(id)?;
                let strings = self.plan.strings_symbol.ok_or(EmitError::MissingString(id))?;
                self.function.i32_const_memory(strings, string.offset as i32);
                self.function.instruction(&WasmInstruction::I64ExtendI32U);
            }
            | Literal::Char(character) => {
                self.function.instruction(&WasmInstruction::I64Const(RuntimeWord::index(
                    character as usize,
                )? as i64));
            }
        }
        Ok(())
    }

    fn emit_boxed(&mut self, bits: u64) {
        WordEmitter::new(&mut self.function, self.plan.alloc_function())
            .boxed(bits, self.locals.pointer());
    }

    fn emit_stack(&mut self, id: StackId) -> Result<(), EmitError> {
        zydeco_foundation::stack::maybe_grow(|| {
            let target = self.locals.stack(id)?;
            match self.arena.inner.stacks[&id].clone() {
                | sps::Stack::Var(sps::StackVar) => {
                    self.function.instruction(&WasmInstruction::LocalGet(0));
                }
                | sps::Stack::Arg(sps::Cons(value, stack)) => {
                    self.emit_value(value)?;
                    self.emit_stack(stack)?;
                    self.function.call(self.plan.pair_function());
                }
                | sps::Stack::Tag(sps::Cons(dtor, stack)) => {
                    self.function
                        .instruction(&WasmInstruction::I64Const(
                            RuntimeWord::index(dtor.idx)? as i64
                        ));
                    self.emit_stack(stack)?;
                    self.function.call(self.plan.pair_function());
                }
                | sps::Stack::Ext(sps::StackExt::ContinuationPackage(
                    sps::ContinuationPackage { code, residual },
                )) => {
                    self.emit_value(code)?;
                    self.emit_stack(residual)?;
                    self.function.call(self.plan.pair_function());
                }
            }
            self.function.instruction(&WasmInstruction::LocalTee(target));
            Ok(())
        })
    }

    fn emit_pattern(&mut self, id: VPatId) -> Result<(), EmitError> {
        zydeco_foundation::stack::maybe_grow(|| {
            let bindee = self.locals.pattern(id)?;
            self.function.instruction(&WasmInstruction::LocalSet(bindee));
            match self.arena.inner.vpats[&id].clone() {
                | ValuePattern::Hole(_) | ValuePattern::Triv(_) => {}
                | ValuePattern::Var(def) => {
                    self.function.instruction(&WasmInstruction::LocalGet(bindee));
                    self.function
                        .instruction(&WasmInstruction::LocalSet(self.locals.variable(def)?));
                }
                | ValuePattern::Ctor(sps::Ctor(ctor, body)) => {
                    let expected_tag = RuntimeWord::index(ctor.idx)? as i64;
                    self.function.instruction(&WasmInstruction::LocalGet(bindee));
                    self.function.instruction(&WasmInstruction::I64Const(1));
                    self.function.instruction(&WasmInstruction::I64And);
                    self.function.instruction(&WasmInstruction::I64Eqz);
                    self.function
                        .instruction(&WasmInstruction::If(BlockType::Result(ValType::I64)));
                    self.load_local_word(bindee, 0);
                    self.function.instruction(&WasmInstruction::I64Const(expected_tag));
                    self.function.instruction(&WasmInstruction::I64Ne);
                    self.function.instruction(&WasmInstruction::If(BlockType::Empty));
                    self.function.instruction(&WasmInstruction::Unreachable);
                    self.function.instruction(&WasmInstruction::End);
                    self.load_local_word(bindee, 1);
                    self.function.instruction(&WasmInstruction::Else);
                    self.function.instruction(&WasmInstruction::LocalGet(bindee));
                    self.function.instruction(&WasmInstruction::I64Const(expected_tag));
                    self.function.instruction(&WasmInstruction::I64Ne);
                    self.function.instruction(&WasmInstruction::If(BlockType::Empty));
                    self.function.instruction(&WasmInstruction::Unreachable);
                    self.function.instruction(&WasmInstruction::End);
                    self.function
                        .instruction(&WasmInstruction::I64Const(RuntimeWord::index(0)? as i64));
                    self.function.instruction(&WasmInstruction::End);
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
                                ProductFields::byte_offset(index)?,
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
        })
    }

    fn emit_extern(&mut self, role: BuiltinValueRole, stack: StackId) -> Result<(), EmitError> {
        let import = self.plan.host(role)?.clone();
        self.emit_stack(stack)?;
        self.function.instruction(&WasmInstruction::LocalSet(self.locals.scratch_stack));
        for _ in 0..import.arity {
            self.load_local_word(self.locals.scratch_stack, 0);
            self.load_local_word(self.locals.scratch_stack, 1);
            self.function.instruction(&WasmInstruction::LocalSet(self.locals.scratch_stack));
        }
        WordEmitter::new(&mut self.function, self.plan.alloc_function()).spare_box(import.spare);
        self.function.call(import.function);
        match import.mode {
            | HostCallKind::Returning => self.resume_returning_call(),
            | HostCallKind::Control => self.resume_control_call(),
        }
        Ok(())
    }

    fn resume_returning_call(&mut self) {
        self.function.instruction(&WasmInstruction::LocalSet(self.locals.result));
        self.function.instruction(&WasmInstruction::LocalGet(self.locals.result));
        self.load_local_word(self.locals.scratch_stack, 1);
        self.function.call(self.plan.pair_function());
        self.load_local_word(self.locals.scratch_stack, 0);
        self.emit_indirect_tail_call();
    }

    fn resume_control_call(&mut self) {
        self.function.instruction(&WasmInstruction::LocalSet(self.locals.transfer_ptr));

        self.load_transfer_word(0);
        self.function.instruction(&WasmInstruction::I64Const(2));
        self.function.instruction(&WasmInstruction::I64GtU);
        self.function.instruction(&WasmInstruction::If(BlockType::Empty));
        self.function.instruction(&WasmInstruction::Unreachable);
        self.function.instruction(&WasmInstruction::End);

        self.load_transfer_word(0);
        self.function.instruction(&WasmInstruction::I64Const(2));
        self.function.instruction(&WasmInstruction::I64Eq);
        self.function.instruction(&WasmInstruction::If(BlockType::Empty));
        self.load_transfer_word(24);
        self.function.instruction(&WasmInstruction::LocalGet(self.locals.scratch_stack));
        self.function.call(self.plan.pair_function());
        self.function.instruction(&WasmInstruction::LocalSet(self.locals.scratch_stack));
        self.function.instruction(&WasmInstruction::End);

        self.load_transfer_word(0);
        self.function.instruction(&WasmInstruction::I64Eqz);
        self.function.instruction(&WasmInstruction::I32Eqz);
        self.function.instruction(&WasmInstruction::If(BlockType::Empty));
        self.load_transfer_word(16);
        self.function.instruction(&WasmInstruction::LocalGet(self.locals.scratch_stack));
        self.function.call(self.plan.pair_function());
        self.function.instruction(&WasmInstruction::LocalSet(self.locals.scratch_stack));
        self.function.instruction(&WasmInstruction::End);

        self.load_transfer_word(8);
        self.function.instruction(&WasmInstruction::LocalSet(self.locals.scratch_word));
        self.load_local_word(self.locals.scratch_word, 0);
        self.function.instruction(&WasmInstruction::LocalGet(self.locals.scratch_stack));
        self.function.call(self.plan.pair_function());
        self.load_local_word(self.locals.scratch_word, 1);
        self.emit_indirect_tail_call();
    }

    fn emit_memory_kernel(
        &mut self, call: &zydeco_stackir::low::scalar::MemoryCall,
    ) -> Result<(), EmitError> {
        let scalar = call.kernel.scalar();
        for binding in &call.address_bindings {
            self.emit_value(binding.value)?;
            self.emit_pattern(binding.binder)?;
        }
        for &input in call.inputs.iter().rev() {
            self.emit_value(input)?;
            self.function.instruction(&WasmInstruction::Drop);
        }
        self.emit_value(call.store_address)?;
        self.function.instruction(&WasmInstruction::Drop);
        self.emit_value(call.load_address)?;
        WordEmitter::new(&mut self.function, self.plan.alloc_function()).raw_load(scalar);
        self.function.instruction(&WasmInstruction::LocalSet(self.locals.scalar_base));
        for (index, &input) in call.inputs.iter().enumerate() {
            let local = self.locals.scalar_base + index as u32 + 1;
            self.function.instruction(&WasmInstruction::LocalGet(self.locals.value(input)?));
            self.function.instruction(&WasmInstruction::LocalSet(local));
            WordEmitter::new(&mut self.function, self.plan.alloc_function())
                .memory_decode(call.kernel.region().inputs[index + 1].into(), local);
        }
        self.function
            .instruction(&WasmInstruction::LocalGet(self.locals.value(call.store_address)?));
        WordEmitter::new(&mut self.function, self.plan.alloc_function())
            .scalar_kernel(&call.kernel, self.locals.scalar_base);
        WordEmitter::new(&mut self.function, self.plan.alloc_function())
            .raw_store(scalar, self.locals.scratch_word);
        Ok(())
    }

    fn emit_scalar(&mut self, id: ValueId) -> Result<(), EmitError> {
        let call = self.plan.scalars.call(id).clone();
        // Match the value evaluation order used by Stack IR and ZASM lowering.
        for &operand in call.inputs.iter().rev() {
            self.emit_value(operand)?;
            self.function.instruction(&WasmInstruction::Drop);
        }
        for (index, operand) in call.inputs.into_iter().enumerate() {
            self.function.instruction(&WasmInstruction::LocalGet(self.locals.value(operand)?));
            self.function
                .instruction(&WasmInstruction::LocalSet(self.locals.scalar_base + index as u32));
        }
        WordEmitter::new(&mut self.function, self.plan.alloc_function()).scalar_region(
            &call.program,
            self.locals.scalar_base,
            self.locals.pointer(),
        );
        Ok(())
    }

    fn load_local_word(&mut self, local: u32, index: usize) {
        self.function.instruction(&WasmInstruction::LocalGet(local));
        self.function.instruction(&WasmInstruction::I32WrapI64);
        self.function
            .instruction(&WasmInstruction::I64Load(ProductFields::word_at_const(index as u32)));
    }

    fn load_transfer_word(&mut self, offset: u64) {
        self.function.instruction(&WasmInstruction::LocalGet(self.locals.transfer_ptr));
        self.function.instruction(&WasmInstruction::I64Load(MemArg { offset, ..WORD_MEMORY }));
    }

    fn emit_indirect_tail_call(&mut self) {
        self.function.instruction(&WasmInstruction::I64Const(1));
        self.function.instruction(&WasmInstruction::I64ShrU);
        self.function.instruction(&WasmInstruction::I32WrapI64);
        self.function.instruction(&WasmInstruction::I32Const(2));
        self.function.instruction(&WasmInstruction::I32Shl);
        self.function.i32_const_memory(CASES_SYMBOL, 0);
        self.function.instruction(&WasmInstruction::I32Add);
        self.function.instruction(&WasmInstruction::I32Load(CASES_ALIGNMENT));
        self.function.return_call_indirect(CASE_TYPE, TableSymbol);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasmparser::{Parser, Payload, Validator};
    use zydeco_stackir::{SpsScheme, arena::Construct as _, low::SpsLowArena};

    struct Fixture {
        arena: SpsLowArena,
    }

    impl Fixture {
        fn new() -> Self {
            Self { arena: SpsLowArena::default() }
        }

        fn hole(&mut self) -> CompuId {
            let stack = sps::StackVar.build(&mut self.arena, None);
            sps::SHole(stack).build(&mut self.arena, None)
        }

        fn closed_let_chain(mut self, bindings: usize) -> SpsLowProgram {
            let mut body = self.hole();
            for _ in 0..bindings {
                let bindee = sps::Triv.build(&mut self.arena, None);
                let binder = sps::Hole.build(&mut self.arena, None);
                body = sps::LetValue { binder, bindee, tail: body }.build(&mut self.arena, None);
            }
            SpsLowProgram::try_new(self.arena, body, SpsScheme::BranchJoin).unwrap()
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
            Literal::Float(zydeco_syntax::FloatLiteral::from(1.5)).build(&mut fixture.arena, None);
        let binder = sps::Hole.build(&mut fixture.arena, None);
        let root = sps::LetValue { binder, bindee, tail: body }.build(&mut fixture.arena, None);
        let program = SpsLowProgram::try_new(fixture.arena, root, SpsScheme::BranchJoin).unwrap();
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

        // pair, one root case, and entry. The sixteen LetValue nodes are
        // structured instructions inside the root case rather than functions.
        assert_eq!(bodies, 3);
    }

    #[test]
    fn local_labels_stay_in_their_function_and_closed_labels_become_cases() {
        // label p • -> _ ! • in compare 1 < 2 | true -> jump p ! • | false -> jump p ! • end,
        // bound as a local label and then as a closed one.
        for (kind, cases) in [(sps::LabelKind::Local, 1), (sps::LabelKind::Closed, 2)] {
            let mut fixture = Fixture::new();
            let label: DefId = fixture.arena.admin.fresh_def();
            let body = fixture.hole();
            let jumps = [0, 1].map(|_| {
                let stack = sps::StackVar.build(&mut fixture.arena, None);
                sps::JumpLabel { label, stack }.build(&mut fixture.arena, None)
            });
            let operands = [1, 2].map(|value| {
                Literal::Integer(sps::IntegerLiteral::Int(value)).build(&mut fixture.arena, None)
            });
            let tail = sps::CompareBranch {
                operation: sps::ComparisonOp::Integer(
                    sps::IntegerType::Int,
                    sps::ComparisonPredicate::Lt,
                ),
                operands,
                when_true: jumps[0],
                when_false: jumps[1],
            }
            .build(&mut fixture.arena, None);
            let root = sps::LetLabel { label, kind, recursive: false, body, tail }
                .build(&mut fixture.arena, None);
            let program =
                SpsLowProgram::try_new(fixture.arena, root, SpsScheme::Duplicating).unwrap();
            let module = Emitter::new(&program).run().unwrap();
            Validator::new().validate_all(module.as_bytes()).unwrap();
            let bodies = Parser::new(0)
                .parse_all(module.as_bytes())
                .filter(|payload| matches!(payload, Ok(Payload::CodeSectionEntry(_))))
                .count();
            // pair, entry, and one case per root or closed label.
            assert_eq!(bodies, 2 + cases);
        }
    }

    #[test]
    fn emission_is_deterministic() {
        let program = Fixture::new().closed_let_chain(8);
        let first = Emitter::new(&program).run().unwrap();
        let second = Emitter::new(&program).run().unwrap();

        assert_eq!(first, second);
    }

    #[test]
    fn per_case_local_plan_isolates_tiny_blocks_from_large_roots() {
        let mut fixture = Fixture::new();
        let label: DefId = fixture.arena.admin.fresh_def();
        let environment = sps::Hole.build(&mut fixture.arena, None);
        let block_body = fixture.hole();
        let block = sps::Block {
            label,
            entry: sps::EntryParameters::Closure { environment },
            body: block_body,
        }
        .build(&mut fixture.arena, None);

        let mut tail = fixture.hole();
        for _ in 0..20 {
            let bindee = sps::Triv.build(&mut fixture.arena, None);
            let binder = sps::Hole.build(&mut fixture.arena, None);
            tail = sps::LetValue { binder, bindee, tail }.build(&mut fixture.arena, None);
        }
        let binder = sps::Hole.build(&mut fixture.arena, None);
        let root = sps::LetValue { binder, bindee: block, tail }.build(&mut fixture.arena, None);
        let program = SpsLowProgram::try_new(fixture.arena, root, SpsScheme::BranchJoin).unwrap();
        let module = Emitter::new(&program).run().unwrap();
        Validator::new().validate_all(module.as_bytes()).unwrap();

        let local_counts = Parser::new(0)
            .parse_all(module.as_bytes())
            .filter_map(|payload| match payload {
                | Ok(Payload::CodeSectionEntry(body)) => {
                    let count: u32 = body
                        .get_locals_reader()
                        .unwrap()
                        .into_iter()
                        .map(|entry| entry.unwrap().0)
                        .sum();
                    Some(count)
                }
                | _ => None,
            })
            .collect::<Vec<_>>();
        // pair, root case (0), tiny block case (1), entry.
        assert_eq!(local_counts.len(), 4);
        let root_locals = local_counts[1];
        let block_locals = local_counts[2];
        assert!(root_locals > 40, "expected large root local count, got {root_locals}");
        assert!(block_locals < 12, "expected tiny block local count (< 12), got {block_locals}");
    }

    #[test]
    fn recursive_local_and_closed_labels_validate() {
        for kind in [sps::LabelKind::Local, sps::LabelKind::Closed] {
            let mut fixture = Fixture::new();
            let label: DefId = fixture.arena.admin.fresh_def();
            let done = fixture.hole();
            let self_stack = sps::StackVar.build(&mut fixture.arena, None);
            let self_jump =
                sps::JumpLabel { label, stack: self_stack }.build(&mut fixture.arena, None);
            let operands = [1, 2].map(|value| {
                Literal::Integer(sps::IntegerLiteral::Int(value)).build(&mut fixture.arena, None)
            });
            let body = sps::CompareBranch {
                operation: sps::ComparisonOp::Integer(
                    sps::IntegerType::Int,
                    sps::ComparisonPredicate::Lt,
                ),
                operands,
                when_true: done,
                when_false: self_jump,
            }
            .build(&mut fixture.arena, None);
            let tail_stack = sps::StackVar.build(&mut fixture.arena, None);
            let tail = sps::JumpLabel { label, stack: tail_stack }.build(&mut fixture.arena, None);
            let root = sps::LetLabel { label, kind, recursive: true, body, tail }
                .build(&mut fixture.arena, None);
            let program =
                SpsLowProgram::try_new(fixture.arena, root, SpsScheme::Duplicating).unwrap();
            let module = Emitter::new(&program).run().unwrap();
            Validator::new().validate_all(module.as_bytes()).unwrap();
        }
    }

    #[test]
    fn nullary_and_non_nullary_constructors_validate_without_nullary_allocation() {
        let mut fixture = Fixture::new();
        let triv = sps::Triv.build(&mut fixture.arena, None);
        let nullary = sps::Ctor(sps::CtorIdx { idx: 0, name: "None".into() }, triv)
            .build(&mut fixture.arena, None);
        assert_eq!(fixture.arena.inner.nullary_ctor_tag(nullary), Some(0));

        let lit = Literal::Integer(sps::IntegerLiteral::Int(42)).build(&mut fixture.arena, None);
        let boxed = sps::Ctor(sps::CtorIdx { idx: 1, name: "Some".into() }, lit)
            .build(&mut fixture.arena, None);
        assert_eq!(fixture.arena.inner.nullary_ctor_tag(boxed), None);

        let y: DefId = fixture.arena.admin.fresh_def();
        let arm0 = sps::Matcher {
            binder: sps::Ctor(
                sps::CtorIdx { idx: 0, name: "None".into() },
                sps::Hole.build(&mut fixture.arena, None),
            )
            .build(&mut fixture.arena, None),
            tail: fixture.hole(),
        };
        let arm1 = sps::Matcher {
            binder: sps::Ctor(
                sps::CtorIdx { idx: 1, name: "Some".into() },
                sps::Hole.build(&mut fixture.arena, None),
            )
            .build(&mut fixture.arena, None),
            tail: fixture.hole(),
        };
        let branch = sps::SCoprodMatch { scrut: nullary, arms: vec![arm0, arm1] }
            .build(&mut fixture.arena, None);
        let y_pat = y.build(&mut fixture.arena, None);
        let root = sps::LetValue { binder: y_pat, bindee: boxed, tail: branch }
            .build(&mut fixture.arena, None);
        let program = SpsLowProgram::try_new(fixture.arena, root, SpsScheme::Duplicating).unwrap();
        let module = Emitter::new(&program).run().unwrap();
        Validator::new().validate_all(module.as_bytes()).unwrap();
    }
}
