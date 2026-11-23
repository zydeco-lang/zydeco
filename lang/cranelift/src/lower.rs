use crate::EmitError;
use crate::externs;
use crate::liveness::CaseLiveness;
use crate::module::DeclaredSymbols;
use crate::plan::{Case, ModulePlan};
use crate::scalar;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{
    Block as ClifBlock, BlockArg, FuncRef, Function, GlobalValue, InstBuilder, MemFlagsData,
    SigRef, StackSlot, StackSlotData, StackSlotKind, TrapCode, Value as ClifValue, types,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Switch, Variable};
use cranelift_module::{FuncId, Module};
use cranelift_object::ObjectModule;
use std::collections::{HashMap, HashSet};
use zydeco_abi::RuntimeFailure;
use zydeco_machine::clif::{
    ALLOC_KIND_OPAQUE, ALLOC_KIND_SCANNED, BLOCK_HEADER_BYTES, BLOCK_HEADER_META_OFFSET,
    BLOCK_HEADER_SIZE_OFFSET, VMCTX_ALLOC_LIMIT, VMCTX_ALLOC_PTR, VMCTX_SPS_LIMIT, VMCTX_SPS_SP,
};
use zydeco_machine::closure::ClosureField;
use zydeco_machine::word::RuntimeWord;
use zydeco_stackir::low::syntax::{
    self as sps, CompuId, Computation, DefId, SpsLowArena, Stack, StackId, VPatId, Value, ValueExt,
    ValueId, ValuePattern,
};
use zydeco_syntax::Literal;

const MAX_PEELED_LABEL_ARGS: usize = 8;

#[derive(Clone, Debug)]
pub(crate) enum EvalValue {
    Word(ClifValue),
    Unboxed(Vec<ClifValue>),
}

#[derive(Clone)]
struct LocalLabelTarget {
    block: ClifBlock,
    peeled: usize,
    reached: bool,
}

enum StackFrame {
    Value(ValueId),
    Tag(i64),
}

pub(crate) struct CaseEmitter<'a, 'b> {
    pub(crate) arena: &'a SpsLowArena,
    pub(crate) plan: &'a ModulePlan,
    pub(crate) symbols: &'a DeclaredSymbols,
    pub(crate) module: &'a mut ObjectModule,
    pub(crate) builder: FunctionBuilder<'b>,
    liveness: CaseLiveness,
    pub(crate) vmctx: ClifValue,
    sps_sp: Variable,
    current_compu: CompuId,
    def_vars: HashMap<DefId, Vec<Variable>>,
    defined_defs: HashSet<DefId>,
    pub(crate) temp_vars: Vec<Variable>,
    local_labels: HashMap<DefId, LocalLabelTarget>,
    roots_slot: Option<StackSlot>,
    func_refs: HashMap<FuncId, FuncRef>,
    string_gvs: HashMap<usize, GlobalValue>,
    tail_sig_ref: Option<SigRef>,
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn emit_case(
    arena: &SpsLowArena, plan: &ModulePlan, symbols: &DeclaredSymbols, module: &mut ObjectModule,
    func: &mut Function, fb_ctx: &mut FunctionBuilderContext, case: Case, liveness: CaseLiveness,
) -> Result<(), EmitError> {
    let mut builder = FunctionBuilder::new(func, fb_ctx);
    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    let params = builder.block_params(entry_block);
    let vmctx = params[0];
    let entry_sp = params[1];
    let sps_sp = builder.declare_var(types::I64);
    builder.def_var(sps_sp, entry_sp);

    let mut emitter = CaseEmitter {
        arena,
        plan,
        symbols,
        module,
        builder,
        liveness,
        vmctx,
        sps_sp,
        current_compu: case.body,
        def_vars: HashMap::new(),
        defined_defs: HashSet::new(),
        temp_vars: Vec::new(),
        local_labels: HashMap::new(),
        roots_slot: None,
        func_refs: HashMap::new(),
        string_gvs: HashMap::new(),
        tail_sig_ref: None,
    };

    if let Some(entry) = case.entry {
        for (_, pat) in entry.words() {
            let word = emitter.pop_sps_word();
            emitter.bind_pattern(pat, EvalValue::Word(word))?;
        }
    }
    emitter.emit_compu(case.body)?;
    let target_config = emitter.module.target_config();
    emitter.builder.seal_all_blocks();
    emitter.builder.finalize(target_config);
    Ok(())
}

impl<'a, 'b> CaseEmitter<'a, 'b> {
    pub(crate) fn current_sp(&mut self) -> ClifValue {
        self.builder.use_var(self.sps_sp)
    }

    pub(crate) fn set_sp(&mut self, sp: ClifValue) {
        self.builder.def_var(self.sps_sp, sp);
    }

    pub(crate) fn push_temp(&mut self, value: ClifValue) -> Variable {
        let var = self.builder.declare_var(types::I64);
        self.builder.def_var(var, value);
        self.temp_vars.push(var);
        var
    }

    pub(crate) fn func_ref(&mut self, id: FuncId) -> FuncRef {
        if let Some(&fref) = self.func_refs.get(&id) {
            return fref;
        }
        let fref = self.module.declare_func_in_func(id, self.builder.func);
        self.func_refs.insert(id, fref);
        fref
    }

    pub(crate) fn string_addr(&mut self, index: usize) -> ClifValue {
        let gv = if let Some(&gv) = self.string_gvs.get(&index) {
            gv
        } else {
            let data_id = self.symbols.strings[index];
            let gv = self.module.declare_data_in_func(data_id, self.builder.func);
            self.string_gvs.insert(index, gv);
            gv
        };
        self.builder.ins().symbol_value(types::I64, gv)
    }

    pub(crate) fn tail_sig_ref(&mut self) -> SigRef {
        if let Some(sig_ref) = self.tail_sig_ref {
            return sig_ref;
        }
        let sig_ref = self.builder.import_signature(self.symbols.tail_sig.clone());
        self.tail_sig_ref = Some(sig_ref);
        sig_ref
    }

    pub(crate) fn emit_fail(&mut self, failure: RuntimeFailure) {
        let fail_ref = self.func_ref(self.symbols.fail);
        let code = self.builder.ins().iconst(types::I32, i64::from(failure.code()));
        self.builder.ins().call(fail_ref, &[code]);
        let trap = TrapCode::user(1).expect("user trap code 1 is valid");
        self.builder.ins().trap(trap);
    }

    pub(crate) fn ensure_sps_space(&mut self, needed_words: usize) {
        if needed_words == 0 {
            return;
        }
        let needed_bytes = (needed_words * 8) as i64;
        let sp = self.current_sp();
        let limit = self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            self.vmctx,
            VMCTX_SPS_LIMIT,
        );
        let avail = self.builder.ins().isub(limit, sp);
        let has_room =
            self.builder.ins().icmp_imm_s(IntCC::UnsignedGreaterThanOrEqual, avail, needed_bytes);

        let fast_block = self.builder.create_block();
        let slow_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);
        self.builder.set_cold_block(slow_block);

        self.builder.ins().brif(has_room, fast_block, &[], slow_block, &[]);

        self.builder.switch_to_block(fast_block);
        self.builder.ins().jump(merge_block, &[BlockArg::Value(sp)]);

        self.builder.switch_to_block(slow_block);
        let grow_ref = self.func_ref(self.symbols.grow_sps);
        let needed = self.builder.ins().iconst(types::I64, needed_words as i64);
        let call = self.builder.ins().call(grow_ref, &[self.vmctx, sp, needed]);
        let new_sp = self.builder.inst_results(call)[0];
        self.builder.ins().jump(merge_block, &[BlockArg::Value(new_sp)]);

        self.builder.switch_to_block(merge_block);
        let merged_sp = self.builder.block_params(merge_block)[0];
        self.set_sp(merged_sp);
    }

    pub(crate) fn push_sps_words(&mut self, words: &[ClifValue]) {
        if words.is_empty() {
            return;
        }
        self.ensure_sps_space(words.len());
        let sp = self.current_sp();
        for (idx, &word) in words.iter().enumerate() {
            self.builder.ins().store(MemFlagsData::trusted(), word, sp, (idx * 8) as i32);
        }
        let next_sp = self.builder.ins().iadd_imm_s(sp, (words.len() * 8) as i64);
        self.set_sp(next_sp);
    }

    pub(crate) fn pop_sps_word(&mut self) -> ClifValue {
        let sp = self.current_sp();
        let next_sp = self.builder.ins().iadd_imm_s(sp, -8);
        self.set_sp(next_sp);
        self.builder.ins().load(types::I64, MemFlagsData::trusted(), next_sp, 0)
    }

    fn live_root_variables(&self) -> Vec<Variable> {
        let mut vars = Vec::new();
        let mut live_defs: Vec<DefId> = self
            .liveness
            .live_defs(self.current_compu)
            .filter(|def| self.defined_defs.contains(def))
            .collect();
        live_defs.sort_unstable();
        for def in live_defs {
            if let Some(bound) = self.def_vars.get(&def) {
                vars.extend_from_slice(bound);
            }
        }
        vars.extend_from_slice(&self.temp_vars);
        vars
    }

    pub(crate) fn emit_alloc(&mut self, kind: usize, size_words: usize) -> ClifValue {
        let total_bytes = (BLOCK_HEADER_BYTES + size_words * 8) as i64;
        let bump_ptr = self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            self.vmctx,
            VMCTX_ALLOC_PTR,
        );
        let limit = self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            self.vmctx,
            VMCTX_ALLOC_LIMIT,
        );
        let avail = self.builder.ins().isub(limit, bump_ptr);
        let has_room =
            self.builder.ins().icmp_imm_s(IntCC::UnsignedGreaterThanOrEqual, avail, total_bytes);

        let live_vars = self.live_root_variables();
        let fast_block = self.builder.create_block();
        let slow_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);
        for _ in &live_vars {
            self.builder.append_block_param(merge_block, types::I64);
        }
        self.builder.set_cold_block(slow_block);

        self.builder.ins().brif(has_room, fast_block, &[], slow_block, &[]);

        self.emit_alloc_fast(
            fast_block,
            merge_block,
            bump_ptr,
            kind,
            size_words,
            total_bytes,
            &live_vars,
        );
        self.emit_alloc_slow(slow_block, merge_block, kind, size_words, &live_vars);

        self.builder.switch_to_block(merge_block);
        let params: Vec<ClifValue> = self.builder.block_params(merge_block).to_vec();
        for (&var, &param) in live_vars.iter().zip(&params[1..]) {
            self.builder.def_var(var, param);
        }
        params[0]
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_alloc_fast(
        &mut self, fast_block: ClifBlock, merge_block: ClifBlock, bump_ptr: ClifValue, kind: usize,
        size_words: usize, total_bytes: i64, live_vars: &[Variable],
    ) {
        self.builder.switch_to_block(fast_block);
        let words_val = self.builder.ins().iconst(types::I64, size_words as i64);
        let kind_val = self.builder.ins().iconst(types::I64, kind as i64);
        self.builder.ins().store(
            MemFlagsData::trusted(),
            words_val,
            bump_ptr,
            BLOCK_HEADER_SIZE_OFFSET,
        );
        self.builder.ins().store(
            MemFlagsData::trusted(),
            kind_val,
            bump_ptr,
            BLOCK_HEADER_META_OFFSET,
        );
        let next_ptr = self.builder.ins().iadd_imm_s(bump_ptr, total_bytes);
        self.builder.ins().store(MemFlagsData::trusted(), next_ptr, self.vmctx, VMCTX_ALLOC_PTR);
        let payload = self.builder.ins().iadd_imm_s(bump_ptr, BLOCK_HEADER_BYTES as i64);
        let mut args = Vec::with_capacity(1 + live_vars.len());
        args.push(BlockArg::Value(payload));
        for &var in live_vars {
            args.push(BlockArg::Value(self.builder.use_var(var)));
        }
        self.builder.ins().jump(merge_block, &args);
    }

    fn emit_alloc_slow(
        &mut self, slow_block: ClifBlock, merge_block: ClifBlock, kind: usize, size_words: usize,
        live_vars: &[Variable],
    ) {
        self.builder.switch_to_block(slow_block);
        let (roots_ptr, roots_len) = if live_vars.is_empty() {
            (self.builder.ins().iconst(types::I64, 0), self.builder.ins().iconst(types::I64, 0))
        } else {
            let needed_bytes = u32::try_from(live_vars.len() * 8).expect("GC root frame overflow");
            let slot = match self.roots_slot {
                | Some(slot) => {
                    if self.builder.func.sized_stack_slots[slot].size < needed_bytes {
                        self.builder.func.sized_stack_slots[slot].size = needed_bytes;
                    }
                    slot
                }
                | None => {
                    let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        needed_bytes,
                        3,
                    ));
                    self.roots_slot = Some(slot);
                    slot
                }
            };
            let ptr = self.builder.ins().stack_addr(types::I64, slot, 0);
            for (idx, &var) in live_vars.iter().enumerate() {
                let val = self.builder.use_var(var);
                self.builder.ins().store(MemFlagsData::trusted(), val, ptr, (idx * 8) as i32);
            }
            let len = self.builder.ins().iconst(types::I64, live_vars.len() as i64);
            (ptr, len)
        };

        let sp = self.current_sp();
        self.builder.ins().store(MemFlagsData::trusted(), sp, self.vmctx, VMCTX_SPS_SP);
        let alloc_ref = self.func_ref(self.symbols.alloc_slow);
        let words_val = self.builder.ins().iconst(types::I64, size_words as i64);
        let kind_val = self.builder.ins().iconst(types::I64, kind as i64);
        let call = self
            .builder
            .ins()
            .call(alloc_ref, &[self.vmctx, words_val, kind_val, roots_ptr, roots_len]);
        let payload = self.builder.inst_results(call)[0];

        let mut args = Vec::with_capacity(1 + live_vars.len());
        args.push(BlockArg::Value(payload));
        for idx in 0..live_vars.len() {
            let reloaded = self.builder.ins().load(
                types::I64,
                MemFlagsData::trusted(),
                roots_ptr,
                (idx * 8) as i32,
            );
            args.push(BlockArg::Value(reloaded));
        }
        self.builder.ins().jump(merge_block, &args);
    }

    pub(crate) fn save_defined_defs(&self) -> HashSet<DefId> {
        self.defined_defs.clone()
    }

    pub(crate) fn restore_defined_defs(&mut self, saved: HashSet<DefId>) {
        self.defined_defs = saved;
    }

    pub(crate) fn emit_compu(&mut self, id: CompuId) -> Result<(), EmitError> {
        zydeco_foundation::stack::maybe_grow(|| self.emit_compu_inner(id))
    }

    fn emit_compu_inner(&mut self, mut id: CompuId) -> Result<(), EmitError> {
        loop {
            self.current_compu = id;
            if let Some(call) = self.plan.scalars.memory(id).cloned() {
                scalar::emit_memory_call(self, &call)?;
                id = call.next;
                continue;
            }
            match self.arena.inner.compus[&id].clone() {
                | Computation::Hole(_) => {
                    self.emit_fail(RuntimeFailure::PatternMatch);
                    return Ok(());
                }
                | Computation::ProductMatch(sps::SProductMatch { scrut, binder, body }) => {
                    let value = self.emit_value(scrut)?;
                    self.bind_pattern(binder, value)?;
                    id = body;
                }
                | Computation::LetValue(sps::LetValue { binder, bindee, tail }) => {
                    if !self.plan.scalars.elided(id) {
                        let value = self.emit_value(bindee)?;
                        self.bind_pattern(binder, value)?;
                    }
                    id = tail;
                }
                | Computation::LetStack(sps::LetStack { bindee, tail, .. }) => {
                    self.emit_stack(bindee)?;
                    id = tail;
                }
                | Computation::LetArg(sps::LetArg {
                    binder: sps::Cons(binder, _),
                    bindee,
                    tail,
                }) => {
                    let word = self.eval_let_arg(bindee)?;
                    self.bind_pattern(binder, EvalValue::Word(word))?;
                    id = tail;
                }
                | Computation::Compare(cmp) => {
                    return scalar::emit_compare_branch(self, cmp);
                }
                | Computation::CoprodMatch(sps::SCoprodMatch { scrut, arms }) => {
                    return self.emit_coprod_match(scrut, arms);
                }
                | Computation::CoCase(sps::SCoMatch { scrut, arms }) => {
                    return self.emit_cocase(scrut, arms);
                }
                | Computation::Memory(step) => {
                    id = scalar::emit_memory_step(self, step)?;
                }
                | Computation::ExternCall(sps::ExternCall { function, stack }) => {
                    return externs::emit_extern_call(self, function, stack);
                }
                | Computation::Ext(ext) => {
                    return self.emit_compu_ext(ext);
                }
            }
        }
    }

    fn emit_compu_ext(&mut self, ext: sps::CompuExt) -> Result<(), EmitError> {
        match ext {
            | sps::CompuExt::OpenClosure(sps::OpenClosure { package, environment, code, body }) => {
                self.emit_open_closure(package, environment, code)?;
                self.emit_compu(body)
            }
            | sps::CompuExt::OpenContinuation(sps::OpenContinuation { package, code, body }) => {
                let code_word = self.eval_open_continuation(package)?;
                self.bind_pattern(code, EvalValue::Word(code_word))?;
                self.emit_compu(body)
            }
            | sps::CompuExt::Jump(jump) => self.emit_jump(jump),
            | sps::CompuExt::LetLabel(let_label) => self.emit_let_label(let_label),
            | sps::CompuExt::JumpLabel(jump_label) => self.emit_jump_label(jump_label),
        }
    }

    fn emit_open_closure(
        &mut self, package: ValueId, environment: VPatId, code: VPatId,
    ) -> Result<(), EmitError> {
        if self.plan.unboxing.values.contains(package) {
            match self.arena.inner.values[&package].clone() {
                | Value::Ext(ValueExt::ClosurePackage(sps::ClosurePackage {
                    environment: env_val,
                    code: code_val,
                })) => {
                    let temp_base = self.temp_vars.len();
                    let env_eval = self.emit_value(env_val)?;
                    let env_vars: Vec<Variable> = match &env_eval {
                        | EvalValue::Word(w) => vec![self.push_temp(*w)],
                        | EvalValue::Unboxed(ws) => ws.iter().map(|&w| self.push_temp(w)).collect(),
                    };
                    let code_word = self.emit_word(code_val)?;
                    let reloaded: Vec<ClifValue> =
                        env_vars.into_iter().map(|v| self.builder.use_var(v)).collect();
                    self.temp_vars.truncate(temp_base);
                    let env_reloaded = match env_eval {
                        | EvalValue::Word(_) => EvalValue::Word(reloaded[0]),
                        | EvalValue::Unboxed(_) => EvalValue::Unboxed(reloaded),
                    };
                    self.bind_pattern(environment, env_reloaded)?;
                    self.bind_pattern(code, EvalValue::Word(code_word))?;
                    return Ok(());
                }
                | Value::Var(def) if self.plan.unboxing.unboxed_vars.contains_key(def) => {
                    let vars =
                        self.def_vars.get(&def).ok_or(EmitError::MissingVariable(def))?.clone();
                    let env_word = self.builder.use_var(vars[0]);
                    let code_word = self.builder.use_var(vars[1]);
                    self.bind_pattern(environment, EvalValue::Word(env_word))?;
                    self.bind_pattern(code, EvalValue::Word(code_word))?;
                    return Ok(());
                }
                | _ => {}
            }
        }
        let pkg = self.emit_word(package)?;
        let env_word = self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            pkg,
            ClosureField::Environment.offset() as i32,
        );
        let code_word = self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            pkg,
            ClosureField::Code.offset() as i32,
        );
        self.bind_pattern(environment, EvalValue::Word(env_word))?;
        self.bind_pattern(code, EvalValue::Word(code_word))?;
        Ok(())
    }

    fn eval_open_continuation(&mut self, package: StackId) -> Result<ClifValue, EmitError> {
        if let Stack::Ext(sps::StackExt::ContinuationPackage(sps::ContinuationPackage {
            code,
            residual,
        })) = self.arena.inner.stacks[&package].clone()
        {
            let temp_base = self.temp_vars.len();
            let code_word = self.emit_word(code)?;
            let code_var = self.push_temp(code_word);
            self.emit_stack(residual)?;
            let reloaded = self.builder.use_var(code_var);
            self.temp_vars.truncate(temp_base);
            return Ok(reloaded);
        }
        self.emit_stack(package)?;
        Ok(self.pop_sps_word())
    }

    fn eval_let_arg(&mut self, bindee: StackId) -> Result<ClifValue, EmitError> {
        if let Stack::Arg(sps::Cons(val, rest)) = self.arena.inner.stacks[&bindee].clone()
            && matches!(self.arena.inner.stacks[&rest], Stack::Var(_))
        {
            return self.emit_word(val);
        }
        self.emit_stack(bindee)?;
        Ok(self.pop_sps_word())
    }

    fn emit_jump(&mut self, jump: sps::Jump) -> Result<(), EmitError> {
        let temp_base = self.temp_vars.len();
        let arg_word = self.emit_word(jump.argument.word().1)?;
        let arg_var = self.push_temp(arg_word);

        let frames = self.collect_stack_frames(jump.stack)?;
        let frame_vars = self.eval_stack_frames(&frames)?;
        let target_word = self.emit_word(jump.target)?;
        let target_var = self.push_temp(target_word);

        let total_push = frame_vars.len() + 1;
        self.ensure_sps_space(total_push);
        let sp = self.current_sp();
        for (idx, var) in frame_vars.iter().rev().enumerate() {
            let word = self.builder.use_var(*var);
            self.builder.ins().store(MemFlagsData::trusted(), word, sp, (idx * 8) as i32);
        }
        let reloaded_arg = self.builder.use_var(arg_var);
        self.builder.ins().store(
            MemFlagsData::trusted(),
            reloaded_arg,
            sp,
            (frame_vars.len() * 8) as i32,
        );
        let next_sp = self.builder.ins().iadd_imm_s(sp, (total_push * 8) as i64);
        self.set_sp(next_sp);

        let target_code = self.builder.use_var(target_var);
        self.temp_vars.truncate(temp_base);
        let tail_sig = self.tail_sig_ref();
        self.builder.ins().return_call_indirect(tail_sig, target_code, &[self.vmctx, next_sp]);
        Ok(())
    }

    fn emit_let_label(&mut self, let_label: sps::LetLabel) -> Result<(), EmitError> {
        if let_label.kind == sps::LabelKind::Closed {
            return self.emit_compu(let_label.tail);
        }
        let mut peeled = Vec::new();
        let mut inner_body = let_label.body;
        while peeled.len() < MAX_PEELED_LABEL_ARGS
            && let Computation::LetArg(sps::LetArg { binder: sps::Cons(pat, _), bindee, tail }) =
                self.arena.inner.compus[&inner_body].clone()
            && matches!(self.arena.inner.stacks[&bindee], Stack::Var(_))
            && !self.plan.unboxing.patterns.contains(pat)
        {
            peeled.push(pat);
            inner_body = tail;
        }

        let label_block = self.builder.create_block();
        self.builder.append_block_param(label_block, types::I64);
        for _ in &peeled {
            self.builder.append_block_param(label_block, types::I64);
        }

        let saved_defined = self.save_defined_defs();
        self.local_labels.insert(
            let_label.label,
            LocalLabelTarget { block: label_block, peeled: peeled.len(), reached: false },
        );
        self.emit_compu(let_label.tail)?;

        let target = self
            .local_labels
            .remove(&let_label.label)
            .expect("local label remains registered until tail completes");
        self.restore_defined_defs(saved_defined);

        self.builder.switch_to_block(label_block);
        if !target.reached {
            let trap = TrapCode::user(1).expect("user trap code 1 is valid");
            self.builder.ins().trap(trap);
            return Ok(());
        }

        let params: Vec<ClifValue> = self.builder.block_params(label_block).to_vec();
        self.set_sp(params[0]);
        for (pat, &arg_val) in peeled.into_iter().zip(&params[1..]) {
            self.bind_pattern(pat, EvalValue::Word(arg_val))?;
        }
        if let_label.recursive {
            self.local_labels.insert(let_label.label, target);
        }
        self.emit_compu(inner_body)?;
        if let_label.recursive {
            self.local_labels.remove(&let_label.label);
        }
        Ok(())
    }

    fn emit_jump_label(&mut self, jump: sps::JumpLabel) -> Result<(), EmitError> {
        if let Some(target) = self.local_labels.get(&jump.label).cloned() {
            self.local_labels.get_mut(&jump.label).expect("checked above").reached = true;
            let mut cursor = jump.stack;
            let mut syntactic = Vec::new();
            while syntactic.len() < target.peeled
                && let Stack::Arg(sps::Cons(val, rest)) = self.arena.inner.stacks[&cursor]
            {
                syntactic.push(val);
                cursor = rest;
            }

            let temp_base = self.temp_vars.len();
            let mut syn_vars = Vec::with_capacity(syntactic.len());
            for val in syntactic {
                let w = self.emit_word(val)?;
                syn_vars.push(self.push_temp(w));
            }
            self.emit_stack(cursor)?;

            let mut args = Vec::with_capacity(1 + target.peeled);
            let shortfall = target.peeled - syn_vars.len();
            let mut popped = Vec::with_capacity(shortfall);
            for _ in 0..shortfall {
                popped.push(self.pop_sps_word());
            }
            args.push(BlockArg::Value(self.current_sp()));
            for var in syn_vars {
                args.push(BlockArg::Value(self.builder.use_var(var)));
            }
            for w in popped {
                args.push(BlockArg::Value(w));
            }
            self.temp_vars.truncate(temp_base);
            self.builder.ins().jump(target.block, &args);
            return Ok(());
        }

        let &case_index =
            self.plan.case_labels.get(&jump.label).ok_or(EmitError::MissingLabel(jump.label))?;
        self.emit_stack(jump.stack)?;
        let sp = self.current_sp();
        let fref = self.func_ref(self.symbols.cases[case_index]);
        self.builder.ins().return_call(fref, &[self.vmctx, sp]);
        Ok(())
    }

    fn emit_coprod_match(
        &mut self, scrut: ValueId, arms: Vec<sps::Matcher<VPatId, CompuId>>,
    ) -> Result<(), EmitError> {
        let scrut_val = self.emit_word(scrut)?;
        let all_ctors = arms
            .iter()
            .all(|arm| matches!(self.arena.inner.vpats[&arm.binder], ValuePattern::Ctor(_)));
        if !all_ctors {
            let [sps::Matcher { binder, tail }] = arms.as_slice() else {
                return Err(EmitError::InvalidCoprodMatch);
            };
            self.bind_pattern(*binder, EvalValue::Word(scrut_val))?;
            return self.emit_compu(*tail);
        }

        let (tag_val, payload_val) = self.split_ctor_word(scrut_val)?;
        let raw_idx = self.builder.ins().ushr_imm_u(tag_val, 1);
        let fail_block = self.builder.create_block();
        self.builder.set_cold_block(fail_block);
        let saved_defined = self.save_defined_defs();

        let mut switch = Switch::new();
        let mut arm_blocks = Vec::with_capacity(arms.len());
        let mut seen = HashSet::new();
        for arm in arms {
            let ValuePattern::Ctor(sps::Ctor(ctor, body)) =
                self.arena.inner.vpats[&arm.binder].clone()
            else {
                unreachable!("verified all arms are Ctor patterns");
            };
            let arm_block = self.builder.create_block();
            if seen.insert(ctor.idx) {
                switch.set_entry(ctor.idx as u128, arm_block);
                arm_blocks.push((arm_block, body, arm.tail));
            }
        }
        switch.emit(&mut self.builder, raw_idx, fail_block);

        self.builder.switch_to_block(fail_block);
        self.emit_fail(RuntimeFailure::PatternMatch);

        for (arm_block, body, tail) in arm_blocks {
            self.restore_defined_defs(saved_defined.clone());
            self.builder.switch_to_block(arm_block);
            self.bind_pattern(body, EvalValue::Word(payload_val))?;
            self.emit_compu(tail)?;
        }
        Ok(())
    }

    fn split_ctor_word(&mut self, scrut: ClifValue) -> Result<(ClifValue, ClifValue), EmitError> {
        let low_bit = self.builder.ins().band_imm_s(scrut, 1);
        let is_imm = self.builder.ins().icmp_imm_s(IntCC::NotEqual, low_bit, 0);
        let heap_block = self.builder.create_block();
        let imm_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);
        self.builder.append_block_param(merge_block, types::I64);

        self.builder.ins().brif(is_imm, imm_block, &[], heap_block, &[]);

        self.builder.switch_to_block(heap_block);
        let h_tag = self.builder.ins().load(types::I64, MemFlagsData::trusted(), scrut, 0);
        let h_pay = self.builder.ins().load(types::I64, MemFlagsData::trusted(), scrut, 8);
        self.builder.ins().jump(merge_block, &[BlockArg::Value(h_tag), BlockArg::Value(h_pay)]);

        self.builder.switch_to_block(imm_block);
        let unit = self.builder.ins().iconst(types::I64, RuntimeWord::index(0)? as i64);
        self.builder.ins().jump(merge_block, &[BlockArg::Value(scrut), BlockArg::Value(unit)]);

        self.builder.switch_to_block(merge_block);
        let params = self.builder.block_params(merge_block);
        Ok((params[0], params[1]))
    }

    fn emit_cocase(
        &mut self, scrut: StackId,
        arms: Vec<sps::CoMatcher<sps::Cons<sps::DtorIdx, sps::StackVar>, CompuId>>,
    ) -> Result<(), EmitError> {
        self.emit_stack(scrut)?;
        let dtor_tag = self.pop_sps_word();
        let raw_idx = self.builder.ins().ushr_imm_u(dtor_tag, 1);
        let fail_block = self.builder.create_block();
        self.builder.set_cold_block(fail_block);
        let saved_defined = self.save_defined_defs();

        let mut switch = Switch::new();
        let mut arm_blocks = Vec::with_capacity(arms.len());
        let mut seen = HashSet::new();
        for sps::CoMatcher { dtor: sps::Cons(dtor, _), tail } in arms {
            let arm_block = self.builder.create_block();
            if seen.insert(dtor.idx) {
                switch.set_entry(dtor.idx as u128, arm_block);
                arm_blocks.push((arm_block, tail));
            }
        }
        switch.emit(&mut self.builder, raw_idx, fail_block);

        self.builder.switch_to_block(fail_block);
        self.emit_fail(RuntimeFailure::PatternMatch);

        for (arm_block, tail) in arm_blocks {
            self.restore_defined_defs(saved_defined.clone());
            self.builder.switch_to_block(arm_block);
            self.emit_compu(tail)?;
        }
        Ok(())
    }

    fn collect_stack_frames(&self, mut id: StackId) -> Result<Vec<StackFrame>, EmitError> {
        let mut frames = Vec::new();
        loop {
            match &self.arena.inner.stacks[&id] {
                | Stack::Var(_) => break,
                | Stack::Arg(sps::Cons(val, rest)) => {
                    frames.push(StackFrame::Value(*val));
                    id = *rest;
                }
                | Stack::Tag(sps::Cons(dtor, rest)) => {
                    frames.push(StackFrame::Tag(RuntimeWord::index(dtor.idx)? as i64));
                    id = *rest;
                }
                | Stack::Ext(sps::StackExt::ContinuationPackage(sps::ContinuationPackage {
                    code,
                    residual,
                })) => {
                    frames.push(StackFrame::Value(*code));
                    id = *residual;
                }
            }
        }
        Ok(frames)
    }

    fn eval_stack_frames(&mut self, frames: &[StackFrame]) -> Result<Vec<Variable>, EmitError> {
        let mut vars = Vec::with_capacity(frames.len());
        for frame in frames {
            let word = match frame {
                | StackFrame::Value(val) => self.emit_word(*val)?,
                | StackFrame::Tag(tag) => self.builder.ins().iconst(types::I64, *tag),
            };
            vars.push(self.push_temp(word));
        }
        Ok(vars)
    }

    pub(crate) fn emit_stack(&mut self, id: StackId) -> Result<(), EmitError> {
        let frames = self.collect_stack_frames(id)?;
        if frames.is_empty() {
            return Ok(());
        }
        let temp_base = self.temp_vars.len();
        let vars = self.eval_stack_frames(&frames)?;
        self.ensure_sps_space(vars.len());
        let sp = self.current_sp();
        for (idx, var) in vars.into_iter().rev().enumerate() {
            let word = self.builder.use_var(var);
            self.builder.ins().store(MemFlagsData::trusted(), word, sp, (idx * 8) as i32);
        }
        let next_sp = self.builder.ins().iadd_imm_s(sp, (frames.len() * 8) as i64);
        self.set_sp(next_sp);
        self.temp_vars.truncate(temp_base);
        Ok(())
    }

    pub(crate) fn emit_word(&mut self, id: ValueId) -> Result<ClifValue, EmitError> {
        match self.emit_value(id)? {
            | EvalValue::Word(w) => Ok(w),
            | EvalValue::Unboxed(_) => {
                unreachable!("unboxed value appeared where single machine word was expected")
            }
        }
    }

    pub(crate) fn emit_value(&mut self, id: ValueId) -> Result<EvalValue, EmitError> {
        zydeco_foundation::stack::maybe_grow(|| self.emit_value_inner(id))
    }

    fn emit_value_inner(&mut self, id: ValueId) -> Result<EvalValue, EmitError> {
        let unboxed = self.plan.unboxing.values.contains(id);
        match self.arena.inner.values[&id].clone() {
            | Value::Hole(_) | Value::Triv(_) => {
                Ok(EvalValue::Word(self.builder.ins().iconst(types::I64, 1)))
            }
            | Value::Var(def) => {
                if let Some(&case_index) = self.plan.case_labels.get(&def) {
                    let fref = self.func_ref(self.symbols.cases[case_index]);
                    return Ok(EvalValue::Word(self.builder.ins().func_addr(types::I64, fref)));
                }
                let vars = self.def_vars.get(&def).ok_or(EmitError::MissingVariable(def))?.clone();
                if unboxed && self.plan.unboxing.unboxed_vars.contains_key(def) {
                    let words = vars.into_iter().map(|v| self.builder.use_var(v)).collect();
                    Ok(EvalValue::Unboxed(words))
                } else {
                    Ok(EvalValue::Word(self.builder.use_var(vars[0])))
                }
            }
            | Value::Ext(ValueExt::Block(block)) => {
                let &case_index = self
                    .plan
                    .case_labels
                    .get(&block.label)
                    .ok_or(EmitError::MissingLabel(block.label))?;
                let fref = self.func_ref(self.symbols.cases[case_index]);
                Ok(EvalValue::Word(self.builder.ins().func_addr(types::I64, fref)))
            }
            | Value::Ext(ValueExt::ClosurePackage(sps::ClosurePackage { environment, code })) => {
                let temp_base = self.temp_vars.len();
                let env_w = self.emit_word(environment)?;
                let env_var = self.push_temp(env_w);
                let code_w = self.emit_word(code)?;
                let code_var = self.push_temp(code_w);
                if unboxed {
                    let e = self.builder.use_var(env_var);
                    let c = self.builder.use_var(code_var);
                    self.temp_vars.truncate(temp_base);
                    return Ok(EvalValue::Unboxed(vec![e, c]));
                }
                let ptr = self.emit_alloc(ALLOC_KIND_SCANNED, sps::ClosurePackage::FIELDS);
                let e = self.builder.use_var(env_var);
                let c = self.builder.use_var(code_var);
                self.temp_vars.truncate(temp_base);
                self.builder.ins().store(
                    MemFlagsData::trusted(),
                    e,
                    ptr,
                    ClosureField::Environment.offset() as i32,
                );
                self.builder.ins().store(
                    MemFlagsData::trusted(),
                    c,
                    ptr,
                    ClosureField::Code.offset() as i32,
                );
                Ok(EvalValue::Word(ptr))
            }
            | Value::Ctor(sps::Ctor(ctor, body)) => {
                if let Some(tag) = self.arena.inner.nullary_ctor_tag(id) {
                    let imm = RuntimeWord::index(tag)? as i64;
                    return Ok(EvalValue::Word(self.builder.ins().iconst(types::I64, imm)));
                }
                let temp_base = self.temp_vars.len();
                let body_w = self.emit_word(body)?;
                let body_var = self.push_temp(body_w);
                let ptr = self.emit_alloc(ALLOC_KIND_SCANNED, 2);
                let reloaded = self.builder.use_var(body_var);
                self.temp_vars.truncate(temp_base);
                let tag_imm =
                    self.builder.ins().iconst(types::I64, RuntimeWord::index(ctor.idx)? as i64);
                self.builder.ins().store(MemFlagsData::trusted(), tag_imm, ptr, 0);
                self.builder.ins().store(MemFlagsData::trusted(), reloaded, ptr, 8);
                Ok(EvalValue::Word(ptr))
            }
            | Value::VCons(sps::VCons { items, layout }) => {
                self.emit_vcons(items, layout.arity, unboxed)
            }
            | Value::Literal(lit) => self.emit_literal(id, lit).map(EvalValue::Word),
            | Value::Primitive(_) => scalar::emit_scalar_call(self, id).map(EvalValue::Word),
            | Value::AddrOffset(sps::AddrOffset { base, displacement }) => {
                let temp_base = self.temp_vars.len();
                let disp_w = self.emit_word(displacement)?;
                let disp_var = self.push_temp(disp_w);
                let base_w = self.emit_word(base)?;
                let reloaded_disp = self.builder.use_var(disp_var);
                self.temp_vars.truncate(temp_base);
                let raw_off = self.builder.ins().sshr_imm_u(reloaded_disp, 1);
                Ok(EvalValue::Word(self.builder.ins().iadd(base_w, raw_off)))
            }
        }
    }

    fn emit_vcons(
        &mut self, items: Vec<ValueId>, arity: usize, unboxed: bool,
    ) -> Result<EvalValue, EmitError> {
        let temp_base = self.temp_vars.len();
        let explicit = items.len();
        let mut vars = Vec::with_capacity(explicit);
        for item in items {
            let w = self.emit_word(item)?;
            vars.push(self.push_temp(w));
        }
        if unboxed && explicit == arity {
            let words = vars.into_iter().map(|v| self.builder.use_var(v)).collect();
            self.temp_vars.truncate(temp_base);
            return Ok(EvalValue::Unboxed(words));
        }
        let ptr = self.emit_alloc(ALLOC_KIND_SCANNED, arity);
        for (index, var) in vars.into_iter().enumerate() {
            let word = self.builder.use_var(var);
            if index + 1 == explicit && explicit < arity {
                for dest in index..arity {
                    let elem = self.builder.ins().load(
                        types::I64,
                        MemFlagsData::trusted(),
                        word,
                        ((dest - index) * 8) as i32,
                    );
                    self.builder.ins().store(MemFlagsData::trusted(), elem, ptr, (dest * 8) as i32);
                }
            } else {
                self.builder.ins().store(MemFlagsData::trusted(), word, ptr, (index * 8) as i32);
            }
        }
        self.temp_vars.truncate(temp_base);
        Ok(EvalValue::Word(ptr))
    }

    fn emit_literal(&mut self, id: ValueId, lit: Literal) -> Result<ClifValue, EmitError> {
        match lit {
            | Literal::Integer(int) => match int.encode_runtime()? {
                | zydeco_syntax::word::EncodedScalar::Immediate(w) => {
                    Ok(self.builder.ins().iconst(types::I64, w as i64))
                }
                | zydeco_syntax::word::EncodedScalar::Boxed(bits) => {
                    Ok(self.emit_alloc_opaque_const(bits as i64))
                }
            },
            | Literal::Float(flt) => match flt.encode_runtime() {
                | zydeco_syntax::word::EncodedScalar::Immediate(w) => {
                    Ok(self.builder.ins().iconst(types::I64, w as i64))
                }
                | zydeco_syntax::word::EncodedScalar::Boxed(bits) => {
                    Ok(self.emit_alloc_opaque_const(bits as i64))
                }
            },
            | Literal::String(_) => {
                let &idx = self.plan.string_indices.get(&id).expect("string literal planned");
                Ok(self.string_addr(idx))
            }
            | Literal::Char(ch) => {
                Ok(self.builder.ins().iconst(types::I64, RuntimeWord::index(ch as usize)? as i64))
            }
        }
    }

    fn emit_alloc_opaque_const(&mut self, raw_bits: i64) -> ClifValue {
        let ptr = self.emit_alloc(ALLOC_KIND_OPAQUE, 1);
        let bits = self.builder.ins().iconst(types::I64, raw_bits);
        self.builder.ins().store(MemFlagsData::trusted(), bits, ptr, 0);
        ptr
    }

    pub(crate) fn bind_pattern(
        &mut self, pat_id: VPatId, value: EvalValue,
    ) -> Result<(), EmitError> {
        zydeco_foundation::stack::maybe_grow(|| self.bind_pattern_inner(pat_id, value))
    }

    fn bind_pattern_inner(&mut self, pat_id: VPatId, value: EvalValue) -> Result<(), EmitError> {
        match (self.arena.inner.vpats[&pat_id].clone(), value) {
            | (ValuePattern::Hole(_) | ValuePattern::Triv(_), _) => Ok(()),
            | (ValuePattern::Var(def), EvalValue::Word(w)) => {
                let var = *self
                    .def_vars
                    .entry(def)
                    .or_insert_with(|| vec![self.builder.declare_var(types::I64)])
                    .first()
                    .unwrap();
                self.builder.def_var(var, w);
                self.defined_defs.insert(def);
                Ok(())
            }
            | (ValuePattern::Var(def), EvalValue::Unboxed(words)) => {
                if self.plan.unboxing.unboxed_vars.get(def).copied() != Some(words.len()) {
                    return Err(EmitError::InvalidUnboxingPattern(pat_id));
                }
                let vars = self.def_vars.entry(def).or_insert_with(|| {
                    (0..words.len()).map(|_| self.builder.declare_var(types::I64)).collect()
                });
                let vars_cloned = vars.clone();
                for (var, word) in vars_cloned.into_iter().zip(words) {
                    self.builder.def_var(var, word);
                }
                self.defined_defs.insert(def);
                Ok(())
            }
            | (ValuePattern::VCons(sps::VCons { items, .. }), EvalValue::Unboxed(words)) => {
                if !self.plan.unboxing.patterns.contains(pat_id) || items.len() != words.len() {
                    return Err(EmitError::InvalidUnboxingPattern(pat_id));
                }
                for (pat, word) in items.into_iter().zip(words) {
                    self.bind_pattern(pat, EvalValue::Word(word))?;
                }
                Ok(())
            }
            | (ValuePattern::Alias(sps::Alias(pats)), EvalValue::Unboxed(words)) => {
                for pat in pats {
                    self.bind_pattern(pat, EvalValue::Unboxed(words.clone()))?;
                }
                Ok(())
            }
            | (_, EvalValue::Unboxed(_)) => Err(EmitError::InvalidUnboxingPattern(pat_id)),
            | (ValuePattern::Alias(sps::Alias(pats)), EvalValue::Word(w)) => {
                for pat in pats {
                    self.bind_pattern(pat, EvalValue::Word(w))?;
                }
                Ok(())
            }
            | (ValuePattern::VCons(sps::VCons { items, layout }), EvalValue::Word(w)) => {
                let explicit = items.len();
                for (index, pat) in items.into_iter().enumerate() {
                    let elem = if index + 1 == explicit && explicit < layout.arity {
                        self.builder.ins().iadd_imm_s(w, (index * 8) as i64)
                    } else {
                        self.builder.ins().load(
                            types::I64,
                            MemFlagsData::trusted(),
                            w,
                            (index * 8) as i32,
                        )
                    };
                    self.bind_pattern(pat, EvalValue::Word(elem))?;
                }
                Ok(())
            }
            | (ValuePattern::Ctor(sps::Ctor(ctor, body)), EvalValue::Word(w)) => {
                let expected = RuntimeWord::index(ctor.idx)? as i64;
                let (tag, payload) = self.split_ctor_word(w)?;
                let ok = self.builder.ins().icmp_imm_s(IntCC::Equal, tag, expected);
                let ok_block = self.builder.create_block();
                let fail_block = self.builder.create_block();
                self.builder.set_cold_block(fail_block);
                self.builder.ins().brif(ok, ok_block, &[], fail_block, &[]);

                self.builder.switch_to_block(fail_block);
                self.emit_fail(RuntimeFailure::PatternMatch);

                self.builder.switch_to_block(ok_block);
                self.bind_pattern(body, EvalValue::Word(payload))
            }
        }
    }
}
