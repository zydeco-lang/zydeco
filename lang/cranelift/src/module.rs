use crate::externs;
use crate::liveness::CaseLiveness;
use crate::lower;
use crate::plan::ModulePlan;
use crate::target::create_object_module;
use crate::{CExportEntry, EmitError, NativeArtifact, NativeEntry, NativeTarget};
use cranelift_codegen::Context;
use cranelift_codegen::ir::{AbiParam, InstBuilder, MemFlagsData, Signature, types};
use cranelift_codegen::isa::CallConv;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use std::collections::HashMap;
use zydeco_abi::HostCallMode;
use zydeco_machine::clif::{
    CLIF_ALLOC_SLOW_SYMBOL, CLIF_ENTRY_BEGIN_SYMBOL, CLIF_ENTRY_END_SYMBOL, CLIF_ENTRY_SYMBOL,
    CLIF_FAIL_SYMBOL, CLIF_GROW_SPS_SYMBOL, CLIF_HOST_PREFIX, VMCTX_SPS_SP,
};
use zydeco_stackir::SpsLowProgram;
use zydeco_syntax::{BuiltinValueRole, ForeignSymbolName};

const RETURN_CONT_SYMBOL: &str = "zydeco_clif_return_cont";

/// Emit a standalone relocatable object defining the shared per-library `AtomicBool` guard word.
pub fn emit_guard_object(
    target: NativeTarget, guard: &ForeignSymbolName,
) -> Result<Vec<u8>, EmitError> {
    let mut module = create_object_module(target, guard.as_str())?;
    let data_id = module.declare_data(guard.as_str(), Linkage::Export, true, false)?;
    let mut desc = DataDescription::new();
    desc.define_zeroinit(8);
    desc.set_align(8);
    module.define_data(data_id, &desc)?;
    module
        .finish()
        .emit()
        .map_err(|err| EmitError::Target(format!("object emission failed: {err}")))
}

pub(crate) struct DeclaredSymbols {
    pub(crate) tail_sig: Signature,
    pub(crate) cases: Vec<FuncId>,
    pub(crate) strings: Vec<DataId>,
    pub(crate) alloc_slow: FuncId,
    pub(crate) grow_sps: FuncId,
    pub(crate) fail: FuncId,
    pub(crate) entry_begin: Option<FuncId>,
    pub(crate) entry_end: Option<FuncId>,
    pub(crate) return_cont: Option<FuncId>,
    pub(crate) c_export: Option<FuncId>,
    pub(crate) guard: Option<DataId>,
    pub(crate) hosts: HashMap<BuiltinValueRole, FuncId>,
    pub(crate) foreigns: HashMap<String, FuncId>,
    pub(crate) units: HashMap<String, FuncId>,
}

pub(crate) struct ModuleEmitter<'a> {
    program: &'a SpsLowProgram,
    entry: &'a NativeEntry,
    plan: ModulePlan,
    module: cranelift_object::ObjectModule,
    symbols: DeclaredSymbols,
}

impl<'a> ModuleEmitter<'a> {
    pub(crate) fn new(
        program: &'a SpsLowProgram, target: NativeTarget, entry: &'a NativeEntry, plan: ModulePlan,
    ) -> Result<Self, EmitError> {
        let mut module = create_object_module(target, "zydeco_module")?;
        let symbols = declare_symbols(&mut module, entry, &plan)?;
        Ok(Self { program, entry, plan, module, symbols })
    }

    pub(crate) fn emit(mut self) -> Result<NativeArtifact, EmitError> {
        let arena = self.program.arena();
        let mut fb_ctx = FunctionBuilderContext::new();
        let mut ctx = self.module.make_context();

        for &case in &self.plan.cases {
            let liveness = CaseLiveness::analyze(arena, &self.plan, case);
            ctx.clear();
            ctx.func.signature = self.symbols.tail_sig.clone();
            lower::emit_case(
                arena,
                &self.plan,
                &self.symbols,
                &mut self.module,
                &mut ctx.func,
                &mut fb_ctx,
                case,
                liveness,
            )?;
            self.module.define_function(self.symbols.cases[case.index], &mut ctx)?;
        }

        match self.entry {
            | NativeEntry::Process => self.emit_process_entry(&mut fb_ctx, &mut ctx)?,
            | NativeEntry::C(c_entry) => self.emit_c_entry(c_entry, &mut fb_ctx, &mut ctx)?,
            | NativeEntry::Unit(_) => {}
        }

        let object = self
            .module
            .finish()
            .emit()
            .map_err(|err| EmitError::Target(format!("object emission failed: {err}")))?;

        Ok(NativeArtifact {
            object,
            foreign_libraries: self.plan.foreign_libraries,
            foreign_imports: self.plan.foreign_imports,
            unit_imports: self.plan.unit_imports,
        })
    }

    fn emit_process_entry(
        &mut self, fb_ctx: &mut FunctionBuilderContext, ctx: &mut Context,
    ) -> Result<(), EmitError> {
        let default_cc = self.module.isa().default_call_conv();
        let sig = simple_sig(default_cc, &[types::I64], &[]);
        let entry_id = self.module.declare_function(CLIF_ENTRY_SYMBOL, Linkage::Export, &sig)?;
        ctx.clear();
        ctx.func.signature = sig;
        {
            let mut builder = FunctionBuilder::new(&mut ctx.func, fb_ctx);
            let block = builder.create_block();
            builder.append_block_params_for_function_params(block);
            builder.switch_to_block(block);
            let vmctx = builder.block_params(block)[0];
            let sps_sp =
                builder.ins().load(types::I64, MemFlagsData::trusted(), vmctx, VMCTX_SPS_SP);
            let case_ref = self.module.declare_func_in_func(self.symbols.cases[0], builder.func);
            builder.ins().call(case_ref, &[vmctx, sps_sp]);
            builder.ins().return_(&[]);
            builder.seal_all_blocks();
            builder.finalize(self.module.target_config());
        }
        self.module.define_function(entry_id, ctx)?;
        Ok(())
    }

    fn emit_c_entry(
        &mut self, c_entry: &CExportEntry, fb_ctx: &mut FunctionBuilderContext, ctx: &mut Context,
    ) -> Result<(), EmitError> {
        let return_cont = self.symbols.return_cont.expect("C entry declares return_cont");
        let c_export = self.symbols.c_export.expect("C entry declares export wrapper");

        ctx.clear();
        ctx.func.signature = self.symbols.tail_sig.clone();
        {
            let mut builder = FunctionBuilder::new(&mut ctx.func, fb_ctx);
            let block = builder.create_block();
            builder.append_block_params_for_function_params(block);
            builder.switch_to_block(block);
            let sps_sp = builder.block_params(block)[1];
            let slot_addr = builder.ins().iadd_imm_s(sps_sp, -8);
            let result = builder.ins().load(types::I64, MemFlagsData::trusted(), slot_addr, 0);
            builder.ins().return_(&[result]);
            builder.seal_all_blocks();
            builder.finalize(self.module.target_config());
        }
        self.module.define_function(return_cont, ctx)?;

        ctx.clear();
        let default_cc = self.module.isa().default_call_conv();
        ctx.func.signature = externs::c_abi_signature(default_cc, &c_entry.signature);
        externs::emit_c_export_wrapper(
            c_entry,
            &self.symbols,
            &mut self.module,
            &mut ctx.func,
            fb_ctx,
        )?;
        self.module.define_function(c_export, ctx)?;
        Ok(())
    }
}

fn declare_symbols(
    module: &mut cranelift_object::ObjectModule, entry: &NativeEntry, plan: &ModulePlan,
) -> Result<DeclaredSymbols, EmitError> {
    let default_cc = module.isa().default_call_conv();
    let tail_sig = simple_sig(CallConv::Tail, &[types::I64, types::I64], &[types::I64]);

    let cases = declare_cases(module, entry, plan, &tail_sig)?;
    let strings = declare_strings(module, plan)?;

    let alloc_slow = module.declare_function(
        CLIF_ALLOC_SLOW_SYMBOL,
        Linkage::Import,
        &simple_sig(default_cc, &[types::I64; 5], &[types::I64]),
    )?;
    let grow_sps = module.declare_function(
        CLIF_GROW_SPS_SYMBOL,
        Linkage::Import,
        &simple_sig(default_cc, &[types::I64; 3], &[types::I64]),
    )?;
    let fail = module.declare_function(
        CLIF_FAIL_SYMBOL,
        Linkage::Import,
        &simple_sig(default_cc, &[types::I32], &[]),
    )?;

    let (entry_begin, entry_end, return_cont, c_export, guard) =
        declare_c_entry_symbols(module, entry, default_cc, &tail_sig)?;
    let hosts = declare_hosts(module, plan, default_cc)?;
    let foreigns = declare_foreigns(module, entry, plan, default_cc, c_export)?;
    let units = declare_units(module, entry, plan, &tail_sig, cases[0])?;

    Ok(DeclaredSymbols {
        tail_sig,
        cases,
        strings,
        alloc_slow,
        grow_sps,
        fail,
        entry_begin,
        entry_end,
        return_cont,
        c_export,
        guard,
        hosts,
        foreigns,
        units,
    })
}

fn declare_cases(
    module: &mut cranelift_object::ObjectModule, entry: &NativeEntry, plan: &ModulePlan,
    tail_sig: &Signature,
) -> Result<Vec<FuncId>, EmitError> {
    let mut cases = Vec::with_capacity(plan.cases.len());
    for case in &plan.cases {
        let id = if case.index == 0
            && let NativeEntry::Unit(symbol) = entry
        {
            module.declare_function(symbol.as_str(), Linkage::Export, tail_sig)?
        } else {
            module.declare_function(
                &format!("zydeco_case_{}", case.index),
                Linkage::Local,
                tail_sig,
            )?
        };
        cases.push(id);
    }
    Ok(cases)
}

fn declare_strings(
    module: &mut cranelift_object::ObjectModule, plan: &ModulePlan,
) -> Result<Vec<DataId>, EmitError> {
    let mut strings = Vec::with_capacity(plan.static_strings.len());
    for literal in &plan.static_strings {
        let data_id = module.declare_data(&literal.symbol, Linkage::Local, false, false)?;
        let mut desc = DataDescription::new();
        desc.define(literal.bytes.clone().into_boxed_slice());
        desc.set_align(8);
        module.define_data(data_id, &desc)?;
        strings.push(data_id);
    }
    Ok(strings)
}

fn declare_c_entry_symbols(
    module: &mut cranelift_object::ObjectModule, entry: &NativeEntry, default_cc: CallConv,
    tail_sig: &Signature,
) -> Result<
    (Option<FuncId>, Option<FuncId>, Option<FuncId>, Option<FuncId>, Option<DataId>),
    EmitError,
> {
    let NativeEntry::C(c_entry) = entry else {
        return Ok((None, None, None, None, None));
    };
    let begin = module.declare_function(
        CLIF_ENTRY_BEGIN_SYMBOL,
        Linkage::Import,
        &simple_sig(default_cc, &[types::I64], &[types::I64]),
    )?;
    let end = module.declare_function(
        CLIF_ENTRY_END_SYMBOL,
        Linkage::Import,
        &simple_sig(default_cc, &[types::I64], &[]),
    )?;
    let return_cont = module.declare_function(RETURN_CONT_SYMBOL, Linkage::Local, tail_sig)?;
    let export_sig = externs::c_abi_signature(default_cc, &c_entry.signature);
    let c_export =
        module.declare_function(c_entry.symbol.as_str(), Linkage::Export, &export_sig)?;
    let guard = module.declare_data(c_entry.guard.as_str(), Linkage::Import, true, false)?;
    Ok((Some(begin), Some(end), Some(return_cont), Some(c_export), Some(guard)))
}

fn declare_hosts(
    module: &mut cranelift_object::ObjectModule, plan: &ModulePlan, default_cc: CallConv,
) -> Result<HashMap<BuiltinValueRole, FuncId>, EmitError> {
    let mut hosts = HashMap::with_capacity(plan.host_roles.len());
    for &role in &plan.host_roles {
        let mut sig = module.make_signature();
        sig.call_conv = default_cc;
        sig.params.push(AbiParam::new(types::I64));
        for _ in 0..role.arity() {
            sig.params.push(AbiParam::new(types::I64));
        }
        if role.spare_box().is_some() {
            sig.params.push(AbiParam::new(types::I64));
        }
        if role.call_mode() == HostCallMode::Returning {
            sig.returns.push(AbiParam::new(types::I64));
        }
        let symbol = format!("{CLIF_HOST_PREFIX}{}", role.host_name());
        let id = module.declare_function(&symbol, Linkage::Import, &sig)?;
        hosts.insert(role, id);
    }
    Ok(hosts)
}

fn declare_foreigns(
    module: &mut cranelift_object::ObjectModule, entry: &NativeEntry, plan: &ModulePlan,
    default_cc: CallConv, c_export: Option<FuncId>,
) -> Result<HashMap<String, FuncId>, EmitError> {
    let mut foreigns = HashMap::new();
    for import in &plan.foreign_imports {
        let symbol = import.target.symbol.as_str();
        if foreigns.contains_key(symbol) {
            continue;
        }
        let func_id = if let (NativeEntry::C(c_entry), Some(exported)) = (entry, c_export)
            && c_entry.symbol == import.target.symbol
        {
            exported
        } else {
            let sig = externs::c_abi_signature(default_cc, &import.signature);
            module.declare_function(symbol, Linkage::Import, &sig)?
        };
        foreigns.insert(symbol.to_owned(), func_id);
    }
    Ok(foreigns)
}

fn declare_units(
    module: &mut cranelift_object::ObjectModule, entry: &NativeEntry, plan: &ModulePlan,
    tail_sig: &Signature, case_0: FuncId,
) -> Result<HashMap<String, FuncId>, EmitError> {
    let mut units = HashMap::new();
    for unit in &plan.unit_imports {
        let symbol = unit.target.symbol.as_str();
        if units.contains_key(symbol) {
            continue;
        }
        let func_id = if let NativeEntry::Unit(unit_sym) = entry
            && *unit_sym == unit.target.symbol
        {
            case_0
        } else {
            module.declare_function(symbol, Linkage::Import, tail_sig)?
        };
        units.insert(symbol.to_owned(), func_id);
    }
    Ok(units)
}

pub(crate) fn simple_sig(
    call_conv: CallConv, params: &[types::Type], returns: &[types::Type],
) -> Signature {
    let mut sig = Signature::new(call_conv);
    for &ty in params {
        sig.params.push(AbiParam::new(ty));
    }
    for &ty in returns {
        sig.returns.push(AbiParam::new(ty));
    }
    sig
}
