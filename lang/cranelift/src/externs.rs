use crate::EmitError;
use crate::lower::CaseEmitter;
use crate::module::DeclaredSymbols;
use crate::scalar::{decode_integer, encode_integer};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{
    AbiParam, BlockArg, Function, InstBuilder, MemFlagsData, Signature, TrapCode,
    Value as ClifValue, types,
};
use cranelift_codegen::isa::CallConv;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::Module;
use cranelift_object::ObjectModule;
use zydeco_abi::{HostCallMode, RuntimeFailure, SpareBox};
use zydeco_machine::clif::{
    ALLOC_KIND_OPAQUE, VMCTX_ALLOC_LIMIT, VMCTX_ALLOC_PTR, VMCTX_SPS_LIMIT, VMCTX_SPS_SP,
    VMCTX_TRANSFER_ARITY, VMCTX_TRANSFER_CLOSURE, VMCTX_TRANSFER_FIRST, VMCTX_TRANSFER_SECOND,
};
use zydeco_machine::closure::ClosureField;
use zydeco_machine::word::ScalarRepresentation;
use zydeco_stackir::low::syntax::{ExternalFunction, StackId};
use zydeco_syntax::{
    BuiltinValueRole, ForeignImport, ForeignParameter, ForeignResult, ForeignSignature, IntegerType,
};

pub(crate) fn c_abi_signature(call_conv: CallConv, sig: &ForeignSignature) -> Signature {
    let mut out = Signature::new(call_conv);
    for &param in sig.parameters() {
        out.params.push(c_abi_param_for_param(param));
    }
    if let ForeignResult::Integer(ty) = sig.result() {
        out.returns.push(c_abi_param_for_int(ty));
    }
    out
}

fn c_abi_param_for_param(param: ForeignParameter) -> AbiParam {
    match param {
        | ForeignParameter::Address => AbiParam::new(types::I64),
        | ForeignParameter::Integer(ty) => c_abi_param_for_int(ty),
    }
}

fn c_abi_param_for_int(ty: IntegerType) -> AbiParam {
    let extend = |ty_ir| {
        let param = AbiParam::new(ty_ir);
        if ty.is_signed() { param.sext() } else { param.uext() }
    };
    match ty {
        | IntegerType::Int8 | IntegerType::UInt8 => extend(types::I8),
        | IntegerType::Int16 | IntegerType::UInt16 => extend(types::I16),
        | IntegerType::Int32 | IntegerType::UInt32 => extend(types::I32),
        | IntegerType::Int64 | IntegerType::UInt64 | IntegerType::Int | IntegerType::UInt => {
            AbiParam::new(types::I64)
        }
    }
}

pub(crate) fn emit_extern_call(
    em: &mut CaseEmitter<'_, '_>, function: ExternalFunction, stack: StackId,
) -> Result<(), EmitError> {
    match function {
        | ExternalFunction::Host(role) => emit_host_call(em, role, stack),
        | ExternalFunction::Foreign(import) => emit_foreign_call(em, &import, stack),
        | ExternalFunction::Unit(unit) => {
            let unit_id = em.symbols.units[unit.target.symbol.as_str()];
            em.emit_stack(stack)?;
            let sp = em.current_sp();
            let fref = em.func_ref(unit_id);
            em.builder.ins().return_call(fref, &[em.vmctx, sp]);
            Ok(())
        }
    }
}

fn emit_host_call(
    em: &mut CaseEmitter<'_, '_>, role: BuiltinValueRole, stack: StackId,
) -> Result<(), EmitError> {
    em.emit_stack(stack)?;
    let arity = role.arity();
    let mut popped_args = Vec::with_capacity(arity);
    for _ in 0..arity {
        popped_args.push(em.pop_sps_word());
    }

    match role.call_mode() {
        | HostCallMode::Returning => {
            let ret_code = em.pop_sps_word();
            let args = if role.spare_box() == Some(SpareBox::Opaque) {
                let temp_base = em.temp_vars.len();
                let arg_vars: Vec<_> = popped_args.iter().map(|&w| em.push_temp(w)).collect();
                let spare = em.emit_alloc(ALLOC_KIND_OPAQUE, 1);
                let mut call_args = Vec::with_capacity(2 + arg_vars.len());
                call_args.push(em.vmctx);
                for var in arg_vars {
                    call_args.push(em.builder.use_var(var));
                }
                call_args.push(spare);
                em.temp_vars.truncate(temp_base);
                call_args
            } else {
                let mut call_args = Vec::with_capacity(1 + popped_args.len());
                call_args.push(em.vmctx);
                call_args.extend(popped_args);
                call_args
            };

            let sp = em.current_sp();
            em.builder.ins().store(MemFlagsData::trusted(), sp, em.vmctx, VMCTX_SPS_SP);
            let host_id = em.symbols.hosts[&role];
            let host_ref = em.func_ref(host_id);
            let call = em.builder.ins().call(host_ref, &args);
            let res = em.builder.inst_results(call)[0];
            em.push_sps_words(&[res]);
            let next_sp = em.current_sp();
            let tail_sig = em.tail_sig_ref();
            em.builder.ins().return_call_indirect(tail_sig, ret_code, &[em.vmctx, next_sp]);
            Ok(())
        }
        | HostCallMode::Control => {
            let mut call_args = Vec::with_capacity(1 + popped_args.len());
            call_args.push(em.vmctx);
            call_args.extend(popped_args);
            let sp = em.current_sp();
            em.builder.ins().store(MemFlagsData::trusted(), sp, em.vmctx, VMCTX_SPS_SP);
            let host_id = em.symbols.hosts[&role];
            let host_ref = em.func_ref(host_id);
            em.builder.ins().call(host_ref, &call_args);

            let arity = em.builder.ins().load(
                types::I64,
                MemFlagsData::trusted(),
                em.vmctx,
                VMCTX_TRANSFER_ARITY,
            );
            let closure = em.builder.ins().load(
                types::I64,
                MemFlagsData::trusted(),
                em.vmctx,
                VMCTX_TRANSFER_CLOSURE,
            );
            let first = em.builder.ins().load(
                types::I64,
                MemFlagsData::trusted(),
                em.vmctx,
                VMCTX_TRANSFER_FIRST,
            );
            let second = em.builder.ins().load(
                types::I64,
                MemFlagsData::trusted(),
                em.vmctx,
                VMCTX_TRANSFER_SECOND,
            );

            em.ensure_sps_space(3);
            let sp = em.current_sp();
            let b0 = em.builder.create_block();
            let b1_or_2 = em.builder.create_block();
            let b1 = em.builder.create_block();
            let b2 = em.builder.create_block();
            let dispatch = em.builder.create_block();
            em.builder.append_block_param(dispatch, types::I64);

            let is_zero = em.builder.ins().icmp_imm_s(IntCC::Equal, arity, 0);
            em.builder.ins().brif(is_zero, b0, &[], b1_or_2, &[]);

            em.builder.switch_to_block(b1_or_2);
            let is_one = em.builder.ins().icmp_imm_s(IntCC::Equal, arity, 1);
            em.builder.ins().brif(is_one, b1, &[], b2, &[]);

            em.builder.switch_to_block(b0);
            let env0 = em.builder.ins().load(
                types::I64,
                MemFlagsData::trusted(),
                closure,
                ClosureField::Environment.offset() as i32,
            );
            em.builder.ins().store(MemFlagsData::trusted(), env0, sp, 0);
            let sp0 = em.builder.ins().iadd_imm_s(sp, 8);
            em.builder.ins().jump(dispatch, &[BlockArg::Value(sp0)]);

            em.builder.switch_to_block(b1);
            em.builder.ins().store(MemFlagsData::trusted(), first, sp, 0);
            let env1 = em.builder.ins().load(
                types::I64,
                MemFlagsData::trusted(),
                closure,
                ClosureField::Environment.offset() as i32,
            );
            em.builder.ins().store(MemFlagsData::trusted(), env1, sp, 8);
            let sp1 = em.builder.ins().iadd_imm_s(sp, 16);
            em.builder.ins().jump(dispatch, &[BlockArg::Value(sp1)]);

            em.builder.switch_to_block(b2);
            em.builder.ins().store(MemFlagsData::trusted(), second, sp, 0);
            em.builder.ins().store(MemFlagsData::trusted(), first, sp, 8);
            let env2 = em.builder.ins().load(
                types::I64,
                MemFlagsData::trusted(),
                closure,
                ClosureField::Environment.offset() as i32,
            );
            em.builder.ins().store(MemFlagsData::trusted(), env2, sp, 16);
            let sp2 = em.builder.ins().iadd_imm_s(sp, 24);
            em.builder.ins().jump(dispatch, &[BlockArg::Value(sp2)]);

            em.builder.switch_to_block(dispatch);
            let final_sp = em.builder.block_params(dispatch)[0];
            em.set_sp(final_sp);
            let code = em.builder.ins().load(
                types::I64,
                MemFlagsData::trusted(),
                closure,
                ClosureField::Code.offset() as i32,
            );
            let tail_sig = em.tail_sig_ref();
            em.builder.ins().return_call_indirect(tail_sig, code, &[em.vmctx, final_sp]);
            Ok(())
        }
    }
}

fn emit_foreign_call(
    em: &mut CaseEmitter<'_, '_>, import: &ForeignImport, stack: StackId,
) -> Result<(), EmitError> {
    em.emit_stack(stack)?;
    let params = import.signature.parameters();
    let mut popped = Vec::with_capacity(params.len());
    for _ in 0..params.len() {
        popped.push(em.pop_sps_word());
    }
    let ret_code = em.pop_sps_word();
    let sp = em.current_sp();
    em.builder.ins().store(MemFlagsData::trusted(), sp, em.vmctx, VMCTX_SPS_SP);

    let mut c_args = Vec::with_capacity(params.len());
    for (&param, word) in params.iter().zip(popped) {
        let arg = match param {
            | ForeignParameter::Address => word,
            | ForeignParameter::Integer(ty) => {
                let raw = decode_integer(em, ty, word);
                reduce_int_for_c(em, ty, raw)
            }
        };
        c_args.push(arg);
    }

    let f_id = em.symbols.foreigns[import.target.symbol.as_str()];
    let f_ref = em.func_ref(f_id);
    let call = em.builder.ins().call(f_ref, &c_args);

    let res_word = match import.signature.result() {
        | ForeignResult::Unit => em.builder.ins().iconst(types::I64, 1),
        | ForeignResult::Integer(ty) => {
            let raw_c = em.builder.inst_results(call)[0];
            let raw_i64 = extend_int_from_c(em, ty, raw_c);
            validate_tag_range(em, ty, raw_i64);
            encode_integer(em, ty, raw_i64)
        }
    };
    em.push_sps_words(&[res_word]);
    let next_sp = em.current_sp();
    let tail_sig = em.tail_sig_ref();
    em.builder.ins().return_call_indirect(tail_sig, ret_code, &[em.vmctx, next_sp]);
    Ok(())
}

fn reduce_int_for_c(em: &mut CaseEmitter<'_, '_>, ty: IntegerType, raw: ClifValue) -> ClifValue {
    match ty {
        | IntegerType::Int8 | IntegerType::UInt8 => em.builder.ins().ireduce(types::I8, raw),
        | IntegerType::Int16 | IntegerType::UInt16 => em.builder.ins().ireduce(types::I16, raw),
        | IntegerType::Int32 | IntegerType::UInt32 => em.builder.ins().ireduce(types::I32, raw),
        | _ => raw,
    }
}

fn extend_int_from_c(em: &mut CaseEmitter<'_, '_>, ty: IntegerType, raw: ClifValue) -> ClifValue {
    match ty {
        | IntegerType::Int8 | IntegerType::Int16 | IntegerType::Int32 => {
            em.builder.ins().sextend(types::I64, raw)
        }
        | IntegerType::UInt8 | IntegerType::UInt16 | IntegerType::UInt32 => {
            em.builder.ins().uextend(types::I64, raw)
        }
        | _ => raw,
    }
}

fn validate_tag_range(em: &mut CaseEmitter<'_, '_>, ty: IntegerType, raw: ClifValue) {
    if !matches!(ty, IntegerType::Int | IntegerType::UInt) {
        return;
    }
    let shifted = em.builder.ins().ishl_imm_u(raw, 1);
    let roundtrip = if ty.is_signed() {
        em.builder.ins().sshr_imm_u(shifted, 1)
    } else {
        em.builder.ins().ushr_imm_u(shifted, 1)
    };
    let ok = em.builder.ins().icmp(IntCC::Equal, raw, roundtrip);
    let ok_block = em.builder.create_block();
    let fail_block = em.builder.create_block();
    em.builder.set_cold_block(fail_block);
    em.builder.ins().brif(ok, ok_block, &[], fail_block, &[]);

    em.builder.switch_to_block(fail_block);
    em.emit_fail(RuntimeFailure::IntegerRange);

    em.builder.switch_to_block(ok_block);
}

struct WrapperHelper {
    alloc_slow: cranelift_codegen::ir::FuncRef,
    grow_sps: cranelift_codegen::ir::FuncRef,
    fail: cranelift_codegen::ir::FuncRef,
}

pub(crate) fn emit_c_export_wrapper(
    c_entry: &crate::CExportEntry, symbols: &DeclaredSymbols, module: &mut ObjectModule,
    func: &mut Function, fb_ctx: &mut FunctionBuilderContext,
) -> Result<(), EmitError> {
    let mut builder = FunctionBuilder::new(func, fb_ctx);
    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);

    let c_params: Vec<ClifValue> = builder.block_params(entry_block).to_vec();
    let guard_gv = module.declare_data_in_func(symbols.guard.expect("guard data"), builder.func);
    let guard_ptr = builder.ins().symbol_value(types::I64, guard_gv);
    let begin_ref = module.declare_func_in_func(symbols.entry_begin.expect("begin"), builder.func);
    let end_ref = module.declare_func_in_func(symbols.entry_end.expect("end"), builder.func);
    let ret_cont_ref =
        module.declare_func_in_func(symbols.return_cont.expect("ret_cont"), builder.func);
    let case0_ref = module.declare_func_in_func(symbols.cases[0], builder.func);
    let helper = WrapperHelper {
        alloc_slow: module.declare_func_in_func(symbols.alloc_slow, builder.func),
        grow_sps: module.declare_func_in_func(symbols.grow_sps, builder.func),
        fail: module.declare_func_in_func(symbols.fail, builder.func),
    };

    let begin_call = builder.ins().call(begin_ref, &[guard_ptr]);
    let vmctx = builder.inst_results(begin_call)[0];
    let sp_var = builder.declare_var(types::I64);
    let init_sp = builder.ins().load(types::I64, MemFlagsData::trusted(), vmctx, VMCTX_SPS_SP);
    builder.def_var(sp_var, init_sp);

    let needed = 1 + c_params.len();
    wrapper_ensure_sps(&mut builder, vmctx, sp_var, helper.grow_sps, needed);
    let ret_addr = builder.ins().func_addr(types::I64, ret_cont_ref);
    wrapper_push_word(&mut builder, vmctx, sp_var, ret_addr);

    for (idx, &param) in c_entry.signature.parameters().iter().enumerate().rev() {
        let word = match param {
            | ForeignParameter::Address => c_params[idx],
            | ForeignParameter::Integer(ty) => {
                wrapper_encode_c_param(&mut builder, vmctx, sp_var, &helper, ty, c_params[idx])
            }
        };
        wrapper_push_word(&mut builder, vmctx, sp_var, word);
    }

    let call_sp = builder.use_var(sp_var);
    let run_call = builder.ins().call(case0_ref, &[vmctx, call_sp]);
    let res_word = builder.inst_results(run_call)[0];

    let c_ret = match c_entry.signature.result() {
        | ForeignResult::Unit => None,
        | ForeignResult::Integer(ty) => Some(wrapper_decode_c_ret(&mut builder, ty, res_word)),
    };

    builder.ins().call(end_ref, &[vmctx]);
    match c_ret {
        | Some(v) => {
            builder.ins().return_(&[v]);
        }
        | None => {
            builder.ins().return_(&[]);
        }
    }
    builder.seal_all_blocks();
    builder.finalize(module.target_config());
    Ok(())
}

fn wrapper_push_word(
    builder: &mut FunctionBuilder<'_>, vmctx: ClifValue, sp_var: Variable, word: ClifValue,
) {
    let sp = builder.use_var(sp_var);
    builder.ins().store(MemFlagsData::trusted(), word, sp, 0);
    let next_sp = builder.ins().iadd_imm_s(sp, 8);
    builder.def_var(sp_var, next_sp);
    builder.ins().store(MemFlagsData::trusted(), next_sp, vmctx, VMCTX_SPS_SP);
}

fn wrapper_ensure_sps(
    builder: &mut FunctionBuilder<'_>, vmctx: ClifValue, sp_var: Variable,
    grow_sps: cranelift_codegen::ir::FuncRef, needed_words: usize,
) {
    let sp = builder.use_var(sp_var);
    let limit = builder.ins().load(types::I64, MemFlagsData::trusted(), vmctx, VMCTX_SPS_LIMIT);
    let avail = builder.ins().isub(limit, sp);
    let ok = builder.ins().icmp_imm_s(
        IntCC::UnsignedGreaterThanOrEqual,
        avail,
        (needed_words * 8) as i64,
    );
    let fast = builder.create_block();
    let slow = builder.create_block();
    let merge = builder.create_block();
    builder.append_block_param(merge, types::I64);
    builder.set_cold_block(slow);
    builder.ins().brif(ok, fast, &[], slow, &[]);

    builder.switch_to_block(fast);
    builder.ins().jump(merge, &[BlockArg::Value(sp)]);

    builder.switch_to_block(slow);
    let n = builder.ins().iconst(types::I64, needed_words as i64);
    let call = builder.ins().call(grow_sps, &[vmctx, sp, n]);
    let grown = builder.inst_results(call)[0];
    builder.ins().jump(merge, &[BlockArg::Value(grown)]);

    builder.switch_to_block(merge);
    let merged = builder.block_params(merge)[0];
    builder.def_var(sp_var, merged);
}

fn wrapper_encode_c_param(
    builder: &mut FunctionBuilder<'_>, vmctx: ClifValue, sp_var: Variable, helper: &WrapperHelper,
    ty: IntegerType, val: ClifValue,
) -> ClifValue {
    let raw_i64 = match ty {
        | IntegerType::Int8 | IntegerType::Int16 | IntegerType::Int32 => {
            builder.ins().sextend(types::I64, val)
        }
        | IntegerType::UInt8 | IntegerType::UInt16 | IntegerType::UInt32 => {
            builder.ins().uextend(types::I64, val)
        }
        | _ => val,
    };
    if matches!(ty, IntegerType::Int | IntegerType::UInt) {
        let shifted = builder.ins().ishl_imm_u(raw_i64, 1);
        let roundtrip = if ty.is_signed() {
            builder.ins().sshr_imm_u(shifted, 1)
        } else {
            builder.ins().ushr_imm_u(shifted, 1)
        };
        let ok = builder.ins().icmp(IntCC::Equal, raw_i64, roundtrip);
        let ok_blk = builder.create_block();
        let fail_blk = builder.create_block();
        builder.set_cold_block(fail_blk);
        builder.ins().brif(ok, ok_blk, &[], fail_blk, &[]);
        builder.switch_to_block(fail_blk);
        let code = builder.ins().iconst(types::I32, i64::from(RuntimeFailure::IntegerRange.code()));
        builder.ins().call(helper.fail, &[code]);
        builder.ins().trap(TrapCode::user(1).expect("user trap code 1"));
        builder.switch_to_block(ok_blk);
    }
    if ty.representation() == ScalarRepresentation::Immediate {
        let shifted = builder.ins().ishl_imm_u(raw_i64, 1);
        return builder.ins().bor_imm_u(shifted, 1);
    }

    let bump = builder.ins().load(types::I64, MemFlagsData::trusted(), vmctx, VMCTX_ALLOC_PTR);
    let limit = builder.ins().load(types::I64, MemFlagsData::trusted(), vmctx, VMCTX_ALLOC_LIMIT);
    let avail = builder.ins().isub(limit, bump);
    let has_room = builder.ins().icmp_imm_s(IntCC::UnsignedGreaterThanOrEqual, avail, 24);
    let fast = builder.create_block();
    let slow = builder.create_block();
    let merge = builder.create_block();
    builder.append_block_param(merge, types::I64);
    builder.set_cold_block(slow);
    builder.ins().brif(has_room, fast, &[], slow, &[]);

    builder.switch_to_block(fast);
    let w1 = builder.ins().iconst(types::I64, 1);
    let k = builder.ins().iconst(types::I64, ALLOC_KIND_OPAQUE as i64);
    builder.ins().store(MemFlagsData::trusted(), w1, bump, 0);
    builder.ins().store(MemFlagsData::trusted(), k, bump, 8);
    let next_bump = builder.ins().iadd_imm_s(bump, 24);
    builder.ins().store(MemFlagsData::trusted(), next_bump, vmctx, VMCTX_ALLOC_PTR);
    let f_payload = builder.ins().iadd_imm_s(bump, 16);
    builder.ins().jump(merge, &[BlockArg::Value(f_payload)]);

    builder.switch_to_block(slow);
    let sp = builder.use_var(sp_var);
    builder.ins().store(MemFlagsData::trusted(), sp, vmctx, VMCTX_SPS_SP);
    let w1s = builder.ins().iconst(types::I64, 1);
    let ks = builder.ins().iconst(types::I64, ALLOC_KIND_OPAQUE as i64);
    let z = builder.ins().iconst(types::I64, 0);
    let call = builder.ins().call(helper.alloc_slow, &[vmctx, w1s, ks, z, z]);
    let s_payload = builder.inst_results(call)[0];
    builder.ins().jump(merge, &[BlockArg::Value(s_payload)]);

    builder.switch_to_block(merge);
    let payload = builder.block_params(merge)[0];
    builder.ins().store(MemFlagsData::trusted(), raw_i64, payload, 0);
    payload
}

fn wrapper_decode_c_ret(
    builder: &mut FunctionBuilder<'_>, ty: IntegerType, word: ClifValue,
) -> ClifValue {
    let raw = match ty.representation() {
        | ScalarRepresentation::Immediate => {
            if ty.is_signed() {
                builder.ins().sshr_imm_u(word, 1)
            } else {
                builder.ins().ushr_imm_u(word, 1)
            }
        }
        | ScalarRepresentation::OpaqueBox => {
            builder.ins().load(types::I64, MemFlagsData::trusted(), word, 0)
        }
    };
    match ty {
        | IntegerType::Int8 | IntegerType::UInt8 => builder.ins().ireduce(types::I8, raw),
        | IntegerType::Int16 | IntegerType::UInt16 => builder.ins().ireduce(types::I16, raw),
        | IntegerType::Int32 | IntegerType::UInt32 => builder.ins().ireduce(types::I32, raw),
        | _ => raw,
    }
}
