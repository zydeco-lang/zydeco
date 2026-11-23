use crate::EmitError;
use crate::lower::{CaseEmitter, EvalValue};
use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{BlockArg, InstBuilder, MemFlagsData, Value as ClifValue, types};
use zydeco_abi::RuntimeFailure;
use zydeco_machine::clif::ALLOC_KIND_OPAQUE;
use zydeco_machine::word::ScalarRepresentation;
use zydeco_stackir::low::scalar::MemoryCall;
use zydeco_stackir::low::syntax::{self as sps, CompuId, ValueId};
use zydeco_syntax::memory::{AccessWidth, MemoryScalar};
use zydeco_syntax::primitive::{
    ComparisonOp, ComparisonPredicate, FloatArithmetic, IntegerArithmetic, PrimitiveOp,
};
use zydeco_syntax::scalar::{KernelStep, ScalarStep, ScalarType};
use zydeco_syntax::{FloatLiteral, FloatType, IntegerLiteral, IntegerType, Literal};

pub(crate) fn emit_scalar_call(
    em: &mut CaseEmitter<'_, '_>, id: ValueId,
) -> Result<ClifValue, EmitError> {
    let call = em.plan.scalars.call(id).clone();
    let region = call.program.region();
    let temp_base = em.temp_vars.len();
    let mut rev_vars = Vec::with_capacity(call.inputs.len());
    for &input in call.inputs.iter().rev() {
        let word = em.emit_word(input)?;
        rev_vars.push(em.push_temp(word));
    }
    let mut vals: Vec<ClifValue> =
        rev_vars.into_iter().rev().map(|var| em.builder.use_var(var)).collect();
    em.temp_vars.truncate(temp_base);

    for step in &region.steps {
        let out = match *step {
            | ScalarStep::Decode { ty, value } => decode_scalar(em, ty, vals[value.0]),
            | ScalarStep::Encode { ty, raw } => encode_scalar(em, ty, vals[raw.0]),
            | ScalarStep::Arithmetic { operation, operands } => {
                emit_primitive_op(em, operation, vals[operands[0].0], vals[operands[1].0])
            }
        };
        vals.push(out);
    }
    Ok(vals[region.result.0])
}

pub(crate) fn emit_compare_branch(
    em: &mut CaseEmitter<'_, '_>, cmp: sps::CompareBranch,
) -> Result<(), EmitError> {
    let temp_base = em.temp_vars.len();
    let rhs_w = em.emit_word(cmp.operands[1])?;
    let rhs_var = em.push_temp(rhs_w);
    let lhs_w = em.emit_word(cmp.operands[0])?;
    let reloaded_rhs = em.builder.use_var(rhs_var);
    em.temp_vars.truncate(temp_base);

    let cond = match cmp.operation {
        | ComparisonOp::Integer(ty, pred) => {
            let lhs = decode_scalar(em, ScalarType::Integer(ty), lhs_w);
            let rhs = decode_scalar(em, ScalarType::Integer(ty), reloaded_rhs);
            let cc = match (ty.is_signed(), pred) {
                | (_, ComparisonPredicate::Eq) => IntCC::Equal,
                | (true, ComparisonPredicate::Lt) => IntCC::SignedLessThan,
                | (false, ComparisonPredicate::Lt) => IntCC::UnsignedLessThan,
                | (true, ComparisonPredicate::Gt) => IntCC::SignedGreaterThan,
                | (false, ComparisonPredicate::Gt) => IntCC::UnsignedGreaterThan,
            };
            em.builder.ins().icmp(cc, lhs, rhs)
        }
        | ComparisonOp::Float(ty, pred) => {
            let lhs = decode_scalar(em, ScalarType::Float(ty), lhs_w);
            let rhs = decode_scalar(em, ScalarType::Float(ty), reloaded_rhs);
            let cc = match pred {
                | ComparisonPredicate::Eq => FloatCC::Equal,
                | ComparisonPredicate::Lt => FloatCC::LessThan,
                | ComparisonPredicate::Gt => FloatCC::GreaterThan,
            };
            em.builder.ins().fcmp(cc, lhs, rhs)
        }
    };

    let true_block = em.builder.create_block();
    let false_block = em.builder.create_block();
    em.builder.ins().brif(cond, true_block, &[], false_block, &[]);

    let saved_defined = em.save_defined_defs();
    em.builder.switch_to_block(true_block);
    em.emit_compu(cmp.when_true)?;

    em.restore_defined_defs(saved_defined);
    em.builder.switch_to_block(false_block);
    em.emit_compu(cmp.when_false)
}

pub(crate) fn emit_memory_step(
    em: &mut CaseEmitter<'_, '_>, step: sps::MemoryStep,
) -> Result<CompuId, EmitError> {
    match step {
        | sps::MemoryStep::Load { scalar, address, result, next } => {
            let addr = em.emit_word(address)?;
            let raw = raw_load(em, scalar, addr);
            validate_memory_carrier(em, scalar, raw);
            let encoded = encode_memory_value(em, scalar, raw);
            em.bind_pattern(result, EvalValue::Word(encoded))?;
            Ok(next)
        }
        | sps::MemoryStep::Store { scalar, address, value, next } => {
            let temp_base = em.temp_vars.len();
            let val_w = em.emit_word(value)?;
            let val_var = em.push_temp(val_w);
            let addr = em.emit_word(address)?;
            let reloaded_val = em.builder.use_var(val_var);
            em.temp_vars.truncate(temp_base);
            let raw = decode_memory_value(em, scalar, reloaded_val);
            raw_store(em, scalar, addr, raw);
            Ok(next)
        }
    }
}

pub(crate) fn emit_memory_call(
    em: &mut CaseEmitter<'_, '_>, call: &MemoryCall,
) -> Result<(), EmitError> {
    for binding in &call.address_bindings {
        let value = em.emit_value(binding.value)?;
        em.bind_pattern(binding.binder, value)?;
    }

    let temp_base = em.temp_vars.len();
    let mut rev_inputs = Vec::with_capacity(call.inputs.len());
    for &input in call.inputs.iter().rev() {
        let w = em.emit_word(input)?;
        rev_inputs.push(em.push_temp(w));
    }
    let store_w = em.emit_word(call.store_address)?;
    let store_var = em.push_temp(store_w);
    let load_w = em.emit_word(call.load_address)?;
    let load_var = em.push_temp(load_w);

    let input_words: Vec<ClifValue> =
        rev_inputs.into_iter().rev().map(|v| em.builder.use_var(v)).collect();
    let load_addr = em.builder.use_var(load_var);
    let store_addr = em.builder.use_var(store_var);
    em.temp_vars.truncate(temp_base);

    let scalar = call.kernel.scalar();
    let region = call.kernel.region();
    let loaded_raw = raw_load(em, scalar, load_addr);
    let mut vals = Vec::with_capacity(1 + input_words.len() + region.steps.len());
    vals.push(loaded_raw);
    for (idx, word) in input_words.into_iter().enumerate() {
        vals.push(decode_scalar(em, region.inputs[idx + 1], word));
    }
    for step in &region.steps {
        let out = match step {
            | KernelStep::Literal(lit) => raw_literal(em, lit),
            | KernelStep::Arithmetic { operation, operands } => {
                emit_primitive_op(em, *operation, vals[operands[0].0], vals[operands[1].0])
            }
        };
        vals.push(out);
    }
    raw_store(em, scalar, store_addr, vals[region.result.0]);
    Ok(())
}

fn raw_literal(em: &mut CaseEmitter<'_, '_>, lit: &Literal) -> ClifValue {
    match *lit {
        | Literal::Integer(int) => {
            let bits = match int {
                | IntegerLiteral::Int8(v) => i64::from(v),
                | IntegerLiteral::Int16(v) => i64::from(v),
                | IntegerLiteral::Int32(v) => i64::from(v),
                | IntegerLiteral::Int64(v) | IntegerLiteral::Int(v) => v,
                | IntegerLiteral::UInt8(v) => i64::from(v),
                | IntegerLiteral::UInt16(v) => i64::from(v),
                | IntegerLiteral::UInt32(v) => i64::from(v),
                | IntegerLiteral::UInt64(v) | IntegerLiteral::UInt(v) => v as i64,
                | IntegerLiteral::Unresolved(_) => {
                    unreachable!("unresolved integer literal reached codegen")
                }
            };
            em.builder.ins().iconst(types::I64, bits)
        }
        | Literal::Float(FloatLiteral::Float32(bits)) => {
            em.builder.ins().f32const(f32::from_bits(bits))
        }
        | Literal::Float(FloatLiteral::Float64(bits)) => {
            em.builder.ins().f64const(f64::from_bits(bits))
        }
        | Literal::Char(ch) => em.builder.ins().iconst(types::I64, i64::from(ch as u32)),
        | Literal::String(_) => unreachable!("kernel literal is never a string"),
    }
}

pub(crate) fn decode_scalar(
    em: &mut CaseEmitter<'_, '_>, ty: ScalarType, word: ClifValue,
) -> ClifValue {
    match ty {
        | ScalarType::Integer(int_ty) => decode_integer(em, int_ty, word),
        | ScalarType::Float(FloatType::Float32) => {
            let shifted = em.builder.ins().ushr_imm_u(word, 1);
            let bits32 = em.builder.ins().ireduce(types::I32, shifted);
            em.builder.ins().bitcast(types::F32, MemFlagsData::new(), bits32)
        }
        | ScalarType::Float(FloatType::Float64) => {
            em.builder.ins().load(types::F64, MemFlagsData::trusted(), word, 0)
        }
    }
}

pub(crate) fn decode_integer(
    em: &mut CaseEmitter<'_, '_>, ty: IntegerType, word: ClifValue,
) -> ClifValue {
    match ty.representation() {
        | ScalarRepresentation::Immediate => {
            let shifted = if ty.is_signed() {
                em.builder.ins().sshr_imm_u(word, 1)
            } else {
                em.builder.ins().ushr_imm_u(word, 1)
            };
            narrow_integer(em, ty, shifted)
        }
        | ScalarRepresentation::OpaqueBox => {
            em.builder.ins().load(types::I64, MemFlagsData::trusted(), word, 0)
        }
    }
}

pub(crate) fn encode_scalar(
    em: &mut CaseEmitter<'_, '_>, ty: ScalarType, raw: ClifValue,
) -> ClifValue {
    match ty {
        | ScalarType::Integer(int_ty) => encode_integer(em, int_ty, raw),
        | ScalarType::Float(FloatType::Float32) => {
            let bits32 = em.builder.ins().bitcast(types::I32, MemFlagsData::new(), raw);
            let bits64 = em.builder.ins().uextend(types::I64, bits32);
            let shifted = em.builder.ins().ishl_imm_u(bits64, 1);
            em.builder.ins().bor_imm_u(shifted, 1)
        }
        | ScalarType::Float(FloatType::Float64) => {
            let ptr = em.emit_alloc(ALLOC_KIND_OPAQUE, 1);
            em.builder.ins().store(MemFlagsData::trusted(), raw, ptr, 0);
            ptr
        }
    }
}

pub(crate) fn encode_integer(
    em: &mut CaseEmitter<'_, '_>, ty: IntegerType, raw: ClifValue,
) -> ClifValue {
    match ty.representation() {
        | ScalarRepresentation::Immediate => {
            let shifted = em.builder.ins().ishl_imm_u(raw, 1);
            em.builder.ins().bor_imm_u(shifted, 1)
        }
        | ScalarRepresentation::OpaqueBox => {
            let ptr = em.emit_alloc(ALLOC_KIND_OPAQUE, 1);
            em.builder.ins().store(MemFlagsData::trusted(), raw, ptr, 0);
            ptr
        }
    }
}

fn narrow_integer(em: &mut CaseEmitter<'_, '_>, ty: IntegerType, val: ClifValue) -> ClifValue {
    match ty {
        | IntegerType::Int8 => {
            let r = em.builder.ins().ireduce(types::I8, val);
            em.builder.ins().sextend(types::I64, r)
        }
        | IntegerType::Int16 => {
            let r = em.builder.ins().ireduce(types::I16, val);
            em.builder.ins().sextend(types::I64, r)
        }
        | IntegerType::Int32 => {
            let r = em.builder.ins().ireduce(types::I32, val);
            em.builder.ins().sextend(types::I64, r)
        }
        | IntegerType::Int => {
            let s = em.builder.ins().ishl_imm_u(val, 1);
            em.builder.ins().sshr_imm_u(s, 1)
        }
        | IntegerType::Int64 | IntegerType::UInt64 => val,
        | IntegerType::UInt8 => {
            let r = em.builder.ins().ireduce(types::I8, val);
            em.builder.ins().uextend(types::I64, r)
        }
        | IntegerType::UInt16 => {
            let r = em.builder.ins().ireduce(types::I16, val);
            em.builder.ins().uextend(types::I64, r)
        }
        | IntegerType::UInt32 => {
            let r = em.builder.ins().ireduce(types::I32, val);
            em.builder.ins().uextend(types::I64, r)
        }
        | IntegerType::UInt => {
            let s = em.builder.ins().ishl_imm_u(val, 1);
            em.builder.ins().ushr_imm_u(s, 1)
        }
    }
}

fn emit_primitive_op(
    em: &mut CaseEmitter<'_, '_>, op: PrimitiveOp, lhs: ClifValue, rhs: ClifValue,
) -> ClifValue {
    match op {
        | PrimitiveOp::Float(_, arith) => match arith {
            | FloatArithmetic::Add => em.builder.ins().fadd(lhs, rhs),
            | FloatArithmetic::Sub => em.builder.ins().fsub(lhs, rhs),
            | FloatArithmetic::Mul => em.builder.ins().fmul(lhs, rhs),
            | FloatArithmetic::Div => em.builder.ins().fdiv(lhs, rhs),
        },
        | PrimitiveOp::Integer(ty, arith) => match arith {
            | IntegerArithmetic::Add => {
                let raw = em.builder.ins().iadd(lhs, rhs);
                narrow_integer(em, ty, raw)
            }
            | IntegerArithmetic::Sub => {
                let raw = em.builder.ins().isub(lhs, rhs);
                narrow_integer(em, ty, raw)
            }
            | IntegerArithmetic::Mul => {
                let raw = em.builder.ins().imul(lhs, rhs);
                narrow_integer(em, ty, raw)
            }
            | IntegerArithmetic::Div => emit_div_rem(em, ty, lhs, rhs, false),
            | IntegerArithmetic::Mod => emit_div_rem(em, ty, lhs, rhs, true),
        },
    }
}

fn emit_div_rem(
    em: &mut CaseEmitter<'_, '_>, ty: IntegerType, lhs: ClifValue, rhs: ClifValue, is_rem: bool,
) -> ClifValue {
    let is_zero = em.builder.ins().icmp_imm_s(IntCC::Equal, rhs, 0);
    let ok_block = em.builder.create_block();
    let fail_block = em.builder.create_block();
    em.builder.set_cold_block(fail_block);
    em.builder.ins().brif(is_zero, fail_block, &[], ok_block, &[]);

    em.builder.switch_to_block(fail_block);
    em.emit_fail(if is_rem {
        RuntimeFailure::IntegerRemainderByZero
    } else {
        RuntimeFailure::IntegerDivisionByZero
    });

    em.builder.switch_to_block(ok_block);
    let raw = if ty == IntegerType::Int64 {
        let is_neg_one = em.builder.ins().icmp_imm_s(IntCC::Equal, rhs, -1);
        let neg_one_block = em.builder.create_block();
        let norm_block = em.builder.create_block();
        let merge_block = em.builder.create_block();
        em.builder.append_block_param(merge_block, types::I64);
        em.builder.set_cold_block(neg_one_block);
        em.builder.ins().brif(is_neg_one, neg_one_block, &[], norm_block, &[]);

        em.builder.switch_to_block(neg_one_block);
        let edge_res = if is_rem {
            em.builder.ins().iconst(types::I64, 0)
        } else {
            em.builder.ins().ineg(lhs)
        };
        em.builder.ins().jump(merge_block, &[BlockArg::Value(edge_res)]);

        em.builder.switch_to_block(norm_block);
        let norm_res =
            if is_rem { em.builder.ins().srem(lhs, rhs) } else { em.builder.ins().sdiv(lhs, rhs) };
        em.builder.ins().jump(merge_block, &[BlockArg::Value(norm_res)]);

        em.builder.switch_to_block(merge_block);
        em.builder.block_params(merge_block)[0]
    } else if ty.is_signed() {
        if is_rem { em.builder.ins().srem(lhs, rhs) } else { em.builder.ins().sdiv(lhs, rhs) }
    } else if is_rem {
        em.builder.ins().urem(lhs, rhs)
    } else {
        em.builder.ins().udiv(lhs, rhs)
    };
    narrow_integer(em, ty, raw)
}

fn raw_load(em: &mut CaseEmitter<'_, '_>, scalar: MemoryScalar, addr: ClifValue) -> ClifValue {
    let flags = MemFlagsData::new();
    match scalar {
        | MemoryScalar::Address => em.builder.ins().load(types::I64, flags, addr, 0),
        | MemoryScalar::Float(FloatType::Float32) => {
            em.builder.ins().load(types::F32, flags, addr, 0)
        }
        | MemoryScalar::Float(FloatType::Float64) => {
            em.builder.ins().load(types::F64, flags, addr, 0)
        }
        | MemoryScalar::Integer(ty) => match (scalar.width(), ty.is_signed()) {
            | (AccessWidth::Bits8, true) => em.builder.ins().sload8(types::I64, flags, addr, 0),
            | (AccessWidth::Bits8, false) => em.builder.ins().uload8(types::I64, flags, addr, 0),
            | (AccessWidth::Bits16, true) => em.builder.ins().sload16(types::I64, flags, addr, 0),
            | (AccessWidth::Bits16, false) => em.builder.ins().uload16(types::I64, flags, addr, 0),
            | (AccessWidth::Bits32, true) => em.builder.ins().sload32(flags, addr, 0),
            | (AccessWidth::Bits32, false) => em.builder.ins().uload32(flags, addr, 0),
            | (AccessWidth::Bits64, _) => em.builder.ins().load(types::I64, flags, addr, 0),
        },
    }
}

fn raw_store(em: &mut CaseEmitter<'_, '_>, scalar: MemoryScalar, addr: ClifValue, raw: ClifValue) {
    let flags = MemFlagsData::new();
    match scalar.width() {
        | AccessWidth::Bits8 => {
            em.builder.ins().istore8(flags, raw, addr, 0);
        }
        | AccessWidth::Bits16 => {
            em.builder.ins().istore16(flags, raw, addr, 0);
        }
        | AccessWidth::Bits32 => match scalar {
            | MemoryScalar::Float(FloatType::Float32) => {
                em.builder.ins().store(flags, raw, addr, 0);
            }
            | _ => {
                em.builder.ins().istore32(flags, raw, addr, 0);
            }
        },
        | AccessWidth::Bits64 => {
            em.builder.ins().store(flags, raw, addr, 0);
        }
    }
}

fn validate_memory_carrier(em: &mut CaseEmitter<'_, '_>, scalar: MemoryScalar, raw: ClifValue) {
    let MemoryScalar::Integer(ty @ (IntegerType::Int | IntegerType::UInt)) = scalar else {
        return;
    };
    let shifted = em.builder.ins().ishl_imm_u(raw, 1);
    let roundtrip = if ty.is_signed() {
        em.builder.ins().sshr_imm_u(shifted, 1)
    } else {
        em.builder.ins().ushr_imm_u(shifted, 1)
    };
    let in_range = em.builder.ins().icmp(IntCC::Equal, raw, roundtrip);
    let ok_block = em.builder.create_block();
    let fail_block = em.builder.create_block();
    em.builder.set_cold_block(fail_block);
    em.builder.ins().brif(in_range, ok_block, &[], fail_block, &[]);

    em.builder.switch_to_block(fail_block);
    em.emit_fail(RuntimeFailure::IntegerRange);

    em.builder.switch_to_block(ok_block);
}

fn encode_memory_value(
    em: &mut CaseEmitter<'_, '_>, scalar: MemoryScalar, raw: ClifValue,
) -> ClifValue {
    match scalar.value_type() {
        | Some(ty) => encode_scalar(em, ty, raw),
        | None => raw,
    }
}

fn decode_memory_value(
    em: &mut CaseEmitter<'_, '_>, scalar: MemoryScalar, word: ClifValue,
) -> ClifValue {
    match scalar.value_type() {
        | Some(ty) => decode_scalar(em, ty, word),
        | None => word,
    }
}
