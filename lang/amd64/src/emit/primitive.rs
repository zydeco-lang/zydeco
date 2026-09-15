//! Inline scalar arithmetic, with boxing only on the result's representation boundary.

use super::*;
use zydeco_syntax::word::ScalarRepresentation;

use zydeco_syntax::scalar::{
    ScalarId, ScalarProgram, ScalarSlot, ScalarStep, ScalarStorage, ScalarType,
};

impl<'a> Emit<'a> for ScalarProgram {
    type Env = ProgId;

    fn emit(&self, id: ProgId, em: &mut Emitter) {
        let storage = em.frames.scalars[&id].clone();
        let inputs = self.region().inputs.len();
        let values = storage.value_words();
        let raw = storage.raw_words();
        // Existing arguments remain roots. Initialize all additional value homes
        // before any allocation, and keep the raw area below the root cursor.
        for _ in inputs..values {
            em.asm.text.push(Instr::Push(Arg32::Unsigned(1)));
        }
        em.shift_stack_parity((values - inputs) as i64);
        em.asm.text.push(Instr::Mov(MovArgs::ToReg(Reg::R13, Arg64::Reg(Reg::Rsp))));
        if raw != 0 {
            em.asm.text.push(Instr::Sub(BinArgs::ToReg(Reg::Rsp, Arg32::Signed((raw * 8) as i32))));
            em.shift_stack_parity(raw as i64);
        }
        for (index, step) in self.region().steps.iter().enumerate() {
            let target = ScalarId(inputs + index);
            match *step {
                | ScalarStep::Decode { ty, value } => {
                    em.scalar_load(&storage, inputs, value, Reg::Rax);
                    match ty {
                        | ScalarType::Integer(ty) => em.decode_integer(Reg::Rax, ty),
                        | ScalarType::Float(FloatType::Float32) => {
                            em.asm.text.push(Instr::Shr(ShArgs { reg: Reg::Rax, by: 1 }))
                        }
                        | ScalarType::Float(FloatType::Float64) => {
                            em.asm.text.push(Instr::Mov(MovArgs::ToReg(
                                Reg::Rax,
                                Arg64::Mem(MemRef { reg: Reg::Rax, offset: 0 }),
                            )))
                        }
                    }
                }
                | ScalarStep::Encode { ty, raw } => {
                    em.scalar_load(&storage, inputs, raw, Reg::Rax);
                    match ty.representation() {
                        | ScalarRepresentation::Immediate => em.tag_integer(),
                        | ScalarRepresentation::OpaqueBox => {
                            em.asm
                                .text
                                .push(Instr::Mov(MovArgs::ToReg(Reg::R12, Arg64::Reg(Reg::Rax))));
                            em.emit_alloc_call_at(1, AllocationKind::Opaque, id, Reg::R13);
                            em.asm.text.push(Instr::Mov(MovArgs::ToMem(
                                MemRef { reg: Reg::Rax, offset: 0 },
                                Reg32::Reg(Reg::R12),
                            )));
                        }
                    }
                }
                | ScalarStep::Arithmetic { operation, operands } => {
                    em.scalar_load(&storage, inputs, operands[0], Reg::Rax);
                    em.scalar_load(&storage, inputs, operands[1], Reg::Rcx);
                    em.raw_arithmetic(operation, id, index);
                }
            }
            em.asm.text.push(Instr::Mov(MovArgs::ToMem(
                Emitter::scalar_home(&storage, inputs, target),
                Reg32::Reg(Reg::Rax),
            )));
        }
        em.scalar_load(&storage, inputs, self.region().result, Reg::Rax);
        em.asm
            .text
            .push(Instr::Add(BinArgs::ToReg(Reg::Rsp, Arg32::Signed(((values + raw) * 8) as i32))));
        em.shift_stack_parity(-((values + raw) as i64));
        em.asm.text.push(Instr::Push(Arg32::Reg(Reg::Rax)));
        em.shift_stack_parity(1);
    }
}

impl Emitter<'_> {
    fn scalar_home(storage: &ScalarStorage, inputs: usize, id: ScalarId) -> MemRef {
        match storage.slot(id) {
            | ScalarSlot::Value(index) => {
                // Input homes already exist above the newly reserved value homes.
                let index = if index < inputs {
                    storage.value_words() - inputs + index
                } else {
                    index - inputs
                };
                MemRef { reg: Reg::R13, offset: (index * 8) as i32 }
            }
            | ScalarSlot::Raw(index) => MemRef { reg: Reg::Rsp, offset: (index * 8) as i32 },
        }
    }
}

impl Emitter<'_> {
    fn scalar_load(&mut self, storage: &ScalarStorage, inputs: usize, id: ScalarId, reg: Reg) {
        self.asm.text.push(Instr::Mov(MovArgs::ToReg(
            reg,
            Arg64::Mem(Emitter::scalar_home(storage, inputs, id)),
        )));
    }

    fn raw_arithmetic(&mut self, operation: PrimitiveOp, id: ProgId, index: usize) {
        self.asm.text.push(Instr::Comment(format!("primitive: {operation}")));
        match operation {
            | PrimitiveOp::Integer(ty, operation) => {
                let args = BinArgs::ToReg(Reg::Rax, Arg32::Reg(Reg::Rcx));
                match operation {
                    | IntegerArithmetic::Add => self.asm.text.push(Instr::Add(args)),
                    | IntegerArithmetic::Sub => self.asm.text.push(Instr::Sub(args)),
                    | IntegerArithmetic::Mul => self.asm.text.push(Instr::IMul(args)),
                    | IntegerArithmetic::Div | IntegerArithmetic::Mod => {
                        self.integer_division(ty, operation == IntegerArithmetic::Mod, id, index)
                    }
                }
                self.narrow_integer(ty);
            }
            | PrimitiveOp::Float(ty, operation) => {
                let opcode = match (ty, operation) {
                    | (FloatType::Float32, FloatArithmetic::Add) => FloatOpcode::Addss,
                    | (FloatType::Float32, FloatArithmetic::Sub) => FloatOpcode::Subss,
                    | (FloatType::Float32, FloatArithmetic::Mul) => FloatOpcode::Mulss,
                    | (FloatType::Float32, FloatArithmetic::Div) => FloatOpcode::Divss,
                    | (FloatType::Float64, FloatArithmetic::Add) => FloatOpcode::Addsd,
                    | (FloatType::Float64, FloatArithmetic::Sub) => FloatOpcode::Subsd,
                    | (FloatType::Float64, FloatArithmetic::Mul) => FloatOpcode::Mulsd,
                    | (FloatType::Float64, FloatArithmetic::Div) => FloatOpcode::Divsd,
                };
                self.asm.text.extend([
                    Instr::ToXmm(Xmm::Xmm0, Reg::Rax),
                    Instr::ToXmm(Xmm::Xmm1, Reg::Rcx),
                    Instr::FloatBinary(opcode, Xmm::Xmm0, Xmm::Xmm1),
                    Instr::FromXmm(Reg::Rax, Xmm::Xmm0),
                ]);
            }
        }
    }

    fn primitive_label(id: ProgId, suffix: &str) -> String {
        format!("primitive_{}_{}", id.concise_inner().replace('#', "_"), suffix)
    }

    fn decode_integer(&mut self, reg: Reg, ty: IntegerType) {
        self.asm.text.push(if ty.representation() == ScalarRepresentation::OpaqueBox {
            Instr::Mov(MovArgs::ToReg(reg, Arg64::Mem(MemRef { reg, offset: 0 })))
        } else if ty.is_signed() {
            Instr::Sar(ShArgs { reg, by: 1 })
        } else {
            Instr::Shr(ShArgs { reg, by: 1 })
        });
    }

    fn integer_division(&mut self, ty: IntegerType, remainder: bool, id: ProgId, index: usize) {
        let nonzero = Self::primitive_label(id, &format!("nonzero_{index}"));
        let divide = Self::primitive_label(id, &format!("divide_{index}"));
        let done = Self::primitive_label(id, &format!("divided_{index}"));
        self.asm.text.extend([
            Instr::Test(BinArgs::ToReg(Reg::Rcx, Arg32::Reg(Reg::Rcx))),
            Instr::JCC(ConditionCode::NZ, JmpArgs::Label(nonzero.clone())),
        ]);
        self.emit_aligned_call(JmpArgs::Label(
            if remainder {
                "zydeco_integer_remainder_by_zero"
            } else {
                "zydeco_integer_division_by_zero"
            }
            .into(),
        ));
        self.asm.text.extend([Instr::Ud2, Instr::Label(nonzero)]);
        if ty == IntegerType::Int64 {
            self.asm.text.extend([
                Instr::Cmp(BinArgs::ToReg(Reg::Rcx, Arg32::Signed(-1))),
                Instr::JCC(ConditionCode::NE, JmpArgs::Label(divide.clone())),
                Instr::Mov(MovArgs::ToReg(Reg::Rdx, Arg64::Signed(i64::MIN))),
                Instr::Cmp(BinArgs::ToReg(Reg::Rax, Arg32::Reg(Reg::Rdx))),
                Instr::JCC(ConditionCode::NE, JmpArgs::Label(divide.clone())),
            ]);
            if remainder {
                self.asm.text.push(Instr::Xor(BinArgs::ToReg(Reg::Rax, Arg32::Reg(Reg::Rax))));
            }
            self.asm.text.extend([Instr::Jmp(JmpArgs::Label(done.clone())), Instr::Label(divide)]);
        }
        if ty.is_signed() {
            self.asm.text.extend([Instr::Cqo, Instr::IDiv(Reg::Rcx)]);
        } else {
            self.asm.text.extend([
                Instr::Xor(BinArgs::ToReg(Reg::Rdx, Arg32::Reg(Reg::Rdx))),
                Instr::Div(Reg::Rcx),
            ]);
        }
        if remainder {
            self.asm.text.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::Rdx))));
        }
        if ty == IntegerType::Int64 {
            self.asm.text.push(Instr::Label(done));
        }
    }

    fn narrow_integer(&mut self, ty: IntegerType) {
        let by = 64 - ty.bits();
        if by != 0 {
            self.asm.text.push(Instr::Shl(ShArgs { reg: Reg::Rax, by }));
            self.asm.text.push(if ty.is_signed() {
                Instr::Sar(ShArgs { reg: Reg::Rax, by })
            } else {
                Instr::Shr(ShArgs { reg: Reg::Rax, by })
            });
        }
    }

    fn tag_integer(&mut self) {
        self.asm.text.push(Instr::Lea(
            Reg::Rax,
            LeaArgs::Displace {
                base: Reg::Rax,
                scaled_index: Some((Reg::Rax, 1)),
                offset: Some(1),
            },
        ));
    }
}
