//! Inline scalar arithmetic, with boxing only on the result's representation boundary.

use super::*;

impl<'a> Emit<'a> for PrimitiveOp {
    type Env = ProgId;

    fn emit(&self, id: ProgId, em: &mut Emitter) {
        em.asm.text.extend([
            Instr::Comment(format!("primitive: {self}")),
            Instr::Pop(Loc::Reg(Reg::Rax)),
            Instr::Pop(Loc::Reg(Reg::Rcx)),
        ]);
        em.shift_stack_parity(-2);
        match *self {
            | PrimitiveOp::Integer(ty, operation) => {
                em.decode_integer(Reg::Rax, ty);
                em.decode_integer(Reg::Rcx, ty);
                let args = BinArgs::ToReg(Reg::Rax, Arg32::Reg(Reg::Rcx));
                match operation {
                    | IntegerArithmetic::Add => em.asm.text.push(Instr::Add(args)),
                    | IntegerArithmetic::Sub => em.asm.text.push(Instr::Sub(args)),
                    | IntegerArithmetic::Mul => em.asm.text.push(Instr::IMul(args)),
                    | IntegerArithmetic::Div | IntegerArithmetic::Mod => {
                        em.integer_division(ty, operation == IntegerArithmetic::Mod, id);
                    }
                }
                em.narrow_integer(ty);
                em.tag_integer();
            }
            | PrimitiveOp::Float(ty, operation) => {
                for reg in [Reg::Rax, Reg::Rcx] {
                    em.asm.text.push(match ty {
                        | FloatType::Float32 => Instr::Shr(ShArgs { reg, by: 1 }),
                        | FloatType::Float64 => {
                            Instr::Mov(MovArgs::ToReg(reg, Arg64::Mem(MemRef { reg, offset: 0 })))
                        }
                    });
                }
                let instruction = match (ty, operation) {
                    | (FloatType::Float32, FloatArithmetic::Add) => FloatOpcode::Addss,
                    | (FloatType::Float32, FloatArithmetic::Sub) => FloatOpcode::Subss,
                    | (FloatType::Float32, FloatArithmetic::Mul) => FloatOpcode::Mulss,
                    | (FloatType::Float32, FloatArithmetic::Div) => FloatOpcode::Divss,
                    | (FloatType::Float64, FloatArithmetic::Add) => FloatOpcode::Addsd,
                    | (FloatType::Float64, FloatArithmetic::Sub) => FloatOpcode::Subsd,
                    | (FloatType::Float64, FloatArithmetic::Mul) => FloatOpcode::Mulsd,
                    | (FloatType::Float64, FloatArithmetic::Div) => FloatOpcode::Divsd,
                };
                em.asm.text.extend([
                    Instr::ToXmm(Xmm::Xmm0, Reg::Rax),
                    Instr::ToXmm(Xmm::Xmm1, Reg::Rcx),
                    Instr::FloatBinary(instruction, Xmm::Xmm0, Xmm::Xmm1),
                    Instr::FromXmm(Reg::Rax, Xmm::Xmm0),
                ]);
                match ty {
                    | FloatType::Float32 => em.tag_integer(),
                    | FloatType::Float64 => em.box_scalar(id),
                }
            }
        }
        em.asm.text.push(Instr::Push(Arg32::Reg(Reg::Rax)));
        em.shift_stack_parity(1);
    }
}

impl Emitter<'_> {
    fn primitive_label(id: ProgId, suffix: &str) -> String {
        format!("primitive_{}_{}", id.concise_inner().replace('#', "_"), suffix)
    }

    fn decode_integer(&mut self, reg: Reg, ty: IntegerType) {
        self.asm.text.push(if ty.is_signed() {
            Instr::Sar(ShArgs { reg, by: 1 })
        } else {
            Instr::Shr(ShArgs { reg, by: 1 })
        });
    }

    fn integer_division(&mut self, ty: IntegerType, remainder: bool, id: ProgId) {
        let nonzero = Self::primitive_label(id, "nonzero");
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

    fn box_scalar(&mut self, id: ProgId) {
        // Raw scalar bits must stay out of the GC's tagged control-stack roots.
        // Like FFI result marshalling, keep them in a callee-saved scratch register.
        self.asm.text.push(Instr::Mov(MovArgs::ToReg(Reg::R12, Arg64::Reg(Reg::Rax))));
        self.emit_alloc_call(1, AllocationKind::Opaque, id);
        self.asm.text.push(Instr::Mov(MovArgs::ToMem(
            MemRef { reg: Reg::Rax, offset: 0 },
            Reg32::Reg(Reg::R12),
        )));
    }
}
