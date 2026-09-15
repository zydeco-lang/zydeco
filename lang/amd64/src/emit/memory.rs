//! Ordered, exact-width accesses to unmanaged storage.

use super::*;
use zydeco_syntax::scalar::{KernelStep, ScalarKernel};
use zydeco_syntax::{
    memory::{AccessKind, MemoryAccess, MemoryScalar},
    word::ScalarRepresentation,
};

impl<'a> Emit<'a> for ScalarKernel {
    type Env = ProgId;

    fn emit(&self, id: ProgId, em: &mut Emitter) {
        let region = self.region();
        let words = self.definitions() + 1;
        let home = |index| MemRef { reg: Reg::Rsp, offset: (index * 8) as i32 };
        em.asm.text.extend([
            Instr::Comment("raw memory kernel: begin".into()),
            Instr::Pop(Loc::Reg(Reg::Rax)),
            Instr::Pop(Loc::Reg(Reg::Rcx)),
            Instr::Sub(BinArgs::ToReg(Reg::Rsp, Arg32::Signed((words * 8) as i32))),
            Instr::Mov(MovArgs::ToMem(home(self.definitions()), Reg32::Reg(Reg::Rcx))),
            Instr::LoadMemory(ScalarMemoryMove {
                width: self.scalar().width(),
                address: MemRef { reg: Reg::Rax, offset: 0 },
            }),
            Instr::Mov(MovArgs::ToMem(home(0), Reg32::Reg(Reg::Rax))),
        ]);
        em.shift_stack_parity(words as i64 - 2);
        // No allocation or source call occurs inside this raw area. Fatal arithmetic
        // helpers cannot return; all ordinary live values remain outside the area.
        for (index, &ty) in region.inputs.iter().enumerate().skip(1) {
            em.asm
                .text
                .push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Mem(home(words + index - 1)))));
            match ty {
                | zydeco_syntax::scalar::ScalarType::Integer(ty) => em.decode_integer(Reg::Rax, ty),
                | zydeco_syntax::scalar::ScalarType::Float(FloatType::Float32) => {
                    em.asm.text.push(Instr::Shr(ShArgs { reg: Reg::Rax, by: 1 }))
                }
                | zydeco_syntax::scalar::ScalarType::Float(FloatType::Float64) => {
                    em.asm.text.push(Instr::Mov(MovArgs::ToReg(
                        Reg::Rax,
                        Arg64::Mem(MemRef { reg: Reg::Rax, offset: 0 }),
                    )))
                }
            }
            em.asm.text.push(Instr::Mov(MovArgs::ToMem(home(index), Reg32::Reg(Reg::Rax))));
        }
        for (index, step) in region.steps.iter().enumerate() {
            match step {
                | KernelStep::Literal(literal) => {
                    let ty = zydeco_syntax::scalar::ScalarType::of_literal(literal)
                        .expect("verified scalar");
                    let bits = MemoryScalar::from(ty).bits(literal).expect("typed scalar bits");
                    em.asm.text.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Unsigned(bits))));
                }
                | KernelStep::Arithmetic { operation, operands } => {
                    em.asm.text.extend([
                        Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Mem(home(operands[0].0)))),
                        Instr::Mov(MovArgs::ToReg(Reg::Rcx, Arg64::Mem(home(operands[1].0)))),
                    ]);
                    em.raw_arithmetic(*operation, id, index);
                }
            }
            em.asm.text.push(Instr::Mov(MovArgs::ToMem(
                home(region.inputs.len() + index),
                Reg32::Reg(Reg::Rax),
            )));
        }
        let consumed = words + region.inputs.len() - 1;
        em.asm.text.extend([
            Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Mem(home(region.result.0)))),
            Instr::Mov(MovArgs::ToReg(Reg::Rcx, Arg64::Mem(home(self.definitions())))),
            Instr::StoreMemory(ScalarMemoryStore {
                width: self.scalar().width(),
                address: MemRef { reg: Reg::Rcx, offset: 0 },
            }),
            Instr::Add(BinArgs::ToReg(Reg::Rsp, Arg32::Signed((consumed * 8) as i32))),
            Instr::Comment("raw memory kernel: end".into()),
        ]);
        em.shift_stack_parity(-(consumed as i64));
    }
}

impl<'a> Emit<'a> for MemoryAccess {
    type Env = ProgId;

    fn emit(&self, id: ProgId, em: &mut Emitter) {
        match self.kind {
            | AccessKind::Load => {
                em.asm.text.extend([
                    Instr::Pop(Loc::Reg(Reg::Rax)),
                    Instr::LoadMemory(ScalarMemoryMove {
                        width: self.scalar.width(),
                        address: MemRef { reg: Reg::Rax, offset: 0 },
                    }),
                ]);
                em.shift_stack_parity(-1);
                em.validate_memory_carrier(self.scalar, id);
                if let MemoryScalar::Integer(ty) = self.scalar {
                    em.narrow_integer(ty);
                }
                if let Some(ty) = self.scalar.value_type() {
                    match ty.representation() {
                        | ScalarRepresentation::Immediate => em.tag_integer(),
                        | ScalarRepresentation::OpaqueBox => {
                            // Raw bits live in a callee-saved register, outside the root range.
                            em.asm
                                .text
                                .push(Instr::Mov(MovArgs::ToReg(Reg::R12, Arg64::Reg(Reg::Rax))));
                            em.emit_alloc_call(1, AllocationKind::Opaque, id);
                            em.asm.text.push(Instr::Mov(MovArgs::ToMem(
                                MemRef { reg: Reg::Rax, offset: 0 },
                                Reg32::Reg(Reg::R12),
                            )));
                        }
                    }
                }
                em.asm.text.push(Instr::Push(Arg32::Reg(Reg::Rax)));
                em.shift_stack_parity(1);
            }
            | AccessKind::Store => {
                em.asm
                    .text
                    .extend([Instr::Pop(Loc::Reg(Reg::Rcx)), Instr::Pop(Loc::Reg(Reg::Rax))]);
                em.shift_stack_parity(-2);
                match self.scalar {
                    | MemoryScalar::Integer(ty) => em.decode_integer(Reg::Rax, ty),
                    | MemoryScalar::Float(FloatType::Float32) => {
                        em.asm.text.push(Instr::Shr(ShArgs { reg: Reg::Rax, by: 1 }))
                    }
                    | MemoryScalar::Float(FloatType::Float64) => em.asm.text.push(Instr::Mov(
                        MovArgs::ToReg(Reg::Rax, Arg64::Mem(MemRef { reg: Reg::Rax, offset: 0 })),
                    )),
                    | MemoryScalar::Address => {}
                }
                em.asm.text.push(Instr::StoreMemory(ScalarMemoryStore {
                    width: self.scalar.width(),
                    address: MemRef { reg: Reg::Rcx, offset: 0 },
                }));
            }
        }
    }
}

impl Emitter<'_> {
    fn validate_memory_carrier(&mut self, scalar: MemoryScalar, id: ProgId) {
        let MemoryScalar::Integer(ty @ (IntegerType::Int | IntegerType::UInt)) = scalar else {
            return;
        };
        let valid = format!("memory_carrier_{}", id.concise_inner().replace('#', "_"));
        self.asm.text.extend([
            Instr::Mov(MovArgs::ToReg(Reg::Rcx, Arg64::Reg(Reg::Rax))),
            Instr::Shl(ShArgs { reg: Reg::Rcx, by: 1 }),
            if ty.is_signed() {
                Instr::Sar(ShArgs { reg: Reg::Rcx, by: 1 })
            } else {
                Instr::Shr(ShArgs { reg: Reg::Rcx, by: 1 })
            },
            Instr::Cmp(BinArgs::ToReg(Reg::Rax, Arg32::Reg(Reg::Rcx))),
            Instr::JCC(ConditionCode::E, JmpArgs::Label(valid.clone())),
        ]);
        self.emit_aligned_call(JmpArgs::Label("zydeco_integer_out_of_range".into()));
        self.asm.text.extend([Instr::Ud2, Instr::Label(valid)]);
    }
}
