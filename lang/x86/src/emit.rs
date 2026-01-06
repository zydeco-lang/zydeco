use super::syntax::*;
use derive_more::{Deref, DerefMut};
use std::collections::{BTreeMap, HashMap, HashSet};
use zydeco_assembly::{
    arena::{AssemblyArena, AssemblyArenaRefLike},
    syntax::{self as sa, Atom, Instruction, Intrinsic, ProgId, Program, SymbolInner},
};
use zydeco_stackir::arena::StackArena;
use zydeco_statics::tyck::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_syntax::*;

pub const ENV_REG: Reg = Reg::Rbp;

pub trait Emit<'a> {
    type Env;
    fn emit(&self, env: Self::Env, em: &mut Emitter);
}

pub struct Emitter<'e> {
    pub spans: &'e SpanArena,
    pub scoped: &'e ScopedArena,
    pub statics: &'e StaticsArena,
    pub stack: &'e StackArena,
    pub assembly: &'e AssemblyArena,

    pub asm: AsmFile,

    tables: Vec<JumpTable>,
    visited: HashSet<ProgId>,
}

impl<'e> Emitter<'e> {
    pub fn new(
        spans: &'e SpanArena, scoped: &'e ScopedArena, statics: &'e StaticsArena,
        stack: &'e StackArena, assembly: &'e AssemblyArena,
    ) -> Self {
        Self {
            spans,
            scoped,
            statics,
            stack,
            assembly,
            asm: AsmFile::new(),
            tables: Vec::new(),
            visited: HashSet::new(),
        }
    }

    pub fn run(mut self) -> AsmFile {
        self.asm.text.extend([
            // zydeco_abort
            Instr::Extern("zydeco_abort".to_string()),
            // zydeco_alloc
            Instr::Extern("zydeco_alloc".to_string()),
        ]);

        // Emit the externs
        for sa::Extern { name, .. } in self.assembly.externs.iter() {
            let label = format!("zydeco_{}", name);
            self.asm.text.push(Instr::Extern(label));
        }

        // Emit rust_call_zydeco functions
        self.asm.text.extend([
            Instr::Global("rust_call_zydeco_0".to_string()),
            Instr::Label("rust_call_zydeco_0".to_string()),
            Instr::Comment("discard the return address that is not needed".to_string()),
            Instr::Pop(Loc::Reg(Reg::Rax)),
            Instr::Comment("push the __env__ (which is in rsi) to the stack".to_string()),
            Instr::Push(Arg32::Reg(Reg::Rsi)),
            Instr::Comment("call the zydeco function __code__ (which is in rdi)".to_string()),
            Instr::Jmp(JmpArgs::Reg(Reg::Rdi)),
        ]);
        self.asm.text.extend([
            Instr::Global("rust_call_zydeco_1".to_string()),
            Instr::Label("rust_call_zydeco_1".to_string()),
            Instr::Comment("discard the return address that is not needed".to_string()),
            Instr::Pop(Loc::Reg(Reg::Rax)),
            Instr::Comment("push the argument 0 (which is in rdx) to the stack".to_string()),
            Instr::Push(Arg32::Reg(Reg::Rdx)),
            Instr::Comment("push the __env__ (which is in rsi) to the stack".to_string()),
            Instr::Push(Arg32::Reg(Reg::Rsi)),
            Instr::Comment("call the zydeco function __code__ (which is in rdi)".to_string()),
            Instr::Jmp(JmpArgs::Reg(Reg::Rdi)),
        ]);

        // Emit wrappers for externs that converts from Zydeco calling convention to x86-64 calling convention
        for sa::Extern { name, arity } in self.assembly.externs.iter() {
            let zydeco_extern_name = format!("zydeco_{}", name);
            let wrapper_inner_name = format!("wrapper_{}", name);
            self.asm.text.push(Instr::Label(wrapper_inner_name.clone()));
            for i in 1..=*arity {
                // place the arguments accordingly
                // using system V AMD64 ABI
                if i <= 6 {
                    let reg = match i {
                        | 1 => Reg::Rdi,
                        | 2 => Reg::Rsi,
                        | 3 => Reg::Rdx,
                        | 4 => Reg::Rcx,
                        | 5 => Reg::R8,
                        | 6 => Reg::R9,
                        | _ => unreachable!(),
                    };
                    self.asm.text.push(Instr::Pop(Loc::Reg(reg)));
                } else {
                    // load to stack - but it's already on the stack
                    // we just need to make sure the position is correct
                    todo!()
                }
            }
            // Todo: a jump or a call? Maybe we should group the externs by
            // whether it's tail called or not?
            self.asm.text.push(Instr::Call(zydeco_extern_name));
        }

        self.asm.text.extend([
            Instr::Global("entry".to_string()),
            Instr::Label("entry".to_string()),
            Instr::Comment("initialize environment".to_string()),
            // initialize the environment
            Instr::Mov(MovArgs::ToReg(ENV_REG, Arg64::Reg(Reg::Rdi))),
        ]);

        // Assert that there's only one entry point, and emit it
        assert_eq!(self.assembly.entry.len(), 1, "expected exactly one entry point");
        let (entry, ()) = self.assembly.entry.iter().next().unwrap();
        entry.emit(EnvMap::new(), &mut self);

        // Emit the named blocks
        for (prog_id, _) in &self.assembly.programs {
            // Todo: context of blocks should be passed from ZASM
            // let context = sa::Context::new();
            let env = EnvMap::new();
            if let Some(label) = self.assembly.prog_label(prog_id) {
                self.asm.text.push(Instr::Label(label));
                prog_id.emit(env, &mut self);
            }
        }

        // Emit the jump tables
        for table in &self.tables {
            let label = table.rodata_label();
            self.asm.rodata.extend([
                Instr::Comment(format!("jump table for {}", table.id.concise_inner())),
                Instr::Label(label.clone()),
            ]);
            for (idx, (name, prog_id)) in table.arms.iter().enumerate() {
                self.asm.rodata.extend([Instr::Comment(format!(
                    "arm {} for {}",
                    name.clone().unwrap_or_else(|| format!("#{}", idx)),
                    prog_id.concise()
                ))]);
                let arm_label = self.assembly.prog_label(prog_id).expect("block name not found");
                self.asm.rodata.push(Instr::Dq(arm_label));
            }
        }

        self.asm
    }
}

#[derive(Debug, Clone, Deref, DerefMut)]
pub struct EnvMap(
    #[deref]
    #[deref_mut]
    HashMap<sa::VarId, i32>,
);
impl EnvMap {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
    fn alloc(&mut self, var_id: sa::VarId) -> i32 {
        let cur = self.len() as i32;
        self.0.insert(var_id, cur);
        cur
    }
}

struct JumpTable {
    id: ProgId,
    arms: Vec<(Option<String>, ProgId)>,
}
impl JumpTable {
    fn rodata_label(&self) -> String {
        format!("jump_table_{}", self.id.concise_inner().replace('#', "_"))
    }
}

impl<'a> Emit<'a> for ProgId {
    type Env = EnvMap;
    fn emit(&self, mut env: Self::Env, em: &mut Emitter) {
        // Avoid infinite loops
        assert!(!em.visited.contains(self), "infinite loop detected");
        em.visited.insert(*self);

        // Emit the program
        match &em.assembly.programs[self] {
            | Program::Instruction(instr, next) => {
                instr.emit(&mut env, em);
                next.emit(env, em);
            }
            | Program::Jump(sa::Jump(target)) => {
                match em.assembly.prog_label(target) {
                    | Some(label) => {
                        // if the target is a named block, then jump to the label
                        em.asm.text.push(Instr::Jmp(JmpArgs::Label(label)));
                    }
                    | None => {
                        // otherwise, directly emit the target program
                        em.asm.text.push(Instr::Comment(format!(
                            "inlined jump to {}",
                            target.concise_inner()
                        )));
                        target.emit(EnvMap::new(), em);
                    }
                }
            }
            | Program::EqJump(sa::EqJump(target)) => {
                // pop two values, compare, and jump to target if equal
                // if target is not a named block, we skip it instead

                // first, pop two values
                em.asm.text.push(Instr::Pop(Loc::Reg(Reg::Rax)));
                em.asm.text.push(Instr::Pop(Loc::Reg(Reg::Rcx)));
                // then compare
                em.asm.text.push(Instr::Cmp(BinArgs::ToReg(Reg::Rax, Arg32::Reg(Reg::Rcx))));
                match em.assembly.prog_label(target) {
                    | Some(label) => {
                        // then jump to target if equal
                        em.asm.text.push(Instr::JCC(ConditionCode::E, JmpArgs::Label(label)));
                    }
                    | None => {
                        let label =
                            format!("eq_jump_skip_{}", target.concise_inner().replace('#', "_"));
                        // otherwise, directly emit the target program
                        em.asm
                            .text
                            .push(Instr::JCC(ConditionCode::NE, JmpArgs::Label(label.clone())));
                        target.emit(EnvMap::new(), em);
                        em.asm.text.push(Instr::Label(label));
                    }
                }
            }
            | Program::PopJump(sa::PopJump) => {
                // pop value and jump to it
                em.asm.text.push(Instr::Pop(Loc::Reg(Reg::Rax)));
                em.asm.text.push(Instr::Jmp(JmpArgs::Reg(Reg::Rax)));
            }
            | Program::LeapJump(sa::LeapJump) => {
                // pop value
                em.asm.text.push(Instr::Pop(Loc::Reg(Reg::Rcx)));
                // pop address
                em.asm.text.push(Instr::Pop(Loc::Reg(Reg::Rax)));
                // push the value back
                em.asm.text.push(Instr::Push(Arg32::Reg(Reg::Rcx)));
                // jump to the address
                em.asm.text.push(Instr::Jmp(JmpArgs::Reg(Reg::Rax)));
            }
            | Program::PopBranch(sa::PopBranch(arms)) => {
                // pop tag and jump to the corresponding program
                em.asm.text.push(Instr::Pop(Loc::Reg(Reg::Rax)));
                // register the jump table
                let sorted_arms: BTreeMap<_, _> = arms
                    .iter()
                    .map(|(sa::Tag { idx, name }, prog_id)| (idx, (name, prog_id)))
                    .collect();
                let table = JumpTable {
                    id: *self,
                    arms: sorted_arms
                        .into_iter()
                        .map(|(_, (name, prog_id))| (name.clone(), prog_id.clone()))
                        .collect(),
                };
                let label = table.rodata_label();
                em.tables.push(table);
                // emit jump to the jump table arm
                // jmp     [rel jumptable + rax * 8]
                em.asm.text.push(Instr::Jmp(JmpArgs::RelLabel(RelLabel {
                    label,
                    offset: Some((Reg::Rax, 8)),
                })));
            }
            | Program::Extern(sa::Extern { name, arity }) => {
                em.asm.text.push(Instr::Comment(format!("extern: {:?}, {:?}", name, arity)));
                em.asm.text.extend([Instr::Jmp(JmpArgs::Label(format!("wrapper_{}", name)))]);
            }
            | Program::Panic(_) => {
                em.asm.text.push(Instr::Comment("panic".to_string()));
                // TODO: Implement panic
                todo!()
            }
        }
    }
}

impl<'a> Emit<'a> for Instruction {
    type Env = &'a mut EnvMap;
    fn emit(&self, env: Self::Env, em: &mut Emitter) {
        match self {
            | Instruction::PackProduct(sa::Pack(sa::ProductMarker)) => {
                // Pack two values into a pair
                em.asm.text.extend([
                    Instr::Comment("pack_product".to_string()),
                    Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Signed(2))),
                    Instr::Call("zydeco_alloc".to_string()),
                    // returned pointer is in rax
                    // pop, and store to [rax]
                    Instr::Pop(Loc::Reg(Reg::Rcx)),
                    Instr::Mov(MovArgs::ToMem(
                        MemRef { reg: Reg::Rax, offset: 0 },
                        Reg32::Reg(Reg::Rcx),
                    )),
                    // pop again, and store to [rax + 8]
                    Instr::Pop(Loc::Reg(Reg::Rcx)),
                    Instr::Mov(MovArgs::ToMem(
                        MemRef { reg: Reg::Rax, offset: 8 },
                        Reg32::Reg(Reg::Rcx),
                    )),
                    // push the pointer back to stack
                    Instr::Push(Arg32::Reg(Reg::Rax)),
                ]);
            }
            | Instruction::UnpackProduct(sa::Unpack(sa::ProductMarker)) => {
                // Unpack a pair into two values
                em.asm.text.extend([
                    Instr::Comment("unpack_product".to_string()),
                    Instr::Pop(Loc::Reg(Reg::Rax)),
                    // load [rax + 8] and push
                    Instr::Mov(MovArgs::ToReg(
                        Reg::Rcx,
                        Arg64::Mem(MemRef { reg: Reg::Rax, offset: 8 }),
                    )),
                    Instr::Push(Arg32::Reg(Reg::Rcx)),
                    // load [rax] and push
                    Instr::Mov(MovArgs::ToReg(
                        Reg::Rcx,
                        Arg64::Mem(MemRef { reg: Reg::Rax, offset: 0 }),
                    )),
                    Instr::Push(Arg32::Reg(Reg::Rcx)),
                ]);
            }
            | Instruction::PushContext(sa::Push(sa::ContextMarker)) => {
                // Push context pointer onto stack
                em.asm.text.extend([
                    Instr::Comment("push_context".to_string()),
                    Instr::Push(Arg32::Reg(ENV_REG)),
                ]);
            }
            | Instruction::PopContext(sa::Pop(sa::ContextMarker)) => {
                // Pop context pointer from stack
                em.asm.text.extend([
                    Instr::Comment("pop_context".to_string()),
                    Instr::Pop(Loc::Reg(ENV_REG)),
                ]);
            }
            | Instruction::PushArg(sa::Push(atom)) => {
                // Push argument onto stack
                atom.emit(env, em);
            }
            | Instruction::PopArg(sa::Pop(var_id)) => {
                // Pop argument from stack into variable
                let var_name = &em.assembly.variables[&var_id];
                let idx = env.alloc(*var_id);
                em.asm.text.extend([
                    Instr::Comment(format!("pop_arg {}{}", var_name.plain(), var_id.concise())),
                    // pop from stack
                    Instr::Pop(Loc::Reg(Reg::Rax)),
                    // store to [r10 + 8 * idx]
                    Instr::Mov(MovArgs::ToMem(
                        MemRef { reg: ENV_REG, offset: 8 * idx },
                        Reg32::Reg(Reg::Rax),
                    )),
                ])
            }
            | Instruction::PushTag(sa::Push(tag)) => {
                // Push tag onto stack
                em.asm.text.extend([
                    Instr::Comment(format!("push_tag {}", tag.idx)),
                    // push tag to stack
                    Instr::Push(Arg32::Signed(tag.idx as i32)),
                ]);
            }
            | Instruction::Intrinsic(intrinsic) => {
                intrinsic.emit((), em);
            }
            | Instruction::Swap(sa::Swap) => {
                // Swap the top two values on the stack
                em.asm.text.extend([
                    Instr::Pop(Loc::Reg(Reg::Rax)),
                    Instr::Pop(Loc::Reg(Reg::Rcx)),
                    Instr::Push(Arg32::Reg(Reg::Rax)),
                    Instr::Push(Arg32::Reg(Reg::Rcx)),
                ]);
            }
            | Instruction::Clear(_) => {
                // Clear variables from context
                em.asm.text.push(Instr::Comment("clear".to_string()));
                // TODO: Implement context clearing
                todo!()
            }
        }
    }
}

impl<'a> Emit<'a> for Atom {
    type Env = &'a EnvMap;
    fn emit(&self, env: Self::Env, em: &mut Emitter) {
        match self {
            | Atom::Var(var_id) => {
                let var_name = &em.assembly.variables[var_id];
                em.asm.text.push(Instr::Comment(format!(
                    "push_var {}{}",
                    var_name.plain(),
                    var_id.concise()
                )));
                // log::trace!("instrs:");
                // for instr in em.instrs.iter() {
                //     log::trace!("\t{}", instr);
                // }
                // log::trace!("var: {}{}", var_name.plain(), var_id.concise());
                let idx = env.get(var_id).expect("variable not found");
                // load [r10 + 8 * idx] and push
                em.asm.text.extend([
                    Instr::Mov(MovArgs::ToReg(
                        Reg::Rax,
                        Arg64::Mem(MemRef { reg: ENV_REG, offset: 8 * idx }),
                    )),
                    Instr::Push(Arg32::Reg(Reg::Rax)),
                ]);
            }
            | Atom::Sym(sym_id) => {
                let symbol = &em.assembly.symbols[sym_id];
                match symbol.inner.clone() {
                    | SymbolInner::Prog(prog_id) => {
                        em.asm.text.push(Instr::Comment(format!(
                            "push_sym_prog {}{}",
                            symbol.name.clone(),
                            sym_id.concise()
                        )));
                        // push the program id
                        let label = em.assembly.prog_label(&prog_id).expect("block name not found");
                        em.asm.text.extend([
                            Instr::Lea(
                                Reg::Rax,
                                LeaArgs::RelLabel(RelLabel { label, offset: None }),
                            ),
                            Instr::Push(Arg32::Reg(Reg::Rax)),
                        ]);
                    }
                    | SymbolInner::Undefined(sa::Undefined) => {
                        unreachable!("undefined symbol should never be emitted")
                    }
                    | SymbolInner::String(s) => {
                        em.asm.text.push(Instr::Comment(format!("push_sym_str {:?}", s)));
                        todo!()
                    }
                }
            }
            | Atom::Imm(imm) => match imm.clone() {
                | sa::Imm::Triv(Triv) => {
                    em.asm.text.push(Instr::Comment("push_imm_triv".to_string()));
                    em.asm.text.extend([Instr::Push(Arg32::Signed(0))]);
                }
                | sa::Imm::Int(i) => {
                    em.asm.text.push(Instr::Comment(format!("push_imm_int {:?}", i)));
                    em.asm.text.extend([Instr::Push(Arg32::Signed(i as i32))]);
                }
                | sa::Imm::Char(c) => {
                    em.asm.text.push(Instr::Comment(format!("push_imm_char {:?}", c)));
                    em.asm.text.extend([Instr::Push(Arg32::Signed(c as i32))]);
                }
            },
        }
    }
}

impl<'a> Emit<'a> for Intrinsic {
    type Env = ();
    fn emit(&self, (): Self::Env, em: &mut Emitter) {
        let Intrinsic { name, arity } = self;
        match (*name, arity) {
            | (_, 2) => {
                em.asm
                    .text
                    .extend([Instr::Pop(Loc::Reg(Reg::Rax)), Instr::Pop(Loc::Reg(Reg::Rcx))]);
                fn emit_ba(op: fn(BinArgs) -> Instr, em: &mut Emitter) {
                    em.asm.text.push(op(BinArgs::ToReg(Reg::Rax, Arg32::Reg(Reg::Rcx))));
                }
                fn emit_cc(cc: ConditionCode, em: &mut Emitter) {
                    em.asm.text.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Signed(0))));
                    em.asm.text.push(Instr::SetCC(cc, Reg8::Al));
                }
                match *name {
                    | "add" => {
                        emit_ba(Instr::Add, em);
                    }
                    | "sub" => {
                        emit_ba(Instr::Sub, em);
                    }
                    | "mul" => {
                        emit_ba(Instr::IMul, em);
                    }
                    | "and" => {
                        emit_ba(Instr::And, em);
                    }
                    | "or" => {
                        emit_ba(Instr::Or, em);
                    }
                    | "xor" => {
                        emit_ba(Instr::Xor, em);
                    }
                    | "int_eq" => {
                        emit_ba(Instr::Cmp, em);
                        emit_cc(ConditionCode::E, em);
                    }
                    | "int_lt" => {
                        emit_ba(Instr::Cmp, em);
                        emit_cc(ConditionCode::L, em);
                    }
                    | "int_gt" => {
                        emit_ba(Instr::Cmp, em);
                        emit_cc(ConditionCode::G, em);
                    }
                    | _ => {
                        unimplemented!("intrinsic {} with arity {} not implemented", name, arity)
                    }
                }
            }
            | _ => unimplemented!("intrinsic {} with arity {} not implemented", name, arity),
        }
    }
}
