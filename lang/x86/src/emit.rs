use super::syntax::*;
use derive_more::{Deref, DerefMut};
use std::collections::{BTreeMap, HashMap, HashSet};
use zydeco_assembly::arena::{AssemblyArena, AssemblyArenaRefLike};
use zydeco_assembly::syntax::{self as sa, Atom, Instruction, ProgId, Program, SymbolInner};
use zydeco_stack::arena::StackArena;
use zydeco_statics::tyck::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_syntax::*;

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

    pub instrs: Vec<Instr>,

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
            instrs: Vec::new(),
            tables: Vec::new(),
            visited: HashSet::new(),
        }
    }

    pub fn run(mut self) -> Vec<Instr> {
        self.instrs.extend([
            // section .text
            Instr::Section(".text".to_string()),
            // zydeco_abort
            Instr::Extern("zydeco_abort".to_string()),
            // zydeco_alloc
            Instr::Extern("zydeco_alloc".to_string()),
        ]);

        // Emit the externs
        let mut mentioned_externs = Vec::new();
        for (var_id, ()) in &self.assembly.externs {
            let var_name = &self.assembly.variables[var_id];
            mentioned_externs.push(var_id.to_owned());
            let label = format!("zydeco_{}", var_name);
            self.instrs.push(Instr::Extern(label));
        }
        // for (_, symbol) in &self.assembly.symbols {
        //     match symbol.inner.clone() {
        //         | SymbolInner::Extern(sa::Extern) => {
        //             mentioned_externs.push(symbol.name.clone());
        //             let label = format!("zydeco_{}", symbol.name);
        //             self.instrs.push(Instr::Extern(label));
        //         }
        //         | SymbolInner::Triv(_) | SymbolInner::Prog(_) | SymbolInner::Literal(_) => {}
        //     }
        // }

        // Emit rust_call_zydeco functions
        self.instrs.extend([
            Instr::Global("rust_call_zydeco_0".to_string()),
            Instr::Label("rust_call_zydeco_0".to_string()),
            Instr::Comment("discard the return address that is not needed".to_string()),
            Instr::Pop(Loc::Reg(Reg::Rax)),
            Instr::Comment("push the __env__ (which is in rsi) to the stack".to_string()),
            Instr::Push(Arg32::Reg(Reg::Rsi)),
            Instr::Comment("call the zydeco function __code__ (which is in rdi)".to_string()),
            Instr::Jmp(JmpArgs::Reg(Reg::Rdi)),
        ]);
        self.instrs.extend([
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

        // Emit wrappers for externs
        for var_id in mentioned_externs.iter() {
            let extern_name = &self.assembly.variables[var_id];
            let zydeco_extern_name = format!("zydeco_{}", extern_name);
            let wrapper_inner_name = format!("wrapper_{}", extern_name);
            let num_args: usize = match extern_name.plain() {
                | "exit" => 1,
                | "read_line" => 1,
                | "write_line" => 2,
                | _ => todo!("unhandled extern: {}", extern_name),
            };

            // emit the wrapper inner
            self.instrs.extend([
                Instr::Label(wrapper_inner_name.clone()),
                // remove the empty __env__ passed
                Instr::Pop(Loc::Reg(Reg::Rax)),
            ]);
            for i in 1..=num_args {
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
                    self.instrs.push(Instr::Pop(Loc::Reg(reg)));
                } else {
                    // load to stack
                    todo!()
                }
            }
            self.instrs.push(Instr::Call(zydeco_extern_name));
        }

        let mut globals = EnvMap::new();
        self.instrs.extend([Instr::Global("entry".to_string()), Instr::Label("entry".to_string())]);
        // initialize the environment and the heap
        self.instrs.push(Instr::Comment("initialize environment and heap".to_string()));
        self.instrs.push(Instr::Mov(MovArgs::ToReg(Reg::R10, Arg64::Reg(Reg::Rdi))));
        self.instrs.push(Instr::Mov(MovArgs::ToReg(Reg::R11, Arg64::Reg(Reg::Rsi))));
        // initialize the externs
        for var_id in mentioned_externs.iter() {
            let extern_name = &self.assembly.variables[var_id];
            let wrapper_inner_name = format!("wrapper_{}", extern_name);
            self.instrs.extend([
                Instr::Label(format!("{}", extern_name)),
                // alloc 2
                Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Signed(2))),
                Instr::Call("zydeco_alloc".to_string()),
                // put __env__ (which is triv, which is 0) into the first slot
                Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::Rax, offset: 0 }, Reg32::Imm(0))),
                // put __code__ (which is the wrapper_inner_name) into the second slot
                Instr::Lea(
                    Reg::Rcx,
                    LeaArgs::RelLabel(RelLabel { label: wrapper_inner_name, offset: None }),
                ),
                Instr::Mov(MovArgs::ToMem(
                    MemRef { reg: Reg::Rax, offset: 8 },
                    Reg32::Reg(Reg::Rcx),
                )),
            ]);
            let idx = globals.alloc(*var_id);
            // push the pointer to the environment
            self.instrs.push(Instr::Mov(MovArgs::ToMem(
                MemRef { reg: Reg::R10, offset: 8 * idx },
                Reg32::Reg(Reg::Rax),
            )));
        }

        // Assert that there's only one entry point, and emit it
        assert_eq!(self.assembly.entry.len(), 1, "expected exactly one entry point");
        let (entry, ()) = self.assembly.entry.iter().next().unwrap();
        entry.emit(globals.clone(), &mut self);

        // Emit the named blocks
        // Todo: context of blocks should be passed from ZASM
        for (prog_id, _) in &self.assembly.programs {
            if let Some(label) = self.assembly.prog_label(prog_id) {
                self.instrs.push(Instr::Label(label));
                prog_id.emit(globals.clone(), &mut self);
            }
        }

        self.instrs.push(Instr::Section(".rodata".to_string()));
        // Emit the jump tables
        for table in &self.tables {
            let label = table.rodata_label();
            self.instrs.extend([
                Instr::Comment(format!("jump table for {}", table.id.concise_inner())),
                Instr::Label(label.clone()),
            ]);
            for (idx, (name, prog_id)) in table.arms.iter().enumerate() {
                self.instrs.extend([Instr::Comment(format!(
                    "arm {} for {}",
                    name.clone().unwrap_or_else(|| format!("#{}", idx)),
                    prog_id.concise()
                ))]);
                let arm_label = self.assembly.prog_label(prog_id).expect("block name not found");
                self.instrs.push(Instr::Dq(arm_label));
            }
        }

        self.instrs
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
                        em.instrs.push(Instr::Jmp(JmpArgs::Label(label)));
                    }
                    | None => {
                        // otherwise, directly emit the target program
                        em.instrs.push(Instr::Comment(format!(
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
                em.instrs.push(Instr::Pop(Loc::Reg(Reg::Rax)));
                em.instrs.push(Instr::Pop(Loc::Reg(Reg::Rcx)));
                // then compare
                em.instrs.push(Instr::Cmp(BinArgs::ToReg(Reg::Rax, Arg32::Reg(Reg::Rcx))));
                match em.assembly.prog_label(target) {
                    | Some(label) => {
                        // then jump to target if equal
                        em.instrs.push(Instr::JCC(ConditionCode::E, JmpArgs::Label(label)));
                    }
                    | None => {
                        let label =
                            format!("eq_jump_skip_{}", target.concise_inner().replace('#', "_"));
                        // otherwise, directly emit the target program
                        em.instrs
                            .push(Instr::JCC(ConditionCode::NE, JmpArgs::Label(label.clone())));
                        target.emit(EnvMap::new(), em);
                        em.instrs.push(Instr::Label(label));
                    }
                }
            }
            | Program::PopJump(sa::PopJump) => {
                // pop value and jump to it
                em.instrs.push(Instr::Pop(Loc::Reg(Reg::Rax)));
                em.instrs.push(Instr::Jmp(JmpArgs::Reg(Reg::Rax)));
            }
            | Program::LeapJump(sa::LeapJump) => {
                // pop value
                em.instrs.push(Instr::Pop(Loc::Reg(Reg::Rcx)));
                // pop address
                em.instrs.push(Instr::Pop(Loc::Reg(Reg::Rax)));
                // push the value back
                em.instrs.push(Instr::Push(Arg32::Reg(Reg::Rcx)));
                // jump to the address
                em.instrs.push(Instr::Jmp(JmpArgs::Reg(Reg::Rax)));
            }
            | Program::PopBranch(sa::PopBranch(arms)) => {
                // pop tag and jump to the corresponding program
                em.instrs.push(Instr::Pop(Loc::Reg(Reg::Rax)));
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
                em.instrs.push(Instr::Jmp(JmpArgs::RelLabel(RelLabel {
                    label,
                    offset: Some((Reg::Rax, 8)),
                })));
            }
            | Program::Panic(_) => {
                em.instrs.push(Instr::Comment("panic".to_string()));
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
                em.instrs.extend([
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
                em.instrs.extend([
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
                em.instrs.extend([
                    Instr::Comment("push_context".to_string()),
                    Instr::Push(Arg32::Reg(Reg::R10)),
                ]);
            }
            | Instruction::PopContext(sa::Pop(sa::ContextMarker)) => {
                // Pop context pointer from stack
                em.instrs.extend([
                    Instr::Comment("pop_context".to_string()),
                    Instr::Pop(Loc::Reg(Reg::R10)),
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
                em.instrs.extend([
                    Instr::Comment(format!("pop_arg {}{}", var_name.plain(), var_id.concise())),
                    // pop from stack
                    Instr::Pop(Loc::Reg(Reg::Rax)),
                    // store to [r10 + 8 * idx]
                    Instr::Mov(MovArgs::ToMem(
                        MemRef { reg: Reg::R10, offset: 8 * idx },
                        Reg32::Reg(Reg::Rax),
                    )),
                ])
            }
            | Instruction::PushTag(sa::Push(tag)) => {
                // Push tag onto stack
                em.instrs.extend([
                    Instr::Comment(format!("push_tag {}", tag.idx)),
                    // push tag to stack
                    Instr::Push(Arg32::Signed(tag.idx as i32)),
                ]);
            }
            | Instruction::Swap(sa::Swap) => {
                // Swap the top two values on the stack
                em.instrs.extend([
                    Instr::Pop(Loc::Reg(Reg::Rax)),
                    Instr::Pop(Loc::Reg(Reg::Rcx)),
                    Instr::Push(Arg32::Reg(Reg::Rax)),
                    Instr::Push(Arg32::Reg(Reg::Rcx)),
                ]);
            }
            | Instruction::Clear(_) => {
                // Clear variables from context
                em.instrs.push(Instr::Comment("clear".to_string()));
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
                em.instrs.push(Instr::Comment(format!(
                    "push_var {}{}",
                    var_name.plain(),
                    var_id.concise()
                )));
                // println!("instrs:");
                // for instr in em.instrs.iter() {
                //     println!("\t{}", instr);
                // }
                println!("var: {}{}", var_name.plain(), var_id.concise());
                let idx = env.get(var_id).expect("variable not found");
                // load [r10 + 8 * idx] and push
                em.instrs.extend([
                    Instr::Mov(MovArgs::ToReg(
                        Reg::Rax,
                        Arg64::Mem(MemRef { reg: Reg::R10, offset: 8 * idx }),
                    )),
                    Instr::Push(Arg32::Reg(Reg::Rax)),
                ]);
            }
            | Atom::Sym(sym_id) => {
                let symbol = &em.assembly.symbols[sym_id];
                match symbol.inner.clone() {
                    | SymbolInner::Prog(prog_id) => {
                        em.instrs.push(Instr::Comment(format!(
                            "push_sym_prog {}{}",
                            symbol.name.clone(),
                            sym_id.concise()
                        )));
                        // push the program id
                        let label = em.assembly.prog_label(&prog_id).expect("block name not found");
                        em.instrs.extend([
                            Instr::Lea(
                                Reg::Rax,
                                LeaArgs::RelLabel(RelLabel { label, offset: None }),
                            ),
                            Instr::Push(Arg32::Reg(Reg::Rax)),
                        ]);
                    }
                    | SymbolInner::Extern(_) => {
                        em.instrs.push(Instr::Comment("push_sym_extern".to_string()));
                        let label = symbol.name.clone();
                        em.instrs.extend([
                            Instr::Lea(
                                Reg::Rax,
                                LeaArgs::RelLabel(RelLabel { label, offset: None }),
                            ),
                            Instr::Push(Arg32::Reg(Reg::Rax)),
                        ]);
                    }
                    | SymbolInner::Triv(_) => {
                        em.instrs.push(Instr::Comment("push_sym_triv".to_string()));
                        em.instrs.extend([Instr::Push(Arg32::Signed(0))]);
                    }
                    | SymbolInner::Literal(lit) => {
                        em.instrs.push(Instr::Comment(format!("push_sym_lit {:?}", lit)));
                        match lit {
                            | Literal::Int(i) => {
                                em.instrs.extend([Instr::Push(Arg32::Signed(i as i32))]);
                            }
                            | _ => todo!(),
                        }
                    }
                }
            }
        }
    }
}
