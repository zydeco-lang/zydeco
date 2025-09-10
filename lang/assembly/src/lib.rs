//! The pointer to control stack is stored in Rsp.
//! The pointer to environment stack is stored in R10.
//! The pointer to the heap is stored in R11.

use derive_more::{Deref, DerefMut};
use zydeco_statics::{arena::*, syntax::*};
use zydeco_x86::syntax::*;

pub struct Emitter<'e> {
    pub scoped: ScopedArena,
    pub statics: StaticsArena,
    pub instrs: &'e mut Vec<Instr>,
}

/// A backward allocated environment stack
#[derive(Clone, Debug, Deref, DerefMut)]
pub struct Env {
    #[deref]
    #[deref_mut]
    map: im::HashMap<DefId, i32>,
}

impl Env {
    pub fn new() -> Self {
        Self { map: im::HashMap::new() }
    }
    /// Always call alloc before assigning.
    pub fn alloc(&mut self, def: DefId) {
        self.map.insert(def, 0);
        self.map.iter_mut().for_each(|(_def, idx)| {
            *idx -= 1;
        });
    }
    pub fn layout_msg(&self, scoped: &ScopedArena) -> String {
        self.iter()
            .map(|(def, offset)| (offset, def))
            .collect::<std::collections::BTreeMap<_, _>>()
            .into_iter()
            .map(|(offset, def)| format!("{}: {}", scoped.def(&def).plain(), offset))
            .collect::<Vec<_>>()
            .join(", ")
    }
}

pub enum CompileKont {
    Next(Env),
    Done,
}

impl<'e> Emitter<'e> {
    pub fn new(scoped: ScopedArena, statics: StaticsArena, instrs: &'e mut Vec<Instr>) -> Self {
        Self { scoped, statics, instrs }
    }
    pub fn write(&mut self, instrs: impl IntoIterator<Item = Instr>) {
        self.instrs.extend(instrs);
    }

    pub fn run(&mut self) {
        self.write([
            // section .text
            Instr::Section(".text".to_string()),
            // global entry
            Instr::Global("entry".to_string()),
            // zydeco_abort
            Instr::Extern("zydeco_abort".to_string()),
            // zydeco_alloc
            Instr::Extern("zydeco_alloc".to_string()),
        ]);

        // zydeco extern implementations
        self.write([
            // add
            Instr::Label("add".to_string()),
            // pop twice and add them to rax
            Instr::Pop(Loc::Reg(Reg::Rax)),
            Instr::Pop(Loc::Reg(Reg::Rdi)),
            Instr::Add(BinArgs::ToReg(Reg::Rax, Arg32::Reg(Reg::Rdi))),
            // pop address and jump
            Instr::Pop(Loc::Reg(Reg::Rdi)),
            Instr::Jmp(JmpArgs::Reg(Reg::Rdi)),
        ]);
        self.write([
            // sub
            Instr::Label("sub".to_string()),
            // pop twice and add them to rax
            Instr::Pop(Loc::Reg(Reg::Rax)),
            Instr::Pop(Loc::Reg(Reg::Rdi)),
            Instr::Sub(BinArgs::ToReg(Reg::Rax, Arg32::Reg(Reg::Rdi))),
            // pop address and jump
            Instr::Pop(Loc::Reg(Reg::Rdi)),
            Instr::Jmp(JmpArgs::Reg(Reg::Rdi)),
        ]);
        self.write([
            // mul
            Instr::Label("mul".to_string()),
            // pop twice and add them to rax
            Instr::Pop(Loc::Reg(Reg::Rax)),
            Instr::Pop(Loc::Reg(Reg::Rdi)),
            Instr::IMul(BinArgs::ToReg(Reg::Rax, Arg32::Reg(Reg::Rdi))),
            // pop address and jump
            Instr::Pop(Loc::Reg(Reg::Rdi)),
            Instr::Jmp(JmpArgs::Reg(Reg::Rdi)),
        ]);
        self.write([
            // int_eq
            Instr::Label("int_eq".to_string()),
            // pop twice and add them to rax
            Instr::Pop(Loc::Reg(Reg::Rax)),
            Instr::Pop(Loc::Reg(Reg::Rdi)),
            Instr::Cmp(BinArgs::ToReg(Reg::Rax, Arg32::Reg(Reg::Rdi))),
            Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Signed(0))),
            Instr::SetCC(ConditionCode::E, Reg8::Al),
            // pop address and jump
            Instr::Pop(Loc::Reg(Reg::Rdi)),
            Instr::Jmp(JmpArgs::Reg(Reg::Rdi)),
        ]);

        let lbl_entry = "entry".to_string();
        let lbl_entry_kont = "entry_kont".to_string();

        // label entry
        self.write([
            // entry:
            Instr::Label(lbl_entry),
            // move Rdi to R10 (env)
            Instr::Mov(MovArgs::ToReg(Reg::R10, Arg64::Reg(Reg::Rdi))),
            // move Rsi to R11 (heap)
            Instr::Mov(MovArgs::ToReg(Reg::R11, Arg64::Reg(Reg::Rsi))),
            // push entry_kont
            Instr::Lea(Reg::Rdi, LeaArgs::RelLabel(lbl_entry_kont.clone())),
            Instr::Push(Arg32::Reg(Reg::Rdi)),
        ]);

        // compile declarations
        let mut scc = self.scoped.top.clone();
        let mut env = Env::new();
        'out: loop {
            let frontier = scc.top();
            if frontier.is_empty() {
                break 'out;
            }
            for group in frontier {
                for decl in group.iter() {
                    let Some(_) = self.statics.decls.get(decl) else {
                        continue;
                    };
                    match self.decl(&decl, env) {
                        | CompileKont::Next(new_env) => env = new_env,
                        | CompileKont::Done => break 'out,
                    }
                }
                scc.release(group);
            }
        }

        // label entry_kont
        self.write([
            // entry_kont:
            Instr::Label(lbl_entry_kont.clone()),
            Instr::Ret,
        ]);
    }

    pub fn decl(&mut self, decl: &DeclId, mut env: Env) -> CompileKont {
        use Declaration as Decl;
        match self.statics.decl(decl) {
            | Decl::TAliasBody(_) => CompileKont::Next(env),
            | Decl::VAliasBody(VAliasBody { binder, bindee }) => {
                // assume binder is a variable
                use ValuePattern as VPat;
                match self.statics.vpat(&binder) {
                    | VPat::Var(def) => {
                        // compile a value to Rax
                        self.value(bindee, &env);
                        env.alloc(def);
                        self.write([
                            Instr::Comment(format!(
                                "[write] def value {}",
                                self.scoped.def(&def).plain()
                            )),
                            Instr::Comment(format!("[layout] {}", env.layout_msg(&self.scoped),)),
                            Instr::Mov(MovArgs::ToMem(
                                MemRef { reg: Reg::R10, offset: 0 },
                                Reg32::Reg(Reg::Rax),
                            )),
                            Instr::Add(BinArgs::ToReg(Reg::R10, Arg32::Signed(8))),
                        ]);
                        CompileKont::Next(env)
                    }
                    | _ => unreachable!(),
                }
            }
            | Decl::VAliasHead(VAliasHead { binder, ty: _ }) => {
                // assume it's implemented
                use ValuePattern as VPat;
                match self.statics.vpat(&binder) {
                    | VPat::Var(def) => {
                        let name = self.scoped.def(&def).0;
                        env.alloc(def);
                        self.write([
                            Instr::Lea(Reg::Rax, LeaArgs::RelLabel(name)),
                            Instr::Mov(MovArgs::ToMem(
                                MemRef { reg: Reg::R10, offset: 0 },
                                Reg32::Reg(Reg::Rax),
                            )),
                            Instr::Add(BinArgs::ToReg(Reg::R10, Arg32::Signed(8))),
                        ]);
                        CompileKont::Next(env)
                    }
                    | _ => unreachable!(),
                }
            }
            | Decl::Exec(Exec(compu)) => {
                self.compu(compu, env);
                CompileKont::Done
            }
        }
    }

    pub fn value(&mut self, value: ValueId, env: &Env) {
        // assume only variables and integers will present
        match self.statics.value(&value) {
            | Value::Hole(Hole) => {
                self.write([Instr::Jmp(JmpArgs::Label("zydeco_abort".to_string()))]);
            }
            | Value::Var(def) => {
                let offset = env[&def] as i32 * 8;
                self.write([
                    Instr::Comment(format!("[read] variable {}", self.scoped.def(&def).plain())),
                    Instr::Comment(format!("[layout] {}", env.layout_msg(&self.scoped),)),
                    Instr::Mov(MovArgs::ToReg(
                        Reg::Rax,
                        Arg64::Mem(MemRef { reg: Reg::R10, offset }),
                    )),
                ]);
            }
            | Value::Thunk(Thunk(body)) => {
                let lbl_thunk = format!("thunk{}", value.concise_inner());
                let lbl_kont = format!("thunk_kont{}", value.concise_inner());

                self.write([
                    Instr::Jmp(JmpArgs::Label(lbl_kont.clone())),
                    Instr::Label(lbl_thunk.clone()),
                ]);
                self.compu(body, env.clone());
                self.write([
                    Instr::Label(lbl_kont.clone()),
                    Instr::Lea(Reg::Rax, LeaArgs::RelLabel(lbl_thunk.clone())),
                ]);
            }
            | Value::Ctor(Ctor(tag, _body)) => {
                // assume constructors are only C style enums
                let ty = self.statics.annotations_value[&value];
                let Fillable::Done(Type::Data(data)) = self.statics.types[&ty] else {
                    unreachable!()
                };
                let Some(i) = self.statics.datas[&data]
                    .iter()
                    .position(|(tag_branch, _ty)| &tag == tag_branch)
                else {
                    unreachable!()
                };
                self.write([Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Signed(i as i64)))])
            }
            | Value::Triv(Triv) => {}
            | Value::VCons(Cons(a, b)) => {
                self.write([
                    // allocate two slots on the heap
                    Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Unsigned(2))),
                    Instr::Call("zydeco_alloc".to_string()),
                    Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Reg(Reg::Rax))),
                ]);
                self.value(a, &env);
                self.write([Instr::Mov(MovArgs::ToMem(
                    MemRef { reg: Reg::Rdi, offset: 0 },
                    Reg32::Reg(Reg::Rax),
                ))]);
                self.value(b, &env);
                self.write([Instr::Mov(MovArgs::ToMem(
                    MemRef { reg: Reg::Rdi, offset: 8 },
                    Reg32::Reg(Reg::Rax),
                ))]);
                // move the pointer to Rax
                self.write([Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::Rdi)))]);
            }
            | Value::TCons(_cons) => todo!(),
            | Value::Lit(lit) => match lit {
                | Literal::Int(i) => {
                    self.write([Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Signed(i)))]);
                }
                | Literal::String(_) => todo!(),
                | Literal::Char(_) => todo!(),
            },
        }
    }

    pub fn compu(&mut self, compu: CompuId, mut env: Env) {
        use Computation as Compu;
        match self.statics.compu(&compu) {
            | Compu::Hole(Hole) => {
                self.write([Instr::Jmp(JmpArgs::Label("zydeco_abort".to_string()))]);
            }
            | Compu::VAbs(Abs(param, body)) => {
                // assume param is a variable
                use ValuePattern as VPat;
                match self.statics.vpat(&param) {
                    | VPat::Var(def) => {
                        env.alloc(def);
                        self.write([
                            Instr::Comment(format!(
                                "[write] function argument {}",
                                self.scoped.def(&def).plain()
                            )),
                            Instr::Comment(format!("[layout] {}", env.layout_msg(&self.scoped),)),
                            // pop argument to env
                            Instr::Pop(Loc::Mem(MemRef { reg: Reg::R10, offset: 0 })),
                            Instr::Add(BinArgs::ToReg(Reg::R10, Arg32::Signed(8))),
                        ]);
                        self.compu(body, env);
                    }
                    | _ => unreachable!(),
                }
            }
            | Compu::VApp(App(body, arg)) => {
                self.value(arg, &env);
                self.write([Instr::Push(Arg32::Reg(Reg::Rax))]);
                self.compu(body, env);
            }
            | Compu::TAbs(Abs(_, body)) => {
                self.compu(body, env);
            }
            | Compu::TApp(App(body, _)) => {
                self.compu(body, env);
            }
            | Compu::Fix(Fix(param, body)) => {
                // assume binder is a variable
                use ValuePattern as VPat;
                match self.statics.vpat(&param) {
                    | VPat::Var(def) => {
                        let lbl_fix = format!("fix{}", compu.concise_inner());
                        env.alloc(def);
                        self.write([
                            Instr::Comment(format!(
                                "[write] fix address {}",
                                self.scoped.def(&def).plain()
                            )),
                            Instr::Comment(format!("[layout] {}", env.layout_msg(&self.scoped),)),
                            Instr::Lea(Reg::Rax, LeaArgs::RelLabel(lbl_fix.clone())),
                            Instr::Mov(MovArgs::ToMem(
                                MemRef { reg: Reg::R10, offset: 0 },
                                Reg32::Reg(Reg::Rax),
                            )),
                            Instr::Add(BinArgs::ToReg(Reg::R10, Arg32::Signed(8))),
                            Instr::Label(lbl_fix.clone()),
                        ]);
                        self.compu(body, env);
                    }
                    | _ => unreachable!(),
                }
            }
            | Compu::Force(Force(body)) => {
                self.value(body, &env);
                // only call to labels are allowed for now
                self.write([Instr::Jmp(JmpArgs::Reg(Reg::Rax))])
            }
            | Compu::Ret(Return(value)) => {
                self.value(value, &env);
                self.write([
                    Instr::Comment(format!("[form] ret")),
                    // and then pop address and jump
                    Instr::Pop(Loc::Reg(Reg::Rdi)),
                    Instr::Jmp(JmpArgs::Reg(Reg::Rdi)),
                ]);
            }
            | Compu::Do(Bind { binder, bindee, tail }) => {
                let lbl_kont = format!("kont{}", compu.concise_inner());
                // push kont
                // and then push env
                self.write([
                    Instr::Comment(format!("[form] do")),
                    Instr::Lea(Reg::Rdi, LeaArgs::RelLabel(lbl_kont.clone())),
                    Instr::Push(Arg32::Reg(Reg::R10)),
                    Instr::Push(Arg32::Reg(Reg::Rdi)),
                ]);
                self.compu(bindee, env.clone());
                // assume binder is a variable
                use ValuePattern as VPat;
                match self.statics.vpat(&binder) {
                    | VPat::Var(def) => {
                        // label kont
                        self.write([Instr::Label(lbl_kont.clone())]);
                        env.alloc(def);
                        // move result from rax to env
                        self.write([
                            Instr::Pop(Loc::Reg(Reg::R10)),
                            Instr::Comment(format!(
                                "[write] return value {}",
                                self.scoped.def(&def).plain()
                            )),
                            Instr::Comment(format!("[layout] {}", env.layout_msg(&self.scoped),)),
                            Instr::Mov(MovArgs::ToMem(
                                MemRef { reg: Reg::R10, offset: 0 },
                                Reg32::Reg(Reg::Rax),
                            )),
                            Instr::Add(BinArgs::ToReg(Reg::R10, Arg32::Signed(8))),
                        ]);
                        self.compu(tail, env);
                    }
                    | _ => unreachable!(),
                }
            }
            | Compu::Let(Let { binder, bindee, tail }) => {
                self.value(bindee, &env);
                // assume binder is either:
                // - a variable
                // - a cons of variables
                use ValuePattern as VPat;
                match self.statics.vpat(&binder) {
                    | VPat::Var(def) => {
                        env.alloc(def);
                        self.write([
                            Instr::Comment(format!(
                                "[write] let value {}",
                                self.scoped.def(&def).plain()
                            )),
                            Instr::Comment(format!("[layout] {}", env.layout_msg(&self.scoped),)),
                            Instr::Mov(MovArgs::ToMem(
                                MemRef { reg: Reg::R10, offset: 0 },
                                Reg32::Reg(Reg::Rax),
                            )),
                            Instr::Add(BinArgs::ToReg(Reg::R10, Arg32::Signed(8))),
                        ]);
                        self.compu(tail, env);
                    }
                    | VPat::VCons(Cons(a, b)) => {
                        let VPat::Var(def_a) = self.statics.vpat(&a) else { unreachable!() };
                        let VPat::Var(def_b) = self.statics.vpat(&b) else { unreachable!() };
                        env.alloc(def_a);
                        env.alloc(def_b);
                        // use Rdi as temp
                        self.write([
                            Instr::Comment(format!(
                                "[write] let tuple ({}, {})",
                                self.scoped.def(&def_a).plain(),
                                self.scoped.def(&def_b).plain()
                            )),
                            Instr::Comment(format!("[layout] {}", env.layout_msg(&self.scoped),)),
                            // Rax is tuple
                            // move tuple.0 to idx_a through Rdi
                            Instr::Mov(MovArgs::ToReg(
                                Reg::Rdi,
                                Arg64::Mem(MemRef { reg: Reg::Rax, offset: 0 }),
                            )),
                            Instr::Mov(MovArgs::ToMem(
                                MemRef { reg: Reg::R10, offset: 0 },
                                Reg32::Reg(Reg::Rdi),
                            )),
                            // move tuple.1 to idx_b through Rdi
                            Instr::Mov(MovArgs::ToReg(
                                Reg::Rdi,
                                Arg64::Mem(MemRef { reg: Reg::Rax, offset: 8 }),
                            )),
                            Instr::Mov(MovArgs::ToMem(
                                MemRef { reg: Reg::R10, offset: 8 },
                                Reg32::Reg(Reg::Rdi),
                            )),
                            Instr::Add(BinArgs::ToReg(Reg::R10, Arg32::Signed(16))),
                        ]);
                        self.compu(tail, env);
                    }
                    | _ => unreachable!(),
                }
            }
            | Compu::Match(Match { scrut, arms }) => {
                self.value(scrut, &env);
                let ty = self.statics.annotations_value[&scrut];
                let Fillable::Done(Type::Data(data)) = self.statics.types[&ty] else {
                    unreachable!()
                };
                for Matcher { binder, tail } in arms {
                    // assume only C style enums
                    use ValuePattern as VPat;
                    let VPat::Ctor(Ctor(tag, _)) = self.statics.vpat(&binder) else {
                        unreachable!()
                    };
                    let Some(i) = self.statics.datas[&data]
                        .iter()
                        .position(|(tag_branch, _ty)| &tag == tag_branch)
                    else {
                        unreachable!()
                    };

                    let lbl_kont = format!("matcher{}_not_{}", compu.concise_inner(), tag.plain());

                    self.write([
                        Instr::Cmp(BinArgs::ToReg(Reg::Rax, Arg32::Signed(i as i32))),
                        Instr::JCC(ConditionCode::NE, JmpArgs::Label(lbl_kont.clone())),
                    ]);

                    self.compu(tail, env.clone());
                    self.write([Instr::Label(lbl_kont)]);
                }
                // abort if no match
                self.write([Instr::Jmp(JmpArgs::Label("zydeco_abort".to_string()))]);
            }
            | Compu::CoMatch(CoMatch { arms }) => {
                // skip definition of comatchers
                let lbl_kont = format!("comatch{}", compu.concise_inner());
                self.write([Instr::Jmp(JmpArgs::Label(lbl_kont.clone()))]);
                // define comatchers
                let mut lbl_dtors = std::collections::HashMap::new();
                for CoMatcher { dtor, tail } in arms {
                    let lbl_dtor = format!("comatcher{}_{}", compu.concise_inner(), dtor.plain());
                    lbl_dtors.insert(dtor.clone(), lbl_dtor.clone());
                    self.write([Instr::Label(lbl_dtor)]);
                    self.compu(tail, env.clone());
                }
                // construct the computation tuple
                let codata = {
                    let ty = self.statics.annotations_compu[&compu];
                    match self.statics.types[&ty] {
                        | Fillable::Done(Type::CoData(codata)) => {
                            self.statics.codatas[&codata].clone()
                        }
                        | _ => unreachable!(),
                    }
                };
                self.write([
                    Instr::Label(lbl_kont),
                    // pop idx from stack
                    Instr::Pop(Loc::Reg(Reg::Rax)),
                ]);

                // jump to pointer according to idx
                for (idx, (dtor, _)) in codata.iter().enumerate() {
                    let lbl_dtor = &lbl_dtors[dtor];
                    self.write([
                        Instr::Cmp(BinArgs::ToReg(Reg::Rax, Arg32::Signed(idx as i32))),
                        Instr::JCC(ConditionCode::E, JmpArgs::Label(lbl_dtor.clone())),
                    ]);
                }

                // abort if no match
                self.write([Instr::Jmp(JmpArgs::Label("zydeco_abort".to_string()))]);
            }
            | Compu::Dtor(Dtor(head, dtor)) => {
                let codata = {
                    let ty = self.statics.annotations_compu[&head];
                    match self.statics.types[&ty] {
                        | Fillable::Done(Type::CoData(codata)) => {
                            self.statics.codatas[&codata].clone()
                        }
                        | _ => unreachable!(),
                    }
                };
                let Some(i) = codata.iter().position(|(d, _ty)| &dtor == d) else { unreachable!() };
                // push idx to stack
                self.write([Instr::Push(Arg32::Signed(i as i32))]);
                self.compu(head, env);
            }
        }
    }
}
