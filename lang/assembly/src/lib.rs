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

#[derive(Clone, Debug, Deref, DerefMut)]
pub struct Env(im::HashMap<DefId, usize>);

impl Env {
    pub fn new() -> Self {
        Self(im::HashMap::new())
    }
    pub fn alloc(&mut self, def: DefId) -> usize {
        let idx = self.len();
        self.insert(def, idx);
        idx
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
            Instr::Pop(Loc::Reg(Reg::R10)),
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
            Instr::Pop(Loc::Reg(Reg::R10)),
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
            Instr::Pop(Loc::Reg(Reg::R10)),
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
            Instr::Pop(Loc::Reg(Reg::R10)),
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
            Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Label(lbl_entry_kont.clone()))),
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
                        let idx = env.alloc(def) as i32 * 8;
                        self.write([Instr::Mov(MovArgs::ToMem(
                            MemRef { reg: Reg::R10, offset: idx },
                            Reg32::Reg(Reg::Rax),
                        ))]);
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
                        let offset = env.alloc(def) as i32 * 8;
                        self.write([
                            Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Label(name))),
                            Instr::Mov(MovArgs::ToMem(
                                MemRef { reg: Reg::R10, offset },
                                Reg32::Reg(Reg::Rax),
                            )),
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
                self.write([Instr::Mov(MovArgs::ToReg(
                    Reg::Rax,
                    Arg64::Mem(MemRef { reg: Reg::R10, offset }),
                ))]);
            }
            | Value::Thunk(Thunk(body)) => {
                let lbl_thunk = format!("thunk{}", value.concise_inner());
                let lbl_kont = format!("kont{}", value.concise_inner());

                self.write([
                    Instr::Jmp(JmpArgs::Label(lbl_kont.clone())),
                    Instr::Label(lbl_thunk.clone()),
                ]);
                self.compu(body, env.clone());
                self.write([
                    Instr::Label(lbl_kont.clone()),
                    Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Label(lbl_thunk.clone()))),
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
                self.write([Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::Rdi, offset: 0 }, Reg32::Reg(Reg::Rax)))]);
                self.value(b, &env);
                self.write([Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::Rdi, offset: 8 }, Reg32::Reg(Reg::Rax)))]);
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
                        let offset = env.alloc(def) as i32 * 8;
                        self.write([
                            // pop argument to env
                            Instr::Pop(Loc::Mem(MemRef { reg: Reg::R10, offset })),
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
                        let offset = env.alloc(def) as i32 * 8;
                        self.write([
                            Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Label(lbl_fix.clone()))),
                            Instr::Mov(MovArgs::ToMem(
                                MemRef { reg: Reg::R10, offset },
                                Reg32::Reg(Reg::Rax),
                            )),
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
                    // pop env
                    // and then pop address and jump
                    // Instr::Pop(Loc::Reg(Reg::R10)),
                    Instr::Pop(Loc::Reg(Reg::R10)),
                    Instr::Pop(Loc::Reg(Reg::Rdi)),
                    Instr::Jmp(JmpArgs::Reg(Reg::Rdi)),
                ]);
            }
            | Compu::Do(Bind { binder, bindee, tail }) => {
                let lbl_kont = format!("kont{}", compu.concise_inner());
                // push kont
                // and then push env
                self.write([
                    Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Label(lbl_kont.clone()))),
                    Instr::Push(Arg32::Reg(Reg::Rdi)),
                    Instr::Push(Arg32::Reg(Reg::R10)),
                    // Instr::Push(Arg32::Reg(Reg::R10)),
                ]);
                self.compu(bindee, env.clone());
                // assume binder is a variable
                use ValuePattern as VPat;
                match self.statics.vpat(&binder) {
                    | VPat::Var(def) => {
                        // label kont
                        self.write([Instr::Label(lbl_kont.clone())]);
                        let offset = env.alloc(def) as i32 * 8;
                        // move result from rax to env
                        self.write([Instr::Mov(MovArgs::ToMem(
                            MemRef { reg: Reg::R10, offset },
                            Reg32::Reg(Reg::Rax),
                        ))]);
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
                        let idx = env.alloc(def) as i32 * 8;
                        self.write([Instr::Mov(MovArgs::ToMem(
                            MemRef { reg: Reg::R10, offset: idx },
                            Reg32::Reg(Reg::Rax),
                        ))]);
                        self.compu(tail, env);
                    }
                    | VPat::VCons(Cons(a, b)) => {
                        let VPat::Var(def_a) = self.statics.vpat(&a) else {
                            unreachable!()
                        };
                        let VPat::Var(def_b) = self.statics.vpat(&b) else {
                            unreachable!()
                        };
                        let idx_a = env.alloc(def_a) as i32 * 8;
                        let idx_b = env.alloc(def_b) as i32 * 8;
                        // use Rdi as temp
                        self.write([
                            // Rax is tuple
                            // move tuple.0 to idx_a through Rdi
                            Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Mem(MemRef { reg: Reg::Rax, offset: 0 }))),
                            Instr::Mov(MovArgs::ToMem(
                                MemRef { reg: Reg::R10, offset: idx_a },
                                Reg32::Reg(Reg::Rdi),
                            )),
                            // move tuple.1 to idx_b through Rdi
                            Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Mem(MemRef { reg: Reg::Rax, offset: 8 }))),
                            Instr::Mov(MovArgs::ToMem(
                                MemRef { reg: Reg::R10, offset: idx_b },
                                Reg32::Reg(Reg::Rdi),
                            )),
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
                for (idx, Matcher { binder, tail }) in arms.into_iter().enumerate() {
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

                    let lbl_kont = format!("matcher{}_{}", compu.concise_inner(), idx);

                    self.write([
                        Instr::Cmp(BinArgs::ToReg(Reg::Rax, Arg32::Signed(i as i32))),
                        Instr::JCC(ConditionCode::NE, JmpArgs::Label(lbl_kont.clone())),
                    ]);

                    self.compu(tail, env.clone());
                    self.write([Instr::Label(lbl_kont)]);
                }
            }
            | Compu::CoMatch(_) => todo!(),
            | Compu::Dtor(_) => todo!(),
        }
    }
}
