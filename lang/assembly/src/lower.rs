use super::syntax::{Instruction as Instr, *};
use zydeco_statics::{WellFormedProgram, wf::syntax as sw};
pub type ContextMap = std::collections::HashMap<sw::DefId, VarId>;

pub trait Lower {
    type Kont;
    type Out;
    fn lower(&self, ctx: Context, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out;
}

pub struct Lowerer<'a> {
    pub object: AssemblyArena,
    pub wf: &'a WellFormedProgram,
    pub ctx_map: ContextMap,
}

impl<'a> Lowerer<'a> {
    pub fn new(alloc: ArcGlobalAlloc, wf: &'a WellFormedProgram) -> Self {
        let object = AssemblyArena::new(alloc);
        Self { object, wf, ctx_map: ContextMap::new() }
    }
    pub fn run(mut self) -> AssemblyArena {
        for (compu, ()) in &self.wf.entry {
            let prog = compu.lower(Context::from_iter([]), &mut self, ());
            self.object.entry.insert(prog, ());
        }
        self.object
    }
}
impl AsMut<AssemblyArena> for Lowerer<'_> {
    fn as_mut(&mut self) -> &mut AssemblyArena {
        &mut self.object
    }
}

impl Lower for sw::CompuId {
    type Kont = ();
    type Out = ProgId;

    fn lower(&self, ctx: Context, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let compu = lo.wf.compus[self].clone();
        use sw::Computation as Compu;
        match compu {
            | Compu::Hole(Hole) => lo.object.prog_anon(Program::Panic(Panic), ctx),
            | Compu::VAbs(Abs(param, body)) => {
                param.lower(ctx, lo, Box::new(move |ctx, lo| body.lower(ctx, lo, kont)))
            }
            | Compu::VApp(App(body, arg)) => arg.lower(
                ctx,
                lo,
                Box::new(move |arg, ctx, lo| {
                    let prog = body.lower(ctx.clone(), lo, kont);
                    lo.object.prog_anon(Program::Instruction(Instr::PushArg(Push(arg)), prog), ctx)
                }),
            ),
            | Compu::TAbs(Abs(_param, body)) => body.lower(ctx, lo, kont),
            | Compu::TApp(App(body, _arg)) => body.lower(ctx, lo, kont),
            | Compu::Fix(Fix(param, body)) => {
                // assume param is always a variable
                let sw::ValuePattern::Var(param) = lo.wf.vpats[&param].clone() else {
                    unreachable!()
                };
                let name = lo.wf.defs[&param].plain().to_string();
                let fix = lo.object.variables.alloc(VarName(name.clone()));
                let mut body = body.lower([fix].into_iter().chain(ctx.clone()).collect(), lo, kont);
                let mut params = ctx.clone();
                while let Some(var) = params.0.pop() {
                    body = lo.prog_anon(
                        Program::Instruction(Instr::PopArg(Pop(var)), body),
                        params.clone(),
                    );
                }
                assert!(params.0.is_empty());
                let body = lo.prog_anon(
                    Program::Instruction(Instr::PopArg(Pop(fix)), body),
                    [].into_iter().collect(),
                );
                // the body here is the fixed point program
                lo.object.labels.insert(body, Label(name));
                let jmp =
                    lo.instr(Instr::PushArg(Push(Atom::Label(body))), ctx.clone(), |ctx, lo| {
                        lo.prog_anon(Program::Jump(Jump(body)), ctx)
                    });
                let args = ctx.clone();
                let prog = args.into_iter().fold(jmp, |prog, arg| {
                    lo.object.prog_anon(
                        Program::Instruction(Instr::PushArg(Push(Atom::Var(arg))), prog),
                        ctx.clone(),
                    )
                });
                prog
            }
            | Compu::Force(Force(body)) => body.lower(
                ctx,
                lo,
                Box::new(move |body, ctx, lo| {
                    lo.instr(Instr::PushArg(Push(body)), ctx, |ctx, lo| {
                        lo.object.prog_anon(Program::Call(Call), ctx)
                    })
                }),
            ),
            | Compu::Ret(Return(body)) => {
                body.lower(ctx, lo, Box::new(move |body, ctx, lo| lo.prog_anon(Return(body), ctx)))
            }
            | Compu::Do(Bind { binder, bindee, tail }) => {
                let tail = binder.lower(
                    ctx.clone(),
                    lo,
                    Box::new(move |ctx, lo| tail.lower(ctx, lo, kont)),
                );
                let bindee = bindee.lower(ctx.clone(), lo, kont);
                lo.prog_anon(Program::Bind(Bind { binder: (), bindee, tail }), ctx)
            }
            | Compu::Let(Let { binder, bindee, tail }) => bindee.lower(
                ctx,
                lo,
                Box::new(move |bindee, ctx, lo| {
                    lo.instr(Instr::PushArg(Push(bindee)), ctx, |ctx, lo| {
                        binder.lower(ctx, lo, Box::new(move |ctx, lo| tail.lower(ctx, lo, kont)))
                    })
                }),
            ),
            | Compu::Match(Match { scrut, arms }) => {
                let ty = lo.wf.annotations_value[&scrut];
                let sw::Type::Data(data) = lo.wf.types[&ty] else { unreachable!() };
                let indices = rustc_hash::FxHashMap::from_iter(
                    (lo.wf.datas[&data].iter())
                        .enumerate()
                        .map(|(idx, (ctor, _ty))| (ctor.clone(), idx)),
                );
                let branches = Vec::from_iter(arms.into_iter().map(|Matcher { binder, tail }| {
                    use sw::ValuePattern as VPat;
                    let VPat::Ctor(Ctor(ctor, binder)) = lo.wf.vpats[&binder].clone() else {
                        unreachable!()
                    };
                    let idx = indices[&ctor];
                    let name = Some(ctor.plain().to_string());
                    let tag = Tag { idx, name };
                    let branch = binder.lower(
                        ctx.clone(),
                        lo,
                        Box::new(move |ctx, lo| tail.lower(ctx, lo, kont)),
                    );
                    (tag, branch)
                }));
                scrut.lower(
                    ctx,
                    lo,
                    Box::new(move |scrut, ctx, lo| {
                        lo.instr(Instr::PushArg(Push(scrut)), ctx, |ctx, lo| {
                            // tag (/) value (/) ...
                            lo.instr(Instr::UnpackProduct(Unpack(Product)), ctx, |ctx, lo| {
                                lo.prog_anon(Program::Branch(Branch(branches)), ctx)
                            })
                        })
                    }),
                )
            }
            | Compu::CoMatch(CoMatch { arms }) => {
                let ty = lo.wf.annotations_compu[self];
                let sw::Type::CoData(codata) = lo.wf.types[&ty] else { unreachable!() };
                let indices = rustc_hash::FxHashMap::from_iter(
                    (lo.wf.codatas[&codata].iter())
                        .enumerate()
                        .map(|(idx, (dtor, _ty))| (dtor.clone(), idx)),
                );
                let branches = Vec::from_iter(arms.into_iter().map(|CoMatcher { dtor, tail }| {
                    let idx = indices[&dtor];
                    let name = Some(dtor.plain().to_string());
                    let tag = Tag { idx, name };
                    (tag, tail.lower(ctx.clone(), lo, kont))
                }));
                lo.prog_anon(Program::Branch(Branch(branches)), ctx)
            }
            | Compu::Dtor(Dtor(body, dtor)) => {
                let ty = lo.wf.annotations_compu[self];
                let sw::Type::CoData(data) = lo.wf.types[&ty] else { unreachable!() };
                let idx = (lo.wf.codatas[&data].iter())
                    .position(|(tag_branch, _ty)| &dtor == tag_branch)
                    .unwrap();
                let tag = Tag { idx, name: Some(dtor.plain().to_string()) };
                lo.instr(Instr::PushTag(Push(tag)), ctx, |ctx, lo| body.lower(ctx, lo, kont))
            }
        }
    }
}

impl Lower for sw::VPatId {
    type Kont = Box<dyn FnOnce(Context, &mut Lowerer) -> ProgId>;
    type Out = ProgId;

    fn lower(&self, ctx: Context, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let vpat = lo.wf.vpats[self].clone();
        use sw::ValuePattern as VPat;
        match vpat {
            | VPat::Hole(Hole) => lo.object.prog_anon(Program::Panic(Panic), ctx),
            | VPat::Var(def) => {
                let name = lo.wf.defs[&def].plain().to_string();
                let var = lo.object.variables.alloc(VarName(name));
                lo.instr(Instr::PopArg(Pop(var)), ctx, |ctx, lo| {
                    // add the variable to the context
                    lo.ctx_map.insert(def, var);
                    let ctx = ctx.into_iter().chain([var]).collect();
                    kont(ctx, lo)
                })
            }
            | VPat::Ctor(Ctor(_ctor, _body)) => unreachable!(),
            | VPat::Triv(Triv) => {
                let var = lo.object.variables.alloc(VarName("triv".to_string()));
                lo.instr(Instr::PopArg(Pop(var)), ctx, |ctx, lo| kont(ctx, lo))
            }
            | VPat::VCons(Cons(a, b)) => {
                lo.instr(Instr::UnpackProduct(Unpack(Product)), ctx, |ctx, lo| {
                    a.lower(
                        ctx,
                        lo,
                        Box::new(move |ctx, lo| {
                            b.lower(ctx, lo, Box::new(move |ctx, lo| kont(ctx, lo)))
                        }),
                    )
                })
            }
            | VPat::TCons(Cons(_ty, inner)) => inner.lower(ctx, lo, kont),
        }
    }
}

impl Lower for sw::ValueId {
    type Kont = Box<dyn FnOnce(Atom, Context, &mut Lowerer) -> ProgId>;
    type Out = ProgId;

    fn lower(&self, ctx: Context, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let value = lo.wf.values[self].clone();
        use sw::Value;
        match value {
            | Value::Hole(Hole) => lo.object.prog_anon(Program::Panic(Panic), ctx),
            | Value::Var(def) => {
                let atom = if let Some(()) = lo.wf.externals.get(&def) {
                    let ext = lo.wf.defs[&def].plain().to_string();
                    Atom::External(ext)
                } else {
                    Atom::Var(lo.ctx_map[&def])
                };
                kont(atom, ctx, lo)
            }
            | Value::Thunk(Thunk(body)) => {
                let body = body.lower(ctx.clone(), lo, ());
                lo.instr(Instr::PushArg(Push(Atom::Label(body))), ctx, |ctx, lo| {
                    lo.instr(Instr::PackClosure(Pack(Closure(ctx.clone()))), ctx, |ctx, lo| {
                        let var = lo.object.variables.alloc(VarName("thunk".to_string()));
                        lo.instr(Instr::PopArg(Pop(var)), ctx, |ctx, lo| {
                            kont(Atom::Var(var), ctx, lo)
                        })
                    })
                })
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                let ty = lo.wf.annotations_value[self];
                let sw::Type::Data(data) = lo.wf.types[&ty] else { unreachable!() };
                let idx = (lo.wf.datas[&data].iter())
                    .position(|(tag_branch, _ty)| &ctor == tag_branch)
                    .unwrap();
                body.lower(
                    ctx,
                    lo,
                    Box::new(move |atom, ctx, lo| {
                        // tag (/) value (/) ...
                        lo.instr(Instr::PushArg(Push(atom)), ctx, |ctx, lo| {
                            let tag = Tag { idx, name: Some(ctor.plain().to_string()) };
                            lo.instr(Instr::PushTag(Push(tag)), ctx, |ctx, lo| {
                                lo.instr(Instr::PackProduct(Pack(Product)), ctx, |ctx, lo| {
                                    let res =
                                        lo.object.variables.alloc(VarName("ctor".to_string()));
                                    lo.instr(Instr::PopArg(Pop(res)), ctx, |ctx, lo| {
                                        kont(Atom::Var(res), ctx, lo)
                                    })
                                })
                            })
                        })
                    }),
                )
            }
            | Value::Triv(Triv) => kont(Atom::Literal(Literal::Int(0)), ctx, lo),
            | Value::VCons(Cons(a, b)) => a.lower(
                ctx,
                lo,
                Box::new(move |a, ctx, lo| {
                    b.lower(
                        ctx,
                        lo,
                        Box::new(move |b, ctx, lo| {
                            let res = lo.object.variables.alloc(VarName("cons".to_string()));
                            lo.instr(Instr::PushArg(Push(b)), ctx, |ctx, lo| {
                                lo.instr(Instr::PushArg(Push(a)), ctx, |ctx, lo| {
                                    lo.instr(Instr::PackProduct(Pack(Product)), ctx, |ctx, lo| {
                                        lo.instr(Instr::PopArg(Pop(res)), ctx, |ctx, lo| {
                                            kont(Atom::Var(res), ctx, lo)
                                        })
                                    })
                                })
                            })
                        }),
                    )
                }),
            ),
            | Value::TCons(Cons(_ty, inner)) => inner.lower(ctx, lo, kont),
            | Value::Lit(lit) => kont(Atom::Literal(lit), ctx, lo),
        }
    }
}
