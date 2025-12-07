//! Lower from [`zydeco_stack::StackArena`] to [`AssemblyArena`].
//!
//! - All global variables and all values are
//!   compiled into programs that pushes the value onto the stack.
//! - All computations and stacks are compiled into programs.

use super::arena::{AssemblyArena, AssemblyArenaLike};
use super::syntax::{Instruction as Instr, *};
use zydeco_stack::arena::StackArena;
use zydeco_stack::syntax as sk;
use zydeco_statics::tyck::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::syntax as t};
use zydeco_utils::arena::ArcGlobalAlloc;

pub trait Lower {
    type Kont;
    type Out;
    fn lower(&self, ctx: Context, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out;
}

pub struct Lowerer<'a> {
    pub arena: AssemblyArena,
    pub spans: &'a t::SpanArena,
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
    pub stack: &'a StackArena,
}

impl<'a> Lowerer<'a> {
    pub fn new(
        alloc: ArcGlobalAlloc, spans: &'a t::SpanArena, scoped: &'a ScopedArena,
        statics: &'a StaticsArena, stack: &'a StackArena,
    ) -> Self {
        let arena = AssemblyArena::new(alloc);
        Self { arena, spans, scoped, statics, stack }
    }

    pub fn run(mut self) -> AssemblyArena {
        // Compile all globals - each global value is compiled into a program that pushes it onto the stack
        for (def_id, global) in &self.stack.globals {
            match global {
                | sk::Global::Defined(value_id) => {
                    // Compile the global value into a program that pushes it onto the stack
                    let name = self.scoped.defs[def_id].plain().to_string();
                    let label = Label(name.clone());
                    // Lower the value - it will create instructions that push the value onto the stack
                    // The continuation receives the atom representing the value and creates a Return
                    let prog_id = value_id.lower(
                        Context::from_iter([]),
                        &mut self,
                        Box::new(move |atom, ctx, lo| {
                            // The value is already pushed onto the stack by value.lower's instructions
                            // Return the value (the atom represents what's on the stack)
                            lo.arena.prog_anon(Program::Return(Return(atom)), ctx)
                        }),
                    );
                    // Label the program with the global name
                    self.arena.labels.insert(prog_id, label);
                }
                | sk::Global::Extern(_) => {
                    // External globals don't need to be compiled - they're referenced when used
                }
            }
        }
        // Lower entry points from StackArena to AssemblyArena
        for (compu_id, ()) in &self.stack.entry {
            let prog_id = compu_id.lower(Context::from_iter([]), &mut self, ());
            self.arena.entry.insert(prog_id, ());
        }
        self.arena
    }
}

impl AsMut<AssemblyArena> for Lowerer<'_> {
    fn as_mut(&mut self) -> &mut AssemblyArena {
        &mut self.arena
    }
}

impl Lower for sk::VPatId {
    type Kont = Box<dyn FnOnce(Context, &mut Lowerer) -> ProgId>;
    type Out = ProgId;

    fn lower(&self, ctx: Context, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let vpat = lo.statics.vpats[self].clone();
        use zydeco_statics::tyck::syntax::ValuePattern as VPat;
        match vpat {
            | VPat::Hole(Hole) => lo.arena.prog_anon(Program::Panic(Panic), ctx),
            | VPat::Var(def) => {
                let name = lo.scoped.defs[&def].plain().to_string();
                let var = lo.arena.variables.alloc(VarName(name));
                lo.instr(Instr::PopArg(Pop(var)), ctx, |ctx, lo| {
                    let ctx = ctx.into_iter().chain([var]).collect();
                    kont(ctx, lo)
                })
            }
            | VPat::Ctor(Ctor(_ctor, _body)) => unreachable!(),
            | VPat::Triv(Triv) => {
                let var = lo.arena.variables.alloc(VarName("triv".to_string()));
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

impl Lower for sk::ValueId {
    type Kont = Box<dyn FnOnce(Atom, Context, &mut Lowerer) -> ProgId>;
    type Out = ProgId;

    fn lower(&self, ctx: Context, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let value = lo.stack.values[self].clone();
        use sk::Value;
        match value {
            | Value::Hole(Hole) => lo.arena.prog_anon(Program::Panic(Panic), ctx),
            | Value::Var(def) => {
                // Check if it's an external or defined global
                match lo.stack.globals.get(&def) {
                    | Some(sk::Global::Extern(_)) => {
                        let ext = lo.scoped.defs[&def].plain().to_string();
                        let ext_atom = Atom::External(ext.clone());
                        // Push the external value onto the stack
                        lo.instr(Instr::PushArg(Push(ext_atom)), ctx, |ctx, lo| {
                            kont(Atom::External(ext), ctx, lo)
                        })
                    }
                    | Some(sk::Global::Defined(value_id)) => {
                        // Recursively lower the defined value (which will push it)
                        value_id.lower(ctx, lo, kont)
                    }
                    | None => {
                        // Local variable - should be in context, but for now create a variable
                        // TODO: Implement proper context mapping for local variables
                        let name = lo.scoped.defs[&def].plain().to_string();
                        let var = lo.arena.variables.alloc(VarName(name));
                        // Push the variable value onto the stack
                        lo.instr(Instr::PushArg(Push(Atom::Var(var))), ctx, |ctx, lo| {
                            kont(Atom::Var(var), ctx, lo)
                        })
                    }
                }
            }
            | Value::Clo(sk::Clo { capture: _, stack: _, body }) => {
                // Lower the closure body
                let body_prog = body.lower(ctx.clone(), lo, ());
                // Push the closure onto the stack (label, then pack closure)
                lo.instr(Instr::PushArg(Push(Atom::Label(body_prog))), ctx, |ctx, lo| {
                    lo.instr(Instr::PackClosure(Pack(Closure(ctx.clone()))), ctx, |ctx, lo| {
                        // The closure is now on the stack, continue with continuation
                        // Create a temporary variable to represent the value on stack
                        let var = lo.arena.variables.alloc(VarName("thunk".to_string()));
                        kont(Atom::Var(var), ctx, lo)
                    })
                })
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                // Try to find the constructor index by searching through all data definitions
                // This is a fallback approach since we can't easily map stack ValueId to statics ValueId
                let idx = lo
                    .statics
                    .datas
                    .iter()
                    .find_map(|(_data_id, data)| {
                        data.iter().position(|(tag_ctor, _ty)| tag_ctor == &ctor)
                    })
                    .unwrap_or(0);
                body.lower(
                    ctx,
                    lo,
                    Box::new(move |atom, ctx, lo| {
                        // Push the constructor value onto the stack
                        lo.instr(Instr::PushArg(Push(atom)), ctx, |ctx, lo| {
                            let tag = Tag { idx, name: Some(ctor.plain().to_string()) };
                            lo.instr(Instr::PushTag(Push(tag)), ctx, |ctx, lo| {
                                lo.instr(Instr::PackProduct(Pack(Product)), ctx, |ctx, lo| {
                                    // The constructor is now on the stack, continue with continuation
                                    let res = lo.arena.variables.alloc(VarName("ctor".to_string()));
                                    kont(Atom::Var(res), ctx, lo)
                                })
                            })
                        })
                    }),
                )
            }
            | Value::Triv(Triv) => {
                // Push the trivial value (unit) onto the stack
                lo.instr(Instr::PushArg(Push(Atom::Literal(Literal::Int(0)))), ctx, |ctx, lo| {
                    kont(Atom::Literal(Literal::Int(0)), ctx, lo)
                })
            }
            | Value::VCons(Cons(a, b)) => a.lower(
                ctx,
                lo,
                Box::new(move |a_atom, ctx, lo| {
                    b.lower(
                        ctx,
                        lo,
                        Box::new(move |b_atom, ctx, lo| {
                            // Push the cons value onto the stack
                            lo.instr(Instr::PushArg(Push(b_atom)), ctx, |ctx, lo| {
                                lo.instr(Instr::PushArg(Push(a_atom)), ctx, |ctx, lo| {
                                    lo.instr(Instr::PackProduct(Pack(Product)), ctx, |ctx, lo| {
                                        // The cons is now on the stack, continue with continuation
                                        let res =
                                            lo.arena.variables.alloc(VarName("cons".to_string()));
                                        kont(Atom::Var(res), ctx, lo)
                                    })
                                })
                            })
                        }),
                    )
                }),
            ),
            | Value::Lit(lit) => {
                // Push the literal value onto the stack
                lo.instr(Instr::PushArg(Push(Atom::Literal(lit.clone()))), ctx, |ctx, lo| {
                    kont(Atom::Literal(lit), ctx, lo)
                })
            }
        }
    }
}

impl Lower for sk::StackId {
    type Kont = Box<dyn FnOnce(Context, &mut Lowerer) -> ProgId>;
    type Out = ProgId;

    fn lower(&self, ctx: Context, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let stack = lo.stack.stacks[self].clone();
        use sk::Stack;
        match stack {
            | Stack::Kont(sk::Kont { binder, body }) => {
                // Lower the binder pattern, then continue with body
                binder.lower(ctx, lo, Box::new(move |ctx, lo| body.lower(ctx, lo, ())))
            }
            | Stack::Var(sk::Bullet) => {
                // Stack variable - just continue
                kont(ctx, lo)
            }
            | Stack::Arg(Cons(value, rest)) => {
                // Argument stack - recurse on rest first, then push value on top
                rest.lower(
                    ctx,
                    lo,
                    Box::new(move |ctx, lo| {
                        value.lower(
                            ctx,
                            lo,
                            Box::new(move |atom, ctx, lo| {
                                lo.instr(Instr::PushArg(Push(atom)), ctx, |ctx, lo| kont(ctx, lo))
                            }),
                        )
                    }),
                )
            }
            | Stack::Tag(Cons(dtor, rest)) => {
                // Tag stack - push tag, then recurse on rest
                // For destructors, we can't easily get the index without more context
                // Use the destructor name for now
                let tag = Tag {
                    idx: 0, // Destructor indices are less critical, using 0 as placeholder
                    name: Some(dtor.plain().to_string()),
                };
                lo.instr(Instr::PushTag(Push(tag)), ctx, |ctx, lo| rest.lower(ctx, lo, kont))
            }
        }
    }
}

impl Lower for sk::CompuId {
    type Kont = ();
    type Out = ProgId;

    fn lower(&self, ctx: Context, lo: &mut Lowerer, _kont: Self::Kont) -> Self::Out {
        let compu = lo.stack.compus[self].clone();
        use sk::Computation as Compu;
        match compu {
            | Compu::Hole(Hole) => lo.arena.prog_anon(Program::Panic(Panic), ctx),
            | Compu::Fix(Fix(param, body)) => {
                let body_prog = body.lower(ctx.clone(), lo, ());
                let label = Label(format!("fix_{:?}", self));
                // Lower the param first to get the param program
                let param_prog = param.lower(ctx.clone(), lo, Box::new(move |_ctx, _lo| body_prog));
                lo.arena.prog_fix_point(
                    |_arena, self_id| {
                        Program::Bind(Bind { binder: (), bindee: param_prog, tail: self_id })
                    },
                    ctx,
                    label,
                )
            }
            | Compu::Force(sk::SForce { thunk, stack }) => {
                // The thunk value should already be on the stack from lowering
                // Unpack the closure and continue with the stack
                thunk.lower(
                    ctx,
                    lo,
                    Box::new(move |_atom, ctx, lo| {
                        lo.instr(
                            Instr::UnpackClosure(Unpack(Closure(ctx.clone()))),
                            ctx,
                            |ctx, lo| {
                                stack.lower(
                                    ctx,
                                    lo,
                                    Box::new(move |ctx, lo| {
                                        lo.arena.prog_anon(Program::Call(Call), ctx)
                                    }),
                                )
                            },
                        )
                    }),
                )
            }
            | Compu::Ret(sk::SReturn { stack, value }) => {
                // Lower the value (which pushes it onto the stack), then continue with stack
                value.lower(
                    ctx,
                    lo,
                    Box::new(move |atom, ctx, lo| {
                        stack.lower(
                            ctx,
                            lo,
                            Box::new(move |ctx, lo| {
                                // The value is already on the stack from value.lower
                                lo.arena.prog_anon(Program::Return(Return(atom)), ctx)
                            }),
                        )
                    }),
                )
            }
            | Compu::Case(Match { scrut, arms }) => {
                scrut.lower(
                    ctx,
                    lo,
                    Box::new(move |_atom, ctx, lo| {
                        // Lower all arms
                        let mut branches = Vec::new();
                        for arm in arms {
                            let Matcher { binder, tail } = arm;
                            // Extract tag from pattern - check if it's a Ctor pattern
                            let tag = {
                                let vpat = lo.statics.vpats.get(&binder);
                                if let Some(zydeco_statics::tyck::syntax::ValuePattern::Ctor(
                                    Ctor(ctor, _),
                                )) = vpat
                                {
                                    // Get type annotation to find Data type
                                    let idx = lo
                                        .statics
                                        .annotations_vpat
                                        .get(&binder)
                                        .and_then(|&ty_id| match &lo.statics.types[&ty_id] {
                                            | zydeco_statics::tyck::syntax::Fillable::Done(
                                                zydeco_statics::tyck::syntax::Type::Data(data_id),
                                            ) => lo.statics.datas[&data_id]
                                                .iter()
                                                .position(|(tag_ctor, _ty)| tag_ctor == ctor),
                                            | _ => None,
                                        })
                                        .unwrap_or(branches.len());
                                    Tag { idx, name: Some(ctor.plain().to_string()) }
                                } else {
                                    Tag { idx: branches.len(), name: None }
                                }
                            };
                            let branch_prog = binder.lower(
                                ctx.clone(),
                                lo,
                                Box::new(move |ctx, lo| tail.lower(ctx, lo, ())),
                            );
                            branches.push((tag, branch_prog));
                        }
                        lo.arena.prog_anon(Program::Branch(Branch(branches)), ctx)
                    }),
                )
            }
            | Compu::LetValue(Let { binder, bindee, tail }) => bindee.lower(
                ctx,
                lo,
                Box::new(move |_atom, ctx, lo| {
                    binder.lower(ctx, lo, Box::new(move |ctx, lo| tail.lower(ctx, lo, ())))
                }),
            ),
            | Compu::LetStack(Let { binder: _, bindee, tail }) => {
                bindee.lower(ctx, lo, Box::new(move |ctx, lo| tail.lower(ctx, lo, ())))
            }
            | Compu::LetArg(Let { binder: Cons(vpat, _bullet), bindee, tail }) => bindee.lower(
                ctx,
                lo,
                Box::new(move |ctx, lo| {
                    vpat.lower(ctx, lo, Box::new(move |ctx, lo| tail.lower(ctx, lo, ())))
                }),
            ),
            | Compu::CoCase(CoMatch { arms }) => {
                // Lower all arms
                let mut branches = Vec::new();
                for arm in arms {
                    let CoMatcher { dtor, tail } = arm;
                    let branch_prog = tail.lower(ctx.clone(), lo, ());
                    let tag = Tag { idx: branches.len(), name: Some(dtor.0.plain().to_string()) };
                    branches.push((tag, branch_prog));
                }
                lo.arena.prog_anon(Program::Branch(Branch(branches)), ctx)
            }
        }
    }
}
