//! Lower from [`zydeco_stackir::StackArena`] to [`AssemblyArena`].
//!
//! - All global variables and all values are
//!   compiled into programs that pushes the value onto the stack.
//! - All computations and stacks are compiled into programs.

use super::{
    arena::{AssemblyArena, CxKont, Kont},
    syntax::*,
};
use derive_more::{AsMut, AsRef};
use zydeco_stackir::{StackirArena, sps::syntax as sk};
use zydeco_statics::tyck::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_utils::{arena::ArcGlobalAlloc, with::With};

pub trait Lower<'a> {
    type Kont;
    type Out;
    fn lower(&self, lo: &mut Lowerer<'a>, kont: Self::Kont) -> Self::Out;
}

#[derive(AsRef, AsMut)]
pub struct Lowerer<'a> {
    #[as_ref]
    #[as_mut]
    pub arena: AssemblyArena,
    pub spans: &'a SpanArena,
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
    pub stackir: &'a StackirArena,
}

impl<'a> Lowerer<'a> {
    pub fn new(
        alloc: ArcGlobalAlloc, spans: &'a SpanArena, scoped: &'a ScopedArena,
        statics: &'a StaticsArena, stackir: &'a StackirArena,
    ) -> Self {
        let arena = AssemblyArena::new(alloc);
        Self { arena, spans, scoped, statics, stackir }
    }

    pub fn run(mut self) -> AssemblyArena {
        // Lower all builtins
        for (_, builtin) in self.stackir.admin.builtins.iter() {
            let sk::Builtin { name, arity, sort } = builtin.clone();
            if sort == sk::BuiltinSort::Function {
                self.arena.externs.push(Extern { name, arity });
            }
        }

        // Lower entry points from StackArena to AssemblyArena.
        // Each entry compu is already let g1 = v1 in ... in body, so lowering it handles globals.
        for (compu_id, ()) in &self.stackir.inner.entry {
            let whole = (*compu_id).lower(&mut self, Context::new());
            self.arena.entry.insert(whole, ());
        }
        self.arena
    }
}

impl<'a> Lower<'a> for sk::VPatId {
    type Kont = With<Context, Kont<'a, Lowerer<'a>>>;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer<'a>, With { info: cx, inner: kont }: Self::Kont) -> Self::Out {
        let vpat = lo.stackir.inner.vpats[self].clone();
        use sk::ValuePattern as VPat;
        match vpat {
            | VPat::Hole(Hole) => {
                let var = VarName::from("_").build(lo, None);
                let incr = Box::new(move |cx: &Context| cx.clone() + [var]);
                Pop(var).build(lo, With::new(cx, CxKont { incr, kont }))
            }
            | VPat::Var(def_id) => {
                // Pop the value from the stack into the variable
                let name = lo.scoped.defs[&def_id].clone();
                let var = name.build(lo, Some(def_id));
                let incr = Box::new(move |cx: &Context| cx.clone() + [var]);
                Pop(var).build(lo, With::new(cx, CxKont { incr, kont }))
            }
            | VPat::Ctor(Ctor(ctor, param)) => {
                let _ = ctor;
                let _ = param;
                unreachable!("Ctor patterns should not directly appear in ZASM");
                // let vpat_data = *self;
                // // Unpack the pair value
                // Unpack(ProductMarker).build(
                //     lo,
                //     Box::new(move |lo: &mut Lowerer| {
                //         // Compile the remaining pattern
                //         let res = param.lower(lo, kont);
                //         // Push a tag and see if the constructor is the same
                //         let idx = lo.find_ctor_tag_idx_from_vpat(vpat_data, &ctor);
                //         let name = ctor.plain().to_string();
                //         let tag = Tag { idx, name: Some(name) };
                //         Push(tag).build(
                //             lo,
                //             Box::new(move |lo: &mut Lowerer| {
                //                 // Compare the tag with the constructor
                //                 EqJump(res).build(lo, ())
                //             }),
                //         )
                //     }),
                // )
            }
            | VPat::Triv(Triv) => {
                // Pop and do nothing
                let var = VarName::from("_").build(lo, None);
                let incr = Box::new(move |cx: &Context| cx.clone() + [var]);
                Pop(var).build(lo, With::new(cx, CxKont { incr, kont }))
            }
            | VPat::VCons(Cons(a, b)) => {
                // Unpack the pair value from the stack, and then process a and b
                Unpack(ProductMarker).build(lo, {
                    With::new(
                        cx,
                        CxKont::same(Box::new(move |lo: &mut Lowerer, cx| {
                            a.lower(lo, {
                                With::new(
                                    cx,
                                    Box::new(move |lo: &mut Lowerer, cx| {
                                        b.lower(lo, With::new(cx, kont))
                                    }),
                                )
                            })
                        })),
                    )
                })
            }
        }
    }
}

/// Values are compiled into programs that push the value onto the stack.
impl<'a> Lower<'a> for sk::ValueId {
    type Kont = With<Context, Kont<'a, Lowerer<'a>>>;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer<'a>, With { info: cx, inner: kont }: Self::Kont) -> Self::Out {
        let value = lo.stackir.inner.values[self].clone();
        use sk::Value;
        match value {
            | Value::Hole(Hole) => Abort.build(lo, cx),
            | Value::Var(def_id) => {
                // Retrieve the variable from the context
                let _name = lo.scoped.defs[&def_id].clone();
                // log::trace!("lowering var: {}{}", _name.plain(), def_id.concise());
                let atom = match lo.arena.defs.forth(&def_id).clone() {
                    | DefId::Var(var_id) => Atom::Var(var_id),
                    | DefId::Sym(sym_id) => Atom::Sym(sym_id),
                };
                // Push the atom onto the stack
                Push(atom).build(lo, With::new(cx, CxKont::same(kont)))
            }
            | Value::Closure(sk::Closure { capture, stack: sk::Bullet, body }) => {
                assert!(capture.iter().count() == 0, "Capture is not empty");
                let body = body.lower(lo, Context::new());
                // Label the closure code
                let sym = body.build(lo, (Some(String::from("clo")), None));
                // Push the atom to the stack
                let atom = Atom::Sym(sym);
                Push(atom).build(lo, With::new(cx, CxKont::same(kont)))
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                // Push the body onto the stack
                body.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            // Push the constructor tag onto the stack
                            let idx = ctor.idx;
                            let name = ctor.name.plain().to_string();
                            let tag = Tag { idx, name: Some(name) };
                            Push(tag).build(
                                lo,
                                With::new(
                                    cx,
                                    CxKont::same(Box::new(move |lo: &mut Lowerer, cx| {
                                        // Pack them into a pair value
                                        Pack(ProductMarker)
                                            .build(lo, With::new(cx, CxKont::same(kont)))
                                    })),
                                ),
                            )
                        }),
                    ),
                )
            }
            | Value::Triv(Triv) => {
                // Push the trivial value onto the stack
                let atom = Atom::Imm(Imm::Triv(Triv));
                Push(atom).build(lo, With::new(cx, CxKont::same(kont)))
            }
            | Value::VCons(Cons(a, b)) => {
                // Push b and then a onto the stack
                b.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            a.lower(
                                lo,
                                With::new(
                                    cx,
                                    Box::new(move |lo, cx| {
                                        // and then pack the pair value
                                        Pack(ProductMarker)
                                            .build(lo, With::new(cx, CxKont::same(kont)))
                                    }),
                                ),
                            )
                        }),
                    ),
                )
            }
            | Value::Literal(Literal::Int(i)) => {
                // Push the literal value onto the stack
                let atom = Atom::Imm(Imm::Int(i));
                Push(atom).build(lo, With::new(cx, CxKont::same(kont)))
            }
            | Value::Literal(Literal::Char(c)) => {
                // Push the literal value onto the stack
                let atom = Atom::Imm(Imm::Char(c));
                Push(atom).build(lo, With::new(cx, CxKont::same(kont)))
            }
            | Value::Literal(Literal::String(s)) => {
                // Push the literal value onto the stack
                let atom = Atom::Sym(s.build(lo, (Some(String::from("")), None)));
                Push(atom).build(lo, With::new(cx, CxKont::same(kont)))
            }
            | Value::Complex(sk::Complex { operator, operands }) => {
                // Lower all operands onto the stack
                let arity = operands.len();
                let kont: Kont<'_, Lowerer<'_>> = Box::new(move |lo, cx| {
                    Intrinsic { name: operator, arity }.build(lo, With::new(cx, CxKont::same(kont)))
                });
                let kont = operands.into_iter().fold(
                    kont,
                    |kont: Kont<'_, Lowerer<'_>>, operand: sk::ValueId| {
                        Box::new(move |lo, cx| operand.lower(lo, With::new(cx, kont)))
                    },
                );
                kont(lo, cx)
            }
        }
    }
}

impl<'a> Lower<'a> for sk::StackId {
    /// Stacks in ZIR are compiled to instructions
    type Kont = With<Context, Kont<'a, Lowerer<'a>>>;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer<'a>, With { info: cx, inner: kont }: Self::Kont) -> Self::Out {
        let stack = lo.stackir.inner.stacks[self].clone();
        use sk::Stack;
        match stack {
            | Stack::Kont(sk::Kont { binder, body }) => {
                let original_cx = cx.clone();
                // Lower the continuation code into a symbol
                // which is `swap; pop ctx; [[binder]]; [[body]]`
                // The stack shape: [return value, context]
                let code = Swap.build(
                    lo,
                    With::new(
                        Context::new(),
                        CxKont::same(Box::new(move |lo, _| {
                            Pop(ContextMarker).build(
                                lo,
                                With::new(
                                    Context::new(),
                                    CxKont {
                                        // Restore the original context
                                        incr: Box::new(move |_| original_cx),
                                        kont: Box::new(move |lo, cx| {
                                            binder.lower(
                                                lo,
                                                With::new(
                                                    cx,
                                                    Box::new(move |lo, cx| body.lower(lo, cx)),
                                                ),
                                            )
                                        }),
                                    },
                                ),
                            )
                        })),
                    ),
                );
                let sym = code.build(lo, (Some(String::from("kont")), None));
                // Push the context pointer, and then the code
                Push(ContextMarker).build(
                    lo,
                    With::new(
                        cx,
                        CxKont::same(Box::new(move |lo: &mut Lowerer, cx| {
                            Push(Atom::Sym(sym)).build(
                                lo,
                                With::new(
                                    cx,
                                    CxKont::same(Box::new(move |lo: &mut Lowerer, cx| {
                                        // Finally, we do the rest
                                        kont(lo, cx)
                                    })),
                                ),
                            )
                        })),
                    ),
                )
            }
            | Stack::Var(sk::Bullet) => {
                // Do nothing
                kont(lo, cx)
            }
            | Stack::Arg(Cons(value, stack)) => {
                // Finish the stack first
                stack.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            // Push the value onto the stack
                            value.lower(lo, With::new(cx, kont))
                        }),
                    ),
                )
            }
            | Stack::Tag(Cons(dtor, stack)) => {
                // Finish the stack first
                stack.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            // Push the destructor tag onto the stack
                            let idx = dtor.idx;
                            let name = dtor.name.plain().to_string();
                            let tag = Tag { idx, name: Some(name) };
                            Push(tag).build(lo, With::new(cx, CxKont::same(kont)))
                        }),
                    ),
                )
            }
        }
    }
}

impl<'a> Lower<'a> for sk::CompuId {
    type Kont = Context;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer<'a>, cx: Self::Kont) -> Self::Out {
        let compu = lo.stackir.inner.compus[self].clone();
        use sk::Computation as Compu;
        match compu {
            | Compu::Hole(Hole) => Abort.build(lo, cx),
            | Compu::Force(sk::SForce { thunk, stack }) => {
                // Lower the stack first
                stack.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            // Lower the thunk to a value on the stack
                            thunk.lower(
                                lo,
                                With::new(
                                    cx,
                                    Box::new(move |lo, cx| {
                                        // Allocate a new context
                                        Alloc(ContextMarker).build(
                                            lo,
                                            With::new(
                                                cx,
                                                CxKont::clean(Box::new(move |lo, cx| {
                                                    // PopJump to the thunk
                                                    PopJump.build(lo, cx)
                                                })),
                                            ),
                                        )
                                    }),
                                ),
                            )
                        }),
                    ),
                )
            }
            | Compu::Ret(sk::SReturn { stack, value }) => {
                // Lower the stack first
                stack.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            // Lower the value
                            value.lower(
                                lo,
                                With::new(
                                    cx,
                                    Box::new(move |lo, cx| {
                                        // The stack shape: [return value, return address, context]
                                        // Leap jump to the continuation
                                        LeapJump.build(lo, cx)
                                    }),
                                ),
                            )
                        }),
                    ),
                )
            }
            | Compu::Fix(sk::SFix { capture, param, body }) => {
                assert!(
                    capture.iter().count() == 0,
                    "ZASM requires empty capture. Please perform closure conversion first."
                );
                // Label the body code; using trivial value as a placeholder
                let name = lo.scoped.defs[&param].plain();
                let sym = Undefined.build(lo, (Some(name.to_string()), Some(param)));
                // Lower the body
                let body_prog = body.lower(lo, cx.clone());
                // Nominate the body program
                *lo.arena.blocks.entry(body_prog).or_insert(0) += 2;
                lo.arena.symbols.replace(
                    sym,
                    NamedSymbol { name: name.to_string(), inner: Symbol::Prog(body_prog) },
                );
                lo.arena.labels.insert(body_prog, sym);
                // Jump to the body
                Jump(body_prog).build(lo, cx)
            }
            | Compu::Case(Match { scrut, arms }) => {
                // Lower the scrutinee
                scrut.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            // Should we compile to a jump table?
                            // If any branch is not a constructor, we don't compile to a jump table.
                            let is_jump_table =
                                arms.iter().fold(true, |acc, Matcher { binder, tail: _ }| {
                                    use sk::ValuePattern as VPat;
                                    match lo.stackir.inner.vpats[&binder].clone() {
                                        | VPat::Ctor(_) => acc,
                                        | _ => false,
                                    }
                                });
                            if is_jump_table {
                                // Optimization: compile to a jump table
                                let mut lowered_arms = Vec::new();
                                for Matcher { binder, tail } in arms {
                                    // The binder is a constructor or other things.
                                    use sk::ValuePattern as VPat;
                                    match lo.stackir.inner.vpats[&binder].clone() {
                                        | VPat::Ctor(Ctor(ctor, binder)) => {
                                            let idx = ctor.idx;
                                            let name = ctor.name.plain().to_string();
                                            let tag = Tag { idx, name: Some(name) };
                                            // Lower the tail
                                            let tail_prog = binder.lower(
                                                lo,
                                                With::new(
                                                    cx.clone(),
                                                    Box::new(move |lo, cx| tail.lower(lo, cx)),
                                                ),
                                            );
                                            // Nominate the tail program
                                            let _sym = tail_prog
                                                .build(lo, (Some(String::from("arm")), None));
                                            // Add to the jump table
                                            lowered_arms.push((tag, tail_prog));
                                        }
                                        | _ => {
                                            panic!(
                                                "Inrefutable pattern matcher must be unique in ZASM"
                                            )
                                        }
                                    }
                                }
                                // Unpack the value
                                Unpack(ProductMarker).build(
                                    lo,
                                    With::new(
                                        cx,
                                        CxKont::same(Box::new(move |lo: &mut Lowerer, cx| {
                                            // Jump table
                                            PopBranch(lowered_arms).build(lo, cx)
                                        })),
                                    ),
                                )
                            } else {
                                assert!(
                                    arms.len() == 1,
                                    "Inrefutable pattern matcher must be unique in ZASM"
                                );
                                let Matcher { binder, tail } = arms[0];
                                // Basically same as let value
                                binder.lower(
                                    lo,
                                    With::new(
                                        cx,
                                        Box::new(move |lo, cx| {
                                            // Lower the tail
                                            tail.lower(lo, cx)
                                        }),
                                    ),
                                )
                            }
                        }),
                    ),
                )
            }
            | Compu::Join(sk::LetJoin::Value(Let { binder, bindee, tail })) => {
                // Lower the bindee
                bindee.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            // Lower the binder
                            binder.lower(
                                lo,
                                With::new(
                                    cx,
                                    Box::new(move |lo, cx| {
                                        // Lower the tail
                                        tail.lower(lo, cx)
                                    }),
                                ),
                            )
                        }),
                    ),
                )
            }
            | Compu::Join(sk::LetJoin::Stack(Let { binder: sk::Bullet, bindee, tail })) => {
                // Lower the bindee
                bindee.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            // Lower the tail
                            tail.lower(lo, cx)
                        }),
                    ),
                )
            }
            | Compu::LetArg(Let { binder: Cons(param, sk::Bullet), bindee, tail }) => {
                // Lower the bindee
                bindee.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            // Lower the param
                            param.lower(
                                lo,
                                With::new(
                                    cx,
                                    Box::new(move |lo, cx| {
                                        // Lower the tail
                                        tail.lower(lo, cx)
                                    }),
                                ),
                            )
                        }),
                    ),
                )
            }
            | Compu::CoCase(CoMatch { arms }) => {
                let mut lowered_arms = Vec::new();
                for CoMatcher { dtor: Cons(dtor, sk::Bullet), tail } in arms {
                    // Lower the tail
                    let tail_prog = tail.lower(lo, cx.clone());
                    let idx = dtor.idx;
                    let name = dtor.name.plain().to_string();
                    let tag = Tag { idx, name: Some(name) };
                    // Nominate the tail program
                    let _sym = tail_prog.build(lo, (Some(String::from("coarm")), None));
                    // Add to the jump table
                    lowered_arms.push((tag, tail_prog));
                }
                // Create the co-case program
                PopBranch(lowered_arms).build(lo, cx)
            }
            | Compu::ExternCall(sk::ExternCall { function, stack: sk::Bullet }) => {
                Extern { name: function, arity: 0 }.build(lo, cx)
            }
        }
    }
}
