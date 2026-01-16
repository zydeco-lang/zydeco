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
use zydeco_stackir::{arena::StackArena, syntax as sk};
use zydeco_statics::tyck::{arena::StaticsArena, syntax as ss};
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
    pub stack: &'a StackArena,
}

impl<'a> Lowerer<'a> {
    pub fn new(
        alloc: ArcGlobalAlloc, spans: &'a SpanArena, scoped: &'a ScopedArena,
        statics: &'a StaticsArena, stack: &'a StackArena,
    ) -> Self {
        let arena = AssemblyArena::new(alloc);
        Self { arena, spans, scoped, statics, stack }
    }

    pub fn run(mut self) -> AssemblyArena {
        // Lower all builtins
        for (_, builtin) in self.stack.builtins.iter() {
            let sk::Builtin { name, arity, sort } = builtin.clone();
            if sort == sk::BuiltinSort::Function {
                self.arena.externs.push(Extern { name, arity });
            }
        }

        // Lower entry points from StackArena to AssemblyArena
        for (compu_id, ()) in &self.stack.entry {
            let compu_id = *compu_id;

            // All globals are statically known, so compile them as symbols.
            // Collect globals with their def_ids for unified processing
            let globals: Vec<_> = (self.stack.sequence.clone().iter())
                .rev()
                .map(|&def_id| (def_id, self.stack.globals[&def_id]))
                .collect();

            // Build a continuation that initializes all globals in order
            let kont: Kont<'_, Lowerer<'_>> = globals.into_iter().fold(
                Box::new(move |lo: &mut Lowerer, cx| {
                    // After all globals are initialized, lower the entry point
                    compu_id.lower(lo, cx)
                }),
                |kont, (def_id, body)| {
                    // wrap continuation with lowering logic
                    Box::new(move |lo: &mut Lowerer, cx| {
                        body.lower(
                            lo,
                            With::new(
                                cx,
                                Box::new(move |lo: &mut Lowerer, cx| {
                                    let name = lo.scoped.defs[&def_id].clone();
                                    let var = name.build(lo, Some(def_id));
                                    let incr = Box::new(move |cx: &Context| cx.clone() + [var]);
                                    Pop(var).build(lo, With::new(cx, CxKont { incr, kont }))
                                }),
                            ),
                        )
                    })
                },
            );

            // Execute the initialization chain
            let whole = kont(&mut self, Context::new());
            self.arena.entry.insert(whole, ());
        }
        self.arena
    }

    /// Find the constructor tag index in a data type
    fn find_ctor_tag_idx(&self, ty: ss::TypeId, ctor_name: &zydeco_syntax::CtorName) -> usize {
        // Get the data type
        let data = {
            {
                use zydeco_statics::tyck::syntax::*;
                // Get the type annotation
                let mut ty = ty;
                loop {
                    let err_msg = || {
                        let span_str = self
                            .statics
                            .terms
                            .back(&ss::TermId::Type(ty))
                            .and_then(|su_term_id| self.scoped.textual.back(&(*su_term_id).into()))
                            .map(|entity_id| &self.spans[entity_id])
                            .map(|span| format!(" @ {}", span))
                            .unwrap_or_default();
                        let ty_str = {
                            use zydeco_statics::tyck::fmt::*;
                            ty.ugly(&Formatter::new(self.scoped, self.statics))
                        };
                        (span_str, ty_str)
                    };
                    match self.statics.types[&ty] {
                        | Fillable::Done(Type::Data(data)) => {
                            break data;
                        }
                        | Fillable::Done(Type::Abst(abst)) => {
                            ty = self.statics.seals[&abst];
                        }
                        | Fillable::Done(Type::Abs(Abs(_, _body))) => {
                            // ty = body;
                            unreachable!(
                                "unexpected type abstraction in data type {}",
                                ty.ugly(&zydeco_statics::tyck::fmt::Formatter::new(
                                    self.scoped,
                                    self.statics
                                ))
                            );
                        }
                        | Fillable::Done(Type::App(App(_f, _))) => {
                            // ty = f;
                            unreachable!(
                                "unexpected type application in data type {}",
                                ty.ugly(&zydeco_statics::tyck::fmt::Formatter::new(
                                    self.scoped,
                                    self.statics
                                ))
                            );
                        }
                        | Fillable::Fill(fill) => {
                            let AnnId::Type(ty_) = self.statics.solus[&fill] else {
                                let (span_str, ty_str) = err_msg();
                                unreachable!(
                                    "Value type {} is not a type in statics{}",
                                    ty_str, span_str
                                )
                            };
                            ty = ty_;
                        }
                        | _ => {
                            let (span_str, ty_str) = err_msg();
                            unreachable!(
                                "Value type {} is not a data type in statics{}",
                                ty_str, span_str
                            );
                        }
                    }
                }
            }
        };
        // Find the index of the constructor tag
        self.statics.datas[&data]
            .iter()
            .position(|(tag_branch, _ty)| tag_branch == ctor_name)
            .expect("Constructor tag not found")
    }

    /// Find the constructor tag index in a data type, given a stack value
    fn find_ctor_tag_idx_from_value(
        &self, value_id: sk::ValueId, ctor_name: &zydeco_syntax::CtorName,
    ) -> usize {
        // Get the corresponding statics value
        let ss::TermId::Value(value) = self
            .stack
            .terms
            .back(&sk::TermId::Value(value_id))
            .expect("Constructor value not found")
        else {
            unreachable!("Constructor is not a value in statics")
        };
        // Get the type annotation
        let ty = self.statics.annotations_value[value];
        self.find_ctor_tag_idx(ty, ctor_name)
    }

    // /// Find the constructor tag index in a data type, given a stack value pattern
    // fn find_ctor_tag_idx_from_vpat(
    //     &self, vpat_id: sk::VPatId, ctor_name: &zydeco_syntax::CtorName,
    // ) -> usize {
    //     // Get the corresponding statics pattern
    //     let ss::PatId::Value(vpat) =
    //         self.stack.pats.back(&vpat_id).expect("Constructor pattern not found")
    //     else {
    //         unreachable!("Constructor is not a value pattern in statics")
    //     };
    //     // Get the type annotation
    //     let ty = self.statics.annotations_vpat[&vpat].clone();
    //     self.find_ctor_tag_idx(ty, ctor_name)
    // }

    /// Get the codata type from a computation's type annotation
    fn find_dtor_tag_idx(&self, compu: ss::CompuId, dtor_name: &zydeco_syntax::DtorName) -> usize {
        // Get the codata type
        let codata = {
            use zydeco_statics::tyck::syntax::*;
            // Get the type annotation
            let mut ty = self.statics.annotations_compu[&compu];
            loop {
                let err_msg = || {
                    let span_str = self
                        .statics
                        .terms
                        .back(&ss::TermId::Compu(compu))
                        .and_then(|su_term_id| self.scoped.textual.back(&(*su_term_id).into()))
                        .map(|entity_id| &self.spans[entity_id])
                        .map(|span| format!(" @ {}", span))
                        .unwrap_or_default();
                    let ty_str = {
                        use zydeco_statics::tyck::fmt::*;
                        ty.ugly(&Formatter::new(self.scoped, self.statics))
                    };
                    (span_str, ty_str)
                };
                match self.statics.types[&ty] {
                    | Fillable::Done(Type::CoData(codata)) => {
                        break codata;
                    }
                    | Fillable::Done(Type::Abst(abst)) => {
                        ty = self.statics.seals[&abst];
                    }
                    | Fillable::Done(Type::Abs(Abs(_, _body))) => {
                        // ty = body;
                        unreachable!(
                            "unexpected type abstraction in codata type {}",
                            ty.ugly(&zydeco_statics::tyck::fmt::Formatter::new(
                                self.scoped,
                                self.statics
                            ))
                        );
                    }
                    | Fillable::Done(Type::App(App(_f, _))) => {
                        // ty = f;
                        unreachable!(
                            "unexpected type application in codata type {}",
                            ty.ugly(&zydeco_statics::tyck::fmt::Formatter::new(
                                self.scoped,
                                self.statics
                            ))
                        );
                    }
                    | Fillable::Fill(fill) => {
                        let AnnId::Type(ty_) = self.statics.solus[&fill] else {
                            let (span_str, ty_str) = err_msg();
                            unreachable!(
                                "Computation type {} is not a type in statics{}",
                                ty_str, span_str
                            )
                        };
                        ty = ty_;
                    }
                    | _ => {
                        let (span_str, ty_str) = err_msg();
                        unreachable!(
                            "Computation type {} is not a codata type in statics{}",
                            ty_str, span_str
                        );
                    }
                }
            }
        };
        // Find the index of the destructor tag
        self.statics.codatas[&codata]
            .iter()
            .position(|(dtor_branch, _ty)| dtor_branch == dtor_name)
            .expect("Destructor tag not found")
    }

    /// Find the destructor tag index in a codata type, given a computation
    fn find_dtor_tag_idx_from_compu(
        &self, compu_id: sk::CompuId, dtor_name: &zydeco_syntax::DtorName,
    ) -> usize {
        // Get the corresponding statics computation
        let ss::TermId::Compu(compu) =
            self.stack.terms.back(&sk::TermId::Compu(compu_id)).expect("Computation not found")
        else {
            unreachable!("Computation is not a computation in statics")
        };
        self.find_dtor_tag_idx(*compu, dtor_name)
    }

    /// Find the destructor tag index in a codata type, given a stack
    /// Note: This requires the computation context that uses the stack
    fn find_dtor_tag_idx_from_stack(
        &self, stack_id: sk::StackId, dtor_name: &zydeco_syntax::DtorName,
    ) -> usize {
        // Get the corresponding statics stack
        let ss::TermId::Compu(compu) =
            self.stack.terms.back(&sk::TermId::Stack(stack_id)).expect("Stack not found")
        else {
            unreachable!("Stack is not a computation in statics")
        };
        self.find_dtor_tag_idx(*compu, dtor_name)
    }
}

impl<'a> Lower<'a> for sk::VPatId {
    type Kont = With<Context, Kont<'a, Lowerer<'a>>>;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer<'a>, With { info: cx, inner: kont }: Self::Kont) -> Self::Out {
        let vpat = lo.stack.vpats[self].clone();
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
        let value = lo.stack.values[self].clone();
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
                let value_data = *self;
                body.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            // Push the constructor tag onto the stack
                            let idx = lo.find_ctor_tag_idx_from_value(value_data, &ctor);
                            let name = ctor.plain().to_string();
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
        let stack = lo.stack.stacks[self].clone();
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
                let stack_codata = stack;
                stack.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            // Push the destructor tag onto the stack
                            let idx = lo.find_dtor_tag_idx_from_stack(stack_codata, &dtor);
                            let name = dtor.plain().to_string();
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
        let compu = lo.stack.compus[self].clone();
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
                let value_data = scrut;
                // Lower the scrutinee
                scrut.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            if arms.len() == 1 {
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
                            } else {
                                // Optimization: compile to a jump table
                                let mut lowered_arms = Vec::new();
                                for Matcher { binder, tail } in arms {
                                    // The binder is a constructor or other things.
                                    use sk::ValuePattern as VPat;
                                    match lo.stack.vpats[&binder].clone() {
                                        | VPat::Ctor(Ctor(ctor, binder)) => {
                                            let idx =
                                                lo.find_ctor_tag_idx_from_value(value_data, &ctor);
                                            let name = ctor.plain().to_string();
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
                            }
                        }),
                    ),
                )
            }
            | Compu::LetValue(Let { binder, bindee, tail }) => {
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
            | Compu::LetStack(Let { binder: sk::Bullet, bindee, tail }) => {
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
                let compu_id = *self;
                for CoMatcher { dtor: Cons(dtor, sk::Bullet), tail } in arms {
                    // Lower the tail
                    let tail_prog = tail.lower(lo, cx.clone());
                    let idx = lo.find_dtor_tag_idx_from_compu(compu_id, &dtor);
                    let name = dtor.plain().to_string();
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
