//! Lower from [`zydeco_stackir::sps_low::SpsLowArena`] to [`AssemblyArena`].
//!
//! - All global variables and all values are
//!   compiled into programs that pushes the value onto the stack.
//! - All computations and stacks are compiled into programs.

use super::{
    arena::{AssemblyArena, CxKont, Kont},
    syntax::*,
};
use derive_more::{AsMut, AsRef};
use std::collections::HashMap;
use zydeco_stackir::{SpsLowProgram, sps_low::syntax as sk};
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_utils::with::With;

pub trait Lower<'a> {
    type Kont;
    type Out;
    fn lower(&self, lo: &mut Lowerer<'a>, kont: Self::Kont) -> Self::Out;
}

type PendingInstruction<'a> = Box<dyn FnOnce(&mut Lowerer<'a>) + 'a>;

#[derive(AsRef, AsMut)]
pub struct Lowerer<'a> {
    /// Sequential issuer scoped to this lowering run.
    #[as_mut(IdAllocator<AssemblyScope>)]
    allocator: IdAllocator<AssemblyScope>,
    #[as_ref]
    #[as_mut]
    pub arena: AssemblyArena,
    pub spans: &'a SpanArena,
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
    pub sps_low: &'a sk::SpsLowArena,
    pub root: sk::CompuId,
    unboxing: crate::unbox::LocalUnboxing,
    unboxed_var_slots: HashMap<sk::DefId, Vec<VarId>>,
    pending: Vec<PendingInstruction<'a>>,
}

impl<'a> Lowerer<'a> {
    pub fn new(
        spans: &'a SpanArena, scoped: &'a ScopedArena, statics: &'a StaticsArena,
        sps_low: &'a SpsLowProgram,
    ) -> Self {
        let arena = AssemblyArena::default();
        let unboxing = if std::env::var_os("ZYDECO_DISABLE_UNBOXING").is_some() {
            crate::unbox::LocalUnboxing::default()
        } else {
            crate::unbox::LocalUnboxing::collect(sps_low)
        };
        Self {
            allocator: IdAllocator::new(),
            arena,
            spans,
            scoped,
            statics,
            sps_low: sps_low.arena(),
            root: sps_low.root(),
            unboxing,
            unboxed_var_slots: HashMap::new(),
            pending: Vec::new(),
        }
    }

    pub fn run(mut self) -> AssemblyProgram {
        // Lower all builtins
        for builtin in self.sps_low.admin.builtins.values() {
            let sk::Builtin { name, arity, sort } = builtin.clone();
            if let Some(mode) = ExternMode::for_builtin(sort) {
                self.arena.externs.push(Extern { name, arity, mode });
            }
        }

        let sps_low_root = self.root;
        let root = sps_low_root.lower(&mut self, Context::new());
        self.finish_pending();
        AssemblyProgram {
            arena: self.arena,
            root,
            layouts: ArenaAssoc::new(),
            slots: ArenaSparse::new(),
        }
    }

    /// GC class of the runtime value produced by one SPSLow value.
    fn value_field_class(&self, value: sk::ValueId) -> FieldClass {
        match &self.sps_low.inner.values[&value] {
            | sk::Value::Hole(_) | sk::Value::Var(_) | sk::Value::Complex(_) => {
                FieldClass::MaybePointer
            }
            | sk::Value::Block(_) | sk::Value::Triv(_) | sk::Value::Literal(_) => {
                FieldClass::Scalar
            }
            | sk::Value::ClosurePackage(_) | sk::Value::Ctor(_) | sk::Value::VCons(_) => {
                FieldClass::HeapPointer
            }
        }
    }

    fn finish_pending(&mut self) {
        while let Some(pending) = self.pending.pop() {
            pending(self);
        }
    }
}

impl<'a, U> Construct<'a, Instruction, ProgId, Lowerer<'a>> for U
where
    U: Into<Instruction>,
{
    type Site = With<Context, CxKont<'a, Lowerer<'a>>>;

    fn build(
        self, lowerer: &mut Lowerer<'a>,
        With { info: context, inner: CxKont { incr, kont } }: Self::Site,
    ) -> ProgId {
        let id = lowerer.allocator.alloc();
        let instruction = self.into();
        lowerer.pending.push(Box::new(move |lowerer| {
            let next_context = incr(&context);
            let next = kont(lowerer, next_context);
            lowerer.arena.insert_program(id, Program::Instruction(instruction, next), context);
        }));
        id
    }
}

impl<'a> Lower<'a> for sk::VPatId {
    type Kont = With<Context, Kont<'a, Lowerer<'a>>>;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer<'a>, With { info: cx, inner: kont }: Self::Kont) -> Self::Out {
        let vpat = lo.sps_low.inner.vpats[self].clone();
        use sk::ValuePattern as VPat;
        match vpat {
            | VPat::Hole(Hole) => {
                let var = VarName::from("_").build(lo, None);
                let incr = Box::new(move |cx: &Context| cx.clone() + [var]);
                Pop(var).build(lo, With::new(cx, CxKont { incr, kont }))
            }
            | VPat::Var(def_id) => {
                if let Some(&arity) = lo.unboxing.unboxed_vars.get(&def_id) {
                    let name = lo.scoped.defs[&def_id].clone();
                    let vars: Vec<VarId> = (0..arity)
                        .map(|index| {
                            VarName::from(format!("{}#unbox{}", name.plain(), index))
                                .build(lo, None)
                        })
                        .collect();
                    lo.unboxed_var_slots.insert(def_id, vars.clone());
                    let kont = vars.iter().rev().fold(kont, |kont, &var| {
                        let incr = Box::new(move |cx: &Context| cx.clone() + [var]);
                        Box::new(move |lo, cx| {
                            Pop(var).build(lo, With::new(cx, CxKont { incr, kont }))
                        })
                    });
                    kont(lo, cx)
                } else {
                    // Pop the value from the stack into the variable
                    let name = lo.scoped.defs[&def_id].clone();
                    let var = name.build(lo, Some(def_id));
                    let incr = Box::new(move |cx: &Context| cx.clone() + [var]);
                    Pop(var).build(lo, With::new(cx, CxKont { incr, kont }))
                }
            }
            | VPat::Ctor(Ctor(ctor, param)) => {
                let _ = ctor;
                let _ = param;
                unreachable!("Ctor patterns should not directly appear in ZASM");
                // let vpat_data = *self;
                // // Unpack the pair value
                // Unpack(ProductLayout::new(2, 2)).build(
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
            | VPat::Alias(Alias(patterns)) => {
                let alias = VarName::from("__alias__").build(lo, None);
                let kont = patterns.into_iter().rev().fold(
                    kont,
                    |kont: Kont<'a, Lowerer<'a>>, pattern| {
                        Box::new(move |lo, cx| {
                            let pattern_kont: Kont<'a, Lowerer<'a>> =
                                Box::new(move |lo, cx| pattern.lower(lo, With::new(cx, kont)));
                            Push(Atom::Var(alias))
                                .build(lo, With::new(cx, CxKont::same(pattern_kont)))
                        })
                    },
                );
                let incr = Box::new(move |cx: &Context| cx.clone() + [alias]);
                Pop(alias).build(lo, With::new(cx, CxKont { incr, kont }))
            }
            | VPat::Triv(Triv) => {
                let var = VarName::from("_").build(lo, None);
                let incr = Box::new(move |cx: &Context| cx.clone() + [var]);
                Pop(var).build(lo, With::new(cx, CxKont { incr, kont }))
            }
            | VPat::VCons(sk::VCons { items, layout }) => {
                let element_len = items.len();
                let kont =
                    items.into_iter().rev().fold(kont, |kont: Kont<'a, Lowerer<'a>>, item| {
                        Box::new(move |lo, cx| item.lower(lo, With::new(cx, kont)))
                    });
                if lo.unboxing.patterns.contains(self) {
                    kont(lo, cx)
                } else {
                    let product = ProductLayout::new(layout.arity, element_len);
                    Unpack(product).build(lo, With::new(cx, CxKont::same(kont)))
                }
            }
        }
    }
}

/// Values are compiled into programs that push the value onto the stack.
impl<'a> Lower<'a> for sk::ValueId {
    type Kont = With<Context, Kont<'a, Lowerer<'a>>>;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer<'a>, With { info: cx, inner: kont }: Self::Kont) -> Self::Out {
        let value = lo.sps_low.inner.values[self].clone();
        use sk::Value;
        match value {
            | Value::Hole(Hole) => Abort.build(lo, cx),
            | Value::Var(def_id) => {
                if let Some(vars) = lo.unboxed_var_slots.get(&def_id) {
                    let kont = vars.iter().fold(kont, |kont, &var| {
                        Box::new(move |lo, cx| {
                            Push(Atom::Var(var)).build(lo, With::new(cx, CxKont::same(kont)))
                        })
                    });
                    kont(lo, cx)
                } else {
                    let atom = match lo.arena.defs.forth(&def_id).clone() {
                        | DefId::Var(var_id) => Atom::Var(var_id),
                        | DefId::Sym(sym_id) => Atom::Sym(sym_id),
                    };
                    // Push the atom onto the stack
                    Push(atom).build(lo, With::new(cx, CxKont::same(kont)))
                }
            }
            | Value::Block(sk::Block { label, body }) => {
                let name = lo.scoped.defs[&label].plain().to_string();
                let sym = Undefined.build(lo, (Some(name.clone()), Some(label)));
                let body = body.lower(lo, Context::new());
                lo.arena
                    .symbols
                    .replace_existing(sym, NamedSymbol { name, inner: Symbol::Prog(body) });
                lo.arena.labels.insert_new(body, sym);
                Push(Atom::Sym(sym)).build(lo, With::new(cx, CxKont::same(kont)))
            }
            | Value::ClosurePackage(sk::ClosurePackage { environment, code }) => {
                let stack_alloc = lo.unboxing.stack_values.contains(self);
                let kont: Kont<'a, Lowerer<'a>> = if lo.unboxing.values.contains(self) {
                    kont
                } else {
                    let mut product = ProductLayout::new_with_fields(
                        2,
                        2,
                        vec![FieldClass::HeapPointer, FieldClass::Scalar],
                    );
                    if stack_alloc {
                        product.stack_alloc = true;
                    }
                    Box::new(move |lo, cx| {
                        Pack(product).build(lo, With::new(cx, CxKont::same(kont)))
                    })
                };
                [environment, code].into_iter().fold(kont, |kont: Kont<'a, Lowerer<'a>>, value| {
                    Box::new(move |lo, cx| value.lower(lo, With::new(cx, kont)))
                })(lo, cx)
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                let body_class = lo.value_field_class(body);
                let product =
                    ProductLayout::new_with_fields(2, 2, vec![FieldClass::Scalar, body_class]);
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
                                        Pack(product.clone())
                                            .build(lo, With::new(cx, CxKont::same(kont)))
                                    })),
                                ),
                            )
                        }),
                    ),
                )
            }
            | Value::Triv(Triv) => {
                let atom = Atom::Imm(Imm::Triv(Triv));
                Push(atom).build(lo, With::new(cx, CxKont::same(kont)))
            }
            | Value::VCons(sk::VCons { items, layout }) => {
                let stack_alloc = lo.unboxing.stack_values.contains(self);
                let kont: Kont<'a, Lowerer<'a>> = if lo.unboxing.values.contains(self) {
                    kont
                } else {
                    let mut product =
                        ProductLayout::new_with_fields(layout.arity, items.len(), layout.fields);
                    if stack_alloc {
                        product.stack_alloc = true;
                    }
                    Box::new(move |lo, cx| {
                        Pack(product).build(lo, With::new(cx, CxKont::same(kont)))
                    })
                };
                let kont = items.into_iter().fold(kont, |kont: Kont<'a, Lowerer<'a>>, item| {
                    Box::new(move |lo, cx| item.lower(lo, With::new(cx, kont)))
                });
                kont(lo, cx)
            }
            | Value::Literal(Literal::Integer(i)) => {
                // Push the literal value onto the stack
                let atom = Atom::Imm(Imm::Integer(i));
                Push(atom).build(lo, With::new(cx, CxKont::same(kont)))
            }
            | Value::Literal(Literal::Float(value)) => {
                let atom = Atom::Imm(Imm::Float(value));
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
        let stack = lo.sps_low.inner.stacks[self].clone();
        use sk::Stack;
        match stack {
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
            | Stack::ContinuationPackage(sk::ContinuationPackage { code, residual }) => residual
                .lower(
                    lo,
                    With::new(cx, Box::new(move |lo, cx| code.lower(lo, With::new(cx, kont)))),
                ),
        }
    }
}

impl<'a> Lower<'a> for sk::CompuId {
    type Kont = Context;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer<'a>, cx: Self::Kont) -> Self::Out {
        let compu = lo.sps_low.inner.compus[self].clone();
        use sk::Computation as Compu;
        match compu {
            | Compu::Hole(sk::SHole(tail)) => {
                tail.lower(lo, With::new(cx, Box::new(move |lo, cx| Abort.build(lo, cx))))
            }
            | Compu::Jump(sk::Jump { target, stack }) => stack.lower(
                lo,
                With::new(
                    cx,
                    Box::new(move |lo, cx| {
                        target.lower(
                            lo,
                            With::new(
                                cx,
                                Box::new(move |lo, cx| {
                                    Alloc(ContextMarker).build(
                                        lo,
                                        With::new(
                                            cx,
                                            CxKont::clean(Box::new(move |lo, cx| {
                                                PopJump.build(lo, cx)
                                            })),
                                        ),
                                    )
                                }),
                            ),
                        )
                    }),
                ),
            ),
            | Compu::ProductMatch(sk::SProductMatch { scrut, binder, body }) => scrut.lower(
                lo,
                With::new(
                    cx,
                    Box::new(move |lo, cx| {
                        binder.lower(lo, With::new(cx, Box::new(move |lo, cx| body.lower(lo, cx))))
                    }),
                ),
            ),
            | Compu::CoprodMatch(sk::SCoprodMatch { scrut, arms }) => {
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
                                    match lo.sps_low.inner.vpats[binder].clone() {
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
                                    match lo.sps_low.inner.vpats[&binder].clone() {
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
                                Unpack(ProductLayout::new(2, 2)).build(
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
            | Compu::LetValue(sk::LetValue { binder, bindee, body }) => {
                // Lower the bindee
                bindee.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            // Lower the binder
                            binder.lower(
                                lo,
                                With::new(cx, Box::new(move |lo, cx| body.lower(lo, cx))),
                            )
                        }),
                    ),
                )
            }
            | Compu::LetStack(sk::LetStack { bindee, body }) => {
                // Lower the bindee
                bindee.lower(lo, With::new(cx, Box::new(move |lo, cx| body.lower(lo, cx))))
            }
            | Compu::LetArg(sk::LetArg { binder: param, bindee, body }) => {
                // Lower the bindee
                bindee.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            // Lower the param
                            param.lower(
                                lo,
                                With::new(cx, Box::new(move |lo, cx| body.lower(lo, cx))),
                            )
                        }),
                    ),
                )
            }
            | Compu::CoCase(sk::SCoMatch { scrut, arms }) => {
                scrut.lower(
                    lo,
                    With::new(
                        cx.clone(),
                        Box::new(move |lo, cx| {
                            let arms = arms
                                .into_iter()
                                .map(|CoMatcher { dtor: Cons(dtor, sk::Bullet), tail }| {
                                    // Lower the tail
                                    let tail_prog = tail.lower(lo, cx.clone());
                                    let idx = dtor.idx;
                                    let name = dtor.name.plain().to_string();
                                    let tag = Tag { idx, name: Some(name) };
                                    // Nominate the tail program
                                    let _sym =
                                        tail_prog.build(lo, (Some(String::from("coarm")), None));
                                    // Add to the jump table
                                    (tag, tail_prog)
                                })
                                .collect();
                            // Create the co-case program
                            PopBranch(arms).build(lo, cx)
                        }),
                    ),
                )
            }
            | Compu::OpenClosure(sk::OpenClosure { package, environment, code, body }) => {
                let unboxed = lo.unboxing.values.contains(&package);
                package.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            if unboxed {
                                environment.lower(
                                    lo,
                                    With::new(
                                        cx,
                                        Box::new(move |lo, cx| {
                                            code.lower(
                                                lo,
                                                With::new(
                                                    cx,
                                                    Box::new(move |lo, cx| body.lower(lo, cx)),
                                                ),
                                            )
                                        }),
                                    ),
                                )
                            } else {
                                Unpack(ProductLayout::new(2, 2)).build(
                                    lo,
                                    With::new(
                                        cx,
                                        CxKont::same(Box::new(move |lo, cx| {
                                            environment.lower(
                                                lo,
                                                With::new(
                                                    cx,
                                                    Box::new(move |lo, cx| {
                                                        code.lower(
                                                            lo,
                                                            With::new(
                                                                cx,
                                                                Box::new(move |lo, cx| {
                                                                    body.lower(lo, cx)
                                                                }),
                                                            ),
                                                        )
                                                    }),
                                                ),
                                            )
                                        })),
                                    ),
                                )
                            }
                        }),
                    ),
                )
            }
            | Compu::OpenContinuation(sk::OpenContinuation { package, code, body }) => package
                .lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            code.lower(
                                lo,
                                With::new(cx, Box::new(move |lo, cx| body.lower(lo, cx))),
                            )
                        }),
                    ),
                ),
            | Compu::ExternCall(sk::ExternCall { function, stack }) => {
                let builtin = &lo.sps_low.admin.builtins[&function];
                let arity = builtin.arity;
                let mode =
                    ExternMode::for_builtin(builtin.sort.clone()).expect("operator used as extern");
                stack.lower(
                    lo,
                    With::new(
                        cx,
                        Box::new(move |lo, cx| {
                            Extern { name: function, arity, mode }.build(lo, cx)
                        }),
                    ),
                )
            }
        }
    }
}

impl ExternMode {
    fn for_builtin(sort: sk::BuiltinSort) -> Option<Self> {
        match sort {
            | sk::BuiltinSort::Operator => None,
            | sk::BuiltinSort::Function(sk::HostCallMode::Returning) => Some(Self::Returning),
            | sk::BuiltinSort::Function(sk::HostCallMode::Control) => Some(Self::Control),
        }
    }
}
