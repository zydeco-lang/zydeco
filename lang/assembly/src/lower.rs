//! Lower from [`zydeco_stackir::low::SpsLowArena`] to [`AssemblyArena`].
//!
//! - All global variables and all values are
//!   compiled into programs that pushes the value onto the stack.
//! - All computations and stacks are compiled into programs.

use super::{
    arena::{AssemblyArena, AssemblyBuild, CxKont, Kont},
    syntax::*,
};
use derive_more::{AsMut, AsRef};
use std::collections::HashMap;
use zydeco_machine::closure::Closure;
use zydeco_stackir::{SpsLowProgram, arena::DefinitionNames as _, low::syntax as sk};
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_utils::with::With;

#[cfg(test)]
mod tests;

pub trait Lower<'a> {
    type Kont;
    type Out;
    fn lower(&self, lo: &mut Lowerer<'a>, kont: Self::Kont) -> Self::Out;
}

struct PendingInstruction<'a> {
    id: ProgId,
    instruction: Instruction,
    context: Context,
    continuation: CxKont<'a, Lowerer<'a>>,
}

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
    native_frames: bool,
}

impl<'a> Lowerer<'a> {
    pub fn new(
        spans: &'a SpanArena, scoped: &'a ScopedArena, statics: &'a StaticsArena,
        sps_low: &'a SpsLowProgram,
    ) -> Self {
        Self::with_policy(spans, scoped, statics, sps_low, &crate::representation::Local)
    }

    pub fn with_policy<P: crate::representation::RepresentationPolicy + ?Sized>(
        spans: &'a SpanArena, scoped: &'a ScopedArena, statics: &'a StaticsArena,
        sps_low: &'a SpsLowProgram, policy: &P,
    ) -> Self {
        let arena = AssemblyArena::default();
        let unboxing = crate::unbox::LocalUnboxing::with_policy(sps_low, policy);
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
            native_frames: false,
        }
    }

    pub(crate) fn with_native_frames(mut self) -> Self {
        self.native_frames = true;
        self
    }

    pub(crate) fn run(mut self) -> AssemblyBuild {
        let sps_low_root = self.root;
        let root = sps_low_root.lower(&mut self, Context::new());
        if self.native_frames {
            self.arena.frame_entries.insert(root, crate::frames::Entry::Fresh);
        }
        self.finish_pending();
        AssemblyBuild { arena: self.arena, root }
    }

    fn finish_pending(&mut self) {
        while let Some(pending) = self.pending.pop() {
            let PendingInstruction { id, instruction, context, continuation } = pending;
            let next_context = continuation.update.apply(&context);
            let next = (continuation.kont)(self, next_context);
            self.arena.insert_program(id, Program::Instruction(instruction, next), context);
        }
    }
}

impl<'a, U> Construct<'a, Instruction, ProgId, Lowerer<'a>> for U
where
    U: Into<Instruction>,
{
    type Site = With<Context, CxKont<'a, Lowerer<'a>>>;

    fn build(
        self, lowerer: &mut Lowerer<'a>, With { info: context, inner: continuation }: Self::Site,
    ) -> ProgId {
        let id = lowerer.allocator.alloc();
        let instruction = self.into();
        lowerer.pending.push(PendingInstruction { id, instruction, context, continuation });
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
                let update = ContextUpdate::Bind(var);
                Pop(var).build(lo, With::new(cx, CxKont { update, kont }))
            }
            | VPat::Var(def_id) => {
                if let Some(&arity) = lo.unboxing.unboxed_vars.get(&def_id) {
                    let name = lo.sps_low.admin.def_name(lo.scoped, lo.statics, &def_id).clone();
                    let vars: Vec<VarId> = (0..arity)
                        .map(|index| {
                            VarName::from(format!("{}#unbox{}", name.plain(), index))
                                .build(lo, None)
                        })
                        .collect();
                    lo.unboxed_var_slots.insert(def_id, vars.clone());
                    let kont = vars.iter().rev().fold(kont, |kont, &var| {
                        let update = ContextUpdate::Bind(var);
                        Box::new(move |lo, cx| {
                            Pop(var).build(lo, With::new(cx, CxKont { update, kont }))
                        })
                    });
                    kont(lo, cx)
                } else {
                    // Pop the value from the stack into the variable
                    let name = lo.sps_low.admin.def_name(lo.scoped, lo.statics, &def_id).clone();
                    let var = name.build(lo, Some(def_id));
                    let update = ContextUpdate::Bind(var);
                    Pop(var).build(lo, With::new(cx, CxKont { update, kont }))
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
                let update = ContextUpdate::Bind(alias);
                Pop(alias).build(lo, With::new(cx, CxKont { update, kont }))
            }
            | VPat::Triv(Triv) => {
                let var = VarName::from("_").build(lo, None);
                let update = ContextUpdate::Bind(var);
                Pop(var).build(lo, With::new(cx, CxKont { update, kont }))
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
            | Value::Block(sk::Block { label, entry, body }) => {
                let name =
                    lo.sps_low.admin.def_name(lo.scoped, lo.statics, &label).plain().to_string();
                let sym = Undefined.build(lo, (Some(name.clone()), Some(label)));
                let body: Kont<'a, Lowerer<'a>> = Box::new(move |lo, cx| body.lower(lo, cx));
                let body = entry.words().rev().fold(body, |next, (_, pattern)| {
                    Box::new(move |lo, cx| pattern.lower(lo, With::new(cx, next)))
                })(lo, Context::new());
                if lo.native_frames {
                    lo.arena.frame_entries.insert(body, crate::frames::Entry::Fresh);
                }
                lo.arena
                    .symbols
                    .replace_existing(sym, NamedSymbol { name, inner: Symbol::Prog(body) });
                lo.arena.labels.insert_new(body, sym);
                Push(Atom::Sym(sym)).build(lo, With::new(cx, CxKont::same(kont)))
            }
            | Value::ClosurePackage(sk::ClosurePackage { environment, code }) => {
                let kont: Kont<'a, Lowerer<'a>> = if lo.unboxing.values.contains(self) {
                    kont
                } else {
                    let product = ProductLayout::new(Closure::<u64>::WORDS, Closure::<u64>::WORDS);
                    Box::new(move |lo, cx| {
                        Pack(product).build(lo, With::new(cx, CxKont::same(kont)))
                    })
                };
                Closure { environment, code }.into_words().into_iter().fold(
                    kont,
                    |kont: Kont<'a, Lowerer<'a>>, value| {
                        Box::new(move |lo, cx| value.lower(lo, With::new(cx, kont)))
                    },
                )(lo, cx)
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                let product = ProductLayout::new(2, 2);
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
                                        Pack(product).build(lo, With::new(cx, CxKont::same(kont)))
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
                let kont: Kont<'a, Lowerer<'a>> = if lo.unboxing.values.contains(self) {
                    kont
                } else {
                    let product = ProductLayout::new(layout.arity, items.len());
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
            | Value::Primitive(sk::Primitive { operation, operands }) => {
                // Lower all operands onto the stack
                let kont: Kont<'_, Lowerer<'_>> =
                    Box::new(move |lo, cx| operation.build(lo, With::new(cx, CxKont::same(kont))));
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
        if lo.native_frames
            && let Some(entry) = lo.sps_low.inner.continuations.get(self).cloned()
        {
            let sk::Stack::ContinuationPackage(sk::ContinuationPackage { code, .. }) = stack else {
                unreachable!()
            };
            let sk::Value::Block(sk::Block { label, .. }) = lo.sps_low.inner.values[&code] else {
                unreachable!()
            };
            let bindings = entry
                .captures
                .into_iter()
                .map(|capture| {
                    let DefId::Var(source) = lo.arena.defs[&capture.source] else {
                        panic!("capture must have an activation slot")
                    };
                    let name =
                        lo.sps_low.admin.def_name(lo.scoped, lo.statics, &capture.binding).clone();
                    let binding = name.build(lo, Some(capture.binding));
                    (binding, source)
                })
                .collect::<Vec<_>>();
            let entry_context = bindings.iter().map(|(binding, _)| *binding).collect();
            let body = entry.body;
            let resume = entry
                .result
                .lower(lo, With::new(entry_context, Box::new(move |lo, cx| body.lower(lo, cx))));
            let name = lo.sps_low.admin.def_name(lo.scoped, lo.statics, &label).plain().to_string();
            let symbol = resume.build(lo, (Some(name), Some(label)));
            let captures = bindings.iter().map(|(_, source)| *source).collect();
            lo.arena.frame_entries.insert(resume, crate::frames::Entry::Resume { bindings });
            return RetainFrame { entry: resume, captures }.build(
                lo,
                With::new(
                    cx,
                    CxKont::same(Box::new(move |lo, cx| {
                        Push(Atom::Sym(symbol)).build(lo, With::new(cx, CxKont::same(kont)))
                    })),
                ),
            );
        }
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
            | Compu::Jump(sk::Jump { target, argument, stack }) => stack.lower(
                lo,
                With::new(
                    cx,
                    Box::new(move |lo, cx| {
                        argument.word().1.lower(
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
                            // Partial-pattern fallthrough is an empty match, including for
                            // scalar scrutinees. It has no constructor payload to unpack.
                            if arms.is_empty() {
                                return Abort.build(lo, cx);
                            }
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
            | Compu::LetValue(sk::LetValue { binder, bindee, tail: body }) => {
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
            | Compu::LetStack(sk::LetStack { binder: sk::Bullet, bindee, tail: body }) => {
                // Lower the bindee
                bindee.lower(lo, With::new(cx, Box::new(move |lo, cx| body.lower(lo, cx))))
            }
            | Compu::LetArg(sk::LetArg {
                binder: sk::Cons(param, sk::Bullet),
                bindee,
                tail: body,
            }) => {
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
                            let kont: Kont<'a, Lowerer<'a>> =
                                Box::new(move |lo, cx| body.lower(lo, cx));
                            let kont = Closure { environment, code }
                                .into_words()
                                .into_iter()
                                .rev()
                                .fold(kont, |kont: Kont<'a, Lowerer<'a>>, pattern| {
                                    Box::new(move |lo, cx| pattern.lower(lo, With::new(cx, kont)))
                                });
                            if unboxed {
                                kont(lo, cx)
                            } else {
                                Unpack(ProductLayout::new(
                                    Closure::<u64>::WORDS,
                                    Closure::<u64>::WORDS,
                                ))
                                .build(lo, With::new(cx, CxKont::same(kont)))
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
                let external = match function {
                    | sk::ExternalFunction::Host(role) => Extern::Host {
                        role,
                        name: role.host_name(),
                        arity: role.arity(),
                        mode: sk::HostCallMode::for_role(role).into(),
                    },
                    | sk::ExternalFunction::Foreign(import) => Extern::Foreign(import),
                };
                if !lo.arena.externs.contains(&external) {
                    lo.arena.externs.push(external.clone());
                }
                stack.lower(lo, With::new(cx, Box::new(move |lo, cx| external.build(lo, cx))))
            }
        }
    }
}

impl From<sk::HostCallMode> for ExternMode {
    fn from(mode: sk::HostCallMode) -> Self {
        match mode {
            | sk::HostCallMode::Returning => Self::Returning,
            | sk::HostCallMode::Control => Self::Control,
        }
    }
}
