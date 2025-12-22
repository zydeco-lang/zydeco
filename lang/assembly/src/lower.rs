//! Lower from [`zydeco_stack::StackArena`] to [`AssemblyArena`].
//!
//! - All global variables and all values are
//!   compiled into programs that pushes the value onto the stack.
//! - All computations and stacks are compiled into programs.

use super::arena::{AssemblyArena, AssemblyArenaMutLike};
use super::syntax::*;
use zydeco_stack::arena::StackArena;
use zydeco_stack::syntax as sk;
use zydeco_statics::tyck::arena::StaticsArena;
use zydeco_statics::tyck::syntax as ss;
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_utils::arena::ArcGlobalAlloc;

pub trait Lower {
    type Kont;
    type Out;
    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out;
}

pub struct Lowerer<'a> {
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
        // Lower entry points from StackArena to AssemblyArena
        for (compu_id, ()) in &self.stack.entry {
            let compu_id = *compu_id;

            // All globals are statically known, so compile them as symbols.
            // Collect globals with their def_ids for unified processing
            let globals: Vec<_> = (self.stack.sequence.clone().iter())
                .rev()
                .map(|&def_id| (def_id, self.stack.globals[&def_id].clone()))
                .collect();

            // Build a continuation that initializes all globals in order
            let kont: Box<dyn FnOnce(&mut Lowerer) -> ProgId> = globals.into_iter().fold(
                Box::new(move |lo: &mut Lowerer| {
                    // After all globals are initialized, lower the entry point
                    compu_id.lower(lo, ())
                }),
                |kont, (def_id, global)| {
                    match global {
                        | sk::Global::Extern(sk::Extern) => {
                            // Extern: wrap continuation to create symbol, then continue
                            Box::new(move |lo: &mut Lowerer| {
                                let name = lo.scoped.defs[&def_id].clone();
                                // log::trace!(
                                //     "lowered extern: {}{} => {}",
                                //     name.plain(),
                                //     def_id.concise(),
                                //     _sym.concise()
                                // );
                                // let _sym = lo.sym(Some(def_id), name.plain(), Extern);
                                // kont(lo)
                                // Don't make it a symbol. Instead, just make it a variable.
                                let var = lo.arena.var(Some(def_id), name);
                                lo.arena.externs.insert(var, ());
                                lo.instr(Pop(var), kont)
                            })
                        }
                        | sk::Global::Defined(body) => {
                            // Defined: wrap continuation with lowering logic
                            let def_id = def_id;
                            Box::new(move |lo: &mut Lowerer| {
                                body.lower(
                                    lo,
                                    Box::new(move |lo| {
                                        let name = lo.scoped.defs[&def_id].clone();
                                        let var = lo.arena.var(Some(def_id), name);
                                        lo.instr(Pop(var), kont)
                                    }),
                                )
                            })
                        }
                    }
                },
            );

            // Execute the initialization chain
            let whole = kont(&mut self);
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
                            ty = self.statics.seals[&abst].clone();
                        }
                        | Fillable::Done(Type::Abs(Abs(_, body))) => {
                            ty = body;
                        }
                        | Fillable::Done(Type::App(App(f, _))) => {
                            ty = f;
                        }
                        | Fillable::Fill(fill) => {
                            let AnnId::Type(ty_) = self.statics.solus[&fill].clone() else {
                                let (span_str, ty_str) = err_msg();
                                unreachable!(
                                    "Computation type {} is not a codata type in statics{}",
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
        let ty = self.statics.annotations_value[value].clone();
        self.find_ctor_tag_idx(ty, ctor_name)
    }

    /// Find the constructor tag index in a data type, given a stack value pattern
    fn find_ctor_tag_idx_from_vpat(
        &self, vpat_id: sk::VPatId, ctor_name: &zydeco_syntax::CtorName,
    ) -> usize {
        // Get the corresponding statics pattern
        let ss::PatId::Value(vpat) =
            self.stack.pats.back(&vpat_id).expect("Constructor pattern not found")
        else {
            unreachable!("Constructor is not a value pattern in statics")
        };
        // Get the type annotation
        let ty = self.statics.annotations_vpat[&vpat].clone();
        self.find_ctor_tag_idx(ty, ctor_name)
    }

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
                        ty = self.statics.seals[&abst].clone();
                    }
                    | Fillable::Done(Type::Abs(Abs(_, body))) => {
                        ty = body;
                    }
                    | Fillable::Done(Type::App(App(f, _))) => {
                        ty = f;
                    }
                    | Fillable::Fill(fill) => {
                        let AnnId::Type(ty_) = self.statics.solus[&fill].clone() else {
                            let (span_str, ty_str) = err_msg();
                            unreachable!(
                                "Computation type {} is not a codata type in statics{}",
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

impl AsRef<AssemblyArena> for Lowerer<'_> {
    fn as_ref(&self) -> &AssemblyArena {
        &self.arena
    }
}
impl AsMut<AssemblyArena> for Lowerer<'_> {
    fn as_mut(&mut self) -> &mut AssemblyArena {
        &mut self.arena
    }
}

impl Lower for sk::VPatId {
    type Kont = Box<dyn FnOnce(&mut Lowerer) -> ProgId>;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let vpat = lo.stack.vpats[self].clone();
        use sk::ValuePattern as VPat;
        match vpat {
            | VPat::Hole(Hole) => lo.prog_anon(Panic),
            | VPat::Var(def_id) => {
                // Pop the value from the stack into the variable
                let name = lo.scoped.defs[&def_id].clone();
                let var = lo.arena.var(Some(def_id), name);
                lo.instr(Pop(var), kont)
            }
            | VPat::Ctor(Ctor(ctor, param)) => {
                let vpat_data = *self;
                // Unpack the pair value
                lo.instr(
                    Unpack(ProductMarker),
                    Box::new(move |lo: &mut Lowerer| {
                        // Compile the remaining pattern
                        let res = param.lower(lo, kont);
                        // Push a tag and see if the constructor is the same
                        let idx = lo.find_ctor_tag_idx_from_vpat(vpat_data, &ctor);
                        let name = ctor.plain().to_string();
                        let tag = Tag { idx, name: Some(name) };
                        lo.instr(
                            Push(tag),
                            Box::new(move |lo: &mut Lowerer| {
                                // Compare the tag with the constructor
                                lo.prog_anon(EqJump(res))
                            }),
                        )
                    }),
                )
            }
            | VPat::Triv(Triv) => {
                // Pop and do nothing
                let var = lo.arena.var(None, VarName::from("_"));
                lo.instr(Pop(var), kont)
            }
            | VPat::VCons(Cons(a, b)) => {
                // Unpack the pair value from the stack, and then process a and b
                lo.instr(
                    Unpack(ProductMarker),
                    Box::new(move |lo: &mut Lowerer| {
                        a.lower(lo, Box::new(move |lo| b.lower(lo, kont)))
                    }),
                )
            }
        }
    }
}

/// Values are compiled into programs that push the value onto the stack.
impl Lower for sk::ValueId {
    type Kont = Box<dyn FnOnce(&mut Lowerer) -> ProgId>;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let value = lo.stack.values[self].clone();
        use sk::Value;
        match value {
            | Value::Hole(Hole) => lo.prog_anon(Panic),
            | Value::Var(def_id) => {
                // Retrieve the variable from the context
                let _name = lo.scoped.defs[&def_id].clone();
                // log::trace!("lowering var: {}{}", _name.plain(), def_id.concise());
                let atom = match lo.arena.defs.forth(&def_id).clone() {
                    | DefId::Var(var_id) => Atom::Var(var_id),
                    | DefId::Sym(sym_id) => Atom::Sym(sym_id),
                };
                // Push the atom onto the stack
                lo.instr(Push(atom), kont)
            }
            | Value::Clo(sk::Clo { capture, stack: sk::Bullet, body }) => {
                assert!(capture.iter().count() == 0, "Capture is not empty");
                let body = body.lower(lo, ());
                // Label the closure code
                let sym = lo.sym(None, "clo", body);
                // Push the atom to the stack
                let atom = Atom::Sym(sym);
                lo.instr(Push(atom), kont)
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                // Push the body onto the stack
                let value_data = *self;
                body.lower(
                    lo,
                    Box::new(move |lo| {
                        // Push the constructor tag onto the stack
                        let idx = lo.find_ctor_tag_idx_from_value(value_data, &ctor);
                        let name = ctor.plain().to_string();
                        let tag = Tag { idx, name: Some(name) };
                        lo.instr(
                            Push(tag),
                            Box::new(move |lo: &mut Lowerer| {
                                // Pack them into a pair value
                                lo.instr(Pack(ProductMarker), kont)
                            }),
                        )
                    }),
                )
            }
            | Value::Triv(Triv) => {
                // Push the trivial value onto the stack
                let atom = Atom::Sym(lo.arena.sym(None, "", Triv));
                lo.instr(Push(atom), kont)
            }
            | Value::VCons(Cons(a, b)) => {
                // Push b and then a onto the stack
                b.lower(
                    lo,
                    Box::new(move |lo| {
                        a.lower(
                            lo,
                            Box::new(move |lo| {
                                // and then pack the pair value
                                lo.instr(Pack(ProductMarker), kont)
                            }),
                        )
                    }),
                )
            }
            | Value::Lit(lit) => {
                // Push the literal value onto the stack
                let atom = Atom::Sym(lo.arena.sym(None, "", lit));
                lo.instr(Push(atom), kont)
            }
        }
    }
}

impl Lower for sk::StackId {
    /// Stacks in ZIR are compiled to instructions
    type Kont = Box<dyn FnOnce(&mut Lowerer) -> ProgId>;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let stack = lo.stack.stacks[self].clone();
        use sk::Stack;
        match stack {
            | Stack::Kont(sk::Kont { binder, body }) => {
                // Lower the continuation code into a symbol
                // which is `swap; pop ctx; [[binder]]; [[body]]`
                // The stack shape: [return value, context]
                let code = lo.instr(
                    Swap,
                    Box::new(move |lo: &mut Lowerer| {
                        lo.instr(
                            Pop(ContextMarker),
                            Box::new(move |lo: &mut Lowerer| {
                                binder.lower(lo, Box::new(move |lo| body.lower(lo, ())))
                            }),
                        )
                    }),
                );
                let sym = lo.arena.sym(None, "kont", code);
                // Push the context pointer, and then the code
                lo.instr(
                    Push(ContextMarker),
                    Box::new(move |lo: &mut Lowerer| {
                        lo.instr(
                            Push(Atom::Sym(sym)),
                            Box::new(move |lo: &mut Lowerer| {
                                // Finally, we do the rest
                                kont(lo)
                            }),
                        )
                    }),
                )
            }
            | Stack::Var(sk::Bullet) => {
                // Do nothing
                kont(lo)
            }
            | Stack::Arg(Cons(value, stack)) => {
                // Finish the stack first
                stack.lower(
                    lo,
                    Box::new(move |lo| {
                        // Push the value onto the stack
                        value.lower(lo, kont)
                    }),
                )
            }
            | Stack::Tag(Cons(dtor, stack)) => {
                // Finish the stack first
                let stack_codata = stack;
                stack.lower(
                    lo,
                    Box::new(move |lo| {
                        // Push the destructor tag onto the stack
                        let idx = lo.find_dtor_tag_idx_from_stack(stack_codata, &dtor);
                        let name = dtor.plain().to_string();
                        let tag = Tag { idx, name: Some(name) };
                        lo.instr(Push(tag), kont)
                    }),
                )
            }
        }
    }
}

impl Lower for sk::CompuId {
    type Kont = ();
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let compu = lo.stack.compus[self].clone();
        use sk::Computation as Compu;
        match compu {
            | Compu::Hole(Hole) => lo.prog_anon(Panic),
            | Compu::Force(sk::SForce { thunk, stack }) => {
                // Lower the stack first
                stack.lower(
                    lo,
                    Box::new(move |lo| {
                        // Lower the thunk to a value on the stack
                        thunk.lower(
                            lo,
                            Box::new(move |lo| {
                                // PopJump to the thunk
                                lo.prog_anon(PopJump)
                            }),
                        )
                    }),
                )
            }
            | Compu::Ret(sk::SReturn { stack, value }) => {
                // Lower the stack first
                stack.lower(
                    lo,
                    Box::new(move |lo| {
                        // Lower the value
                        value.lower(
                            lo,
                            Box::new(move |lo| {
                                // The stack shape: [return value, return address, context]
                                // Leap jump to the continuation
                                lo.prog_anon(LeapJump)
                            }),
                        )
                    }),
                )
            }
            | Compu::Fix(sk::SFix { capture, param, body }) => {
                assert!(
                    capture.iter().count() == 0,
                    "ZASM requires empty capture. Please perform closure conversion first."
                );
                // Label the body code
                let name = lo.scoped.defs[&param].plain();
                let sym = lo.arena.sym(Some(param), name, Extern);
                // Lower the body
                let body_prog = body.lower(lo, ());
                // Nominate the body program
                *lo.arena.blocks.entry(body_prog).or_insert(0) += 2;
                lo.arena.symbols.replace(
                    sym,
                    Symbol { name: name.to_string(), inner: SymbolInner::Prog(body_prog) },
                );
                lo.arena.labels.insert(body_prog, sym);
                // Jump to the body
                lo.prog_anon(Jump(body_prog))
            }
            | Compu::Case(Match { scrut, arms }) => {
                let value_data = scrut;
                // Lower the scrutinee
                scrut.lower(
                    lo,
                    Box::new(move |lo: &mut Lowerer| {
                        if arms.len() == 1 {
                            let Matcher { binder, tail } = arms[0];
                            // Basically same as let value
                            binder.lower(
                                lo,
                                Box::new(move |lo| {
                                    // Lower the tail
                                    tail.lower(lo, kont)
                                }),
                            )
                        } else {
                            // Optimization: compile to a jump table
                            let mut lowered_arms = Vec::new();
                            for Matcher { binder, tail } in arms {
                                // The binder is a constructor or other things.
                                use sk::ValuePattern as VPat;
                                match lo.stack.vpats[&binder].clone() {
                                    | VPat::Ctor(Ctor(ctor, _)) => {
                                        let idx =
                                            lo.find_ctor_tag_idx_from_value(value_data, &ctor);
                                        let name = ctor.plain().to_string();
                                        let tag = Tag { idx, name: Some(name) };
                                        // Lower the tail
                                        let tail_prog = tail.lower(lo, ());
                                        // Nominate the tail program
                                        let _sym = lo.arena.sym(None, "arm", tail_prog);
                                        // Add to the jump table
                                        lowered_arms.push((tag, tail_prog));
                                    }
                                    | _ => {
                                        panic!("Inrefutable pattern matcher must be unique in ZASM")
                                    }
                                }
                            }
                            // Unpack the value
                            lo.instr(
                                Unpack(ProductMarker),
                                Box::new(move |lo: &mut Lowerer| {
                                    // Jump table
                                    lo.prog_anon(PopBranch(lowered_arms))
                                }),
                            )
                        }
                    }),
                )
            }
            | Compu::LetValue(Let { binder, bindee, tail }) => {
                // Lower the bindee
                bindee.lower(
                    lo,
                    Box::new(move |lo| {
                        // Lower the binder
                        binder.lower(
                            lo,
                            Box::new(move |lo| {
                                // Lower the tail
                                tail.lower(lo, kont)
                            }),
                        )
                    }),
                )
            }
            | Compu::LetStack(Let { binder: sk::Bullet, bindee, tail }) => {
                // Lower the bindee
                bindee.lower(
                    lo,
                    Box::new(move |lo| {
                        // Lower the tail
                        tail.lower(lo, kont)
                    }),
                )
            }
            | Compu::LetArg(Let { binder: Cons(param, sk::Bullet), bindee, tail }) => {
                // Lower the bindee
                bindee.lower(
                    lo,
                    Box::new(move |lo| {
                        // Lower the param
                        param.lower(
                            lo,
                            Box::new(move |lo| {
                                // Lower the tail
                                tail.lower(lo, kont)
                            }),
                        )
                    }),
                )
            }
            | Compu::CoCase(CoMatch { arms }) => {
                let mut lowered_arms = Vec::new();
                let compu_id = *self;
                for CoMatcher { dtor: Cons(dtor, sk::Bullet), tail } in arms {
                    // Lower the tail
                    let tail_prog = tail.lower(lo, ());
                    let idx = lo.find_dtor_tag_idx_from_compu(compu_id, &dtor);
                    let name = dtor.plain().to_string();
                    let tag = Tag { idx, name: Some(name) };
                    // Nominate the tail program
                    let _sym = lo.arena.sym(None, "coarm", tail_prog);
                    // Add to the jump table
                    lowered_arms.push((tag, tail_prog));
                }
                // Create the co-case program
                lo.prog_anon(PopBranch(lowered_arms))
            }
        }
    }
}
