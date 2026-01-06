//! Lower from [`zydeco_stackir::StackArena`] to [`AssemblyArena`].
//!
//! - All global variables and all values are
//!   compiled into programs that pushes the value onto the stack.
//! - All computations and stacks are compiled into programs.

use super::{arena::AssemblyArena, syntax::*};
use derive_more::{AsMut, AsRef};
use zydeco_stackir::{arena::StackArena, syntax as sk};
use zydeco_statics::tyck::{arena::StaticsArena, syntax as ss};
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_utils::arena::ArcGlobalAlloc;

pub trait Lower {
    type Kont;
    type Out;
    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out;
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
        for (name, builtin) in self.stack.builtins.iter() {
            if builtin.sort == sk::BuiltinSort::Function {
                self.arena.externs.push(name);
            }
        }

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
                |kont, (def_id, body)| {
                    // wrap continuation with lowering logic
                    let def_id = def_id;
                    Box::new(move |lo: &mut Lowerer| {
                        body.lower(
                            lo,
                            Box::new(move |lo| {
                                let name = lo.scoped.defs[&def_id].clone();
                                let var = VarName::from(name).build(lo, Some(def_id));
                                Pop(var).build(lo, kont)
                            }),
                        )
                    })
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

impl Lower for sk::VPatId {
    type Kont = Box<dyn FnOnce(&mut Lowerer) -> ProgId>;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let vpat = lo.stack.vpats[self].clone();
        use sk::ValuePattern as VPat;
        match vpat {
            | VPat::Hole(Hole) => {
                let var = VarName::from("_").build(lo, None);
                Pop(var).build(lo, kont)
            }
            | VPat::Var(def_id) => {
                // Pop the value from the stack into the variable
                let name = lo.scoped.defs[&def_id].clone();
                let var = VarName::from(name).build(lo, Some(def_id));
                Pop(var).build(lo, kont)
            }
            | VPat::Ctor(Ctor(ctor, param)) => {
                let vpat_data = *self;
                // Unpack the pair value
                Unpack(ProductMarker).build(
                    lo,
                    Box::new(move |lo: &mut Lowerer| {
                        // Compile the remaining pattern
                        let res = param.lower(lo, kont);
                        // Push a tag and see if the constructor is the same
                        let idx = lo.find_ctor_tag_idx_from_vpat(vpat_data, &ctor);
                        let name = ctor.plain().to_string();
                        let tag = Tag { idx, name: Some(name) };
                        Push(tag).build(
                            lo,
                            Box::new(move |lo: &mut Lowerer| {
                                // Compare the tag with the constructor
                                EqJump(res).build(lo, ())
                            }),
                        )
                    }),
                )
            }
            | VPat::Triv(Triv) => {
                // Pop and do nothing
                let var = VarName::from("_").build(lo, None);
                Pop(var).build(lo, kont)
            }
            | VPat::VCons(Cons(a, b)) => {
                // Unpack the pair value from the stack, and then process a and b
                Unpack(ProductMarker).build(
                    lo,
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
            | Value::Hole(Hole) => Panic.build(lo, ()),
            | Value::Var(def_id) => {
                // Retrieve the variable from the context
                let _name = lo.scoped.defs[&def_id].clone();
                // log::trace!("lowering var: {}{}", _name.plain(), def_id.concise());
                let atom = match lo.arena.defs.forth(&def_id).clone() {
                    | DefId::Var(var_id) => Atom::Var(var_id),
                    | DefId::Sym(sym_id) => Atom::Sym(sym_id),
                };
                // Push the atom onto the stack
                Push(atom).build(lo, kont)
            }
            | Value::Closure(sk::Closure { capture, stack: sk::Bullet, body }) => {
                assert!(capture.iter().count() == 0, "Capture is not empty");
                let body = body.lower(lo, ());
                // Label the closure code
                let sym = body.build(lo, (Some(String::from("clo")), None));
                // Push the atom to the stack
                let atom = Atom::Sym(sym);
                Push(atom).build(lo, kont)
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
                        Push(tag).build(
                            lo,
                            Box::new(move |lo: &mut Lowerer| {
                                // Pack them into a pair value
                                Pack(ProductMarker).build(lo, kont)
                            }),
                        )
                    }),
                )
            }
            | Value::Triv(Triv) => {
                // Push the trivial value onto the stack
                let atom = Atom::Imm(Imm::Triv(Triv));
                Push(atom).build(lo, kont)
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
                                Pack(ProductMarker).build(lo, kont)
                            }),
                        )
                    }),
                )
            }
            | Value::Literal(Literal::Int(i)) => {
                // Push the literal value onto the stack
                let atom = Atom::Imm(Imm::Int(i));
                Push(atom).build(lo, kont)
            }
            | Value::Literal(Literal::Char(c)) => {
                // Push the literal value onto the stack
                let atom = Atom::Imm(Imm::Char(c));
                Push(atom).build(lo, kont)
            }
            | Value::Literal(Literal::String(s)) => {
                // Push the literal value onto the stack
                let atom = Atom::Sym(s.build(lo, (Some(String::from("")), None)));
                Push(atom).build(lo, kont)
            }
            | Value::Complex(sk::Complex { operator, operands }) => {
                // Lower all operands onto the stack
                let arity = operands.len();
                let kont: Box<dyn FnOnce(&mut Lowerer) -> ProgId> =
                    Box::new(move |lo| Intrinsic { name: operator, arity }.build(lo, kont));
                let kont = operands.into_iter().rev().fold(
                    kont,
                    |kont: Box<dyn FnOnce(&mut Lowerer) -> ProgId>, operand: sk::ValueId| {
                        Box::new(move |lo| operand.lower(lo, kont))
                    },
                );
                kont(lo)
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
                let code = Swap.build(
                    lo,
                    Box::new(move |lo: &mut Lowerer| {
                        Pop(ContextMarker).build(
                            lo,
                            Box::new(move |lo: &mut Lowerer| {
                                binder.lower(lo, Box::new(move |lo| body.lower(lo, ())))
                            }),
                        )
                    }),
                );
                let sym = code.build(lo, (Some(String::from("kont")), None));
                // Push the context pointer, and then the code
                Push(ContextMarker).build(
                    lo,
                    Box::new(move |lo: &mut Lowerer| {
                        Push(Atom::Sym(sym)).build(
                            lo,
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
                        Push(tag).build(lo, kont)
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
            | Compu::Hole(Hole) => Panic.build(lo, ()),
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
                                PopJump.build(lo, ())
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
                                LeapJump.build(lo, ())
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
                // Label the body code; using trivial value as a placeholder
                let name = lo.scoped.defs[&param].plain();
                let sym = Undefined.build(lo, (Some(name.to_string()), Some(param)));
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
                Jump(body_prog).build(lo, ())
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
                                        let _sym =
                                            tail_prog.build(lo, (Some(String::from("arm")), None));
                                        // Add to the jump table
                                        lowered_arms.push((tag, tail_prog));
                                    }
                                    | _ => {
                                        panic!("Inrefutable pattern matcher must be unique in ZASM")
                                    }
                                }
                            }
                            // Unpack the value
                            Unpack(ProductMarker).build(
                                lo,
                                Box::new(move |lo: &mut Lowerer| {
                                    // Jump table
                                    PopBranch(lowered_arms).build(lo, ())
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
                    let _sym = tail_prog.build(lo, (Some(String::from("coarm")), None));
                    // Add to the jump table
                    lowered_arms.push((tag, tail_prog));
                }
                // Create the co-case program
                PopBranch(lowered_arms).build(lo, ())
            }
            | Compu::ExternCall(sk::ExternCall { function, stack: sk::Bullet }) => {
                Extern { name: function, arity: 0 }.build(lo, ())
            }
        }
    }
}
