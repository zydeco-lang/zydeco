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
    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out;
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
                        &mut self,
                        Box::new(move |atom, lo| {
                            // The value is already pushed onto the stack by value.lower's instructions
                            // Return the value (the atom represents what's on the stack)
                            lo.arena.prog_anon(Program::Return(Return(atom)))
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
            let prog_id = compu_id.lower(&mut self, ());
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
    type Kont = Box<dyn FnOnce(&mut Lowerer) -> ProgId>;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let vpat = lo.statics.vpats[self].clone();
        use zydeco_statics::tyck::syntax::ValuePattern as VPat;
        match vpat {
            | VPat::Hole(Hole) => todo!(),
            | VPat::Var(def) => todo!(),
            | VPat::Ctor(Ctor(_ctor, _body)) => todo!(),
            | VPat::Triv(Triv) => todo!(),
            | VPat::VCons(Cons(a, b)) => todo!(),
            | VPat::TCons(Cons(_ty, inner)) => todo!(),
        }
    }
}

impl Lower for sk::ValueId {
    type Kont = Box<dyn FnOnce(Atom, &mut Lowerer) -> ProgId>;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let value = lo.stack.values[self].clone();
        use sk::Value;
        match value {
            | Value::Hole(Hole) => todo!(),
            | Value::Var(def) => {
                match lo.stack.globals.get(&def) {
                    | Some(sk::Global::Extern(_)) => todo!(),
                    | Some(sk::Global::Defined(value_id)) => todo!(),
                    | None => todo!(),
                }
            }
            | Value::Clo(sk::Clo { capture: _, stack: _, body }) => todo!(),
            | Value::Ctor(Ctor(ctor, body)) => todo!(),
            | Value::Triv(Triv) => todo!(),
            | Value::VCons(Cons(a, b)) => todo!(),
            | Value::Lit(lit) => todo!(),
        }
    }
}

impl Lower for sk::StackId {
    type Kont = Box<dyn FnOnce(&mut Lowerer) -> ProgId>;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let stack = lo.stack.stacks[self].clone();
        use sk::Stack;
        match stack {
            | Stack::Kont(sk::Kont { binder, body }) => todo!(),
            | Stack::Var(sk::Bullet) => todo!(),
            | Stack::Arg(Cons(value, rest)) => todo!(),
            | Stack::Tag(Cons(dtor, rest)) => todo!(),
        }
    }
}

impl Lower for sk::CompuId {
    type Kont = ();
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer, _kont: Self::Kont) -> Self::Out {
        let compu = lo.stack.compus[self].clone();
        use sk::Computation as Compu;
        match compu {
            | Compu::Hole(Hole) => todo!(),
            | Compu::Fix(Fix(param, body)) => todo!(),
            | Compu::Force(sk::SForce { thunk, stack }) => todo!(),
            | Compu::Ret(sk::SReturn { stack, value }) => todo!(),
            | Compu::Case(Match { scrut, arms }) => todo!(),
            | Compu::LetValue(Let { binder, bindee, tail }) => todo!(),
            | Compu::LetStack(Let { binder: _, bindee, tail }) => todo!(),
            | Compu::LetArg(Let { binder: Cons(vpat, _bullet), bindee, tail }) => todo!(),
            | Compu::CoCase(CoMatch { arms }) => todo!(),
        }
    }
}
