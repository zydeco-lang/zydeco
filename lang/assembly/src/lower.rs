//! Lower from [`zydeco_stack::StackArena`] to [`AssemblyArena`].
//!
//! - All global variables and all values are
//!   compiled into programs that pushes the value onto the stack.
//! - All computations and stacks are compiled into programs.

use super::arena::{AssemblyArena, AssemblyArenaLike};
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
            // For each entry point, we initialize the globals and then lower the computation.
            // All globals are statically known, so compile them as symbols.
            let seq = self.stack.global_seq.clone();
            let res = seq.into_iter().rev().fold(todo!(), |acc, def_id| todo!());
            self.arena.entry.insert(res, ());
        }
        self.arena
    }

    /// Find the constructor tag index in a data type, given a value
    fn find_ctor_tag_idx(
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
        let ty = self.statics.annotations_value[&value].clone();
        // Get the data type
        let ss::Fillable::Done(ss::Type::Data(data)) = self.statics.types[&ty].clone() else {
            unreachable!("Constructor type is not a data type in statics")
        };
        // Find the index of the constructor tag
        self.statics.datas[&data]
            .iter()
            .position(|(tag_branch, _ty)| tag_branch == ctor_name)
            .expect("Constructor tag not found")
    }

    /// Get the codata type from a computation's type annotation
    fn find_dtor_tag_idx(&self, compu: ss::CompuId, dtor_name: &zydeco_syntax::DtorName) -> usize {
        // Get the type annotation
        let ty = self.statics.annotations_compu[&compu].clone();
        // Get the codata type
        let ss::Fillable::Done(ss::Type::CoData(codata)) = self.statics.types[&ty].clone() else {
            unreachable!("Computation type is not a codata type in statics")
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

impl AsMut<AssemblyArena> for Lowerer<'_> {
    fn as_mut(&mut self) -> &mut AssemblyArena {
        &mut self.arena
    }
}

impl Lower for sk::VPatId {
    type Kont = Box<dyn FnOnce(VarId, &mut Lowerer) -> ProgId>;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let vpat = lo.stack.vpats[self].clone();
        use sk::ValuePattern as VPat;
        todo!()
    }
}

/// Values are compiled into programs that push the value onto the stack.
impl Lower for sk::ValueId {
    type Kont = Box<dyn FnOnce(&mut Lowerer) -> ProgId>;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let value = lo.stack.values[self].clone();
        use sk::Value;
        todo!()
    }
}

impl Lower for sk::StackId {
    /// Stacks in ZIR are compiled to instructions
    type Kont = Box<dyn FnOnce(&mut Lowerer) -> ProgId>;
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let stack = lo.stack.stacks[self].clone();
        use sk::Stack;
        todo!()
    }
}

impl Lower for sk::CompuId {
    type Kont = ();
    type Out = ProgId;

    fn lower(&self, lo: &mut Lowerer, _kont: Self::Kont) -> Self::Out {
        let compu = lo.stack.compus[self].clone();
        use sk::Computation as Compu;
        todo!()
    }
}
