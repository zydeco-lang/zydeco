use super::syntax::*;
use zydeco_assembly::arena::AssemblyArena;
use zydeco_stack::arena::StackArena;
use zydeco_statics::tyck::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};

pub trait Emit {}

pub struct Emitter<'e> {
    pub spans: &'e SpanArena,
    pub scoped: &'e ScopedArena,
    pub statics: &'e StaticsArena,
    pub stack: &'e StackArena,
    pub assembly: &'e AssemblyArena,
    pub instrs: Vec<Instr>,
}

impl<'e> Emitter<'e> {
    pub fn new(
        spans: &'e SpanArena, scoped: &'e ScopedArena, statics: &'e StaticsArena,
        stack: &'e StackArena, assembly: &'e AssemblyArena,
    ) -> Self {
        Self { spans, scoped, statics, stack, assembly, instrs: Vec::new() }
    }
    pub fn run(self) -> Vec<Instr> {
        let _ = self;
        self.instrs
    }
}
