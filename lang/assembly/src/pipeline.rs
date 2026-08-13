use crate::{analyze::StackAnalyzer, lower::Lowerer, syntax::AssemblyProgram};
use zydeco_stackir::StackirProgram;
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::syntax::SpanArena};
use zydeco_utils::pass::CompilerPass;

/// Lower Stack IR and establish the stack-layout invariants required by backends.
pub struct LoweringPipeline<'a> {
    spans: &'a SpanArena,
    scoped: &'a ScopedArena,
    statics: &'a StaticsArena,
    stackir: &'a StackirProgram,
}

impl<'a> LoweringPipeline<'a> {
    pub fn new(
        spans: &'a SpanArena, scoped: &'a ScopedArena, statics: &'a StaticsArena,
        stackir: &'a StackirProgram,
    ) -> Self {
        Self { spans, scoped, statics, stackir }
    }

    pub fn run(self) -> AssemblyProgram {
        let mut assembly = Lowerer::new(self.spans, self.scoped, self.statics, self.stackir).run();
        match StackAnalyzer::new(&mut assembly).run() {
            | Ok(_) => assembly,
            | Err(never) => match never {},
        }
    }
}
