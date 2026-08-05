use crate::{analyze::StackAnalyzer, lower::Lowerer, syntax::AssemblyArena};
use zydeco_stackir::StackirArena;
use zydeco_statics::tyck::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::syntax::SpanArena};
use zydeco_utils::pass::CompilerPass;

/// Lower Stack IR and establish the stack-layout invariants required by backends.
pub struct LoweringPipeline<'a> {
    spans: &'a SpanArena,
    scoped: &'a ScopedArena,
    statics: &'a StaticsArena,
    stackir: &'a StackirArena,
}

impl<'a> LoweringPipeline<'a> {
    pub fn new(
        spans: &'a SpanArena, scoped: &'a ScopedArena, statics: &'a StaticsArena,
        stackir: &'a StackirArena,
    ) -> Self {
        Self { spans, scoped, statics, stackir }
    }

    pub fn run(self) -> AssemblyArena {
        let mut assembly = Lowerer::new(self.spans, self.scoped, self.statics, self.stackir).run();
        match StackAnalyzer::new(&mut assembly).run() {
            | Ok(_) => assembly,
            | Err(never) => match never {},
        }
    }
}
