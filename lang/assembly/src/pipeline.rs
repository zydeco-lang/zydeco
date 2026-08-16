use crate::{analyze::StackAnalyzer, lower::Lowerer, syntax::AssemblyProgram};
use zydeco_stackir::SpsLowProgram;
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::syntax::SpanArena};
use zydeco_utils::pass::CompilerPass;

/// Lower Stack IR and establish the stack-layout invariants required by backends.
pub struct LoweringPipeline<'a> {
    spans: &'a SpanArena,
    scoped: &'a ScopedArena,
    statics: &'a StaticsArena,
    sps_low: &'a SpsLowProgram,
}

impl<'a> LoweringPipeline<'a> {
    pub fn new(
        spans: &'a SpanArena, scoped: &'a ScopedArena, statics: &'a StaticsArena,
        sps_low: &'a SpsLowProgram,
    ) -> Self {
        Self { spans, scoped, statics, sps_low }
    }

    pub fn run(self) -> AssemblyProgram {
        let mut assembly = Lowerer::new(self.spans, self.scoped, self.statics, self.sps_low).run();
        // First pass performs stack-directed inlining, mutating the program
        // graph. The pre-inline layouts are therefore stale for root maps.
        match StackAnalyzer::new(&mut assembly).run() {
            | Ok(_) => {}
            | Err(never) => match never {},
        }
        // Second pass measures the final program graph that backends emit.
        match StackAnalyzer::new(&mut assembly).run() {
            | Ok(analysis) => {
                assembly.layouts = analysis.layouts;
                assembly.slots = analysis.slots;
                assembly
            }
            | Err(never) => match never {},
        }
    }
}
