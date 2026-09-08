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
        match StackAnalyzer::new(&mut assembly).run() {
            | Ok(_) => assembly.finish(),
            | Err(never) => match never {},
        }
    }

    pub fn run_native(self) -> Result<crate::frames::NativeProgram, crate::frames::FramePlanError> {
        let mut assembly = Lowerer::new(self.spans, self.scoped, self.statics, self.sps_low)
            .with_native_frames()
            .run();
        match StackAnalyzer::new(&mut assembly).run() {
            | Ok(_) => crate::frames::NativeProgram::prepare(assembly),
            | Err(never) => match never {},
        }
    }
}
