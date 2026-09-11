use crate::representation::{Local, RepresentationPolicy};
use crate::{analyze::StackAnalyzer, lower::Lowerer, syntax::AssemblyProgram};
use zydeco_stackir::SpsLowProgram;
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::syntax::SpanArena};

/// Lower Stack IR and establish the stack-layout invariants required by backends.
pub struct LoweringPipeline<'a, P = Local> {
    spans: &'a SpanArena,
    scoped: &'a ScopedArena,
    statics: &'a StaticsArena,
    sps_low: &'a SpsLowProgram,
    policy: P,
}

impl<'a> LoweringPipeline<'a> {
    pub fn new(
        spans: &'a SpanArena, scoped: &'a ScopedArena, statics: &'a StaticsArena,
        sps_low: &'a SpsLowProgram,
    ) -> Self {
        Self { spans, scoped, statics, sps_low, policy: Local }
    }
}

impl<'a, P: RepresentationPolicy> LoweringPipeline<'a, P> {
    pub fn with_representation<Q: RepresentationPolicy>(
        self, policy: Q,
    ) -> LoweringPipeline<'a, Q> {
        LoweringPipeline {
            spans: self.spans,
            scoped: self.scoped,
            statics: self.statics,
            sps_low: self.sps_low,
            policy,
        }
    }

    pub fn run(self) -> AssemblyProgram {
        let mut assembly =
            Lowerer::with_policy(self.spans, self.scoped, self.statics, self.sps_low, &self.policy)
                .run();
        StackAnalyzer::new(&mut assembly).run();
        assembly.finish()
    }

    pub fn run_native(self) -> Result<crate::frames::NativeProgram, crate::frames::FramePlanError> {
        let mut assembly =
            Lowerer::with_policy(self.spans, self.scoped, self.statics, self.sps_low, &self.policy)
                .with_native_frames()
                .run();
        StackAnalyzer::new(&mut assembly).run();
        crate::frames::NativeProgram::prepare(assembly)
    }
}
