use crate::{
    analyze::StackAnalyzer,
    arena::AssemblyBuild,
    frames::{FramePlanError, NativeProgram},
    lower::Lowerer,
    syntax::AssemblyProgram,
};
use std::convert::Infallible;
use zydeco_foundation::{pass::CompilerPass, pipeline};
use zydeco_stackir::{
    SpsLowProgram,
    low::representation::{Local, RepresentationPolicy},
};

/// Lower Stack IR and establish the stack-layout invariants required by backends.
pub struct LoweringPipeline<P = Local> {
    policy: P,
}

impl LoweringPipeline {
    pub fn new() -> Self {
        Self { policy: Local }
    }
}

impl Default for LoweringPipeline {
    fn default() -> Self {
        Self::new()
    }
}

impl<P: RepresentationPolicy> LoweringPipeline<P> {
    pub fn with_representation<Q: RepresentationPolicy>(self, policy: Q) -> LoweringPipeline<Q> {
        LoweringPipeline { policy }
    }

    /// Select native lowering and checked frame preparation as one reusable pass.
    pub fn with_native_frames(
        self,
    ) -> impl for<'input> CompilerPass<
        &'input SpsLowProgram,
        Output = NativeProgram,
        Error = FramePlanError,
    > {
        let lower = move |program: &SpsLowProgram| {
            Ok::<_, FramePlanError>(
                Lowerer::with_policy(program, &self.policy).with_native_frames().run(),
            )
        };
        pipeline![lower, AnalyzeStack.with_error(), NativeProgram::prepare]
    }
}

impl<P: RepresentationPolicy> CompilerPass<&SpsLowProgram> for LoweringPipeline<P> {
    type Output = AssemblyProgram;
    type Error = Infallible;

    fn run(&mut self, program: &SpsLowProgram) -> Result<Self::Output, Self::Error> {
        let lower = |program: &SpsLowProgram| {
            Ok::<_, Infallible>(Lowerer::with_policy(program, &self.policy).run())
        };
        let finish = |assembly: AssemblyBuild| Ok(assembly.finish());
        pipeline![lower, AnalyzeStack, finish].run(program)
    }
}

struct AnalyzeStack;

impl CompilerPass<AssemblyBuild> for AnalyzeStack {
    type Output = AssemblyBuild;
    type Error = Infallible;

    fn run(&mut self, mut assembly: AssemblyBuild) -> Result<Self::Output, Self::Error> {
        StackAnalyzer::new(&mut assembly).run();
        Ok(assembly)
    }
}
