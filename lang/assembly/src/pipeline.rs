use crate::representation::{Local, RepresentationPolicy};
use crate::{
    analyze::StackAnalyzer,
    arena::AssemblyBuild,
    frames::{FramePlanError, NativeProgram},
    lower::Lowerer,
    syntax::AssemblyProgram,
};
use std::convert::Infallible;
use std::marker::PhantomData;
use zydeco_stackir::SpsLowProgram;
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::syntax::SpanArena};
use zydeco_utils::{
    fold::{Driver, Explicit},
    pass::CompilerPass,
    pipeline,
};

/// Lower Stack IR and establish the stack-layout invariants required by backends.
pub struct LoweringPipeline<'a, P = Local, D = Explicit> {
    spans: &'a SpanArena,
    scoped: &'a ScopedArena,
    statics: &'a StaticsArena,
    policy: P,
    driver: PhantomData<D>,
}

impl<'a> LoweringPipeline<'a> {
    pub fn new(spans: &'a SpanArena, scoped: &'a ScopedArena, statics: &'a StaticsArena) -> Self {
        Self { spans, scoped, statics, policy: Local, driver: PhantomData }
    }
}

impl<'a, P: RepresentationPolicy, D: Driver> LoweringPipeline<'a, P, D> {
    pub fn with_representation<Q: RepresentationPolicy>(
        self, policy: Q,
    ) -> LoweringPipeline<'a, Q, D> {
        LoweringPipeline {
            spans: self.spans,
            scoped: self.scoped,
            statics: self.statics,
            policy,
            driver: PhantomData,
        }
    }

    /// Select continuation storage for CPS lowering, including native entry construction.
    pub fn with_driver<E: Driver>(self) -> LoweringPipeline<'a, P, E> {
        LoweringPipeline {
            spans: self.spans,
            scoped: self.scoped,
            statics: self.statics,
            policy: self.policy,
            driver: PhantomData,
        }
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
                Lowerer::with_policy(self.spans, self.scoped, self.statics, program, &self.policy)
                    .with_native_frames()
                    .run_with_driver::<D>(),
            )
        };
        pipeline![lower, AnalyzeStack.with_error(), NativeProgram::prepare]
    }
}

impl<P: RepresentationPolicy, D: Driver> CompilerPass<&SpsLowProgram>
    for LoweringPipeline<'_, P, D>
{
    type Output = AssemblyProgram;
    type Error = Infallible;

    fn run(&mut self, program: &SpsLowProgram) -> Result<Self::Output, Self::Error> {
        let lower = |program: &SpsLowProgram| {
            Ok::<_, Infallible>(
                Lowerer::with_policy(self.spans, self.scoped, self.statics, program, &self.policy)
                    .run_with_driver::<D>(),
            )
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
