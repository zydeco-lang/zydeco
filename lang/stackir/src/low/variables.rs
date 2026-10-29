//! First-order SPSLow instances of the shared variable summaries.

use super::syntax::*;
pub use crate::variables::{FreeVariables, FreeVars, Vars};

pub type Variables = crate::variables::Variables<SpsLow>;

impl FreeVariables<SpsLow> for ValueExt {
    fn free_variables(&self, facts: &Variables) -> CoContext<DefId> {
        match self {
            | Self::Block(Block { label, entry, body }) => entry.words().fold(
                facts.compu_summary(*body) - Context::singleton(*label),
                |free, (_, pattern)| free - facts.pattern_summary(pattern),
            ),
            | Self::ClosurePackage(ClosurePackage { environment, code }) => {
                facts.value_summary(*environment) + facts.value_summary(*code)
            }
        }
    }
}

impl FreeVariables<SpsLow> for StackExt {
    fn free_variables(&self, facts: &Variables) -> CoContext<DefId> {
        match self {
            | Self::ContinuationPackage(ContinuationPackage { code, residual }) => {
                facts.value_summary(*code) + facts.stack_summary(*residual)
            }
        }
    }
}

impl FreeVariables<SpsLow> for CompuExt {
    fn free_variables(&self, facts: &Variables) -> CoContext<DefId> {
        match self {
            | Self::Jump(Jump { target, argument, stack }) => {
                facts.value_summary(*target)
                    + facts.value_summary(argument.word().1)
                    + facts.stack_summary(*stack)
            }
            // Labels are static code, not variables.
            | Self::LetLabel(LetLabel { body, tail, .. }) => {
                facts.compu_summary(*body) + facts.compu_summary(*tail)
            }
            | Self::JumpLabel(JumpLabel { stack, .. }) => facts.stack_summary(*stack),
            | Self::OpenClosure(OpenClosure { package, environment, code, body }) => {
                facts.value_summary(*package)
                    + (facts.compu_summary(*body)
                        - facts.pattern_summary(*environment)
                        - facts.pattern_summary(*code))
            }
            | Self::OpenContinuation(OpenContinuation { package, code, body }) => {
                facts.stack_summary(*package)
                    + (facts.compu_summary(*body) - facts.pattern_summary(*code))
            }
        }
    }
}

impl Vars<SpsLow> for VPatId {
    fn vars(self, arena: &impl AsRef<SpsLowInnerArena>) -> Context<DefId> {
        Variables::analyze(arena.as_ref(), self.into()).pattern_summary(self)
    }
}

impl FreeVars<SpsLow> for ValueId {
    fn free_vars(self, arena: &impl AsRef<SpsLowInnerArena>) -> CoContext<DefId> {
        Variables::analyze(arena.as_ref(), self.into()).value_summary(self)
    }
}

impl FreeVars<SpsLow> for StackId {
    fn free_vars(self, arena: &impl AsRef<SpsLowInnerArena>) -> CoContext<DefId> {
        Variables::analyze(arena.as_ref(), self.into()).stack_summary(self)
    }
}

impl FreeVars<SpsLow> for CompuId {
    fn free_vars(self, arena: &impl AsRef<SpsLowInnerArena>) -> CoContext<DefId> {
        Variables::analyze(arena.as_ref(), self.into()).compu_summary(self)
    }
}
