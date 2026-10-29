//! Lexical SPS instances of the shared variable summaries.

use super::syntax::*;
pub use crate::variables::{FreeVariables, FreeVars, Vars};

pub type Variables = crate::variables::Variables<Sps>;

impl FreeVariables<Sps> for ValueExt {
    fn free_variables(&self, facts: &Variables) -> CoContext<DefId> {
        match self {
            | Self::Closure(Closure { body, .. }) => facts.compu_summary(*body),
        }
    }
}

impl FreeVariables<Sps> for StackExt {
    fn free_variables(&self, facts: &Variables) -> CoContext<DefId> {
        match self {
            | Self::Kont(Kont { binder, body }) => {
                facts.compu_summary(*body) - facts.pattern_summary(*binder)
            }
        }
    }
}

impl FreeVariables<Sps> for CompuExt {
    fn free_variables(&self, facts: &Variables) -> CoContext<DefId> {
        match self {
            | Self::Force(SForce { thunk, stack }) => {
                facts.value_summary(*thunk) + facts.stack_summary(*stack)
            }
            | Self::Ret(SReturn { stack, value }) => {
                facts.stack_summary(*stack) + facts.value_summary(*value)
            }
            | Self::Fix(SFix { param, stack, body }) => {
                facts.stack_summary(*stack)
                    + (facts.compu_summary(*body) - Context::singleton(*param))
            }
            | Self::LetProc(LetProc { procedure, body, tail, .. }) => {
                facts.compu_summary(*body)
                    + (facts.compu_summary(*tail) - Context::singleton(*procedure))
            }
            | Self::CallProc(CallProc { procedure, stack }) => {
                CoContext::singleton(*procedure) + facts.stack_summary(*stack)
            }
        }
    }
}

impl Vars<Sps> for VPatId {
    fn vars(self, arena: &impl AsRef<SpsInnerArena>) -> Context<DefId> {
        Variables::analyze(arena.as_ref(), self.into()).pattern_summary(self)
    }
}

impl FreeVars<Sps> for ValueId {
    fn free_vars(self, arena: &impl AsRef<SpsInnerArena>) -> CoContext<DefId> {
        Variables::analyze(arena.as_ref(), self.into()).value_summary(self)
    }
}

impl FreeVars<Sps> for StackId {
    fn free_vars(self, arena: &impl AsRef<SpsInnerArena>) -> CoContext<DefId> {
        Variables::analyze(arena.as_ref(), self.into()).stack_summary(self)
    }
}

impl FreeVars<Sps> for CompuId {
    fn free_vars(self, arena: &impl AsRef<SpsInnerArena>) -> CoContext<DefId> {
        Variables::analyze(arena.as_ref(), self.into()).compu_summary(self)
    }
}
