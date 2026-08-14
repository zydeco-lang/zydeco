//! Bound- and free-variable analysis for first-order SPS.

use super::syntax::*;
use zydeco_utils::context::{CoContext, Context};

pub trait Vars {
    fn vars(self, arena: &SpsLowInnerArena) -> Context<DefId>;
}

impl Vars for VPatId {
    fn vars(self, arena: &SpsLowInnerArena) -> Context<DefId> {
        match arena.vpats[&self].clone() {
            | ValuePattern::Hole(Hole) | ValuePattern::Triv(Triv) => Context::new(),
            | ValuePattern::Var(def) => Context::singleton(def),
            | ValuePattern::Ctor(Ctor(_, body)) => body.vars(arena),
            | ValuePattern::Alias(Alias(patterns)) => patterns
                .into_iter()
                .map(|pattern| pattern.vars(arena))
                .fold(Context::new(), |vars, pattern| vars + pattern),
            | ValuePattern::VCons(VCons { items, layout: _ }) => items
                .into_iter()
                .map(|item| item.vars(arena))
                .fold(Context::new(), |vars, item| vars + item),
        }
    }
}

pub trait FreeVars {
    fn free_vars(self, arena: &SpsLowInnerArena) -> CoContext<DefId>;
}

impl FreeVars for ValueId {
    fn free_vars(self, arena: &SpsLowInnerArena) -> CoContext<DefId> {
        match arena.values[&self].clone() {
            | Value::Var(def) => CoContext::singleton(def),
            | Value::Block(Block { label, body }) => {
                body.free_vars(arena) - Context::singleton(label)
            }
            | Value::ClosurePackage(ClosurePackage { environment, code }) => {
                environment.free_vars(arena) + code.free_vars(arena)
            }
            | Value::Ctor(Ctor(_, body)) => body.free_vars(arena),
            | Value::VCons(VCons { items, layout: _ }) => items
                .into_iter()
                .map(|item| item.free_vars(arena))
                .fold(CoContext::new(), |vars, item| vars + item),
            | Value::Complex(Complex { operator: _, operands }) => operands
                .into_iter()
                .map(|operand| operand.free_vars(arena))
                .fold(CoContext::new(), |vars, operand| vars + operand),
            | Value::Hole(Hole) | Value::Triv(Triv) | Value::Literal(_) => CoContext::new(),
        }
    }
}

impl FreeVars for StackId {
    fn free_vars(self, arena: &SpsLowInnerArena) -> CoContext<DefId> {
        match arena.stacks[&self].clone() {
            | Stack::Var(Bullet) => CoContext::new(),
            | Stack::Arg(Cons(value, stack)) => value.free_vars(arena) + stack.free_vars(arena),
            | Stack::Tag(Cons(_, stack)) => stack.free_vars(arena),
            | Stack::ContinuationPackage(ContinuationPackage { code, residual }) => {
                code.free_vars(arena) + residual.free_vars(arena)
            }
        }
    }
}

impl FreeVars for CompuId {
    fn free_vars(self, arena: &SpsLowInnerArena) -> CoContext<DefId> {
        match arena.compus[&self].clone() {
            | Computation::Hole(SHole(stack)) => stack.free_vars(arena),
            | Computation::Jump(Jump { target, stack }) => {
                target.free_vars(arena) + stack.free_vars(arena)
            }
            | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => {
                scrut.free_vars(arena) + (body.free_vars(arena) - binder.vars(arena))
            }
            | Computation::CoprodMatch(SCoprodMatch { scrut, arms }) => {
                scrut.free_vars(arena)
                    + arms
                        .into_iter()
                        .map(|Matcher { binder, tail }| tail.free_vars(arena) - binder.vars(arena))
                        .fold(CoContext::new(), |vars, arm| vars + arm)
            }
            | Computation::LetValue(LetValue { binder, bindee, body }) => {
                bindee.free_vars(arena) + (body.free_vars(arena) - binder.vars(arena))
            }
            | Computation::LetStack(LetStack { bindee, body }) => {
                bindee.free_vars(arena) + body.free_vars(arena)
            }
            | Computation::LetArg(LetArg { binder, bindee, body }) => {
                bindee.free_vars(arena) + (body.free_vars(arena) - binder.vars(arena))
            }
            | Computation::CoCase(SCoMatch { scrut, arms }) => {
                scrut.free_vars(arena)
                    + arms
                        .into_iter()
                        .map(|CoMatcher { dtor: _, tail }| tail.free_vars(arena))
                        .fold(CoContext::new(), |vars, arm| vars + arm)
            }
            | Computation::OpenClosure(OpenClosure { package, environment, code, body }) => {
                package.free_vars(arena)
                    + (body.free_vars(arena) - environment.vars(arena) - code.vars(arena))
            }
            | Computation::OpenContinuation(OpenContinuation { package, code, body }) => {
                package.free_vars(arena) + (body.free_vars(arena) - code.vars(arena))
            }
            | Computation::ExternCall(ExternCall { function: _, stack }) => stack.free_vars(arena),
        }
    }
}
