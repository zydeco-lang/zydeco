use super::syntax::*;
use zydeco_utils::prelude::CoContext;

/// Collect bound variables from patterns.
pub trait Vars {
    fn vars(self, arena: &impl AsRef<StackirArena>) -> Context<DefId>;
}

impl Vars for VPatId {
    fn vars(self, arena: &impl AsRef<StackirArena>) -> Context<DefId> {
        let vpat = arena.as_ref().inner.vpats[&self].clone();
        use ValuePattern as VPat;
        match vpat {
            | VPat::Hole(Hole) => Context::new(),
            | VPat::Var(def_id) => Context::singleton(def_id),
            | VPat::Ctor(Ctor(_ctor, body)) => body.vars(arena),
            | VPat::Triv(Triv) => Context::new(),
            | VPat::VCons(Cons(a, b)) => a.vars(arena) + b.vars(arena),
        }
    }
}

/// Collect free variables from stack IR nodes.
pub trait FreeVars {
    fn free_vars(self, arena: &impl AsRef<StackirArena>) -> CoContext<DefId>;
}

impl FreeVars for ValueId {
    fn free_vars(self, arena: &impl AsRef<StackirArena>) -> CoContext<DefId> {
        let value = arena.as_ref().inner.values[&self].clone();
        match value {
            | Value::Var(def_id) => CoContext::singleton(def_id),
            | Value::Closure(Closure { capture: _, stack: Bullet, body }) => body.free_vars(arena),
            | Value::Ctor(Ctor(_ctor, body)) => body.free_vars(arena),
            | Value::VCons(Cons(a, b)) => a.free_vars(arena) + b.free_vars(arena),
            | Value::Complex(Complex { operator: _, operands }) => operands
                .into_iter()
                .map(|operand| operand.free_vars(arena))
                .fold(CoContext::new(), |acc, x| acc + x),
            | Value::Hole(Hole) | Value::Triv(Triv) | Value::Literal(_) => CoContext::new(),
        }
    }
}

impl FreeVars for StackId {
    fn free_vars(self, arena: &impl AsRef<StackirArena>) -> CoContext<DefId> {
        let stack = arena.as_ref().inner.stacks[&self].clone();
        match stack {
            | Stack::Kont(Kont { binder, body }) => body.free_vars(arena) - binder.vars(arena),
            | Stack::Var(Bullet) => CoContext::new(),
            | Stack::Arg(Cons(a, b)) => a.free_vars(arena) + b.free_vars(arena),
            | Stack::Tag(Cons(_dtor, body)) => body.free_vars(arena),
        }
    }
}

impl FreeVars for CompuId {
    fn free_vars(self, arena: &impl AsRef<StackirArena>) -> CoContext<DefId> {
        let compu = arena.as_ref().inner.compus[&self].clone();
        use Computation as Compu;
        match compu {
            | Compu::Hole(Hole) => CoContext::new(),
            | Compu::Force(SForce { thunk, stack }) => {
                thunk.free_vars(arena) + stack.free_vars(arena)
            }
            | Compu::Ret(SReturn { stack, value }) => {
                stack.free_vars(arena) + value.free_vars(arena)
            }
            | Compu::Fix(SFix { capture: _, param, body }) => {
                body.free_vars(arena) - Context::singleton(param)
            }
            | Compu::Case(Match { scrut, arms }) => {
                scrut.free_vars(arena)
                    + arms
                        .into_iter()
                        .map(|Matcher { binder, tail }| tail.free_vars(arena) - binder.vars(arena))
                        .fold(CoContext::new(), |acc, x| acc + x)
            }
            | Compu::Join(LetJoin::Value(Let { binder, bindee, tail })) => {
                tail.free_vars(arena) - binder.vars(arena) + bindee.free_vars(arena)
            }
            | Compu::Join(LetJoin::Stack(Let { binder: Bullet, bindee, tail })) => {
                bindee.free_vars(arena) + tail.free_vars(arena)
            }
            | Compu::LetArg(Let { binder: Cons(param, Bullet), bindee, tail }) => {
                tail.free_vars(arena) - param.vars(arena) + bindee.free_vars(arena)
            }
            | Compu::CoCase(CoMatch { arms }) => arms
                .into_iter()
                .map(|CoMatcher { dtor: _, tail }| tail.free_vars(arena))
                .fold(CoContext::new(), |acc, x| acc + x),
            | Compu::ExternCall(ExternCall { function: _, stack: Bullet }) => CoContext::new(),
        }
    }
}
