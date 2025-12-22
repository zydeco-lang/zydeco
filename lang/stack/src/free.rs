use super::syntax::*;
use zydeco_utils::prelude::CoContext;

pub trait Vars {
    fn vars(self, arena: &impl AsRef<StackArena>) -> Context<DefId>;
}

impl Vars for VPatId {
    fn vars(self, arena: &impl AsRef<StackArena>) -> Context<DefId> {
        let vpat = arena.as_ref().vpats[&self].clone();
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

pub trait FreeVars {
    fn free_vars(self, arena: &impl AsRef<StackArena>) -> CoContext<DefId>;
}

impl FreeVars for ValueId {
    fn free_vars(self, arena: &impl AsRef<StackArena>) -> CoContext<DefId> {
        let value = arena.as_ref().values[&self].clone();
        match value {
            | Value::Var(def_id) => CoContext::singleton(def_id),
            | Value::Clo(Clo { capture: _, stack: Bullet, body }) => body.free_vars(arena),
            | Value::Ctor(Ctor(_ctor, body)) => body.free_vars(arena),
            | Value::VCons(Cons(a, b)) => a.free_vars(arena) + b.free_vars(arena),
            | Value::Hole(Hole) | Value::Triv(Triv) | Value::Lit(_) => CoContext::new(),
        }
    }
}

impl FreeVars for StackId {
    fn free_vars(self, arena: &impl AsRef<StackArena>) -> CoContext<DefId> {
        let stack = arena.as_ref().stacks[&self].clone();
        match stack {
            | Stack::Kont(Kont { binder, body }) => body.free_vars(arena) - binder.vars(arena),
            | Stack::Var(Bullet) => CoContext::new(),
            | Stack::Arg(Cons(a, b)) => a.free_vars(arena) + b.free_vars(arena),
            | Stack::Tag(Cons(_dtor, body)) => body.free_vars(arena),
        }
    }
}

impl FreeVars for CompuId {
    fn free_vars(self, arena: &impl AsRef<StackArena>) -> CoContext<DefId> {
        let compu = arena.as_ref().compus[&self].clone();
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
            | Compu::LetValue(Let { binder, bindee, tail }) => {
                tail.free_vars(arena) - binder.vars(arena) + bindee.free_vars(arena)
            }
            | Compu::LetStack(Let { binder: Bullet, bindee, tail }) => {
                bindee.free_vars(arena) + tail.free_vars(arena)
            }
            | Compu::LetArg(Let { binder: Cons(param, Bullet), bindee, tail }) => {
                tail.free_vars(arena) - param.vars(arena) + bindee.free_vars(arena)
            }
            | Compu::CoCase(CoMatch { arms }) => arms
                .into_iter()
                .map(|CoMatcher { dtor: _, tail }| tail.free_vars(arena))
                .fold(CoContext::new(), |acc, x| acc + x),
        }
    }
}
