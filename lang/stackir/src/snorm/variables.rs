use super::syntax::*;

/// Collect bound variables from patterns.
pub trait Vars {
    fn vars(self, arena: &impl AsRef<SNormInnerArena>) -> Context<DefId>;
}

impl Vars for VPatId {
    fn vars(self, arena: &impl AsRef<SNormInnerArena>) -> Context<DefId> {
        let vpat = arena.as_ref().svpats[&self].clone();
        match vpat {
            | ValuePattern::Hole(Hole) => Context::new(),
            | ValuePattern::Var(def_id) => Context::singleton(def_id),
            | ValuePattern::Ctor(Ctor(_ctor, body)) => body.vars(arena),
            | ValuePattern::Triv(Triv) => Context::new(),
            | ValuePattern::VCons(Cons(a, b)) => a.vars(arena) + b.vars(arena),
        }
    }
}

/// Collect free variables from stack IR nodes.
pub trait FreeVars {
    fn free_vars(self, arena: &impl AsRef<SNormInnerArena>) -> CoContext<DefId>;
}

impl FreeVars for ValueId {
    fn free_vars(self, arena: &impl AsRef<SNormInnerArena>) -> CoContext<DefId> {
        let value = arena.as_ref().svalues[&self].clone();
        match value {
            | Value::Var(def_id) => CoContext::singleton(def_id),
            | Value::Closure(Closure { stack: Bullet, body }) => body.free_vars(arena),
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
    fn free_vars(self, arena: &impl AsRef<SNormInnerArena>) -> CoContext<DefId> {
        let stack = arena.as_ref().sstacks[&self].clone();
        match stack {
            | Stack::Kont(Kont { binder, body }) => body.free_vars(arena) - binder.vars(arena),
            | Stack::Var(Bullet) => CoContext::new(),
            | Stack::Arg(Cons(a, b)) => a.free_vars(arena) + b.free_vars(arena),
            | Stack::Tag(Cons(_dtor, body)) => body.free_vars(arena),
        }
    }
}

impl FreeVars for CompuId {
    fn free_vars(self, arena: &impl AsRef<SNormInnerArena>) -> CoContext<DefId> {
        let sc = arena.as_ref().scompus[&self].clone();
        use Computation as Compu;
        let compu_fv = match &sc.compu {
            | Compu::Hole(Hole) => CoContext::new(),
            | Compu::Force(SForce { thunk, stack }) => {
                thunk.free_vars(arena) + stack.free_vars(arena)
            }
            | Compu::Ret(SReturn { stack, value }) => {
                stack.free_vars(arena) + value.free_vars(arena)
            }
            | Compu::Fix(SFix { param, body }) => {
                body.free_vars(arena) - Context::singleton(*param)
            }
            | Compu::Case(Match { scrut, arms }) => {
                scrut.free_vars(arena)
                    + arms
                        .iter()
                        .map(|Matcher { binder, tail }| tail.free_vars(arena) - binder.vars(arena))
                        .fold(CoContext::new(), |acc, x| acc + x)
            }
            | Compu::Join(join) => match *join {},
            | Compu::LetArg(Let { binder: Cons(param, Bullet), bindee, tail }) => {
                tail.free_vars(arena) - param.vars(arena) + bindee.free_vars(arena)
            }
            | Compu::CoCase(CoMatch { arms }) => arms
                .iter()
                .map(|CoMatcher { dtor: _, tail }| tail.free_vars(arena))
                .fold(CoContext::new(), |acc, x| acc + x),
            | Compu::ExternCall(ExternCall { function: _, stack: Bullet }) => CoContext::new(),
        };
        sc.assignments.items.iter().fold(compu_fv, |acc, item| match item {
            | AssignItem::Def(AssignDef { def, value }) => {
                acc - Context::singleton(*def) + value.free_vars(arena)
            }
            | AssignItem::Pattern(AssignPattern { pat, value }) => {
                acc - pat.vars(arena) + value.free_vars(arena)
            }
            | AssignItem::Stack(AssignStack { stack }) => acc + stack.free_vars(arena),
        })
    }
}

pub trait StackHoles {
    fn stack_holes(self, arena: &impl AsRef<SNormInnerArena>) -> CoContext<StackId>;
}

impl StackHoles for StackId {
    fn stack_holes(self, arena: &impl AsRef<SNormInnerArena>) -> CoContext<StackId> {
        let stack = arena.as_ref().sstacks[&self].clone();
        use Stack;
        match stack {
            | Stack::Kont(Kont { binder: _, body }) => body.stack_holes(arena),
            | Stack::Var(Bullet) => CoContext::from_iter([self]),
            | Stack::Arg(Cons(_arg, tail)) => tail.stack_holes(arena),
            | Stack::Tag(Cons(_dtor, tail)) => tail.stack_holes(arena),
        }
    }
}

impl StackHoles for CompuId {
    fn stack_holes(self, arena: &impl AsRef<SNormInnerArena>) -> CoContext<StackId> {
        let SComputation { compu, assignments } = arena.as_ref().scompus[&self].clone();
        for item in assignments.items.iter().rev() {
            match item {
                | AssignItem::Def(_) | AssignItem::Pattern(_) => continue,
                | AssignItem::Stack(AssignStack { stack }) => {
                    return stack.stack_holes(arena);
                }
            }
        }
        use Computation as Compu;
        match compu {
            | Compu::Hole(Hole) => unreachable!("stack hole is currently not supported"),
            | Compu::Force(SForce { thunk: _, stack }) => stack.stack_holes(arena),
            | Compu::Ret(SReturn { stack, value: _ }) => stack.stack_holes(arena),
            | Compu::Fix(SFix { .. }) => todo!(),
            | Compu::Case(Match { scrut: _, arms }) => arms
                .iter()
                .map(|Matcher { binder: _, tail }| tail.stack_holes(arena))
                .fold(CoContext::new(), |acc, x| acc + x),
            | Compu::Join(join) => match join {},
            | Compu::LetArg(Let { binder: Cons(_param, Bullet), bindee, tail: _ }) => {
                bindee.stack_holes(arena)
            }
            | Compu::CoCase(CoMatch { .. }) => {
                todo!()
            }
            | Compu::ExternCall(ExternCall { function: _, stack: Bullet }) => todo!(),
        }
    }
}
