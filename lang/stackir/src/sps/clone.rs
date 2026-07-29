use super::syntax::*;
use crate::clone::*;

pub trait DeepClone<Arena> {
    fn deep_clone(&self, arena: &mut Arena, map: &mut DefMap) -> Self;
}

/// Clone a pattern def id, creating a new one and updating the map.
impl<Arena> DeepClone<Arena> for DefId
where
    Arena: AsMut<StackirArena> + AsMut<ScopedArena>,
{
    fn deep_clone(&self, arena: &mut Arena, map: &mut DefMap) -> Self {
        let name = AsMut::<ScopedArena>::as_mut(arena).defs[self].clone();
        let new_def = AsMut::<StackirArena>::as_mut(arena).admin.fresh();
        AsMut::<ScopedArena>::as_mut(arena).insert_def(new_def, name);
        map.insert(*self, new_def);
        new_def
    }
}

impl<Arena> DeepClone<Arena> for VPatId
where
    Arena: AsMut<StackirArena> + AsMut<ScopedArena>,
{
    fn deep_clone(&self, arena: &mut Arena, map: &mut DefMap) -> Self {
        let arena_mut = AsMut::<StackirArena>::as_mut(arena);
        let vpat = arena_mut.inner.vpats[self].clone();
        use ValuePattern as VPat;
        match vpat {
            | VPat::Hole(Hole) => Hole.build(arena, None),
            | VPat::Var(def) => {
                let new_def = def.deep_clone(arena, map);
                new_def.build(arena, None)
            }
            | VPat::Ctor(Ctor(ctor, pat)) => {
                let pat = pat.deep_clone(arena, map);
                Ctor(ctor, pat).build(arena, None)
            }
            | VPat::Triv(Triv) => Triv.build(arena, None),
            | VPat::VCons(VCons { items: ConsN(items, tail), layout }) => {
                let items = items.iter().map(|item| item.deep_clone(arena, map)).collect();
                let tail = tail.deep_clone(arena, map);
                VCons::new(ConsN(items, tail), layout).build(arena, None)
            }
        }
    }
}

impl<Arena> DeepClone<Arena> for ValueId
where
    Arena: AsMut<StackirArena> + AsMut<ScopedArena>,
{
    fn deep_clone(&self, arena: &mut Arena, map: &mut DefMap) -> Self {
        let arena_mut = AsMut::<StackirArena>::as_mut(arena);
        let value = arena_mut.inner.values[self].clone();
        use Value;
        match value {
            | Value::Hole(Hole) => Hole.build(arena, None),
            | Value::Var(def) => {
                let def = map.get(&def).copied().unwrap_or(def);
                def.build(arena, None)
            }
            | Value::Closure(Closure { stack: Bullet, body }) => {
                let body = body.deep_clone(arena, map);
                Closure { stack: Bullet, body }.build(arena, None)
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                let body = body.deep_clone(arena, map);
                Ctor(ctor, body).build(arena, None)
            }
            | Value::Triv(Triv) => Triv.build(arena, None),
            | Value::VCons(VCons { items: ConsN(items, tail), layout }) => {
                let items = items.iter().map(|item| item.deep_clone(arena, map)).collect();
                let tail = tail.deep_clone(arena, map);
                VCons::new(ConsN(items, tail), layout).build(arena, None)
            }
            | Value::Literal(literal) => literal.build(arena, None),
            | Value::Complex(Complex { operator, operands }) => {
                let operands =
                    operands.iter().map(|operand| operand.deep_clone(arena, map)).collect();
                Complex { operator, operands }.build(arena, None)
            }
        }
    }
}

impl<Arena> DeepClone<Arena> for StackId
where
    Arena: AsMut<StackirArena> + AsMut<ScopedArena>,
{
    fn deep_clone(&self, arena: &mut Arena, map: &mut DefMap) -> Self {
        let arena_mut = AsMut::<StackirArena>::as_mut(arena);
        let stack = arena_mut.inner.stacks[self].clone();
        use Stack;
        match stack {
            | Stack::Kont(Kont { binder, body }) => {
                let binder = binder.deep_clone(arena, map);
                let body = body.deep_clone(arena, map);
                Kont { binder, body }.build(arena, None)
            }
            | Stack::Var(Bullet) => Bullet.build(arena, None),
            | Stack::Arg(Cons(arg, stack)) => {
                let arg = arg.deep_clone(arena, map);
                let stack = stack.deep_clone(arena, map);
                Cons(arg, stack).build(arena, None)
            }
            | Stack::Tag(Cons(dtor, stack)) => {
                let stack = stack.deep_clone(arena, map);
                Cons(dtor, stack).build(arena, None)
            }
        }
    }
}

impl<Arena> DeepClone<Arena> for CompuId
where
    Arena: AsMut<StackirArena> + AsMut<ScopedArena>,
{
    fn deep_clone(&self, arena: &mut Arena, map: &mut DefMap) -> Self {
        let arena_mut = AsMut::<StackirArena>::as_mut(arena);
        let compu = arena_mut.inner.compus[self].clone();
        use Computation as Compu;
        match compu {
            | Compu::Hole(SHole(tail)) => {
                let tail = tail.deep_clone(arena, map);
                SHole(tail).build(arena, None)
            }
            | Compu::Force(SForce { thunk, stack }) => {
                let thunk = thunk.deep_clone(arena, map);
                let stack = stack.deep_clone(arena, map);
                SForce { thunk, stack }.build(arena, None)
            }
            | Compu::Ret(SReturn { stack, value }) => {
                let stack = stack.deep_clone(arena, map);
                let value = value.deep_clone(arena, map);
                SReturn { stack, value }.build(arena, None)
            }
            | Compu::Fix(SFix { param, body }) => {
                let param = param.deep_clone(arena, map);
                let body = body.deep_clone(arena, map);
                SFix { param, body }.build(arena, None)
            }
            | Compu::Case(Match { scrut, arms }) => {
                let scrut = scrut.deep_clone(arena, map);
                let arms = arms
                    .iter()
                    .map(|Matcher { binder, tail }| {
                        let binder = binder.deep_clone(arena, map);
                        let tail = tail.deep_clone(arena, map);
                        Matcher { binder, tail }.into()
                    })
                    .collect();
                Match { scrut, arms }.build(arena, None)
            }
            | Compu::Join(join) => match join {
                | LetJoin::Value(Let { binder, bindee, tail }) => {
                    let binder = binder.deep_clone(arena, map);
                    let bindee = bindee.deep_clone(arena, map);
                    let tail = tail.deep_clone(arena, map);
                    Let { binder, bindee, tail }.build(arena, None)
                }
                | LetJoin::Stack(Let { binder: Bullet, bindee, tail }) => {
                    let bindee = bindee.deep_clone(arena, map);
                    let tail = tail.deep_clone(arena, map);
                    Let { binder: Bullet, bindee, tail }.build(arena, None)
                }
            },
            | Compu::LetArg(Let { binder: Cons(param, Bullet), bindee, tail }) => {
                let bindee = bindee.deep_clone(arena, map);
                let param = param.deep_clone(arena, map);
                let tail = tail.deep_clone(arena, map);
                Let { binder: Cons(param, Bullet), bindee, tail }.build(arena, None)
            }
            | Compu::CoCase(SCoMatch { scrut, arms }) => {
                let scrut = scrut.deep_clone(arena, map);
                let arms = arms
                    .into_iter()
                    .map(|CoMatcher { dtor, tail }| {
                        let tail = tail.deep_clone(arena, map);
                        CoMatcher { dtor, tail }.into()
                    })
                    .collect();
                SCoMatch { scrut, arms }.build(arena, None)
            }
            | Compu::ExternCall(ExternCall { function, stack }) => {
                let stack = stack.deep_clone(arena, map);
                ExternCall { function, stack }.build(arena, None)
            }
        }
    }
}
