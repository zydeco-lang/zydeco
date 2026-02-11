use super::syntax::*;
use zydeco_surface::scoped::arena::ScopedArena;

/// Map old def id to new def id.
type DefMap = std::collections::HashMap<DefId, DefId>;

pub trait DeepClone<Arena> {
    fn deep_clone(&self, arena: &mut Arena, map: &mut DefMap) -> Self;
}

/// Clone a pattern def id, creating a new one and updating the map.
impl<Arena> DeepClone<Arena> for DefId
where
    Arena: AsMut<AdminArena> + AsMut<SNormInnerArena> + AsMut<ScopedArena>,
{
    fn deep_clone(&self, arena: &mut Arena, map: &mut DefMap) -> Self {
        let scoped = AsMut::<ScopedArena>::as_mut(arena);
        let name = scoped.defs[self].clone();
        let new_def = scoped.defs.alloc(name);
        map.insert(*self, new_def);
        new_def
    }
}

impl<Arena> DeepClone<Arena> for VPatId
where
    Arena: AsMut<AdminArena> + AsMut<SNormInnerArena> + AsMut<ScopedArena>,
{
    fn deep_clone(&self, arena: &mut Arena, map: &mut DefMap) -> Self {
        let arena_mut = AsMut::<SNormInnerArena>::as_mut(arena);
        let vpat = arena_mut.svpats[self].clone();
        fn build<Arena: AsMut<AdminArena> + AsMut<SNormInnerArena>>(
            vpat: impl Into<ValuePattern>, arena: &mut Arena,
        ) -> VPatId {
            let new_id = AsMut::<AdminArena>::as_mut(arena).allocator.alloc();
            vpat.sbuild(arena, new_id, ())
        }
        use ValuePattern as VPat;
        match vpat {
            | VPat::Hole(Hole) => build(Hole, arena),
            | VPat::Var(def) => {
                let new_def = def.deep_clone(arena, map);
                build(new_def, arena)
            }
            | VPat::Ctor(Ctor(ctor, pat)) => {
                let pat = pat.deep_clone(arena, map);
                build(Ctor(ctor, pat), arena)
            }
            | VPat::Triv(triv) => build(triv, arena),
            | VPat::VCons(Cons(a, b)) => {
                let a = a.deep_clone(arena, map);
                let b = b.deep_clone(arena, map);
                build(Cons(a, b), arena)
            }
        }
    }
}

impl<Arena> DeepClone<Arena> for ValueId
where
    Arena: AsMut<AdminArena> + AsMut<SNormInnerArena> + AsMut<ScopedArena>,
{
    fn deep_clone(&self, arena: &mut Arena, map: &mut DefMap) -> Self {
        let arena_mut = AsMut::<SNormInnerArena>::as_mut(arena);
        let value = arena_mut.svalues[self].clone();
        fn build<Arena: AsMut<AdminArena> + AsMut<SNormInnerArena>>(
            value: impl Into<Value>, arena: &mut Arena,
        ) -> ValueId {
            let new_id = AsMut::<AdminArena>::as_mut(arena).allocator.alloc();
            value.sbuild(arena, new_id, ())
        }
        use Value;
        match value {
            | Value::Hole(Hole) => build(Hole, arena),
            | Value::Var(def) => {
                let def = map[&def];
                build(def, arena)
            }
            | Value::Closure(Closure { stack: Bullet, body }) => {
                let body = body.deep_clone(arena, map);
                build(Closure { stack: Bullet, body }, arena)
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                let body = body.deep_clone(arena, map);
                build(Ctor(ctor, body), arena)
            }
            | Value::Triv(Triv) => build(Triv, arena),
            | Value::VCons(Cons(a, b)) => {
                let a = a.deep_clone(arena, map);
                let b = b.deep_clone(arena, map);
                build(Cons(a, b), arena)
            }
            | Value::Literal(literal) => build(literal, arena),
            | Value::Complex(Complex { operator, operands }) => {
                let operands =
                    operands.iter().map(|operand| operand.deep_clone(arena, map)).collect();
                build(Complex { operator, operands }, arena)
            }
        }
    }
}

impl<Arena> DeepClone<Arena> for StackId
where
    Arena: AsMut<AdminArena> + AsMut<SNormInnerArena> + AsMut<ScopedArena>,
{
    fn deep_clone(&self, arena: &mut Arena, map: &mut DefMap) -> Self {
        let arena_mut = AsMut::<SNormInnerArena>::as_mut(arena);
        let stack = arena_mut.sstacks[self].clone();
        fn build<Arena: AsMut<AdminArena> + AsMut<SNormInnerArena>>(
            stack: impl Into<Stack>, arena: &mut Arena,
        ) -> StackId {
            let new_id = AsMut::<AdminArena>::as_mut(arena).allocator.alloc();
            // Fixme: hole is not correct here
            stack.sbuild(arena, new_id, new_id)
        }
        use Stack;
        match stack {
            | Stack::Kont(Kont { binder, body }) => {
                let binder = binder.deep_clone(arena, map);
                let body = body.deep_clone(arena, map);
                build(Kont { binder, body }, arena)
            }
            | Stack::Var(Bullet) => build(Bullet, arena),
            | Stack::Arg(Cons(arg, stack)) => {
                let arg = arg.deep_clone(arena, map);
                let stack = stack.deep_clone(arena, map);
                build(Cons(arg, stack), arena)
            }
            | Stack::Tag(Cons(dtor, stack)) => {
                let stack = stack.deep_clone(arena, map);
                build(Cons(dtor, stack), arena)
            }
        }
    }
}

impl<Arena> DeepClone<Arena> for CompuId
where
    Arena: AsMut<AdminArena> + AsMut<SNormInnerArena> + AsMut<ScopedArena>,
{
    fn deep_clone(&self, arena: &mut Arena, map: &mut DefMap) -> Self {
        let arena_mut = AsMut::<SNormInnerArena>::as_mut(arena);
        let SComputation { compu, assignments } = arena_mut.scompus[self].clone();
        fn build<Arena: AsMut<AdminArena> + AsMut<SNormInnerArena>>(
            compu: impl Into<Computation<NonJoin>>, new_str: SubstAssignments, arena: &mut Arena,
        ) -> CompuId {
            let new_id = AsMut::<AdminArena>::as_mut(arena).allocator.alloc();
            compu.sbuild(arena, new_id, new_str)
        }
        let new_str = SubstAssignments {
            items: assignments
                .items
                .into_iter()
                .map(|item| match item {
                    | AssignItem::Def(AssignDef { def, value }) => {
                        let def = def.deep_clone(arena, map);
                        let value = value.deep_clone(arena, map);
                        AssignDef { def, value }.into()
                    }
                    | AssignItem::Pattern(AssignPattern { pat, value }) => {
                        let pat = pat.deep_clone(arena, map);
                        let value = value.deep_clone(arena, map);
                        AssignPattern { pat, value }.into()
                    }
                    | AssignItem::Stack(AssignStack { stack }) => {
                        let stack = stack.deep_clone(arena, map);
                        AssignStack { stack }.into()
                    }
                })
                .collect(),
        };
        use Computation as Compu;
        match compu {
            | Compu::Hole(Hole) => build(Hole, new_str, arena),
            | Compu::Force(SForce { thunk, stack }) => {
                let thunk = thunk.deep_clone(arena, map);
                let stack = stack.deep_clone(arena, map);
                build(SForce { thunk, stack }, new_str, arena)
            }
            | Compu::Ret(SReturn { stack, value }) => {
                let stack = stack.deep_clone(arena, map);
                let value = value.deep_clone(arena, map);
                build(SReturn { stack, value }, new_str, arena)
            }
            | Compu::Fix(SFix { capture, param, body }) => {
                assert!(capture.iter().count() == 0, "capture must be empty");
                let param = param.deep_clone(arena, map);
                let body = body.deep_clone(arena, map);
                build(SFix { capture, param, body }, new_str, arena)
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
                build(Match { scrut, arms }, new_str, arena)
            }
            | Compu::Join(join) => match join {},
            | Compu::LetArg(Let { binder: Cons(param, Bullet), bindee, tail }) => {
                let bindee = bindee.deep_clone(arena, map);
                let param = param.deep_clone(arena, map);
                let tail = tail.deep_clone(arena, map);
                build(Let { binder: Cons(param, Bullet), bindee, tail }, new_str, arena)
            }
            | Compu::CoCase(CoMatch { arms }) => {
                let arms = arms
                    .into_iter()
                    .map(|CoMatcher { dtor, tail }| {
                        let tail = tail.deep_clone(arena, map);
                        CoMatcher { dtor, tail }.into()
                    })
                    .collect();
                build(CoMatch { arms }, new_str, arena)
            }
            | Compu::ExternCall(ExternCall { function, stack: Bullet }) => {
                build(ExternCall { function, stack: Bullet }, new_str, arena)
            }
        }
    }
}
