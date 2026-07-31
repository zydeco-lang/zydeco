use super::syntax::*;
use crate::clone::*;

pub trait DeepClone<Arena> {
    fn deep_clone(&self, arena: &mut Arena, map: &mut DefMap) -> Self;
}

/// Clone a pattern def id, creating a new one and updating the map.
impl<Arena> DeepClone<Arena> for DefId
where
    Arena: AsMut<AdminArena> + AsMut<SNormInnerArena> + AsMut<ScopedArena>,
{
    fn deep_clone(&self, arena: &mut Arena, map: &mut DefMap) -> Self {
        let name = AsMut::<ScopedArena>::as_mut(arena).defs[self].clone();
        let new_def = AsMut::<AdminArena>::as_mut(arena).fresh();
        AsMut::<ScopedArena>::as_mut(arena).insert_def(new_def, name);
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
            let new_id = AsMut::<AdminArena>::as_mut(arena).fresh();
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
            | VPat::Triv(Triv) => build(Triv, arena),
            | VPat::VCons(VCons { items: ConsN(items, tail), layout }) => {
                let items = items.iter().map(|item| item.deep_clone(arena, map)).collect();
                let tail = tail.deep_clone(arena, map);
                build(VCons::new(ConsN(items, tail), layout), arena)
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
            let new_id = AsMut::<AdminArena>::as_mut(arena).fresh();
            value.sbuild(arena, new_id, ())
        }
        use Value;
        match value {
            | Value::Hole(Hole) => build(Hole, arena),
            | Value::Var(def) => {
                let def = map.get(&def).copied().unwrap_or(def);
                let value = build(def, arena);
                AsMut::<SNormInnerArena>::as_mut(arena).record_value_user(value);
                value
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
            | Value::VCons(VCons { items: ConsN(items, tail), layout }) => {
                let items = items.iter().map(|item| item.deep_clone(arena, map)).collect();
                let tail = tail.deep_clone(arena, map);
                build(VCons::new(ConsN(items, tail), layout), arena)
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
            let new_id = AsMut::<AdminArena>::as_mut(arena).fresh();
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

impl<Arena> DeepClone<Arena> for AssignItem
where
    Arena: AsMut<AdminArena> + AsMut<SNormInnerArena> + AsMut<ScopedArena>,
{
    fn deep_clone(&self, arena: &mut Arena, map: &mut DefMap) -> Self {
        match self {
            | AssignItem::Def(AssignDef { def, value }) => {
                let value = value.deep_clone(arena, map);
                let def = def.deep_clone(arena, map);
                AssignDef { def, value }.into()
            }
            | AssignItem::Pattern(AssignPattern { pat, value }) => {
                let value = value.deep_clone(arena, map);
                let pat = pat.deep_clone(arena, map);
                AssignPattern { pat, value }.into()
            }
            | AssignItem::Stack(AssignStack { stack }) => {
                AssignStack { stack: stack.deep_clone(arena, map) }.into()
            }
        }
    }
}

impl<Arena> DeepClone<Arena> for SubstAssignments
where
    Arena: AsMut<AdminArena> + AsMut<SNormInnerArena> + AsMut<ScopedArena>,
{
    fn deep_clone(&self, arena: &mut Arena, map: &mut DefMap) -> Self {
        // Assignments are stored from innermost to outermost. Clone them in lexical
        // order so every outer introduction is in `map` before cloning inner references,
        // then restore their substitution-normal storage order.
        let items = self
            .items
            .iter()
            .rev()
            .map(|item| item.deep_clone(arena, map))
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .collect();
        Self { items }
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
            let new_id = AsMut::<AdminArena>::as_mut(arena).fresh();
            compu.sbuild(arena, new_id, new_str)
        }
        let new_str = assignments.deep_clone(arena, map);
        use Computation as Compu;
        match compu {
            | Compu::Hole(SHole(tail)) => {
                let tail = tail.deep_clone(arena, map);
                build(SHole(tail), new_str, arena)
            }
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
            | Compu::Fix(SFix { param, body }) => {
                let param = param.deep_clone(arena, map);
                let body = body.deep_clone(arena, map);
                build(SFix { param, body }, new_str, arena)
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
            | Compu::CoCase(SCoMatch { scrut, arms }) => {
                let scrut = scrut.deep_clone(arena, map);
                let arms = arms
                    .into_iter()
                    .map(|CoMatcher { dtor, tail }| {
                        let tail = tail.deep_clone(arena, map);
                        CoMatcher { dtor, tail }.into()
                    })
                    .collect();
                build(SCoMatch { scrut, arms }, new_str, arena)
            }
            | Compu::ExternCall(ExternCall { function, stack }) => {
                let stack = stack.deep_clone(arena, map);
                build(ExternCall { function, stack }, new_str, arena)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::snorm::substitute::Substitutor;
    use zydeco_statics::tyck::arena::StaticsArena;
    use zydeco_syntax::VarName;

    #[test]
    fn cloned_variable_references_remain_indexed_as_users() {
        let mut snorm = SNormInnerArena::default();
        let mut scoped = ScopedArena::default();
        let statics = StaticsArena::default();
        let mut substitutor =
            Substitutor::new(AdminArena::default(), &mut snorm, &mut scoped, &statics);

        let definition = substitutor.arena.admin.fresh();
        substitutor.scoped.insert_def(definition, VarName("captured".into()));
        let reference = substitutor.arena.admin.fresh();
        Value::Var(definition).sbuild(&mut substitutor, reference, ());

        let cloned = reference.deep_clone(&mut substitutor, &mut DefMap::default());

        assert!(substitutor.snorm.users[&definition].contains(&cloned));
    }

    #[test]
    fn assignment_clone_renames_references_to_outer_bindings() {
        let mut snorm = SNormInnerArena::default();
        let mut scoped = ScopedArena::default();
        let statics = StaticsArena::default();
        let mut substitutor =
            Substitutor::new(AdminArena::default(), &mut snorm, &mut scoped, &statics);

        let original_outer = substitutor.arena.admin.fresh();
        let original_inner = substitutor.arena.admin.fresh();
        substitutor.scoped.insert_def(original_outer, VarName("outer".into()));
        substitutor.scoped.insert_def(original_inner, VarName("inner".into()));

        let outer_value = substitutor.arena.admin.fresh();
        Value::Triv(Triv).sbuild(&mut substitutor, outer_value, ());
        let inner_value = substitutor.arena.admin.fresh();
        Value::Var(original_outer).sbuild(&mut substitutor, inner_value, ());
        let body_value = substitutor.arena.admin.fresh();
        Value::Var(original_inner).sbuild(&mut substitutor, body_value, ());
        let body_stack = substitutor.arena.admin.fresh();
        Bullet.sbuild(&mut substitutor, body_stack, body_stack);
        let body = substitutor.arena.admin.fresh();
        SReturn { stack: body_stack, value: body_value }.sbuild(
            &mut substitutor,
            body,
            SubstAssignments {
                items: [
                    AssignDef { def: original_inner, value: inner_value }.into(),
                    AssignDef { def: original_outer, value: outer_value }.into(),
                ]
                .into_iter()
                .collect(),
            },
        );

        let cloned = body.deep_clone(&mut substitutor, &mut DefMap::default());
        let assignments = &substitutor.snorm.scompus[&cloned].assignments.items;
        let [AssignItem::Def(cloned_inner), AssignItem::Def(cloned_outer)] =
            assignments.iter().collect::<Vec<_>>().as_slice()
        else {
            panic!("expected two cloned definition assignments");
        };
        let Value::Var(inner_reference) = substitutor.snorm.svalues[&cloned_inner.value] else {
            panic!("expected the inner bindee to reference the outer definition");
        };

        assert_ne!(cloned_outer.def, original_outer);
        assert_eq!(inner_reference, cloned_outer.def);
    }
}
