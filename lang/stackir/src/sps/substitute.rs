use super::syntax::*;
use crate::sps::{arena::StackirArena, clone::DeepClone as _};
use indexmap::IndexMap;
use zydeco_surface::scoped::arena::ScopedArena;

#[derive(Clone, Debug, Default)]
pub struct SubstVarMap {
    pub values: IndexMap<DefId, SubstValue>,
}

#[derive(Clone, Debug)]
pub struct SubstValue {
    pub value: ValueId,
    /// The number of times this value has been instantiated during the substitution.
    pub count: usize,
}

impl SubstVarMap {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn insert(&mut self, def_id: DefId, value_id: ValueId) {
        self.values.insert(def_id, SubstValue { value: value_id, count: 0 });
    }
}
impl FromIterator<(DefId, ValueId)> for SubstVarMap {
    fn from_iter<T: IntoIterator<Item = (DefId, ValueId)>>(iter: T) -> Self {
        Self {
            values: IndexMap::from_iter(
                iter.into_iter()
                    .map(|(def_id, value_id)| (def_id, SubstValue { value: value_id, count: 0 })),
            ),
        }
    }
}

/// In-place substitution for stack IR nodes.
pub trait SubstVarInPlace {
    /// Substitute the free variables in the term in place.
    ///
    /// The [`DefId`]s in the map are guaranteed to be free.
    fn subst_var_in_place(
        self, arena: &mut (impl AsMut<StackirArena> + AsMut<ScopedArena>), map: &mut SubstVarMap,
    );
}

impl SubstVarInPlace for ValueId {
    fn subst_var_in_place(
        self, arena: &mut (impl AsMut<StackirArena> + AsMut<ScopedArena>), map: &mut SubstVarMap,
    ) {
        let value = AsMut::<StackirArena>::as_mut(arena).inner.values[&self].clone();

        match value {
            | Value::Var(def_id) => match map.values.get_mut(&def_id) {
                | Some(SubstValue { value: new_value_id, count }) => {
                    let mut new_value_id = *new_value_id;
                    if *count > 0 {
                        new_value_id = new_value_id.deep_clone(arena, &mut Default::default());
                    }
                    *count += 1;
                    let new_value =
                        AsMut::<StackirArena>::as_mut(arena).inner.values[&new_value_id].clone();
                    AsMut::<StackirArena>::as_mut(arena)
                        .inner
                        .values
                        .replace_existing(self, new_value);
                }
                | None => {}
            },
            | Value::Closure(Closure { stack: Bullet, body }) => {
                body.subst_var_in_place(arena, map)
            }
            | Value::Ctor(Ctor(_ctor, body)) => body.subst_var_in_place(arena, map),
            | Value::VCons(Cons(a, b)) => {
                a.subst_var_in_place(arena, map);
                b.subst_var_in_place(arena, map);
            }
            | Value::Complex(Complex { operator: _, operands }) => {
                operands.into_iter().for_each(|operand| {
                    operand.subst_var_in_place(arena, map);
                });
            }
            | Value::Hole(Hole) | Value::Triv(Triv) | Value::Literal(_) => {}
        }
    }
}

impl SubstVarInPlace for StackId {
    fn subst_var_in_place(
        self, arena: &mut (impl AsMut<StackirArena> + AsMut<ScopedArena>), map: &mut SubstVarMap,
    ) {
        let stack = AsMut::<StackirArena>::as_mut(arena).inner.stacks[&self].clone();

        match stack {
            | Stack::Kont(Kont { binder: _, body }) => {
                body.subst_var_in_place(arena, map);
            }
            | Stack::Arg(Cons(val, stack)) => {
                val.subst_var_in_place(arena, map);
                stack.subst_var_in_place(arena, map);
            }
            | Stack::Tag(Cons(_dtor, stack)) => {
                stack.subst_var_in_place(arena, map);
            }
            | Stack::Var(Bullet) => {}
        }
    }
}

impl SubstVarInPlace for CompuId {
    fn subst_var_in_place(
        self, arena: &mut (impl AsMut<StackirArena> + AsMut<ScopedArena>), map: &mut SubstVarMap,
    ) {
        let arena_mut = AsMut::<StackirArena>::as_mut(arena);
        let compu = arena_mut.inner.compus[&self].clone();

        match compu {
            | Computation::Hole(SHole(tail)) => {
                tail.subst_var_in_place(arena, map);
            }
            | Computation::Force(SForce { thunk, stack }) => {
                thunk.subst_var_in_place(arena, map);
                stack.subst_var_in_place(arena, map);
            }
            | Computation::Ret(SReturn { stack, value }) => {
                stack.subst_var_in_place(arena, map);
                value.subst_var_in_place(arena, map);
            }
            | Computation::Fix(SFix { param: _, body }) => {
                // Note: param is bound, so we don't substitute it
                body.subst_var_in_place(arena, map);
            }
            | Computation::Case(Match { scrut, arms }) => {
                scrut.subst_var_in_place(arena, map);
                arms.into_iter().for_each(|Matcher { binder: _, tail }| {
                    tail.subst_var_in_place(arena, map);
                });
            }
            | Computation::Join(LetJoin::Value(Let { binder: _, bindee, tail })) => {
                bindee.subst_var_in_place(arena, map);
                tail.subst_var_in_place(arena, map);
            }
            | Computation::Join(LetJoin::Stack(Let { binder: Bullet, bindee, tail })) => {
                bindee.subst_var_in_place(arena, map);
                tail.subst_var_in_place(arena, map);
            }
            | Computation::LetArg(Let { binder: Cons(_param, Bullet), bindee, tail }) => {
                bindee.subst_var_in_place(arena, map);
                tail.subst_var_in_place(arena, map);
            }
            | Computation::CoCase(SCoMatch { scrut, arms }) => {
                scrut.subst_var_in_place(arena, map);
                arms.into_iter().for_each(|CoMatcher { dtor: _, tail }| {
                    tail.subst_var_in_place(arena, map);
                });
            }
            | Computation::ExternCall(ExternCall { function: _, stack }) => {
                stack.subst_var_in_place(arena, map);
            }
        }
    }
}
