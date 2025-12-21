use super::syntax::*;
use std::collections::HashMap;

pub trait SubstitutionInPlace {
    /// Substitute the free variables in the term in place.
    ///
    /// The [`DefId`]s in the map are guaranteed to be free.
    fn substitute_in_place(self, arena: &mut impl AsMut<StackArena>, map: &HashMap<DefId, ValueId>);
}

impl SubstitutionInPlace for ValueId {
    fn substitute_in_place(
        self, arena: &mut impl AsMut<StackArena>, map: &HashMap<DefId, ValueId>,
    ) {
        let mut arena_mut = arena.as_mut();
        let value = arena_mut.values[&self].clone();

        match value {
            | Value::Var(def_id) => match map.get(&def_id) {
                | Some(new_value_id) => {
                    let new_value = arena_mut.values[&new_value_id].clone();
                    arena_mut.values.replace(self, new_value)
                }
                | None => {}
            },
            | Value::Clo(Clo { capture: _, stack: Bullet, body }) => {
                // Recursively substitute in the body
                body.substitute_in_place(&mut arena_mut, map)
            }
            | Value::Ctor(Ctor(_ctor, body)) => {
                // Recursively substitute in the body
                body.substitute_in_place(&mut arena_mut, map)
            }
            | Value::VCons(Cons(a, b)) => {
                // Recursively substitute in both components
                a.substitute_in_place(&mut arena_mut, map);
                b.substitute_in_place(&mut arena_mut, map);
            }
            | Value::Hole(Hole) | Value::Triv(Triv) | Value::Lit(_) => {}
        }
    }
}

impl SubstitutionInPlace for StackId {
    fn substitute_in_place(
        self, arena: &mut impl AsMut<StackArena>, map: &HashMap<DefId, ValueId>,
    ) {
        let mut arena_mut = arena.as_mut();
        let stack = arena_mut.stacks[&self].clone();

        match stack {
            | Stack::Kont(Kont { binder: _, body }) => {
                // Recursively substitute in the body
                body.substitute_in_place(&mut arena_mut, map);
            }
            | Stack::Arg(Cons(val, stack)) => {
                // Recursively substitute in both the value and the rest of the stack
                val.substitute_in_place(&mut arena_mut, map);
                stack.substitute_in_place(&mut arena_mut, map);
            }
            | Stack::Tag(Cons(_dtor, stack)) => {
                // Recursively substitute in the rest of the stack
                stack.substitute_in_place(&mut arena_mut, map);
            }
            | Stack::Var(Bullet) => {
                // No substitution needed for the bullet
            }
        }
    }
}

impl SubstitutionInPlace for CompuId {
    fn substitute_in_place(
        self, arena: &mut impl AsMut<StackArena>, map: &HashMap<DefId, ValueId>,
    ) {
        let mut arena_mut = arena.as_mut();
        let compu = arena_mut.compus[&self].clone();

        match compu {
            | Computation::Hole(Hole) => {}
            | Computation::Force(SForce { thunk, stack }) => {
                // Recursively substitute in both the thunk and the stack
                thunk.substitute_in_place(&mut arena_mut, map);
                stack.substitute_in_place(&mut arena_mut, map);
            }
            | Computation::Ret(SReturn { stack, value }) => {
                // Recursively substitute in both the stack and the value
                stack.substitute_in_place(&mut arena_mut, map);
                value.substitute_in_place(&mut arena_mut, map);
            }
            | Computation::Fix(SFix { capture: _, param: _, body }) => {
                // Recursively substitute in the body
                // Note: param is bound, so we don't substitute it
                body.substitute_in_place(&mut arena_mut, map);
            }
            | Computation::Case(Match { scrut, arms }) => {
                // Recursively substitute in the scrutinee and all arm bodies
                scrut.substitute_in_place(&mut arena_mut, map);
                arms.into_iter().for_each(|Matcher { binder: _, tail }| {
                    tail.substitute_in_place(&mut arena_mut, map);
                });
            }
            | Computation::LetValue(Let { binder: _, bindee, tail }) => {
                // Recursively substitute in the bindee and tail
                bindee.substitute_in_place(&mut arena_mut, map);
                tail.substitute_in_place(&mut arena_mut, map);
            }
            | Computation::LetStack(Let { binder: Bullet, bindee, tail }) => {
                // Recursively substitute in the bindee and tail
                bindee.substitute_in_place(&mut arena_mut, map);
                tail.substitute_in_place(&mut arena_mut, map);
            }
            | Computation::LetArg(Let { binder: Cons(_param, Bullet), bindee, tail }) => {
                // Recursively substitute in the bindee and tail
                bindee.substitute_in_place(&mut arena_mut, map);
                tail.substitute_in_place(&mut arena_mut, map);
            }
            | Computation::CoCase(CoMatch { arms }) => {
                // Recursively substitute in all arm bodies
                arms.into_iter().for_each(|CoMatcher { dtor: _, tail }| {
                    tail.substitute_in_place(&mut arena_mut, map);
                });
            }
        }
    }
}
