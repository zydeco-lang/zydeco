use super::*;
use crate::high::variables::FreeVars as _;

struct Depth;

impl Depth {
    fn small_stack(test: impl FnOnce() + Send + 'static) {
        std::thread::Builder::new().stack_size(512 * 1024).spawn(test).unwrap().join().unwrap();
    }

    fn returning(arena: &mut StackirArena) -> CompuId {
        let value = Triv.build(arena, None);
        let stack = Bullet.build(arena, None);
        SReturn { stack, value }.build(arena, None)
    }

    fn normalize(arena: StackirArena, root: CompuId) -> StackirProgram {
        let program = BranchJoinProgram::try_new(StackirProgram::new(arena, root)).unwrap();
        Normalizer.run(program).unwrap().into_program()
    }
}

#[test]
fn deep_bindings_prune_total_values_and_retain_traps_on_a_small_stack() {
    Depth::small_stack(|| {
        for trapping in [false, true] {
            let mut arena = StackirArena::default();
            let mut root = Depth::returning(&mut arena);
            for _ in 0..16384 {
                let binder = Hole.build(&mut arena, None);
                let bindee: ValueId = if trapping {
                    Hole.build(&mut arena, None)
                } else {
                    Triv.build(&mut arena, None)
                };
                root = Let { binder, bindee, tail: root }.build(&mut arena, None);
            }
            let normalized = Depth::normalize(arena, root);
            assert_eq!(normalized.arena().inner.compus.len(), if trapping { 16385 } else { 1 });
            assert_eq!(normalized.arena().inner.values.len(), if trapping { 16385 } else { 1 });
        }
    });
}

#[test]
fn deep_suspended_bodies_rebuild_on_a_small_stack() {
    Depth::small_stack(|| {
        let mut arena = StackirArena::default();
        let mut root = Depth::returning(&mut arena);
        for _ in 0..8192 {
            let value = Closure { stack: Bullet, body: root }.build(&mut arena, None);
            let stack = Bullet.build(&mut arena, None);
            root = SReturn { stack, value }.build(&mut arena, None);
        }
        let normalized = Depth::normalize(arena, root);
        assert_eq!(normalized.arena().inner.compus.len(), 8193);
        assert_eq!(normalized.arena().inner.values.len(), 8193);
    });
}

#[test]
fn deep_ambient_substitutions_reduce_and_drop_on_a_small_stack() {
    Depth::small_stack(|| {
        let mut arena = StackirArena::default();
        let mut root = Depth::returning(&mut arena);
        for _ in 0..16384 {
            let thunk = Closure { stack: Bullet, body: root }.build(&mut arena, None);
            let stack = Bullet.build(&mut arena, None);
            root = SForce { thunk, stack }.build(&mut arena, None);
        }
        let normalized = Depth::normalize(arena, root);
        assert_eq!(normalized.arena().inner.compus.len(), 1);
        assert_eq!(normalized.arena().inner.values.len(), 1);
        assert_eq!(normalized.arena().inner.stacks.len(), 1);
    });
}

#[test]
fn deep_argument_stacks_rebuild_on_a_small_stack() {
    Depth::small_stack(|| {
        let mut arena = StackirArena::default();
        let mut stack = Bullet.build(&mut arena, None);
        for _ in 0..16384 {
            let value: ValueId = Triv.build(&mut arena, None);
            stack = Cons(value, stack).build(&mut arena, None);
        }
        let root = SHole(stack).build(&mut arena, None);
        let normalized = Depth::normalize(arena, root);
        assert_eq!(normalized.arena().inner.compus.len(), 1);
        assert_eq!(normalized.arena().inner.values.len(), 16384);
        assert_eq!(normalized.arena().inner.stacks.len(), 16385);
    });
}

#[test]
fn deep_alias_parameters_preserve_bindings_on_a_small_stack() {
    Depth::small_stack(|| {
        let mut arena = StackirArena::default();
        let def: DefId = arena.admin.fresh();
        let mut binder: VPatId = def.build(&mut arena, None);
        for _ in 0..8192 {
            let hole = Hole.build(&mut arena, None);
            binder = Alias(ConsN::from_vec(vec![binder, hole]).unwrap()).build(&mut arena, None);
        }
        let value = def.build(&mut arena, None);
        let stack = Bullet.build(&mut arena, None);
        let tail = SReturn { stack, value }.build(&mut arena, None);
        let stack = Bullet.build(&mut arena, None);
        let root =
            Let { binder: Cons(binder, Bullet), bindee: stack, tail }.build(&mut arena, None);
        let normalized = Depth::normalize(arena, root);
        assert_eq!(normalized.arena().inner.vpats.len(), 16385);
        assert!(normalized.root().free_vars(normalized.arena()).is_empty());
    });
}

#[test]
fn deep_unknown_branches_join_sibling_demands_on_a_small_stack() {
    Depth::small_stack(|| {
        let mut arena = StackirArena::default();
        let scrutinee: DefId = arena.admin.fresh();
        let mut root = Depth::returning(&mut arena);
        for _ in 0..2048 {
            let arms = [root, Depth::returning(&mut arena)]
                .into_iter()
                .enumerate()
                .map(|(idx, tail)| {
                    let payload: VPatId = Hole.build(&mut arena, None);
                    let binder =
                        Ctor(CtorIdx { idx, name: CtorName(format!("+Tag{idx}")) }, payload)
                            .build(&mut arena, None);
                    Matcher { binder, tail }
                })
                .collect();
            let scrut = scrutinee.build(&mut arena, None);
            let tail = SCoprodMatch { scrut, arms }.build(&mut arena, None);
            let stack = Bullet.build(&mut arena, None);
            root = Let { binder: Bullet, bindee: stack, tail }.build(&mut arena, None);
        }
        let binder: VPatId = scrutinee.build(&mut arena, None);
        let stack = Bullet.build(&mut arena, None);
        let root =
            Let { binder: Cons(binder, Bullet), bindee: stack, tail: root }.build(&mut arena, None);
        let normalized = Depth::normalize(arena, root);
        assert_eq!(normalized.arena().inner.compus.len(), 2048 * 3 + 2);
        assert!(normalized.root().free_vars(normalized.arena()).is_empty());
    });
}
