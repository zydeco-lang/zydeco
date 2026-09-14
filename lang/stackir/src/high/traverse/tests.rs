use super::*;
use crate::high::{
    check::{BranchJoinError, BranchJoinValidator},
    variables::{FreeVars, Variables},
};

#[derive(Default)]
struct Counts {
    entries: usize,
    exits: usize,
}
impl Visitor for Counts {
    fn enter(&mut self, _: Node<'_>, _: Edge, _: Occurrence) {
        self.entries += 1;
    }
    fn exit(&mut self, _: Node<'_>) {
        self.exits += 1;
    }
}

#[test]
fn composed_variables_and_ownership_observe_shared_edges_once_without_reexpanding_children() {
    let mut arena = StackirArena::default();
    let definition: DefId = arena.admin.fresh();
    let value = Value::Var(definition).build(&mut arena, None);
    let pair = VCons::new(vec![value, value], ProductLayout { arity: 2 }).build(&mut arena, None);
    let stack = Bullet.build(&mut arena, None);
    let root = SReturn { stack, value: pair }.build(&mut arena, None);
    let mut together = Together {
        first: Together { first: BranchJoinValidator::default(), second: Variables::default() },
        second: Counts::default(),
    };
    Traversal { arena: &arena.inner }.run(root.into(), &mut together);
    assert_eq!(together.first.first.errors(), &[BranchJoinError::SharedValue { value }]);
    let actual = together.first.second.free_variables(root.into()).unwrap();
    assert_eq!(actual.iter().copied().collect::<Vec<_>>(), [definition]);
    assert_eq!(
        actual.iter().collect::<Vec<_>>(),
        root.free_vars(&arena).iter().collect::<Vec<_>>()
    );
    assert_eq!((together.second.entries, together.second.exits), (5, 4));
    let mut separate = BranchJoinValidator::default();
    Traversal { arena: &arena.inner }.run(root.into(), &mut separate);
    assert_eq!(together.first.first.errors(), separate.errors());

    let other = Value::Var(definition).build(&mut arena, None);
    arena.inner.values[&pair] =
        Value::VCons(VCons::new(vec![value, other], ProductLayout { arity: 2 }));
    let mut valid =
        Together { first: BranchJoinValidator::default(), second: Variables::default() };
    Traversal { arena: &arena.inner }.run(root.into(), &mut valid);
    assert!(valid.first.errors().is_empty());
    assert_eq!(
        valid.second.free_variables(root.into()).unwrap().iter().copied().collect::<Vec<_>>(),
        [definition]
    );
}

#[test]
fn bottom_up_variables_respect_lexical_bindings_and_continuation_scopes() {
    let mut arena = StackirArena::default();
    let outer: DefId = arena.admin.fresh();
    let bound: DefId = arena.admin.fresh();
    let binder = ValuePattern::Var(bound).build(&mut arena, None);
    let bindee = Value::Var(outer).build(&mut arena, None);
    let value = Value::Var(bound).build(&mut arena, None);
    let stack = Bullet.build(&mut arena, None);
    let body = SReturn { stack, value }.build(&mut arena, None);
    let continuation = Kont { binder, body }.build(&mut arena, None);
    let root = SReturn { stack: continuation, value: bindee }.build(&mut arena, None);
    let mut analyses =
        Together { first: Variables::default(), second: BranchJoinValidator::default() };
    Traversal { arena: &arena.inner }.run(root.into(), &mut analyses);
    assert!(analyses.second.errors().is_empty());
    assert!(analyses.first.free_variables(continuation.into()).unwrap().is_empty());
    assert_eq!(
        analyses.first.bound_variables(binder).unwrap().iter().copied().collect::<Vec<_>>(),
        [bound]
    );
    assert_eq!(
        analyses.first.free_variables(root.into()).unwrap().iter().copied().collect::<Vec<_>>(),
        [outer]
    );
}

#[test]
fn cyclic_ownership_does_not_prevent_independent_repeated_edges_from_being_reported() {
    let mut arena = StackirArena::default();
    let cyclic: ValueId = arena.admin.fresh();
    arena.inner.values.insert_new(
        cyclic,
        Value::Ctor(Ctor(CtorIdx { idx: 0, name: CtorName("loop".into()) }, cyclic)),
    );
    let shared = Triv.build(&mut arena, None);
    let pair = VCons::new(vec![cyclic, shared, shared], ProductLayout { arity: 3 })
        .build(&mut arena, None);
    let stack = Bullet.build(&mut arena, None);
    let root = SReturn { stack, value: pair }.build(&mut arena, None);
    let mut analyses =
        Together { first: BranchJoinValidator::default(), second: Variables::default() };
    Traversal { arena: &arena.inner }.run(root.into(), &mut analyses);
    assert_eq!(
        analyses.first.errors(),
        &[
            BranchJoinError::SharedValue { value: cyclic },
            BranchJoinError::SharedValue { value: shared },
        ]
    );
    assert!(analyses.second.free_variables(root.into()).is_none());
}
