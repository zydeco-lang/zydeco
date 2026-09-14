use super::*;
use crate::low::variables::Variables;
use zydeco_utils::fold::Recursive;

#[derive(Debug, PartialEq, Eq)]
enum Event {
    Enter(EntityId, Edge, Occurrence),
    Exit(EntityId),
}

#[derive(Default)]
struct Trace(Vec<Event>);

impl Visitor for Trace {
    fn enter(&mut self, node: Node<'_>, edge: Edge, occurrence: Occurrence) {
        self.0.push(Event::Enter(node.id(), edge, occurrence));
    }
    fn exit(&mut self, node: Node<'_>) {
        self.0.push(Event::Exit(node.id()));
    }
}

struct Fixture;

impl Fixture {
    fn analyze(arena: &SpsLowInnerArena, root: EntityId) -> (Variables, Trace) {
        let mut explicit = Together { first: Variables::default(), second: Trace::default() };
        let mut recursive = Together { first: Variables::default(), second: Trace::default() };
        let traversal = Traversal { arena };
        traversal.run_with_driver::<Explicit>(root, &mut explicit);
        traversal.run_with_driver::<Recursive>(root, &mut recursive);
        assert_eq!(explicit.second.0, recursive.second.0);
        let mut separate = Trace::default();
        traversal.run(root, &mut separate);
        assert_eq!(explicit.second.0, separate.0);
        for event in &separate.0 {
            if let Event::Exit(id) = event {
                match *id {
                    | EntityId::Pattern(id) => assert_eq!(
                        explicit.first.bound_variables(id).map(|vars| &vars.0),
                        recursive.first.bound_variables(id).map(|vars| &vars.0),
                    ),
                    | id => {
                        let term: TermId = match id {
                            | EntityId::Value(id) => id.into(),
                            | EntityId::Stack(id) => id.into(),
                            | EntityId::Computation(id) => id.into(),
                            | EntityId::Pattern(_) => unreachable!(),
                        };
                        assert_eq!(
                            explicit
                                .first
                                .free_variables(term.clone())
                                .map(|vars| vars.iter().collect::<Vec<_>>()),
                            recursive
                                .first
                                .free_variables(term)
                                .map(|vars| vars.iter().collect::<Vec<_>>()),
                        );
                    }
                }
            }
        }
        (explicit.first, explicit.second)
    }
}

#[test]
fn drivers_observe_repeated_edges_without_reexpanding_children() {
    let mut arena = SpsLowArena::default();
    let def = arena.admin.fresh_def();
    let value = Value::Var(def).build(&mut arena, None);
    let root = VCons::new(vec![value, value], ProductLayout { arity: 2 }).build(&mut arena, None);
    let (variables, trace) = Fixture::analyze(&arena.inner, EntityId::Value(root));
    assert_eq!(
        variables.free_variables(root.into()).unwrap().iter().copied().collect::<Vec<_>>(),
        [def]
    );
    assert_eq!(
        trace.0,
        [
            Event::Enter(root.into(), Edge::Root, Occurrence::First),
            Event::Enter(value.into(), Edge::Child, Occurrence::First),
            Event::Exit(value.into()),
            Event::Enter(value.into(), Edge::Child, Occurrence::Shared),
            Event::Exit(root.into()),
        ]
    );

    arena.inner.values[&value] =
        Ctor(CtorIdx { idx: 0, name: CtorName("loop".into()) }, value).into();
    let (variables, trace) = Fixture::analyze(&arena.inner, root.into());
    assert!(variables.free_variables(root.into()).is_none());
    assert!(trace.0.contains(&Event::Enter(value.into(), Edge::Child, Occurrence::Cyclic)));
    assert!(trace.0.contains(&Event::Enter(value.into(), Edge::Child, Occurrence::Shared)));
}

#[test]
fn summaries_respect_block_entries_and_opening_scopes() {
    let mut arena = SpsLowArena::default();
    let outer = arena.admin.fresh_def();
    let label = arena.admin.fresh_def();
    let environment = arena.admin.fresh_def();
    let result = arena.admin.fresh_def();
    let opened = arena.admin.fresh_def();
    let code = arena.admin.fresh_def();
    let result_pattern = result.build(&mut arena, None);
    let environment_pattern = environment.build(&mut arena, None);
    let fields = [environment, result, label, opened].map(|def| def.build(&mut arena, None));
    let argument = VCons::new(fields.into(), ProductLayout { arity: 4 }).build(&mut arena, None);
    let target = code.build(&mut arena, None);
    let stack = Bullet.build(&mut arena, None);
    let tail = Jump { target, argument: EntryArgument::Continuation { result: argument }, stack }
        .build(&mut arena, None);
    let package = outer.build(&mut arena, None);
    let opened_pattern = opened.build(&mut arena, None);
    let code_pattern = code.build(&mut arena, None);
    let body = OpenClosure { package, environment: opened_pattern, code: code_pattern, body: tail }
        .build(&mut arena, None);
    let block = Block {
        label,
        entry: EntryParameters::Continuation {
            result: result_pattern,
            environment: environment_pattern,
        },
        body,
    }
    .build(&mut arena, None);
    let ambient = Bullet.build(&mut arena, None);
    let stack = Cons(block, ambient).build(&mut arena, None);
    let tail = SHole(stack).build(&mut arena, None);
    let binder = outer.build(&mut arena, None);
    let bindee = Triv.build(&mut arena, None);
    let root = LetValue { binder, bindee, tail }.build(&mut arena, None);
    let (variables, _) = Fixture::analyze(&arena.inner, EntityId::Computation(root));
    assert!(variables.free_variables(root.into()).unwrap().is_empty());
    assert_eq!(
        variables.free_variables(block.into()).unwrap().iter().copied().collect::<Vec<_>>(),
        [outer]
    );
    let expected: zydeco_utils::context::CoContext<_> =
        [outer, environment, result, label].into_iter().collect();
    assert_eq!(
        variables.free_variables(body.into()).unwrap().iter().collect::<Vec<_>>(),
        expected.iter().collect::<Vec<_>>()
    );
    assert_eq!(variables.bound_variables(result_pattern).unwrap().0, [result]);
    assert_eq!(variables.bound_variables(environment_pattern).unwrap().0, [environment]);

    // The binder applies only to the tail, so its occurrence in the bindee remains free.
    arena.inner.values[&bindee] = Value::Var(outer);
    let (variables, _) = Fixture::analyze(&arena.inner, root.into());
    assert_eq!(
        variables.free_variables(root.into()).unwrap().iter().copied().collect::<Vec<_>>(),
        [outer]
    );
}

#[test]
fn entry_word_order_excludes_continuation_metadata_edges() {
    let mut arena = SpsLowArena::default();
    let label = arena.admin.fresh_def();
    let result = Hole.build(&mut arena, None);
    let environment = Triv.build(&mut arena, None);
    let body_stack = Bullet.build(&mut arena, None);
    let body = SHole(body_stack).build(&mut arena, None);
    let code = Block { label, entry: EntryParameters::Continuation { result, environment }, body }
        .build(&mut arena, None);
    let value: ValueId = Triv.build(&mut arena, None);
    let ambient = Bullet.build(&mut arena, None);
    let residual = Cons(value, ambient).build(&mut arena, None);
    let package = ContinuationPackage { code, residual }.build(&mut arena, None);
    arena
        .inner
        .continuations
        .insert_new(package, ContinuationEntry { result, body, captures: vec![] });
    let (_, trace) = Fixture::analyze(&arena.inner, EntityId::Stack(package));
    let entries = trace
        .0
        .into_iter()
        .filter_map(|event| match event {
            | Event::Enter(id, _, occurrence) => {
                assert_eq!(occurrence, Occurrence::First);
                Some(id)
            }
            | Event::Exit(_) => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        entries,
        [
            package.into(),
            code.into(),
            result.into(),
            environment.into(),
            body.into(),
            body_stack.into(),
            residual.into(),
            value.into(),
            ambient.into()
        ]
    );
}

#[test]
fn empty_and_wide_nodes_keep_all_children_in_source_order() {
    for count in [0, 1, 128] {
        let mut arena = SpsLowArena::default();
        let fields = (0..count).map(|_| Triv.build(&mut arena, None)).collect::<Vec<ValueId>>();
        let root = VCons::new(fields.clone(), ProductLayout { arity: count.max(1) })
            .build(&mut arena, None);
        let (_, trace) = Fixture::analyze(&arena.inner, EntityId::Value(root));
        let children = trace
            .0
            .into_iter()
            .filter_map(|event| match event {
                | Event::Enter(EntityId::Value(id), Edge::Child, Occurrence::First) => Some(id),
                | _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(children, fields);
    }
}

#[test]
fn deep_mixed_syntax_and_summaries_drop_on_a_small_stack() {
    std::thread::Builder::new()
        .stack_size(512 * 1024)
        .spawn(|| {
            let mut arena = SpsLowArena::default();
            let stack = Bullet.build(&mut arena, None);
            let mut root = SHole(stack).build(&mut arena, None);
            for _ in 0..16_384 {
                let bindee = Triv.build(&mut arena, None);
                let hole = Hole.build(&mut arena, None);
                let binder = Alias(ConsN::from_vec(vec![hole]).unwrap()).build(&mut arena, None);
                root = LetValue { binder, bindee, tail: root }.build(&mut arena, None);
            }
            let mut analysis = Together { first: Variables::default(), second: Trace::default() };
            Traversal { arena: &arena.inner }.run(root.into(), &mut analysis);
            assert!(analysis.first.free_variables(root.into()).unwrap().is_empty());
            assert_eq!(analysis.second.0.len(), 2 * (4 * 16_384 + 2));
        })
        .unwrap()
        .join()
        .unwrap();
}
