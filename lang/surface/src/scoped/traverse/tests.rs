use super::*;
use crate::{
    bitter::arena::BitterScope,
    scoped::context::{ContextCollector, TermContexts},
};
use std::{cell::RefCell, convert::Infallible};

struct Fixture {
    allocator: IdAllocator<BitterScope>,
    arena: ScopedArena,
}

impl Fixture {
    fn new() -> Self {
        Self { allocator: IdAllocator::new(), arena: ScopedArena::default() }
    }

    fn term(&mut self, term: impl Into<Term<DefId>>) -> TermId {
        let id = self.allocator.alloc();
        self.arena.terms.insert_new(id, term.into());
        id
    }

    fn pattern(&mut self, pattern: impl Into<Pattern>) -> PatId {
        let id = self.allocator.alloc();
        self.arena.pats.insert_new(id, pattern.into());
        id
    }

    fn variable(&mut self, name: &str) -> (DefId, PatId, TermId) {
        let def = self.allocator.alloc();
        self.arena.defs.insert_new(def, VarName(name.into()));
        (def, self.pattern(def), self.term(Term::Var(def)))
    }

    fn imports(&mut self, depth: usize) -> TermId {
        let mut root = self.term(Triv);
        for _ in 0..depth {
            let left = self.term(SourceBoundary(root));
            let right = self.term(SignatureBoundary(root));
            root = self.term(Term::Cons(vec![left, right]));
        }
        root
    }

    fn assert_free(contexts: &TermContexts, term: TermId, expected: &[DefId]) {
        let mut expected = expected.to_vec();
        expected.sort_unstable();
        assert_eq!(contexts.at(&term).iter().copied().collect::<Vec<_>>(), expected);
    }
}

#[derive(Default)]
struct Visits {
    entries: Vec<EntityId>,
    exits: Vec<EntityId>,
}

impl Visitor for Visits {
    type Break = Infallible;

    fn enter(&mut self, node: Node<'_>) -> ControlFlow<Self::Break> {
        self.entries.push(node.id());
        ControlFlow::Continue(())
    }

    fn exit(&mut self, node: Node<'_>) -> ControlFlow<Self::Break> {
        self.exits.push(node.id());
        ControlFlow::Continue(())
    }
}

#[test]
fn shared_imports_are_summarized_before_repeated_edges_descend() {
    let mut fixture = Fixture::new();
    let root = fixture.imports(12);
    let unreachable = fixture.term(Hole);
    let mut visits = Visits::default();
    let mut contexts = ContextCollector::default();
    let result = Traversal::new(&fixture.arena)
        .run(root.into(), &mut Together { first: &mut contexts, second: &mut visits });
    assert_eq!(result, Ok(ControlFlow::Continue(())));
    assert_eq!(visits.entries.len(), 37);
    assert_eq!(visits.exits.len(), 37);
    assert!(!visits.entries.contains(&unreachable.into()));
    let contexts = contexts.finish();

    // The old post-callback memo guard still walked 16,381 node occurrences.
    let mut occurrences = Visits::default();
    let mut separate = ContextCollector::default();
    assert_eq!(
        Traversal::new(&fixture.arena)
            .with_sharing(Sharing::Occurrences)
            .run(root.into(), &mut occurrences),
        Ok(ControlFlow::Continue(())),
    );
    assert_eq!(occurrences.entries.len(), 16_381);
    assert_eq!(occurrences.exits.len(), 16_381);
    assert_eq!(
        Traversal::new(&fixture.arena).run(root.into(), &mut separate),
        Ok(ControlFlow::Continue(())),
    );
    let separate = separate.finish();
    for id in visits.exits {
        let EntityId::Term(term) = id else { unreachable!() };
        Fixture::assert_free(&contexts, term, &[]);
        assert_eq!(
            contexts.at(&term).iter().collect::<Vec<_>>(),
            separate.at(&term).iter().collect::<Vec<_>>(),
        );
    }
}

#[test]
fn traversal_preserves_binding_annotation_and_view_order() {
    let mut fixture = Fixture::new();
    let (bound, binder, use_bound) = fixture.variable("bound");
    let (function, _, view) = fixture.variable("view");
    let (classifier, _, annotation) = fixture.variable("classifier");
    let (external, _, bindee) = fixture.variable("external");
    let viewed = fixture.pattern(ViewPattern { function: view, pattern: binder });
    let annotated = fixture.pattern(Ann { tm: viewed, ty: annotation });
    let root = fixture.term(Let { binder: annotated, bindee, tail: use_bound });
    let mut visits = Visits::default();
    assert_eq!(
        Traversal::new(&fixture.arena).run(root.into(), &mut visits),
        Ok(ControlFlow::Continue(())),
    );
    assert_eq!(
        visits.entries,
        vec![
            root.into(),
            bindee.into(),
            external.into(),
            annotated.into(),
            viewed.into(),
            view.into(),
            function.into(),
            binder.into(),
            bound.into(),
            annotation.into(),
            classifier.into(),
            use_bound.into(),
        ]
    );
    assert_eq!(
        visits.exits,
        vec![
            external.into(),
            bindee.into(),
            function.into(),
            view.into(),
            bound.into(),
            binder.into(),
            viewed.into(),
            classifier.into(),
            annotation.into(),
            annotated.into(),
            use_bound.into(),
            root.into(),
        ]
    );
    let contexts = TermContexts::collect(&fixture.arena, root);
    Fixture::assert_free(&contexts, root, &[function, classifier, external]);
    Fixture::assert_free(&contexts, use_bound, &[bound]);
    Fixture::assert_free(&contexts, bindee, &[external]);
}

#[test]
fn recursive_definitions_are_leaves_for_structural_cycle_detection() {
    let mut fixture = Fixture::new();
    let (left, left_pattern, use_left) = fixture.variable("left");
    let (right, right_pattern, use_right) = fixture.variable("right");
    let (external, _, use_external) = fixture.variable("external");
    let right_body = fixture.term(Term::Cons(vec![use_left, use_external]));
    let group = fixture.term(RecGroup {
        definitions: vec![
            RecursiveDefinition { binder: left_pattern, bindee: use_right },
            RecursiveDefinition { binder: right_pattern, bindee: right_body },
        ],
        tail: use_left,
    });
    let root = fixture.term(Block(group));
    let contexts = TermContexts::collect(&fixture.arena, root);
    Fixture::assert_free(&contexts, root, &[external]);
    Fixture::assert_free(&contexts, group, &[external]);
    Fixture::assert_free(&contexts, use_left, &[left]);
    Fixture::assert_free(&contexts, use_right, &[right]);
}

#[test]
fn copattern_telescope_summaries_discharge_earlier_binders() {
    let mut fixture = Fixture::new();
    let (outer, _, use_outer) = fixture.variable("outer");
    let (_, first, use_first) = fixture.variable("first");
    let (second, second_pattern, use_second) = fixture.variable("second");
    let first = fixture.pattern(Ann { tm: first, ty: use_outer });
    let second_pattern = fixture.pattern(Ann { tm: second_pattern, ty: use_first });
    let root = fixture.term(CoMatchClauses {
        clauses: vec![CoPatternClause {
            spine: CoPatternSpine {
                head: CoPatternItem::Pat(first),
                tail: vec![
                    CoPatternItem::Dtor(DtorName("next".into())),
                    CoPatternItem::Pat(second_pattern),
                ],
            },
            tail: use_second,
        }],
    });
    let contexts = TermContexts::collect(&fixture.arena, root);
    Fixture::assert_free(&contexts, root, &[outer]);
    Fixture::assert_free(&contexts, use_second, &[second]);
}

#[test]
fn sharing_policy_distinguishes_definition_occurrences() {
    let mut fixture = Fixture::new();
    let (def, _, usage) = fixture.variable("shared");
    let root = fixture.term(Term::Cons(vec![usage, usage]));
    for (sharing, expected) in [
        (Sharing::UniqueNodes, vec![root.into(), usage.into(), def.into()]),
        (
            Sharing::Occurrences,
            vec![root.into(), usage.into(), def.into(), usage.into(), def.into()],
        ),
    ] {
        let mut visits = Visits::default();
        let mut collector = ContextCollector::default();
        assert_eq!(
            Traversal::new(&fixture.arena)
                .with_sharing(sharing)
                .run(root.into(), &mut Together { first: &mut visits, second: &mut collector }),
            Ok(ControlFlow::Continue(())),
        );
        assert_eq!(visits.entries, expected);
        Fixture::assert_free(&collector.finish(), root, &[def]);
    }
}

#[test]
fn cycles_are_rejected_in_both_modes_without_completing_active_nodes() {
    let mut fixture = Fixture::new();
    let root: TermId = fixture.allocator.alloc();
    let child = fixture.term(SourceBoundary(root));
    fixture.arena.terms.insert_new(root, Term::SignatureBoundary(SignatureBoundary(child)));
    let healthy = fixture.term(Triv);
    for sharing in [Sharing::UniqueNodes, Sharing::Occurrences] {
        let traversal = Traversal::new(&fixture.arena).with_sharing(sharing);
        let mut visits = Visits::default();
        assert_eq!(
            traversal.run(root.into(), &mut visits),
            Err(TraversalCycle { node: root.into() }),
        );
        assert_eq!(visits.entries, vec![root.into(), child.into()]);
        assert!(visits.exits.is_empty());
        let mut visits = Visits::default();
        assert_eq!(traversal.run(healthy.into(), &mut visits), Ok(ControlFlow::Continue(())));
        assert_eq!(visits.entries, vec![healthy.into()]);
        assert_eq!(visits.exits, vec![healthy.into()]);
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Event {
    Enter,
    Exit,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Callback {
    observer: usize,
    event: Event,
    node: EntityId,
}

struct Recorder<'a> {
    observer: usize,
    callbacks: &'a RefCell<Vec<Callback>>,
    stop: Option<Callback>,
}

impl Recorder<'_> {
    fn record(&self, event: Event, node: EntityId) -> ControlFlow<Callback> {
        let callback = Callback { observer: self.observer, event, node };
        self.callbacks.borrow_mut().push(callback);
        if self.stop == Some(callback) {
            ControlFlow::Break(callback)
        } else {
            ControlFlow::Continue(())
        }
    }
}

impl Visitor for Recorder<'_> {
    type Break = Callback;

    fn enter(&mut self, node: Node<'_>) -> ControlFlow<Self::Break> {
        self.record(Event::Enter, node.id())
    }

    fn exit(&mut self, node: Node<'_>) -> ControlFlow<Self::Break> {
        self.record(Event::Exit, node.id())
    }
}

#[test]
fn composed_visitors_stop_at_the_first_break_in_event_order() {
    let mut fixture = Fixture::new();
    let first = fixture.term(Triv);
    let second = fixture.term(Hole);
    let root = fixture.term(App(first, second));
    let expected = [
        (Event::Enter, root),
        (Event::Enter, first),
        (Event::Exit, first),
        (Event::Enter, second),
        (Event::Exit, second),
        (Event::Exit, root),
    ]
    .into_iter()
    .flat_map(|(event, node)| {
        [0, 1].into_iter().map(move |observer| Callback { observer, event, node: node.into() })
    })
    .collect::<Vec<_>>();
    let traversal = Traversal::new(&fixture.arena);
    // Reuse the configuration after every break; all traversal state belongs to its run.
    for stop in expected.iter().copied().map(Some).chain([None]) {
        let callbacks = RefCell::new(Vec::new());
        let mut visitors = Together {
            first: Recorder { observer: 0, callbacks: &callbacks, stop },
            second: Recorder { observer: 1, callbacks: &callbacks, stop },
        };
        let result = traversal.run(root.into(), &mut visitors);
        let end = match stop {
            | Some(stop) => {
                assert_eq!(result, Ok(ControlFlow::Break(stop)));
                expected.iter().position(|callback| *callback == stop).unwrap() + 1
            }
            | None => {
                assert_eq!(result, Ok(ControlFlow::Continue(())));
                expected.len()
            }
        };
        assert_eq!(*callbacks.borrow(), expected[..end]);
    }
}

#[test]
fn deep_terms_use_the_explicit_work_stack() {
    let mut fixture = Fixture::new();
    let mut root = fixture.term(Triv);
    for _ in 0..20_000 {
        root = fixture.term(SourceBoundary(root));
    }
    let mut visits = Visits::default();
    assert_eq!(
        Traversal::new(&fixture.arena).run(root.into(), &mut visits),
        Ok(ControlFlow::Continue(()))
    );
    assert_eq!(visits.entries.len(), 20_001);
    assert_eq!(visits.exits.len(), 20_001);
    assert_eq!(visits.entries.first(), visits.exits.last());
    assert!(TermContexts::collect(&fixture.arena, root).at(&root).is_empty());
}
