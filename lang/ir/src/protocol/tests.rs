use super::*;
use zydeco_syntax::{DtorName, IntegerType};

struct Fixture;

impl Fixture {
    fn integer() -> ValueProtocol {
        ValueProtocol::Primitive(PrimitiveType::Integer(IntegerType::Int))
    }

    fn tag(index: usize, name: &str) -> DtorIdx {
        DtorIdx { idx: index, name: DtorName::new(name) }
    }

    fn define(
        graph: &mut ProtocolGraph, id: CodataProtocolId, next: CodataProtocolId,
        result: ValueProtocol,
    ) {
        graph.define_codata(
            id,
            CodataProtocol {
                observations: vec![
                    (
                        Self::tag(0, ".again"),
                        StackProtocol::Argument(
                            Box::new(Self::integer()),
                            Box::new(StackProtocol::Codata(next)),
                        ),
                    ),
                    (Self::tag(1, ".result"), StackProtocol::Continuation(Box::new(result))),
                ],
            },
        );
    }

    fn cycle(graph: &mut ProtocolGraph, result: ValueProtocol) -> CodataProtocolId {
        let first = graph.reserve_codata();
        let second = graph.reserve_codata();
        Self::define(graph, first, second, Self::integer());
        Self::define(graph, second, first, result);
        first
    }
}

#[test]
fn cyclic_agreement_checks_observations_after_a_back_edge() {
    let mut graph = ProtocolGraph::default();
    let a = Fixture::cycle(&mut graph, Fixture::integer());
    let b = Fixture::cycle(&mut graph, Fixture::integer());
    let wrong = Fixture::cycle(&mut graph, ValueProtocol::Primitive(PrimitiveType::Char));
    graph.validate().unwrap();
    assert_ne!(a, b);
    assert!(graph.stacks_agree(&StackProtocol::Codata(a), &StackProtocol::Codata(b)));
    assert!(!graph.stacks_agree(&StackProtocol::Codata(a), &StackProtocol::Codata(wrong)));
    assert!(!graph.stacks_agree(&StackProtocol::Codata(wrong), &StackProtocol::Codata(b)));
}

#[test]
fn cycles_through_returned_thunks_are_compared_without_unfolding() {
    let mut graph = ProtocolGraph::default();
    let a = graph.reserve_codata();
    let b = graph.reserve_codata();
    for id in [a, b] {
        Fixture::define(
            &mut graph,
            id,
            id,
            ValueProtocol::Thunk(Box::new(StackProtocol::Codata(id))),
        );
    }
    graph.validate().unwrap();
    assert!(graph.stacks_agree(&StackProtocol::Codata(a), &StackProtocol::Codata(b)));
    Fixture::define(
        &mut graph,
        b,
        b,
        ValueProtocol::Thunk(Box::new(StackProtocol::Continuation(Box::new(Fixture::integer())))),
    );
    assert!(!graph.stacks_agree(&StackProtocol::Codata(a), &StackProtocol::Codata(b)));
}

#[test]
fn selected_observations_check_tags_and_their_remainders() {
    let mut graph = ProtocolGraph::default();
    let id = graph.reserve_codata();
    Fixture::define(&mut graph, id, id, Fixture::integer());
    let expected = StackProtocol::Codata(id);
    let item = StackProtocol::Argument(Box::new(Fixture::integer()), Box::new(expected.clone()));
    let supplied = StackProtocol::Tag(Fixture::tag(0, ".again"), Box::new(item.clone()));
    assert!(graph.stacks_agree(&expected, &supplied));
    assert!(graph.stacks_agree(&supplied, &expected));
    for (tag, remainder) in [
        (Fixture::tag(0, ".other"), item.clone()),
        (Fixture::tag(1, ".again"), item),
        (Fixture::tag(0, ".again"), StackProtocol::Continuation(Box::new(Fixture::integer()))),
    ] {
        assert!(!graph.stacks_agree(&expected, &StackProtocol::Tag(tag, Box::new(remainder))));
    }
    assert!(graph.stacks_agree(
        &expected,
        &StackProtocol::Tag(Fixture::tag(0, ".again"), Box::new(StackProtocol::Unknown))
    ));
}

#[test]
fn unknown_compatibility_is_not_transitive_equality() {
    let mut graph = ProtocolGraph::default();
    let a = Fixture::cycle(&mut graph, Fixture::integer());
    let unknown = Fixture::cycle(&mut graph, ValueProtocol::Unknown);
    let b = Fixture::cycle(&mut graph, ValueProtocol::Primitive(PrimitiveType::Char));
    assert!(graph.stacks_agree(&StackProtocol::Codata(a), &StackProtocol::Codata(unknown)));
    assert!(graph.stacks_agree(&StackProtocol::Codata(unknown), &StackProtocol::Codata(b)));
    assert!(!graph.stacks_agree(&StackProtocol::Codata(a), &StackProtocol::Codata(b)));
}

#[test]
fn missing_and_unresolved_graph_definitions_are_rejected() {
    let mut graph = ProtocolGraph::default();
    let id = graph.reserve_codata();
    assert_eq!(graph.validate(), Err(ProtocolGraphError::MissingDefinition(id)));
    Fixture::define(&mut graph, id, id, Fixture::integer());
    graph.validate().unwrap();
    let missing = CodataProtocolId(99);
    Fixture::define(&mut graph, id, missing, Fixture::integer());
    assert_eq!(graph.validate(), Err(ProtocolGraphError::MissingDefinition(missing)));
}
