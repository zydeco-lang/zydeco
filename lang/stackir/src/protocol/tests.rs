use super::*;
use zydeco_statics::{
    arena::{StaticsArena, StaticsScope},
    syntax as ss,
};
use zydeco_syntax::{Arrow, DtorName, IntegerType};
use zydeco_utils::prelude::IdAllocator;

struct Fixture;

impl Fixture {
    fn integer() -> ValueProtocol {
        ValueProtocol::Primitive(PrimitiveType::Integer(IntegerType::Int64))
    }

    fn tag(index: usize, name: &str) -> DtorIdx {
        DtorIdx { idx: index, name: DtorName(name.into()) }
    }

    fn define(
        graph: &mut ProtocolGraph, id: CodataProtocolId, next: CodataProtocolId,
        result: ValueProtocol,
    ) {
        graph.codatas[id.0] = Some(CodataProtocol {
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
        });
    }

    fn cycle(graph: &mut ProtocolGraph, result: ValueProtocol) -> CodataProtocolId {
        let first = graph.reserve();
        let second = graph.reserve();
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
    let a = graph.reserve();
    let b = graph.reserve();
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
    let id = graph.reserve();
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
    let id = graph.reserve();
    assert_eq!(graph.validate(), Err(ProtocolGraphError::MissingDefinition(id)));
    Fixture::define(&mut graph, id, id, Fixture::integer());
    graph.validate().unwrap();
    let missing = CodataProtocolId(99);
    Fixture::define(&mut graph, id, missing, Fixture::integer());
    assert_eq!(graph.validate(), Err(ProtocolGraphError::MissingDefinition(missing)));
}

#[test]
fn extraction_preserves_an_argument_prefix_crossing_a_recursive_observation() {
    let mut allocator = IdAllocator::<StaticsScope>::new();
    let kind = allocator.alloc();
    let integer = allocator.alloc();
    let arrow = allocator.alloc();
    let codata_ty = allocator.alloc();
    let codata_id = allocator.alloc();
    let witness = allocator.alloc();
    let sealed = allocator.alloc();
    let mut statics = StaticsArena::default();
    for (id, ty) in [
        (integer, ss::Type::Primitive(ss::PrimitiveTy(PrimitiveType::Integer(IntegerType::Int64)))),
        (arrow, ss::Type::Arrow(Arrow(integer, codata_ty))),
        (codata_ty, ss::Type::CoData(codata_id)),
        (sealed, ss::Type::Abst(witness)),
    ] {
        statics.types_pre.insert_new(id, ss::Fillable::Done(ty), kind);
    }
    statics.seals.insert_new(witness, arrow);
    statics.codatas.insert_new(codata_id, ss::CoData::new([(DtorName(".again".into()), sealed)]));
    let mut source = SourceProtocols::new(&statics);
    let protocol = source.stack(sealed);
    let StackProtocol::Argument(_, rest) = &protocol else { panic!("expected the known prefix") };
    let StackProtocol::Codata(id) = **rest else { panic!("expected a codata reference") };
    source.graph.validate().unwrap();
    assert_eq!(
        source.graph.get(id).unwrap().observation(&Fixture::tag(0, ".again")),
        Some(&protocol)
    );
}
