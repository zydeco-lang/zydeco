use super::*;
use zydeco_syntax::{DtorName, IntegerType};

struct Fixture;

impl Fixture {
    fn integer() -> ValueProtocol {
        ValueProtocol::Primitive(PrimitiveType::Integer(IntegerType::Int))
    }

    fn character() -> ValueProtocol {
        ValueProtocol::Primitive(PrimitiveType::Char)
    }

    fn function(input: ValueProtocol, result: ValueProtocol) -> StackProtocol {
        StackProtocol::Argument(
            Box::new(input),
            Box::new(StackProtocol::Continuation(Box::new(result))),
        )
    }

    fn thunk(stack: StackProtocol) -> ValueProtocol {
        ValueProtocol::Thunk(Box::new(stack))
    }

    fn tag(index: usize, name: &str) -> DtorIdx {
        DtorIdx { idx: index, name: DtorName(name.into()) }
    }

    fn items(
        values: impl DoubleEndedIterator<Item = ValueProtocol>, rest: StackProtocol,
    ) -> StackProtocol {
        values.rev().fold(rest, |rest, value| {
            StackProtocol::Tag(
                Self::tag(1, ".item"),
                Box::new(StackProtocol::Argument(Box::new(value), Box::new(rest))),
            )
        })
    }

    fn stream(
        graph: &mut ProtocolGraph, input: ValueProtocol, result: ValueProtocol,
    ) -> CodataProtocolId {
        let id = graph.reserve();
        graph.codatas[id.0] = Some(CodataProtocol {
            observations: vec![
                (Self::tag(0, ".done"), StackProtocol::Continuation(Box::new(result))),
                (
                    Self::tag(1, ".item"),
                    StackProtocol::Argument(Box::new(input), Box::new(StackProtocol::Codata(id))),
                ),
            ],
        });
        id
    }

    fn agree(graph: &ProtocolGraph, left: &StackProtocol, right: &StackProtocol, expected: bool) {
        graph.validate().unwrap();
        graph.validate_stack(left).unwrap();
        graph.validate_stack(right).unwrap();
        assert_eq!(graph.stacks_agree(left, right), expected);
        assert_eq!(graph.stacks_agree(right, left), expected);
    }
}

#[test]
fn a_parameter_relates_its_argument_and_returned_value_at_each_use() {
    let mut graph = ProtocolGraph::default();
    let a = graph.parameter(ProtocolParameterKind::Value);
    let identity = StackProtocol::Forall(
        a,
        Box::new(Fixture::function(ValueProtocol::Parameter(a), ValueProtocol::Parameter(a))),
    );
    for input in [Fixture::integer(), Fixture::character()] {
        Fixture::agree(&graph, &identity, &Fixture::function(input.clone(), input), true);
    }
    Fixture::agree(
        &graph,
        &identity,
        &Fixture::function(Fixture::integer(), Fixture::character()),
        false,
    );
}

#[test]
fn separate_universal_occurrences_in_one_value_have_independent_instantiations() {
    let mut graph = ProtocolGraph::default();
    let a = graph.parameter(ProtocolParameterKind::Value);
    let body = Fixture::function(ValueProtocol::Parameter(a), ValueProtocol::Parameter(a));
    let identity = StackProtocol::Forall(a, Box::new(body.clone()));
    let actual = ValueProtocol::Product(vec![
        Fixture::thunk(Fixture::function(Fixture::integer(), Fixture::integer())),
        Fixture::thunk(Fixture::function(Fixture::character(), Fixture::character())),
    ]);
    let independent =
        ValueProtocol::Product(vec![Fixture::thunk(identity.clone()), Fixture::thunk(identity)]);
    let shared = ValueProtocol::Product(vec![Fixture::thunk(body.clone()), Fixture::thunk(body)]);
    assert!(graph.values_agree(&independent, &actual));
    assert!(!graph.values_agree(&shared, &actual));
}

#[test]
fn nested_binders_shadow_without_losing_the_outer_parameter_relationship() {
    let mut graph = ProtocolGraph::default();
    let a = graph.parameter(ProtocolParameterKind::Value);
    let inner = Fixture::thunk(StackProtocol::Forall(
        a,
        Box::new(Fixture::function(ValueProtocol::Parameter(a), ValueProtocol::Parameter(a))),
    ));
    let body = StackProtocol::Argument(
        Box::new(ValueProtocol::Parameter(a)),
        Box::new(Fixture::function(inner, ValueProtocol::Parameter(a))),
    );
    let scheme = StackProtocol::Forall(a, Box::new(body));
    let actual_inner =
        Fixture::thunk(Fixture::function(Fixture::character(), Fixture::character()));
    for (result, accepted) in [(Fixture::integer(), true), (Fixture::character(), false)] {
        let actual = StackProtocol::Argument(
            Box::new(Fixture::integer()),
            Box::new(Fixture::function(actual_inner.clone(), result)),
        );
        Fixture::agree(&graph, &scheme, &actual, accepted);
    }
}

#[test]
fn partial_constraints_keep_every_known_conflict_regardless_of_visit_order() {
    let mut graph = ProtocolGraph::default();
    let a = graph.parameter(ProtocolParameterKind::Value);
    let expected = ValueProtocol::Product(vec![ValueProtocol::Parameter(a); 3]);
    let unknown = ValueProtocol::Product(vec![ValueProtocol::Unknown]);
    let integer = ValueProtocol::Product(vec![Fixture::integer()]);
    let character = ValueProtocol::Product(vec![Fixture::character()]);
    for fields in [
        vec![unknown.clone(), integer.clone(), character.clone()],
        vec![integer.clone(), unknown.clone(), character.clone()],
        vec![character, integer.clone(), unknown.clone()],
    ] {
        assert!(!graph.values_agree(&expected, &ValueProtocol::Product(fields)));
    }
    assert!(
        graph.values_agree(
            &expected,
            &ValueProtocol::Product(vec![unknown, integer.clone(), integer])
        )
    );
}

#[test]
fn joining_parameters_propagates_constraints_learned_before_and_after_the_join() {
    let mut graph = ProtocolGraph::default();
    let a = graph.parameter(ProtocolParameterKind::Value);
    let b = graph.parameter(ProtocolParameterKind::Value);
    let left = ValueProtocol::Product(vec![
        ValueProtocol::Parameter(a),
        Fixture::integer(),
        ValueProtocol::Parameter(a),
    ]);
    for (result, accepted) in [(Fixture::integer(), true), (Fixture::character(), false)] {
        let right = ValueProtocol::Product(vec![
            ValueProtocol::Parameter(b),
            ValueProtocol::Parameter(b),
            result,
        ]);
        assert_eq!(graph.values_agree(&left, &right), accepted);
        assert_eq!(graph.values_agree(&right, &left), accepted);
    }
}

#[test]
fn a_computation_parameter_relates_callback_and_installed_continuation_protocols() {
    let mut graph = ProtocolGraph::default();
    let r = graph.parameter(ProtocolParameterKind::Stack);
    let body = StackProtocol::Argument(
        Box::new(Fixture::thunk(StackProtocol::Parameter(r))),
        Box::new(StackProtocol::Parameter(r)),
    );
    let scheme = StackProtocol::Forall(r, Box::new(body));
    let callback = Fixture::thunk(StackProtocol::Continuation(Box::new(Fixture::integer())));
    for (result, accepted) in [(Fixture::integer(), true), (Fixture::character(), false)] {
        Fixture::agree(&graph, &scheme, &Fixture::function(callback.clone(), result), accepted);
    }
}

#[test]
fn parameters_remain_related_across_recursive_observations_but_unknowns_do_not() {
    let mut graph = ProtocolGraph::default();
    let a = graph.parameter(ProtocolParameterKind::Value);
    let stream =
        Fixture::stream(&mut graph, ValueProtocol::Parameter(a), ValueProtocol::Parameter(a));
    let opaque = Fixture::stream(&mut graph, ValueProtocol::Unknown, Fixture::integer());
    let done = StackProtocol::Tag(
        Fixture::tag(0, ".done"),
        Box::new(StackProtocol::Continuation(Box::new(Fixture::integer()))),
    );
    let scheme = StackProtocol::Forall(a, Box::new(StackProtocol::Codata(stream)));
    let matching =
        Fixture::items([Fixture::integer(), Fixture::integer()].into_iter(), done.clone());
    let different = Fixture::items([Fixture::integer(), Fixture::character()].into_iter(), done);
    Fixture::agree(&graph, &scheme, &matching, true);
    Fixture::agree(&graph, &scheme, &different, false);
    Fixture::agree(&graph, &StackProtocol::Codata(opaque), &different, true);
}

#[test]
fn observation_local_binders_stay_conservative_without_forgetting_the_recursive_interface() {
    let mut graph = ProtocolGraph::default();
    let a = graph.parameter(ProtocolParameterKind::Value);
    let id = Fixture::stream(&mut graph, ValueProtocol::Unknown, Fixture::integer());
    graph.codatas[id.0].as_mut().unwrap().observations[1].1 = StackProtocol::Forall(
        a,
        Box::new(StackProtocol::Argument(
            Box::new(ValueProtocol::Parameter(a)),
            Box::new(StackProtocol::Codata(id)),
        )),
    );
    let done = StackProtocol::Tag(
        Fixture::tag(0, ".done"),
        Box::new(StackProtocol::Continuation(Box::new(Fixture::integer()))),
    );
    let actual = Fixture::items([Fixture::integer(), Fixture::character()].into_iter(), done);
    Fixture::agree(&graph, &StackProtocol::Codata(id), &actual, true);
    let bad_tail =
        StackProtocol::Tag(Fixture::tag(2, ".missing"), Box::new(StackProtocol::Unknown));
    let bad = Fixture::items([Fixture::integer(), Fixture::character()].into_iter(), bad_tail);
    Fixture::agree(&graph, &StackProtocol::Codata(id), &bad, false);
}

#[test]
fn one_computation_parameter_can_admit_distinct_observation_selections() {
    let mut graph = ProtocolGraph::default();
    let r = graph.parameter(ProtocolParameterKind::Stack);
    let a = graph.reserve();
    graph.codatas[a.0] = Some(CodataProtocol {
        observations: vec![
            (Fixture::tag(0, ".a"), StackProtocol::Unknown),
            (Fixture::tag(1, ".b"), StackProtocol::Unknown),
        ],
    });
    let expected = ValueProtocol::Product(vec![Fixture::thunk(StackProtocol::Parameter(r)); 3]);
    let first =
        Fixture::thunk(StackProtocol::Tag(Fixture::tag(0, ".a"), Box::new(StackProtocol::Unknown)));
    for (name, accepted) in [(".b", true), (".missing", false)] {
        let second = Fixture::thunk(StackProtocol::Tag(
            Fixture::tag(1, name),
            Box::new(StackProtocol::Unknown),
        ));
        let actual = ValueProtocol::Product(vec![
            first.clone(),
            second,
            Fixture::thunk(StackProtocol::Codata(a)),
        ]);
        assert_eq!(graph.values_agree(&expected, &actual), accepted);
    }
}

#[test]
fn observation_constraints_preserve_canonical_relative_tag_order() {
    let mut graph = ProtocolGraph::default();
    let r = graph.parameter(ProtocolParameterKind::Stack);
    let expected = ValueProtocol::Product(vec![Fixture::thunk(StackProtocol::Parameter(r)); 2]);
    for (first_index, second_index, accepted) in [(0, 1, true), (1, 0, false), (0, 0, false)] {
        let actual = ValueProtocol::Product(vec![
            Fixture::thunk(StackProtocol::Tag(
                Fixture::tag(first_index, ".a"),
                Box::new(StackProtocol::Unknown),
            )),
            Fixture::thunk(StackProtocol::Tag(
                Fixture::tag(second_index, ".b"),
                Box::new(StackProtocol::Unknown),
            )),
        ]);
        assert_eq!(graph.values_agree(&expected, &actual), accepted);
    }
}

#[test]
fn missing_and_wrong_kind_parameter_references_are_rejected() {
    let mut graph = ProtocolGraph::default();
    let value = graph.parameter(ProtocolParameterKind::Value);
    let stack = graph.parameter(ProtocolParameterKind::Stack);
    graph.validate_value(&ValueProtocol::Parameter(value)).unwrap();
    graph.validate_stack(&StackProtocol::Parameter(stack)).unwrap();
    assert_eq!(
        graph.validate_stack(&StackProtocol::Parameter(value)),
        Err(ProtocolGraphError::ParameterKind {
            parameter: value,
            expected: ProtocolParameterKind::Stack,
            found: ProtocolParameterKind::Value,
        })
    );
    assert_eq!(
        graph.validate_value(&ValueProtocol::Parameter(stack)),
        Err(ProtocolGraphError::ParameterKind {
            parameter: stack,
            expected: ProtocolParameterKind::Value,
            found: ProtocolParameterKind::Stack,
        })
    );
    let missing = ProtocolParameterId(99);
    assert_eq!(
        graph.validate_stack(&StackProtocol::Forall(missing, Box::new(StackProtocol::Unknown))),
        Err(ProtocolGraphError::MissingParameter(missing))
    );
}
