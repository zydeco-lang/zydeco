use std::path::PathBuf;
use zydeco_cli::CommandCompiler;
use zydeco_stackir::{
    SpsLowError, SpsLowProgram,
    low::{protocols::ProtocolError, syntax::*},
    protocol::{
        CodataProtocolId, ProtocolGraphError, ProtocolParameterId, ProtocolParameterKind,
        StackProtocol, ValueProtocol,
    },
};
use zydeco_tests::utils::{ExecutionTarget, SourceProgram};

struct Fixture;

impl Fixture {
    fn program() -> SpsLowProgram {
        Self::source("stack-protocols.zy")
    }

    fn source(name: &str) -> SpsLowProgram {
        let path =
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/tests/core").join(name);
        CommandCompiler::default().lower(&path).unwrap().sps_low
    }

    fn integer() -> ValueProtocol {
        ValueProtocol::Primitive(PrimitiveType::Integer(IntegerType::Int64))
    }

    fn worker() -> StackProtocol {
        StackProtocol::Argument(
            Box::new(Self::integer()),
            Box::new(StackProtocol::Continuation(Box::new(Self::integer()))),
        )
    }

    fn polymorphic_worker(arena: &SpsLowInnerArena) -> (ProtocolParameterId, StackProtocol) {
        arena
            .value_protocols
            .iter()
            .find_map(|(_, value)| {
                let ValueProtocol::Thunk(stack) = value else { return None };
                let StackProtocol::Argument(input, rest) = &**stack else { return None };
                let StackProtocol::Parameter(parameter) = **rest else { return None };
                (**input == Self::integer()).then(|| (parameter, *stack.clone()))
            })
            .expect("the forwarder's worker must retain its computation parameter")
    }

    fn stream(arena: &SpsLowInnerArena) -> CodataProtocolId {
        Self::stream_with(arena, Self::integer())
    }

    fn stream_with(arena: &SpsLowInnerArena, input: ValueProtocol) -> CodataProtocolId {
        arena
            .protocols
            .iter()
            .find_map(|(id, definition)| {
                let [(done, result), (item, rest)] = definition.observations.as_slice() else {
                    return None;
                };
                (item.name.0 == ".item"
                    && item.idx == 1
                    && done.name.0 == ".done"
                    && done.idx == 0
                    && *rest
                        == StackProtocol::Argument(
                            Box::new(input.clone()),
                            Box::new(StackProtocol::Codata(id)),
                        )
                    && *result == StackProtocol::Continuation(Box::new(input.clone())))
                .then_some(id)
            })
            .unwrap_or_else(|| {
                panic!(
                    "the source stream must retain its recursive observation graph: {:?}",
                    arena.protocols
                )
            })
    }

    fn item_jump(arena: &SpsLowInnerArena) -> (CompuId, StackId) {
        arena
            .compus
            .iter()
            .find_map(|(id, computation)| {
                let Computation::Jump(Jump { stack, .. }) = computation else {
                    return None;
                };
                let mut stack = *stack;
                loop {
                    match &arena.stacks[&stack] {
                        | Stack::Tag(Cons(tag, _)) if tag.name.0 == ".item" => {
                            return Some((*id, stack));
                        }
                        | Stack::Arg(Cons(_, rest)) | Stack::Tag(Cons(_, rest)) => stack = *rest,
                        | _ => return None,
                    }
                }
            })
            .expect("the recursive caller must push an item observation")
    }
}

#[test]
fn instantiated_recursive_interfaces_survive_entries_and_returned_thunks() {
    let program = Fixture::source("parameterized-protocols.zy");
    let arena = &program.arena().inner;
    let integer = Fixture::stream(arena);
    let character = Fixture::stream_with(arena, ValueProtocol::Primitive(PrimitiveType::Char));
    assert_ne!(integer, character);
    assert!(
        !arena
            .protocols
            .stacks_agree(&StackProtocol::Codata(integer), &StackProtocol::Codata(character),)
    );
    for (input, expected) in
        [(Fixture::integer(), integer), (ValueProtocol::Primitive(PrimitiveType::Char), character)]
    {
        assert!(
            arena.entry_protocols.iter().any(|(_, entry)| {
                let EntryProtocol::Closure(StackProtocol::Argument(found, rest)) = entry else {
                    return false;
                };
                **found == input
                    && matches!(**rest, StackProtocol::Codata(_))
                    && arena.protocols.stacks_agree(rest, &StackProtocol::Codata(expected))
            }),
            "the specialized consumer must retain its input and recursive remainder"
        );
    }
    assert!(
        arena.entry_protocols.iter().any(|(_, entry)| {
            let EntryProtocol::Continuation(ValueProtocol::Thunk(rest)) = entry else {
                return false;
            };
            matches!(**rest, StackProtocol::Codata(_))
                && arena.protocols.stacks_agree(rest, &StackProtocol::Codata(integer))
        }),
        "a dynamically returned thunk must retain its instantiated interface"
    );
}

#[test]
fn an_instantiated_recursive_transfer_checks_arguments_after_the_first_observation() {
    let (mut arena, root) = Fixture::source("parameterized-protocols.zy").into_parts();
    let (jump, second_argument) = arena
        .inner
        .compus
        .iter()
        .find_map(|(id, compu)| {
            let Computation::Jump(Jump { stack, .. }) = compu else { return None };
            let Stack::Tag(Cons(first, argument)) = &arena.inner.stacks[stack] else { return None };
            let Stack::Arg(Cons(_, rest)) = &arena.inner.stacks[argument] else { return None };
            let Stack::Tag(Cons(second, argument)) = &arena.inner.stacks[rest] else { return None };
            (first.name.0 == ".item" && second == first).then_some((*id, *argument))
        })
        .expect("the returned stream receives two consecutive item observations");
    let Stack::Arg(Cons(value, _)) = arena.inner.stacks[&second_argument] else { unreachable!() };
    assert!(matches!(arena.inner.values[&value], Value::Literal(Literal::Integer(_))));
    arena.inner.values[&value] = Value::Literal(Literal::Char('x'));
    let graph = arena.inner.protocols.clone();
    let SpsLowError::Protocol(ProtocolError::Stack { compu, expected, found }) =
        SpsLowProgram::try_new(arena, root).unwrap_err()
    else {
        panic!("the recursive application must retain its known payload requirement")
    };
    assert_eq!(compu, jump);
    let StackProtocol::Codata(id) = expected else { panic!("expected the instantiated interface") };
    assert_eq!(
        graph.get(id).unwrap().observations[0].1,
        StackProtocol::Continuation(Box::new(Fixture::integer()))
    );
    let StackProtocol::Tag(_, first) = found else { unreachable!() };
    let StackProtocol::Argument(input, rest) = *first else { unreachable!() };
    assert_eq!(*input, Fixture::integer(), "the first observation remains well typed");
    let StackProtocol::Tag(_, second) = *rest else { unreachable!() };
    let StackProtocol::Argument(input, _) = *second else { unreachable!() };
    assert_eq!(*input, ValueProtocol::Primitive(PrimitiveType::Char));
}

#[test]
fn parameterized_and_growing_protocols_execute_on_every_backend() {
    for backend in [
        ExecutionTarget::Interpreter,
        ExecutionTarget::Exe,
        ExecutionTarget::WasmAm,
        ExecutionTarget::WasmSps,
    ] {
        for input in ["", "abc"] {
            SourceProgram::setup("tests/core/parameterized-protocols.zy")
                .with_args([input])
                .test(backend);
        }
        SourceProgram::setup("tests/core/growing-protocols.zy").test(backend);
    }
}

#[test]
fn erased_polymorphic_calls_check_repeated_value_parameters_together() {
    let program = Fixture::source("symbolic-protocols.zy");
    assert!(
        program.arena().inner.entry_protocols.iter().any(|(_, entry)| {
            let EntryProtocol::Closure(StackProtocol::Forall(a, body)) = entry else {
                return false;
            };
            let StackProtocol::Forall(r, body) = &**body else { return false };
            let StackProtocol::Argument(count, body) = &**body else { return false };
            let expected = StackProtocol::Argument(
                Box::new(ValueProtocol::Parameter(*a)),
                Box::new(StackProtocol::Argument(
                    Box::new(ValueProtocol::Parameter(*a)),
                    Box::new(StackProtocol::Argument(
                        Box::new(ValueProtocol::Thunk(Box::new(StackProtocol::Argument(
                            Box::new(ValueProtocol::Parameter(*a)),
                            Box::new(StackProtocol::Argument(
                                Box::new(ValueProtocol::Parameter(*a)),
                                Box::new(StackProtocol::Parameter(*r)),
                            )),
                        )))),
                        Box::new(StackProtocol::Parameter(*r)),
                    )),
                )),
            );
            **count == Fixture::integer() && **body == expected
        }),
        "both binders and all of their related occurrences must survive SPS"
    );
    let (mut arena, root) = program.into_parts();
    let (jump, second) = arena
        .inner
        .compus
        .iter()
        .find_map(|(id, compu)| {
            let Computation::Jump(Jump { stack, .. }) = compu else { return None };
            let Stack::Arg(Cons(_, rest)) = &arena.inner.stacks[stack] else { return None };
            let Stack::Arg(Cons(first, rest)) = &arena.inner.stacks[rest] else { return None };
            let Stack::Arg(Cons(second, _)) = &arena.inner.stacks[rest] else { return None };
            matches!(
                (&arena.inner.values[first], &arena.inner.values[second]),
                (
                    Value::Literal(Literal::Integer(IntegerLiteral::Int64(5))),
                    Value::Literal(Literal::Integer(IntegerLiteral::Int64(7)))
                ),
            )
            .then_some((*id, *second))
        })
        .expect("the generic relay receives two concrete integer arguments");
    arena.inner.values[&second] = Value::Literal(Literal::Char('x'));
    let SpsLowError::Protocol(ProtocolError::Stack { compu, expected, found }) =
        SpsLowProgram::try_new(arena, root).unwrap_err()
    else {
        panic!("conflicting instantiations of the same parameter must reject the transfer")
    };
    assert_eq!(compu, jump);
    assert!(matches!(expected, StackProtocol::Forall(_, _)));
    let StackProtocol::Argument(_, rest) = found else { unreachable!() };
    let StackProtocol::Argument(first, rest) = *rest else { unreachable!() };
    let StackProtocol::Argument(second, _) = *rest else { unreachable!() };
    assert_eq!(*first, Fixture::integer());
    assert_eq!(*second, ValueProtocol::Primitive(PrimitiveType::Char));
}

#[test]
fn low_publication_checks_parameter_kinds_before_transfers() {
    let (mut arena, root) = Fixture::source("symbolic-protocols.zy").into_parts();
    let (parameter, _) = arena
        .inner
        .protocols
        .parameters()
        .find(|(_, kind)| *kind == ProtocolParameterKind::Value)
        .unwrap();
    let block = *arena
        .inner
        .entry_protocols
        .iter()
        .find(|(_, entry)| matches!(entry, EntryProtocol::Closure(_)))
        .unwrap()
        .0;
    arena.inner.entry_protocols[&block] =
        EntryProtocol::Closure(StackProtocol::Parameter(parameter));
    assert_eq!(
        SpsLowProgram::try_new(arena, root).unwrap_err(),
        SpsLowError::Protocol(ProtocolError::Graph(ProtocolGraphError::ParameterKind {
            parameter,
            expected: ProtocolParameterKind::Stack,
            found: ProtocolParameterKind::Value
        },))
    );
}

#[test]
fn polymorphic_calls_choose_independent_instantiations_on_every_backend() {
    for backend in [
        ExecutionTarget::Interpreter,
        ExecutionTarget::Exe,
        ExecutionTarget::WasmAm,
        ExecutionTarget::WasmSps,
    ] {
        for input in ["", "abc"] {
            SourceProgram::setup("tests/core/symbolic-protocols.zy")
                .with_args([input])
                .test(backend);
        }
    }
}

#[test]
fn source_protocols_survive_normalization_and_closure_conversion() {
    let program = Fixture::program();
    let entries = &program.arena().inner.entry_protocols;
    let stream = StackProtocol::Codata(Fixture::stream(&program.arena().inner));
    let (parameter, worker) = Fixture::polymorphic_worker(&program.arena().inner);
    for required in [
        EntryProtocol::Closure(Fixture::worker()),
        EntryProtocol::Continuation(ValueProtocol::Thunk(Box::new(Fixture::worker()))),
        EntryProtocol::Continuation(ValueProtocol::Thunk(Box::new(stream.clone()))),
        EntryProtocol::Closure(StackProtocol::Forall(
            parameter,
            Box::new(StackProtocol::Argument(
                Box::new(Fixture::integer()),
                Box::new(StackProtocol::Argument(
                    Box::new(ValueProtocol::Thunk(Box::new(worker.clone()))),
                    Box::new(worker),
                )),
            )),
        )),
        // Recursion is a reference to an observation graph, with no extent bound.
        EntryProtocol::Closure(StackProtocol::Argument(
            Box::new(Fixture::integer()),
            Box::new(stream),
        )),
    ] {
        assert!(entries.iter().any(|(_, entry)| entry == &required), "missing {required}");
    }
}

#[test]
fn recursive_observations_check_tags_payloads_and_residual_protocols() {
    enum Corruption {
        Name,
        Index,
        Payload,
        Residual,
    }
    for corruption in
        [Corruption::Name, Corruption::Index, Corruption::Payload, Corruption::Residual]
    {
        let (mut arena, root) = Fixture::program().into_parts();
        let stream = Fixture::stream(&arena.inner);
        let (jump, tag_id) = Fixture::item_jump(&arena.inner);
        let Stack::Tag(Cons(_, argument)) = arena.inner.stacks[&tag_id].clone() else {
            unreachable!()
        };
        let Stack::Arg(Cons(value, rest)) = arena.inner.stacks[&argument].clone() else {
            unreachable!()
        };
        match corruption {
            | Corruption::Name | Corruption::Index => {
                let Stack::Tag(Cons(tag, _)) = &mut arena.inner.stacks[&tag_id] else {
                    unreachable!()
                };
                if matches!(corruption, Corruption::Name) {
                    tag.name = DtorName(".missing".into());
                } else {
                    tag.idx += 10;
                }
            }
            | Corruption::Payload => {
                arena.inner.values[&value] = Value::Literal(Literal::Char('x'))
            }
            | Corruption::Residual => {
                let original = arena.inner.stacks[&rest].clone().build(&mut arena, None);
                let extra: ValueId = Triv.build(&mut arena, None);
                arena.inner.stacks[&rest] = Stack::Arg(Cons(extra, original));
            }
        }
        let SpsLowError::Protocol(ProtocolError::Stack { compu, expected, found }) =
            SpsLowProgram::try_new(arena, root).unwrap_err()
        else {
            panic!("a malformed recursive transfer must fail its protocol check")
        };
        assert_eq!(compu, jump);
        assert_eq!(
            expected,
            StackProtocol::Argument(
                Box::new(Fixture::integer()),
                Box::new(StackProtocol::Argument(
                    Box::new(ValueProtocol::Thunk(Box::new(StackProtocol::Codata(stream)))),
                    Box::new(StackProtocol::Codata(stream))
                ))
            )
        );
        let StackProtocol::Argument(_, rest) = found else {
            panic!("expected recursion's counter")
        };
        assert!(
            matches!(*rest, StackProtocol::Argument(_, rest) if matches!(*rest, StackProtocol::Tag(_, _)))
        );
    }
}

#[test]
fn codata_cases_retain_branch_protocols_and_complete_observations() {
    for missing_arm in [false, true] {
        let (mut arena, root) = Fixture::program().into_parts();
        let stream = Fixture::stream(&arena.inner);
        let case = *arena
            .inner
            .case_protocols
            .iter()
            .find(|(_, protocol)| **protocol == StackProtocol::Codata(stream))
            .unwrap()
            .0;
        let Computation::CoCase(SCoMatch { arms, .. }) = &arena.inner.compus[&case] else {
            unreachable!()
        };
        let item = arms.iter().find(|arm| arm.dtor.0.name.0 == ".item").unwrap().tail;
        let Computation::LetArg(LetArg { binder: Cons(parameter, _), .. }) =
            arena.inner.compus[&item]
        else {
            unreachable!()
        };
        let expected = if missing_arm {
            let Computation::CoCase(SCoMatch { arms, .. }) = &mut arena.inner.compus[&case] else {
                unreachable!()
            };
            arms.retain(|arm| arm.dtor.0.name.0 == ".item");
            ProtocolError::Observations {
                compu: case,
                expected: arena
                    .inner
                    .protocols
                    .get(stream)
                    .unwrap()
                    .observations
                    .iter()
                    .map(|(tag, _)| tag.clone())
                    .collect(),
                found: arms.iter().map(|arm| arm.dtor.0.clone()).collect(),
            }
        } else {
            let expected = ValueProtocol::Primitive(PrimitiveType::Char);
            arena.inner.pattern_protocols[&parameter] = expected.clone();
            ProtocolError::Parameter { pattern: parameter, expected, found: Fixture::integer() }
        };
        assert_eq!(
            SpsLowProgram::try_new(arena, root).unwrap_err(),
            SpsLowError::Protocol(expected)
        );
    }
}

#[test]
fn low_publication_rejects_a_missing_protocol_graph() {
    let (mut arena, root) = Fixture::program().into_parts();
    let stream = Fixture::stream(&arena.inner);
    arena.inner.protocols = Default::default();
    assert_eq!(
        SpsLowProgram::try_new(arena, root).unwrap_err(),
        SpsLowError::Protocol(ProtocolError::Graph(ProtocolGraphError::MissingDefinition(stream)))
    );
}

#[test]
fn equivalent_codata_interfaces_share_tag_numbers_across_declaration_orders() {
    let path =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/tests/core/codata-order.zy");
    let program = CommandCompiler::default().lower(&path).unwrap().sps_low;
    let graph = &program.arena().inner.protocols;
    let interfaces = graph
        .iter()
        .filter(|(_, definition)| {
            definition.observations.iter().any(|(tag, _)| tag.name.0 == ".read")
        })
        .collect::<Vec<_>>();
    assert!(interfaces.len() >= 2, "both structural declarations retain their descriptors");
    for (id, definition) in &interfaces {
        assert_eq!(
            definition
                .observations
                .iter()
                .map(|(tag, _)| (tag.idx, tag.name.0.as_str()))
                .collect::<Vec<_>>(),
            [(0, ".read"), (1, ".shift")]
        );
        assert!(
            graph
                .stacks_agree(&StackProtocol::Codata(*id), &StackProtocol::Codata(interfaces[0].0))
        );
    }
    for backend in [
        ExecutionTarget::Interpreter,
        ExecutionTarget::Exe,
        ExecutionTarget::WasmAm,
        ExecutionTarget::WasmSps,
    ] {
        SourceProgram::setup("tests/core/codata-order.zy").test(backend);
    }
}

#[test]
fn a_known_result_must_match_the_installed_continuation() {
    let (mut arena, root) = Fixture::program().into_parts();
    let result = arena
        .inner
        .compus
        .iter()
        .find_map(|(_, compu)| {
            let Computation::Jump(Jump {
                argument: EntryArgument::Continuation { result }, ..
            }) = compu
            else {
                return None;
            };
            matches!(arena.inner.values[result], Value::Primitive(_)).then_some(*result)
        })
        .unwrap();
    arena.inner.values[&result] = Value::Literal(Literal::Char('x'));
    assert_eq!(
        SpsLowProgram::try_new(arena, root).unwrap_err(),
        SpsLowError::Protocol(ProtocolError::Value {
            value: result,
            expected: Fixture::integer(),
            found: ValueProtocol::Primitive(PrimitiveType::Char),
        })
    );
}

#[test]
fn entry_protocols_agree_with_their_roles_and_result_parameters() {
    for change_kind in [false, true] {
        let (mut arena, root) = Fixture::program().into_parts();
        let block = arena
            .inner
            .entry_protocols
            .iter()
            .find_map(|(id, protocol)| {
                (*protocol == EntryProtocol::Continuation(Fixture::integer())).then_some(*id)
            })
            .unwrap();
        let Value::Block(Block { entry: EntryParameters::Continuation { result, .. }, .. }) =
            arena.inner.values[&block]
        else {
            unreachable!()
        };
        let expected = if change_kind {
            arena.inner.entry_protocols[&block] = EntryProtocol::Closure(Fixture::worker());
            ProtocolError::EntryKind {
                block,
                expected: EntryKind::Continuation,
                found: EntryKind::Closure,
            }
        } else {
            let found = ValueProtocol::Primitive(PrimitiveType::Char);
            arena.inner.entry_protocols[&block] = EntryProtocol::Continuation(found.clone());
            ProtocolError::Parameter { pattern: result, expected: Fixture::integer(), found }
        };
        assert_eq!(
            SpsLowProgram::try_new(arena, root).unwrap_err(),
            SpsLowError::Protocol(expected)
        );
    }
}

#[test]
fn an_indirect_worker_rejects_an_incompatible_argument_prefix() {
    let (mut arena, root) = Fixture::program().into_parts();
    let (_, worker) = Fixture::polymorphic_worker(&arena.inner);
    let expected_value = ValueProtocol::Thunk(Box::new(worker.clone()));
    let jump = arena
        .inner
        .compus
        .iter()
        .find_map(|(_, compu)| {
            let Computation::OpenClosure(OpenClosure { package, body, .. }) = compu else {
                return None;
            };
            (arena.inner.value_protocols.get(package) == Some(&expected_value)).then_some(*body)
        })
        .unwrap();
    let Computation::Jump(Jump { stack, .. }) = arena.inner.compus[&jump] else {
        panic!("opening must transfer to the worker")
    };
    let wrong: ValueId = Literal::Char('x').build(&mut arena, None);
    let stack = Cons(wrong, stack).build(&mut arena, None);
    let Computation::Jump(transfer) = &mut arena.inner.compus[&jump] else { unreachable!() };
    transfer.stack = stack;
    let SpsLowError::Protocol(ProtocolError::Stack { compu, expected, found }) =
        SpsLowProgram::try_new(arena, root).unwrap_err()
    else {
        panic!("expected a stack protocol mismatch")
    };
    assert_eq!(compu, jump);
    assert_eq!(expected, worker);
    assert!(
        matches!(found, StackProtocol::Argument(value, _) if *value == ValueProtocol::Primitive(PrimitiveType::Char))
    );
}

#[test]
fn dynamic_argument_stacks_and_returned_workers_execute_on_every_backend() {
    for backend in [
        ExecutionTarget::Interpreter,
        ExecutionTarget::Exe,
        ExecutionTarget::WasmAm,
        ExecutionTarget::WasmSps,
    ] {
        for argument in ["abc".to_owned(), "x".repeat(257)] {
            SourceProgram::setup("tests/core/stack-protocols.zy")
                .with_args([argument])
                .test(backend);
        }
    }
}
