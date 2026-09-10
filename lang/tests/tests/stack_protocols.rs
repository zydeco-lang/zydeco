use std::path::PathBuf;
use zydeco_cli::CommandCompiler;
use zydeco_stackir::{
    SpsLowError, SpsLowProgram,
    low::{protocols::ProtocolError, syntax::*},
    protocol::{StackProtocol, ValueProtocol},
};
use zydeco_tests::utils::{SourceProgram, TestBackend};

struct Fixture;

impl Fixture {
    fn program() -> SpsLowProgram {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/core/stack-protocols.zy");
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

    fn polymorphic_worker() -> StackProtocol {
        StackProtocol::Argument(Box::new(Self::integer()), Box::new(StackProtocol::Unknown))
    }
}

#[test]
fn source_protocols_survive_normalization_and_closure_conversion() {
    let program = Fixture::program();
    let entries = &program.arena().inner.entry_protocols;
    for required in [
        EntryProtocol::Closure(Fixture::worker()),
        EntryProtocol::Continuation(ValueProtocol::Thunk(Box::new(Fixture::worker()))),
        EntryProtocol::Closure(StackProtocol::Argument(
            Box::new(Fixture::integer()),
            Box::new(StackProtocol::Argument(
                Box::new(ValueProtocol::Thunk(Box::new(Fixture::polymorphic_worker()))),
                Box::new(Fixture::polymorphic_worker()),
            )),
        )),
        // Recursive codata keeps an opaque remainder, not a guessed stack extent.
        EntryProtocol::Closure(StackProtocol::Argument(
            Box::new(Fixture::integer()),
            Box::new(StackProtocol::Unknown),
        )),
    ] {
        assert!(entries.iter().any(|(_, entry)| entry == &required), "missing {required}");
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
    let expected_value = ValueProtocol::Thunk(Box::new(Fixture::polymorphic_worker()));
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
    assert_eq!(expected, Fixture::polymorphic_worker());
    assert!(
        matches!(found, StackProtocol::Argument(value, _) if *value == ValueProtocol::Primitive(PrimitiveType::Char))
    );
}

#[test]
fn dynamic_argument_stacks_and_returned_workers_execute_on_every_backend() {
    for backend in
        [TestBackend::Interpreter, TestBackend::Amd64, TestBackend::WasmAm, TestBackend::WasmSps]
    {
        for argument in ["abc".to_owned(), "x".repeat(257)] {
            SourceProgram::setup("tests/core/stack-protocols.zy")
                .with_args([argument])
                .test(backend);
        }
    }
}
