use std::path::PathBuf;
use zydeco_cli::CommandCompiler;
use zydeco_stackir::low::syntax::{Computation, ExternalFunction, Value};
use zydeco_syntax::{BuiltinValueRole, HostOp, IntegerArithmetic, IntegerType, PrimitiveOp};
use zydeco_tests::utils::{ExecutionTarget, SourceCase, SourceProgram};

#[test]
fn discarded_computation_results_keep_their_dependencies() {
    for source in [
        "let z = 7 in do _ <- ret z; ! exit 0",
        "let w = 7 in do _ <- ret (w, 6); ! exit 0",
        "let w = 7 in do _ <- (fn (y : Int) => ret w) 3; ! exit 0",
        "let w = 7 in let t = { ret w } in do _ <- ret t; ! exit 0",
        "let val k (_ : Int) : Int = 0 in let w = 5 in let r = k w in ! exit r",
        "let val k (x : Int) : Int = x in do _ <- ret (k 1); ! exit 0",
    ] {
        SourceCase::assert_accepted(SourceCase::run(source));
        SourceCase::assert_accepted(SourceCase::lower(source));
    }
}

fn fixture(relative: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../lib")
        .join(relative)
        .canonicalize()
        .unwrap()
}

/// Dead top-level definitions, dead recursive definitions, and undemanded host
/// operations are absent from the lowered SPSLow IR, while the operations the
/// program calls survive as primitives or external calls.
#[test]
fn dead_definitions_and_undemanded_operations_are_not_emitted() {
    let backend = CommandCompiler::default()
        .lower(&fixture("tests/demand/prune.zy"))
        .expect("demand fixture must lower");
    let arena = &backend.sps_low.arena().inner;
    let arithmetic = arena
        .values
        .iter()
        .filter_map(|(_, value)| match value {
            | Value::Primitive(primitive) => Some(primitive.operation),
            | _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        arithmetic,
        [PrimitiveOp::Integer(IntegerType::Int, IntegerArithmetic::Add)],
        "only the called addition must survive as a primitive:\n{}",
        backend.render_sps_low()
    );
    let host_calls = arena
        .compus
        .iter()
        .filter_map(|(_, compu)| match compu {
            | Computation::ExternCall(call) => match call.function {
                | ExternalFunction::Host(role) => Some(role),
                | _ => None,
            },
            | _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        host_calls,
        [HostOp::Builtin(BuiltinValueRole::Exit)],
        "only the called exit must survive as a host operation:\n{}",
        backend.render_sps_low()
    );
}

mod prune_program {
    use super::*;

    #[test]
    fn interpreter() {
        SourceProgram::setup("tests/demand/prune.zy").test(ExecutionTarget::Interpreter);
    }

    #[test]
    fn exe() {
        SourceProgram::setup("tests/demand/prune.zy").test(ExecutionTarget::Exe);
    }

    #[test]
    fn wasm_sps() {
        SourceProgram::setup("tests/demand/prune.zy").test(ExecutionTarget::WasmSps);
    }
}
