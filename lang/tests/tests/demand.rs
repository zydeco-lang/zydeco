use std::path::PathBuf;
use zydeco_assembly::syntax::{
    BuiltinValueRole, Extern, Instruction, IntegerArithmetic, IntegerType, PrimitiveOp, Program,
    Terminator, scalar::ScalarStep,
};
use zydeco_cli::CommandCompiler;
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
/// operations are absent from the emitted assembly, while the operations the
/// program calls survive as scalar arithmetic or external calls.
#[test]
fn dead_definitions_and_undemanded_operations_are_not_emitted() {
    let backend = CommandCompiler::default()
        .lower(&fixture("tests/demand/prune.zy"))
        .expect("demand fixture must lower");
    let programs = &backend.assembly().arena().programs;
    let arithmetic = programs
        .iter()
        .filter_map(|(_, program)| match program {
            | Program::Instruction(Instruction::Scalar(scalar), _) => Some(scalar.region()),
            | _ => None,
        })
        .flat_map(|region| &region.steps)
        .filter_map(|step| match step {
            | ScalarStep::Arithmetic { operation, .. } => Some(*operation),
            | _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        arithmetic,
        [PrimitiveOp::Integer(IntegerType::Int, IntegerArithmetic::Add)],
        "only the called addition must survive as inline arithmetic:\n{}",
        backend.render_assembly()
    );
    let host_calls = programs
        .iter()
        .filter_map(|(_, program)| match program {
            | Program::Terminator(Terminator::Extern(Extern::Host { role, .. })) => Some(*role),
            | _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        host_calls,
        [BuiltinValueRole::Exit],
        "only the called exit must survive as a host operation:\n{}",
        backend.render_assembly()
    );
}

mod prune_program {
    use super::*;

    #[test]
    fn interpreter() {
        SourceProgram::setup("tests/demand/prune.zy").test(ExecutionTarget::Interpreter);
    }

    #[test]
    fn wasm_sps() {
        SourceProgram::setup("tests/demand/prune.zy").test(ExecutionTarget::WasmSps);
    }

    #[test]
    fn wasm_am() {
        SourceProgram::setup("tests/demand/prune.zy").test(ExecutionTarget::WasmAm);
    }
}
