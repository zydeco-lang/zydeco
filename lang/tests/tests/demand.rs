use std::path::PathBuf;
use zydeco_cli::CommandCompiler;
use zydeco_tests::utils::{SourceCase, SourceProgram, TestBackend};

#[test]
fn discarded_computation_results_keep_their_dependencies() {
    for source in [
        "let z = 7 in do _ <- ret z; ! exit 0",
        "let w = 7 in do _ <- ret (w, 6); ! exit 0",
        "let w = 7 in do _ <- (fn (y : Int64) => ret w) 3; ! exit 0",
        "let w = 7 in let t = { ret w } in do _ <- ret t; ! exit 0",
        "let val k (_ : Int64) : Int64 = 0 in let w = 5 in let r = k w in ! exit r",
        "let val k (x : Int64) : Int64 = x in do _ <- ret (k 1); ! exit 0",
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

fn rendered_assembly(relative: &str) -> String {
    CommandCompiler::default()
        .lower(&fixture(relative))
        .expect("demand fixture must lower")
        .render_assembly()
}

/// Dead top-level definitions, dead recursive definitions, and undemanded host
/// operations are absent from the emitted assembly, while the operations the
/// program calls survive with their extern declarations.
#[test]
fn dead_definitions_and_undemanded_operations_are_not_emitted() {
    let assembly = rendered_assembly("tests/demand/prune.zy");
    assert!(
        assembly.contains("extern:int64_add"),
        "the called operation must survive elimination:\n{assembly}"
    );
    assert!(
        assembly.contains("extern:exit"),
        "the exit operation must survive elimination:\n{assembly}"
    );
    assert!(
        !assembly.contains("extern:int64_mul"),
        "an operation referenced only by a dead definition must be pruned:\n{assembly}"
    );
    assert!(
        !assembly.contains("extern:write_line"),
        "an operation the program never references must be pruned:\n{assembly}"
    );
}

mod prune_program {
    use super::*;

    #[test]
    fn interpreter() {
        SourceProgram::setup("tests/demand/prune.zy").test(TestBackend::Interpreter);
    }

    #[test]
    fn wasm_sps() {
        SourceProgram::setup("tests/demand/prune.zy").test(TestBackend::WasmSps);
    }

    #[test]
    fn wasm_am() {
        SourceProgram::setup("tests/demand/prune.zy").test(TestBackend::WasmAm);
    }
}
