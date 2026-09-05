use std::path::PathBuf;
use zydeco_cli::CommandCompiler;
use zydeco_tests::utils::{SourceProgram, TestBackend};

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
