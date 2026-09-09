use zydeco_cli::RepresentationStrategy;
use zydeco_tests::utils::{SourceProgram, TestBackend};

#[test]
fn policies_preserve_native_execution_and_live_wide_captures() {
    for &strategy in RepresentationStrategy::ALL {
        SourceProgram::setup("tests/core/representation-policies.zy")
            .with_args(["alpha"])
            .with_representation(strategy)
            .test_io(TestBackend::Amd64, "", 0);
    }
}

#[test]
fn policies_preserve_am_wasm_execution() {
    for &strategy in RepresentationStrategy::ALL {
        SourceProgram::setup("tests/core/representation-policies.zy")
            .with_args(["alpha"])
            .with_representation(strategy)
            .test_io(TestBackend::WasmAm, "", 0);
    }
}

#[test]
fn interpreter_agrees_with_the_policy_experiment() {
    SourceProgram::setup("tests/core/representation-policies.zy").test_io(
        TestBackend::Interpreter,
        "",
        0,
    );
}
