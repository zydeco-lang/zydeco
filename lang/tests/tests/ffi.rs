use std::path::PathBuf;
use zydeco_cli::{CommandCompiler, CompileError, TargetArchitecture, TargetOs};
use zydeco_tests::{
    check_source,
    utils::{SourceCase, SourceProgram, TestBackend},
};

check_source!(xxhash_binding, "ffi/xxhash.zy");

fn fixture() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../lib/tests/ffi/xxhash.zy")
        .canonicalize()
        .unwrap()
}

#[test]
fn xxhash_binding_reaches_the_native_c_call_boundary() {
    let backend = CommandCompiler::default().lower(&fixture()).unwrap();

    assert!(backend.render_sps_low().contains("<extern:XXH64/2>"));
    let assembly = backend.emit_amd64(TargetOs::Linux);
    assert!(assembly.contains("extern XXH64"));
    assert!(assembly.contains("call XXH64"));
    assert!(assembly.contains("call zydeco_ffi_borrow_bytes"));
    assert_eq!(
        backend.foreign_libraries().iter().map(|library| library.as_str()).collect::<Vec<_>>(),
        ["xxhash"]
    );
}

#[test]
fn unsupported_backends_report_the_native_import() {
    let backend = CommandCompiler::default().lower(&fixture()).unwrap();

    assert!(matches!(
        backend.emit_llvm(TargetArchitecture::Aarch64, TargetOs::Macos),
        Err(CompileError::ForeignImportUnsupported { backend: "LLVM", .. })
    ));
    let abstract_machine = backend.emit_wasm_am().unwrap_err();
    assert!(abstract_machine.to_string().contains("XXH64"));
    assert!(matches!(abstract_machine, CompileError::WasmAm(_)));
    let stack_passing = backend.emit_wasm_sps().unwrap_err();
    assert!(stack_passing.to_string().contains("XXH64"));
    assert!(matches!(stack_passing, CompileError::WasmSps(_)));
}

#[test]
fn rejects_a_classifier_without_the_borrowed_bytes_protocol() {
    let error = SourceCase::check(
        r#"
let invalid = (
  @[ffi(c, library("xxhash"), symbol("XXH64"))] _
  : Thk (UInt64 -> Ret UInt64)
) in
! exit 0
"#,
    )
    .unwrap_err();

    assert!(error.is_type_error());
}

#[test]
#[ignore = "requires libxxhash in the platform dynamic-library search path"]
fn calls_the_installed_xxhash_library() {
    SourceProgram::setup("tests/ffi/xxhash.zy").test(TestBackend::Interpreter);
}
