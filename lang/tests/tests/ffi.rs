use std::path::PathBuf;
use zydeco_cli::{CommandCompiler, CompileError, TargetArchitecture, TargetOs};
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::{
    check_source,
    utils::{CaseError, SourceCase, SourceProgram, TestBackend},
};

check_source!(xxhash_binding, "ffi/xxhash.zy");

struct FfiCase;

impl FfiCase {
    fn path(name: &str) -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/ffi")
            .join(name)
            .canonicalize()
            .unwrap()
    }

    fn declaration(classifier: &str) -> String {
        format!(
            r#"
let foreign = (
  @(ffi(c,library("zyffi_boundary"),symbol("unused"))) : {classifier}
) in
! exit 0
"#
        )
    }

    fn rejected(classifier: &str, message: &str) {
        let error = SourceCase::check(&Self::declaration(classifier)).unwrap_err();
        let CaseError::Compile(CompileError::Rejected(analysis)) = error else {
            panic!("expected a foreign-classifier diagnostic, got {error}")
        };
        assert!(
            analysis.outcome().root().is_none(),
            "rejected imports must not expose a checked root"
        );
        let diagnostics = analysis.outcome().diagnostics().unwrap();
        assert!(
            diagnostics.iter().any(|diagnostic| {
                diagnostic.code == TyckDiagnosticCode::InvalidForeignClassifier
                    && diagnostic.message.contains(message)
            }),
            "{diagnostics:?}"
        );
    }
}

#[test]
fn xxhash_binding_reaches_the_native_c_call_boundary() {
    let backend = CommandCompiler::default().lower(&FfiCase::path("xxhash.zy")).unwrap();

    assert!(backend.render_sps_low().contains("<extern:XXH64/2>"));
    assert!(backend.render_sps_low().contains("<extern:XXH3_64bits/1>"));
    let assembly = backend.emit_amd64(TargetOs::Linux);
    assert!(assembly.contains("extern XXH64"));
    assert!(assembly.contains("call XXH64"));
    assert!(assembly.contains("call XXH3_64bits"));
    assert!(assembly.contains("call zydeco_ffi_borrow_bytes"));
    assert_eq!(
        backend.foreign_libraries().iter().map(|library| library.as_str()).collect::<Vec<_>>(),
        ["xxhash"]
    );
    let macho = backend.emit_amd64(TargetOs::Macos);
    assert!(macho.contains("call _XXH64"));
    assert!(macho.contains("call _XXH3_64bits"));
}

#[test]
fn unsupported_backends_report_the_native_import() {
    let backend = CommandCompiler::default().lower(&FfiCase::path("xxhash.zy")).unwrap();

    assert!(matches!(
        backend.emit_llvm(TargetArchitecture::Aarch64, TargetOs::Macos),
        Err(CompileError::ForeignImportUnsupported { backend: "LLVM", .. })
    ));
    let abstract_machine = backend.emit_wasm_am().unwrap_err();
    assert!(abstract_machine.to_string().contains("XXH"));
    assert!(matches!(abstract_machine, CompileError::WasmAm(_)));
    let stack_passing = backend.emit_wasm_sps().unwrap_err();
    assert!(stack_passing.to_string().contains("XXH"));
    assert!(matches!(stack_passing, CompileError::WasmSps(_)));
}

#[test]
fn accepts_compositional_classifiers_without_loading_a_library() {
    for classifier in [
        "Thk (Ret UInt64)",
        "Thk (UInt64 -> Ret UInt64)",
        "Thk (Bytes -> Ret UInt64)",
        "Thk (UInt64 -> Bytes -> Bytes -> UInt64 -> Ret UInt64)",
        "Thk (Bytes -> Bytes -> Bytes -> Ret UInt64)",
        "Thk (UInt64 -> UInt64 -> UInt64 -> UInt64 -> UInt64 -> UInt64 -> Ret UInt64)",
    ] {
        SourceCase::check(&FfiCase::declaration(classifier)).unwrap();
    }
}

#[test]
fn rejects_unsupported_classifier_components_with_specific_diagnostics() {
    FfiCase::rejected("UInt64", "requires a thunk");
    FfiCase::rejected("Thk (UInt32 -> Ret UInt64)", "argument 1");
    FfiCase::rejected("Thk (Bytes -> UInt32 -> Ret UInt64)", "argument 2");
    FfiCase::rejected("Thk (Bytes -> Ret Bytes)", "only a `UInt64` result");
    FfiCase::rejected("Thk (UInt64 -> OS)", "must end in `Ret UInt64`");
    FfiCase::rejected("Thk (Bytes -> Bytes -> Bytes -> UInt64 -> Ret UInt64)", "needs 7");
    FfiCase::rejected("Thk (Bytes -> Bytes -> Bytes -> Bytes -> Ret UInt64)", "needs 8");
}

#[test]
fn boundary_fixture_lowers_without_xxhash_specific_shapes() {
    let backend = CommandCompiler::default().lower(&FfiCase::path("boundary.zy")).unwrap();
    let assembly = backend.emit_amd64(TargetOs::Linux);
    for name in ["zero", "echo", "bytes", "mixed", "three_bytes", "six"] {
        assert!(assembly.contains(&format!("call zyffi_{name}")));
    }
    assert_eq!(backend.foreign_libraries()[0].as_str(), "zyffi_boundary");
}

#[test]
#[ignore = "requires libxxhash in the platform dynamic-library search path"]
fn calls_the_installed_xxhash_library() {
    SourceProgram::setup("tests/ffi/xxhash.zy").test(TestBackend::Interpreter);
}

#[test]
#[cfg(unix)]
#[ignore = "requires a C compiler for x86-64, NASM, and the x86-64 Rust runtime target"]
fn native_c_boundary_executes_the_compositional_protocol() {
    use std::process::Command;
    use zydeco_cli::BuildOptions;

    let directory = tempfile::tempdir().unwrap();
    let operating_system = TargetOs::host().unwrap();
    let library = directory.path().join(match operating_system {
        | TargetOs::Macos => "libzyffi_boundary.dylib",
        | TargetOs::Linux => "libzyffi_boundary.so",
    });
    let mut cc = Command::new("cc");
    match operating_system {
        | TargetOs::Macos => {
            cc.args(["-arch", "x86_64", "-dynamiclib"])
                .arg(format!("-Wl,-install_name,{}", library.display()));
        }
        | TargetOs::Linux => {
            cc.args(["-m64", "-shared", "-fPIC"]);
        }
    }
    let output = cc.arg(FfiCase::path("boundary.c")).arg("-o").arg(&library).output().unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    let options = BuildOptions::new(
        directory.path().to_path_buf(),
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../runtime"),
        TargetArchitecture::X86_64,
        operating_system,
    );
    let backend = CommandCompiler::default().lower(&FfiCase::path("boundary.zy")).unwrap();
    let executable = options
        .link_amd64(
            "ffi_program",
            &backend.emit_amd64(operating_system),
            &backend.foreign_libraries(),
        )
        .unwrap();
    let output = Command::new(executable.path())
        .env("LD_LIBRARY_PATH", directory.path())
        .env("DYLD_LIBRARY_PATH", directory.path())
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
}
