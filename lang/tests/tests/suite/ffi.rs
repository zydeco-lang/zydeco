use std::path::PathBuf;
use zydeco_cli::{CommandCompiler, CompileError, TargetArchitecture, TargetOs};
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::{
    check_source,
    utils::{CaseError, SourceCase},
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
fn foreign_imports_supply_only_their_own_implementation_holes() {
    let foreign = "@(ffi(c, library(\"zyffi_boundary\"), symbol(\"zyffi_echo\")))";
    for (implementation, argument, executable) in
        [(foreign, "0", true), ("_", "0", false), (foreign, "_", false)]
    {
        let source = format!(
            "let foreign : Thk (UInt -> Ret UInt) = {implementation} in \
             do _ <- ! foreign {argument}; ! exit 0"
        );
        SourceCase::check(&source).unwrap();
        let result = SourceCase::lower(&source);
        if executable {
            result.unwrap();
        } else {
            assert!(
                matches!(
                    result,
                    Err(CaseError::Compile(CompileError::Executable(
                        zydeco_session::ExecutableError::Hole(_)
                    )))
                ),
                "{source}: {result:?}"
            );
        }
    }
}

#[test]
fn xxhash_binding_reaches_the_native_c_call_boundary() {
    let backend = CommandCompiler::default().lower(&FfiCase::path("xxhash.zy")).unwrap();

    assert!(backend.render_sps_low().contains("<extern:XXH32/3>"));
    for arch in [TargetArchitecture::X86_64, TargetArchitecture::Aarch64] {
        for os in [TargetOs::Linux, TargetOs::Macos] {
            let artifact = backend.emit_native(arch, os).unwrap();
            assert!(!artifact.object.is_empty());
            assert_eq!(
                artifact
                    .foreign_libraries
                    .iter()
                    .map(|library| library.as_str())
                    .collect::<Vec<_>>(),
                ["xxhash"]
            );
            assert!(
                artifact
                    .foreign_imports
                    .iter()
                    .any(|import| import.target.symbol.as_str() == "XXH32")
            );
        }
    }
}

#[test]
fn the_wasm_backend_reports_the_native_import() {
    let backend = CommandCompiler::default().lower(&FfiCase::path("xxhash.zy")).unwrap();

    let stack_passing = backend.emit_wasm_sps().unwrap_err();
    assert!(stack_passing.to_string().contains("XXH"));
    assert!(matches!(stack_passing, CompileError::WasmSps(_)));
}

#[test]
fn accepts_compositional_classifiers_without_loading_a_library() {
    for classifier in [
        "Thk (Ret UInt)",
        "Thk (Ret Unit)",
        "Thk (Int8 -> Int16 -> Int32 -> Ret Int)",
        "Thk (UInt8 -> UInt16 -> UInt32 -> Ret Unit)",
        "Thk (UInt -> Ret UInt)",
        "Thk (Addr -> Int -> Ret UInt)",
        "Thk (UInt -> Addr -> Int -> Addr -> Int -> UInt -> Ret UInt)",
        "Thk (Addr -> Addr -> Addr -> Ret UInt)",
        "Thk (UInt -> UInt -> UInt -> UInt -> UInt -> UInt -> Ret UInt)",
    ] {
        SourceCase::check(&FfiCase::declaration(classifier)).unwrap();
    }
    for integer in
        ["Int8", "Int16", "Int32", "Int64", "Int", "UInt8", "UInt16", "UInt32", "UInt64", "UInt"]
    {
        SourceCase::check(&FfiCase::declaration(&format!("Thk ({integer} -> Ret {integer})")))
            .unwrap();
    }
}

#[test]
fn rejects_unsupported_classifier_components_with_specific_diagnostics() {
    FfiCase::rejected("Thk ((UInt8 * UInt32) -> Ret Unit)", "argument 1");
    FfiCase::rejected("UInt", "requires a thunk");
    FfiCase::rejected("Thk (Float32 -> Ret UInt)", "argument 1");
    FfiCase::rejected("Thk (Addr -> String -> Ret UInt)", "argument 2");
    FfiCase::rejected("Thk (Addr -> Ret Addr)", "supported integer or `Unit`");
    FfiCase::rejected("Thk (UInt -> OS)", "must end in `Ret B`");
    FfiCase::rejected(
        "Thk (Addr -> Int -> Addr -> Int -> Addr -> Int -> UInt -> Ret UInt)",
        "needs 7",
    );
    FfiCase::rejected(
        "Thk (Addr -> Int -> Addr -> Int -> Addr -> Int -> Addr -> Int -> Ret UInt)",
        "needs 8",
    );
}

#[test]
fn boundary_fixture_lowers_without_xxhash_specific_shapes() {
    let backend = CommandCompiler::default().lower(&FfiCase::path("boundary.zy")).unwrap();
    for (arch, os) in [
        (TargetArchitecture::X86_64, TargetOs::Linux),
        (TargetArchitecture::Aarch64, TargetOs::Macos),
    ] {
        let artifact = backend.emit_native(arch, os).unwrap();
        for name in ["zero", "echo", "bytes", "mixed", "three_bytes", "six"] {
            let expected = format!("zyffi_{name}");
            assert!(
                artifact
                    .foreign_imports
                    .iter()
                    .any(|import| import.target.symbol.as_str() == expected),
                "missing {expected} for {arch}/{os:?}"
            );
        }
        assert_eq!(artifact.foreign_libraries[0].as_str(), "zyffi_boundary");
    }
}

#[test]
#[cfg(feature = "system-xxhash")]
fn calls_the_installed_xxhash_library() {
    use zydeco_tests::utils::{ExecutionTarget, SourceProgram};

    SourceProgram::setup("tests/ffi/xxhash.zy").test(ExecutionTarget::Interpreter);
}

#[test]
#[cfg(any(target_os = "linux", target_os = "macos"))]
fn native_c_boundary_executes_the_compositional_protocol() {
    use std::process::Command;
    use zydeco_cli::BuildOptions;

    let directory = tempfile::tempdir().unwrap();
    let arch = TargetArchitecture::host().unwrap();
    let operating_system = TargetOs::host().unwrap();
    let library = directory.path().join(match operating_system {
        | TargetOs::Macos => "libzyffi_boundary.dylib",
        | TargetOs::Linux => "libzyffi_boundary.so",
    });
    let mut cc = Command::new("cc");
    match (arch, operating_system) {
        | (TargetArchitecture::Aarch64, TargetOs::Macos) => {
            cc.args(["-arch", "arm64", "-dynamiclib"])
                .arg(format!("-Wl,-install_name,{}", library.display()));
        }
        | (TargetArchitecture::X86_64, TargetOs::Macos) => {
            cc.args(["-arch", "x86_64", "-dynamiclib"])
                .arg(format!("-Wl,-install_name,{}", library.display()));
        }
        | (TargetArchitecture::X86_64, TargetOs::Linux) => {
            cc.args(["-m64", "-shared", "-fPIC"]);
        }
        | (TargetArchitecture::Aarch64, TargetOs::Linux) => {
            cc.args(["-shared", "-fPIC"]);
        }
    }
    let output = cc.arg(FfiCase::path("boundary.c")).arg("-o").arg(&library).output().unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    let options = BuildOptions::new(
        directory.path().to_path_buf(),
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../runtime"),
        arch,
        operating_system,
    );
    for fixture in ["boundary.zy", "static-layout.zy", "mutable-output.zy", "integers.zy"] {
        let backend = CommandCompiler::default().lower(&FfiCase::path(fixture)).unwrap();
        let native = backend.emit_native(arch, operating_system).unwrap();
        let executable =
            options.link_native("ffi_program", &native.object, &native.foreign_libraries).unwrap();
        let output = Command::new(executable.path())
            .env("LD_LIBRARY_PATH", directory.path())
            .env("DYLD_LIBRARY_PATH", directory.path())
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{fixture}: {}\n{}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        );
    }
    for (symbol, integer) in [
        ("zyffi_int_below_range", "Int"),
        ("zyffi_int_above_range", "Int"),
        ("zyffi_uint_above_range", "UInt"),
    ] {
        let source = directory.path().join("rejected.zy");
        let builtin = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/std/builtin.zy");
        std::fs::write(
            &source,
            format!(
                r#"
param (/Thk; /Ret; /{integer}; /process) : @(import({builtin:?})) in
let foreign : Thk (Ret {integer}) = @(ffi(c, library("zyffi_boundary"), symbol("{symbol}"))) in
do _ <- ! foreign;
! process/exit 42
"#
            ),
        )
        .unwrap();
        let native = CommandCompiler::default()
            .lower(&source)
            .unwrap()
            .emit_native(arch, operating_system)
            .unwrap();
        let executable =
            options.link_native("ffi_rejected", &native.object, &native.foreign_libraries).unwrap();
        let output = Command::new(executable.path())
            .env("LD_LIBRARY_PATH", directory.path())
            .env("DYLD_LIBRARY_PATH", directory.path())
            .output()
            .unwrap();
        assert_eq!(output.status.code(), Some(1));
        assert!(
            String::from_utf8_lossy(&output.stderr)
                .contains("integer exceeds the tagged payload range")
        );
        assert!(output.stdout.is_empty());
    }
}
