use std::{
    path::PathBuf,
    process::{Command, Stdio},
};
use zydeco_cli::{BuildOptions, CommandCompiler, NativeError, TargetArchitecture, TargetOs};
use zydeco_machine::native::ENTRY_SYMBOL;

#[test]
fn packaged_model_links_and_a_mismatched_model_cannot_publish_an_executable() {
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let directory = tempfile::tempdir().unwrap();
    let operating_system = TargetOs::host().unwrap();
    let options = BuildOptions::new(
        directory.path().to_path_buf(),
        workspace.join("runtime"),
        TargetArchitecture::X86_64,
        operating_system,
    );
    // Exercises captured caller values and all three host-resumption arities.
    let backend = CommandCompiler::default()
        .lower(&workspace.join("lib/tests/builtin/host-runtime.zy"))
        .unwrap();
    let zydeco_cli::Amd64Artifact { assembly, foreign_libraries: libraries, .. } =
        backend.emit_amd64(operating_system);
    let executable = options.link_amd64("matching", &assembly, &libraries).unwrap();
    let output = Command::new(executable.path()).stdin(Stdio::null()).output().unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));

    // Generated assembly is the only caller of many runtime exports. Optimized
    // builds must retain these symbols, including when Rust would otherwise use
    // local ThinLTO. Build the already packaged program without mutating process
    // environment shared with the other integration tests.
    let target = match operating_system {
        | TargetOs::Linux => "x86_64-unknown-linux-gnu",
        | TargetOs::Macos => "x86_64-apple-darwin",
    };
    let mut cargo = Command::new("cargo");
    cargo
        .args(["build", "--release", "--target", target, "--manifest-path"])
        .arg(directory.path().join("Cargo.toml"))
        .env("ZYDECO_STATIC_LIB", "zymatching")
        .env("ZYDECO_LIB_DIR", directory.path())
        .env("CARGO_TARGET_DIR", directory.path().join("target"));
    if operating_system == TargetOs::Macos {
        cargo.env("RUSTFLAGS", "-C panic=abort");
    }
    let output = cargo.output().unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    let optimized = directory.path().join("target").join(target).join("release/main");
    let output = Command::new(optimized).stdin(Stdio::null()).output().unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));

    let incompatible = assembly.replace(ENTRY_SYMBOL, "zydeco_entry_frames_incompatible");
    let error = options.link_amd64("mismatched", &incompatible, &libraries).unwrap_err();
    let NativeError::ToolFailed { stderr, .. } = error else {
        panic!("expected the linker to reject the model identity, got {error}");
    };
    assert!(stderr.contains(ENTRY_SYMBOL), "{stderr}");
    assert!(!directory.path().join("mismatched.exe").exists());
    assert!(executable.path().exists(), "a failed build must preserve earlier executables");
}

#[test]
fn returning_calls_retain_slots_without_heap_capture_products() {
    let source =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/tests/builtin/host-return.zy");
    let backend = CommandCompiler::default().lower(&source).unwrap();
    // Portable conversion still spells out the capture environment. Native
    // preparation must remove its allocation, even for nonempty captures.
    assert!(backend.render_sps_low().contains("pack-continuation("));
    let assembly = backend.emit_amd64(TargetOs::host().unwrap()).assembly;
    assert!(assembly.contains("retain activation slots for return"));
    assert!(!assembly.contains("pack_product"), "{assembly}");
    assert!(!assembly.contains("call zydeco_alloc_scanned"), "{assembly}");
}

#[test]
fn tail_calls_and_an_escaping_closure_survive_frame_reuse() {
    use zydeco_tests::utils::{ExecutionTarget, SourceProgram};
    for backend in [ExecutionTarget::Interpreter, ExecutionTarget::Exe] {
        SourceProgram::setup("tests/core/native-frames.zy").test(backend);
    }
}

#[test]
fn compact_environments_execute_the_same_generated_actions() {
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let directory = tempfile::tempdir().unwrap();
    let runtime = directory.path().join("runtime");
    std::fs::create_dir(&runtime).unwrap();
    for entry in std::fs::read_dir(workspace.join("runtime")).unwrap() {
        let path = entry.unwrap().path();
        if path.is_file() {
            std::fs::copy(&path, runtime.join(path.file_name().unwrap())).unwrap();
        }
    }
    let manifest = runtime.join("Cargo.toml");
    let source = std::fs::read_to_string(&manifest).unwrap();
    assert_eq!(source.matches("default = [\"process-entry\"]").count(), 1);
    std::fs::write(
        manifest,
        source.replace(
            "default = [\"process-entry\"]",
            "default = [\"process-entry\", \"compact-environments\"]",
        ),
    )
    .unwrap();
    let operating_system = TargetOs::host().unwrap();
    let options = BuildOptions::new(
        directory.path().join("build"),
        runtime,
        TargetArchitecture::X86_64,
        operating_system,
    );
    for source in [
        "builtin/host-runtime.zy",
        "core/native-frames.zy",
        "core/gc-stress.zy",
        "core/runtime-package-callback.zy",
        "delimcc/reset-shift-k.zy",
    ] {
        let path = workspace.join("lib/tests").join(source);
        let backend = CommandCompiler::default().lower(&path).unwrap();
        let native = backend.emit_amd64(operating_system);
        let executable =
            options.link_amd64("compact", &native.assembly, &native.foreign_libraries).unwrap();
        let output = Command::new(executable.path()).stdin(Stdio::null()).output().unwrap();
        let expected = CommandCompiler::default().test_io(&path, &[], "").unwrap();
        assert_eq!(output.status.code(), Some(expected.code), "{source}: {output:?}");
        assert_eq!(String::from_utf8(output.stdout).unwrap(), expected.output, "{source}");
        assert!(output.stderr.is_empty(), "{source}: {:?}", output.stderr);
    }
}
