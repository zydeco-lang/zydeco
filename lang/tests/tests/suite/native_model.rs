use std::{
    path::PathBuf,
    process::{Command, Stdio},
};
use zydeco_cli::{BuildOptions, CommandCompiler, NativeError, TargetArchitecture, TargetOs};
use zydeco_machine::clif::CLIF_ENTRY_SYMBOL;

#[test]
fn packaged_model_links_and_a_mismatched_model_cannot_publish_an_executable() {
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let directory = tempfile::tempdir().unwrap();
    let arch = TargetArchitecture::host().unwrap();
    let operating_system = TargetOs::host().unwrap();
    let options = BuildOptions::new(
        directory.path().to_path_buf(),
        workspace.join("runtime"),
        arch,
        operating_system,
    );
    // Exercises captured caller values and all three host-resumption arities.
    let backend = CommandCompiler::default()
        .lower(&workspace.join("lib/tests/builtin/host-runtime.zy"))
        .unwrap();
    let zydeco_cli::NativeArtifact { object, foreign_libraries: libraries, .. } =
        backend.emit_native(arch, operating_system).unwrap();
    let executable = options.link_native("matching", &object, &libraries).unwrap();
    let output = Command::new(executable.path()).stdin(Stdio::null()).output().unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));

    // Generated objects are the only caller of many runtime exports. Optimized
    // builds must retain these symbols, including when Rust would otherwise use
    // local ThinLTO. Build the already packaged program without mutating process
    // environment shared with the other integration tests.
    let target = match (arch, operating_system) {
        | (TargetArchitecture::X86_64, TargetOs::Linux) => "x86_64-unknown-linux-gnu",
        | (TargetArchitecture::X86_64, TargetOs::Macos) => "x86_64-apple-darwin",
        | (TargetArchitecture::Aarch64, TargetOs::Linux) => "aarch64-unknown-linux-gnu",
        | (TargetArchitecture::Aarch64, TargetOs::Macos) => "aarch64-apple-darwin",
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

    let incompatible = zydeco_cranelift::Emitter::new(
        &backend.sps_low,
        zydeco_cranelift::NativeTarget::host().unwrap(),
    )
    .with_entry(zydeco_cranelift::NativeEntry::Unit(
        zydeco_syntax::ForeignSymbolName::parse("zydeco_clif_entry_incompatible").unwrap(),
    ))
    .run()
    .unwrap()
    .object;
    let error = options.link_native("mismatched", &incompatible, &libraries).unwrap_err();
    let NativeError::ToolFailed { stderr, .. } = error else {
        panic!("expected the linker to reject the model identity, got {error}");
    };
    assert!(stderr.contains(CLIF_ENTRY_SYMBOL), "{stderr}");
    assert!(!directory.path().join("mismatched.exe").exists());
    assert!(executable.path().exists(), "a failed build must preserve earlier executables");
}

#[test]
fn returning_calls_retain_slots_without_heap_capture_products() {
    let source =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/tests/builtin/host-return.zy");
    let backend = CommandCompiler::default().lower(&source).unwrap();
    // Portable conversion spells out the capture environment before native emission
    // elides heap capture products for host returns.
    assert!(backend.render_sps_low().contains("pack-continuation("));
    let artifact = backend
        .emit_native(TargetArchitecture::host().unwrap(), TargetOs::host().unwrap())
        .unwrap();
    assert!(!artifact.object.is_empty());
    zydeco_tests::utils::SourceProgram::setup("tests/builtin/host-return.zy")
        .test(zydeco_tests::utils::ExecutionTarget::Exe);
}

#[test]
fn tail_calls_and_an_escaping_closure_survive_frame_reuse() {
    use zydeco_tests::utils::{ExecutionTarget, SourceProgram};
    for backend in [ExecutionTarget::Interpreter, ExecutionTarget::Exe] {
        SourceProgram::setup("tests/core/native-frames.zy").test(backend);
    }
}
