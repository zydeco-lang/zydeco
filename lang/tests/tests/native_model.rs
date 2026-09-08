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
    let assembly = backend.emit_amd64(operating_system);
    let libraries = backend.foreign_libraries();
    let executable = options.link_amd64("matching", &assembly, &libraries).unwrap();
    let output = Command::new(executable.path()).stdin(Stdio::null()).output().unwrap();
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
    let assembly = backend.emit_amd64(TargetOs::host().unwrap());
    assert!(assembly.contains("retain activation slots for return"));
    assert!(!assembly.contains("pack_product"), "{assembly}");
    assert!(!assembly.contains("call zydeco_alloc_scanned"), "{assembly}");
}

#[test]
fn tail_calls_and_an_escaping_closure_survive_frame_reuse() {
    use zydeco_tests::utils::{SourceProgram, TestBackend};
    for backend in [TestBackend::Interpreter, TestBackend::Amd64] {
        SourceProgram::setup("tests/core/native-frames.zy").test(backend);
    }
}
