use std::{path::PathBuf, process::Command};

#[test]
fn a_root_term_builds_without_project_configuration() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../lib/tests/builtin/exit.zy");
    let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .arg("build")
        .arg(root)
        .args(["--target", "zir"])
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "source build failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(String::from_utf8_lossy(&output.stdout).contains("[root]"));
}

#[test]
fn the_wasm_targets_write_distinct_valid_core_modules() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../lib/tests/compile/fact.zy");
    let build = tempfile::tempdir().unwrap();
    let build_target = |target: &str, filename: &str| {
        let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .arg("build")
            .arg(&root)
            .args(["--target", target, "--build-dir"])
            .arg(build.path())
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{target} build failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );

        let module_path = build.path().join(filename);
        let module = std::fs::read(&module_path).unwrap();
        wasmparser::validate(&module).unwrap();
        assert_eq!(
            String::from_utf8_lossy(&output.stdout).trim(),
            module_path.display().to_string()
        );
        module
    };

    let am = build_target("wasm-am", "fact.am.wasm");
    let sps = build_target("wasm-sps", "fact.sps.wasm");
    let function_bodies = |module: &[u8]| {
        wasmparser::Parser::new(0)
            .parse_all(module)
            .filter(|payload| matches!(payload, Ok(wasmparser::Payload::CodeSectionEntry(_))))
            .count()
    };
    assert!(function_bodies(&sps) < function_bodies(&am));
}

#[test]
fn a_rejected_root_returns_a_failure_status_with_source_diagnostics() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../lib/tests/fail/annotation.zy");
    let output =
        Command::new(env!("CARGO_BIN_EXE_zydeco")).arg("check").arg(root).output().unwrap();

    assert!(!output.status.success());
    let diagnostic = String::from_utf8_lossy(&output.stderr);
    assert!(diagnostic.contains("tyck.missing-annotation"));
    assert!(diagnostic.contains("Cannot infer the data type of constructor `+True`"));
    assert!(diagnostic.contains("add a type ascription"));
    assert!(!diagnostic.contains("when tycking"));
    assert!(!diagnostic.contains("Error location:"));
    assert!(diagnostic.lines().count() <= 12, "diagnostic was unexpectedly noisy: {diagnostic}");
}

#[test]
fn an_unattached_text_block_emits_a_non_fatal_warning() {
    let root =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../lib/tests/warn/unattached-text.zy");
    let output =
        Command::new(env!("CARGO_BIN_EXE_zydeco")).arg("check").arg(root).output().unwrap();

    assert!(output.status.success());
    let warning = String::from_utf8_lossy(&output.stderr);
    assert!(warning.contains("text block is not attached to an annotation"));
    assert!(warning.contains("this text block contributes no text"));
}

#[test]
fn check_renders_debug_observations_from_the_materialized_program() {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path().join("debug.zy");
    std::fs::write(&root, r#"@[debug("answer")] ret 1"#).unwrap();

    let output =
        Command::new(env!("CARGO_BIN_EXE_zydeco")).arg("check").arg(root).output().unwrap();

    assert!(
        output.status.success(),
        "source check failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let observation = String::from_utf8_lossy(&output.stdout);
    assert!(observation.contains(r#"[debug printing] "answer""#), "{observation}");
    assert!(observation.contains("ret 1"), "{observation}");
    assert!(observation.contains("Ret Int64"), "{observation}");
}
