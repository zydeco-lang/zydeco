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
    assert!(String::from_utf8_lossy(&output.stdout).contains("[entry]"));
}

#[test]
fn a_rejected_root_returns_a_failure_status_with_source_diagnostics() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../lib/tests/fail/annotation.zy");
    let output =
        Command::new(env!("CARGO_BIN_EXE_zydeco")).arg("check").arg(root).output().unwrap();

    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("Missing annotation"));
}

#[test]
fn an_unattached_documentation_comment_emits_a_non_fatal_warning() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../lib/tests/warn/unattached-documentation.zy");
    let output =
        Command::new(env!("CARGO_BIN_EXE_zydeco")).arg("check").arg(root).output().unwrap();

    assert!(output.status.success());
    let warning = String::from_utf8_lossy(&output.stderr);
    assert!(warning.contains("documentation comment is not attached to `@[doc]`"));
    assert!(warning.contains("this documentation block contributes no documentation"));
}
