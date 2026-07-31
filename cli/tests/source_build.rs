use std::{path::PathBuf, process::Command};

#[test]
fn a_root_term_builds_without_project_configuration() {
    let root =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../lib/tests/source/builtin-exit.zy");
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
