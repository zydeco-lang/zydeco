use std::{fs, process::Command};

#[test]
fn fmt_formats_each_source_file_in_place() {
    let directory = tempfile::tempdir().unwrap();
    let first = directory.path().join("first.zy");
    let second = directory.path().join("second.zy");
    fs::write(&first, "(field = field, ((x)))").unwrap();
    fs::write(&second, "exists (Counter = ((Counter as Int) : VType)) . Counter").unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .arg("fmt")
        .arg(&first)
        .arg(&second)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "formatting failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(output.stdout.is_empty());
    assert!(output.stderr.is_empty());
    assert_eq!(fs::read_to_string(first).unwrap(), "(= field, x)\n");
    assert_eq!(
        fs::read_to_string(second).unwrap(),
        "exists (= Counter as Int : VType) . Counter\n"
    );
}

#[test]
fn fmt_rejects_invalid_syntax_without_overwriting_it() {
    let directory = tempfile::tempdir().unwrap();
    let source = directory.path().join("invalid.zy");
    let invalid = "let value =";
    fs::write(&source, invalid).unwrap();

    let output =
        Command::new(env!("CARGO_BIN_EXE_zydeco")).arg("fmt").arg(&source).output().unwrap();

    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("cannot format source"));
    assert_eq!(fs::read_to_string(source).unwrap(), invalid);
}
