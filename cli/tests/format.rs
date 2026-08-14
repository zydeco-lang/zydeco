use std::{fs, process::Command};

#[test]
fn fmt_formats_each_source_file_in_place() {
    let directory = tempfile::tempdir().unwrap();
    let first = directory.path().join("first.zy");
    let second = directory.path().join("second.zy");
    fs::write(&first, "(field = field, ((x)))").unwrap();
    fs::write(&second, "exists (Counter = ((Counter as Int64) : VType)) . Counter").unwrap();

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
        "exists (= Counter as Int64 : VType) . Counter\n"
    );
}

#[test]
fn fmt_check_reports_changes_without_writing() {
    let directory = tempfile::tempdir().unwrap();
    let source = directory.path().join("check.zy");
    let original = "(field = field, ((x)))";
    fs::write(&source, original).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .arg("fmt")
        .arg("--check")
        .arg(&source)
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stdout).ends_with("check.zy\n"));
    assert!(output.stderr.is_empty());
    assert_eq!(fs::read_to_string(&source).unwrap(), original);
}

#[test]
fn fmt_check_is_silent_and_succeeds_for_formatted_files() {
    let directory = tempfile::tempdir().unwrap();
    let source = directory.path().join("check.zy");
    fs::write(&source, "(= field, x)\n").unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .arg("fmt")
        .arg("--check")
        .arg(&source)
        .output()
        .unwrap();

    assert!(output.status.success());
    assert!(output.stdout.is_empty());
    assert!(output.stderr.is_empty());
}

#[test]
fn fmt_layout_modes_control_break_retention() {
    let directory = tempfile::tempdir().unwrap();
    let joined = "! (bool/if) (Ret Int64) greater { ret left } { ret right }\n";
    let wrapped = concat!(
        "! (bool/if)\n",
        "  (Ret Int64)\n",
        "\n",
        "  greater\n",
        "  { ret left }\n",
        "  { ret right }\n",
    );
    let cases = [
        ("--layout", "preserve", wrapped, wrapped),
        (
            "--layout",
            "blank-lines",
            wrapped,
            "! (bool/if) (Ret Int64)\n\n  greater { ret left } { ret right }\n",
        ),
        ("--layout", "ignore", wrapped, joined),
    ];

    for (flag, mode, source, expected) in cases {
        let file = directory.path().join(format!("{mode}.zy"));
        fs::write(&file, source).unwrap();

        let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .arg("fmt")
            .arg(flag)
            .arg(mode)
            .arg(&file)
            .output()
            .unwrap();

        assert!(
            output.status.success(),
            "formatting failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        assert_eq!(fs::read_to_string(&file).unwrap(), expected, "layout mode: {mode}");
    }
}

#[test]
fn fmt_indent_and_width_flags_apply() {
    let directory = tempfile::tempdir().unwrap();
    let source = "begin\nlet x = f\nin ret x\nend\n";

    let indented = directory.path().join("indented.zy");
    fs::write(&indented, source).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .arg("fmt")
        .arg("--indent")
        .arg("4")
        .arg(&indented)
        .output()
        .unwrap();
    assert!(output.status.success());
    assert_eq!(fs::read_to_string(indented).unwrap(), "begin\n    let x = f in\n    ret x\nend\n");

    let wide = directory.path().join("wide.zy");
    fs::write(&wide, "A *\nB\n").unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .arg("fmt")
        .arg("--layout")
        .arg("ignore")
        .arg("--width")
        .arg("200")
        .arg(&wide)
        .output()
        .unwrap();
    assert!(output.status.success());
    assert_eq!(fs::read_to_string(wide).unwrap(), "A * B\n");

    let narrow = directory.path().join("narrow.zy");
    fs::write(&narrow, joined_line()).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .arg("fmt")
        .arg("--layout")
        .arg("ignore")
        .arg("--width")
        .arg("24")
        .arg(&narrow)
        .output()
        .unwrap();
    assert!(output.status.success());
    assert!(fs::read_to_string(narrow).unwrap().contains('\n'));
}

fn joined_line() -> String {
    "! (bool/if) (Ret Int64) greater { ret left } { ret right }\n".to_owned()
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
