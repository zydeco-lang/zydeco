use std::{fs, process::Command};

#[test]
fn fmt_formats_each_source_file_in_place() {
    let directory = tempfile::tempdir().unwrap();
    let first = directory.path().join("first.zy");
    let second = directory.path().join("second.zy");
    fs::write(&first, "(#field = field, ((x)))").unwrap();
    fs::write(&second, "exists (#Counter = ((Counter as Int64) : VType)) . Counter").unwrap();

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
    let original = "(#field = field, ((x)))";
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
fn fmt_format_annotations_control_break_retention() {
    let directory = tempfile::tempdir().unwrap();
    let joined =
        "@[format(layout(ignore))] ! (bool/if) (Ret Int64) greater { ret left } { ret right }\n";
    let wrapped = concat!(
        "@[format(layout(ignore))] ! (bool/if)\n",
        "  (Ret Int64)\n",
        "  greater\n",
        "  { ret left }\n",
        "  { ret right }\n",
    );
    let cases = [("preserve", wrapped, joined), ("joined", joined, joined)];

    for (name, source, expected) in cases {
        let file = directory.path().join(format!("{name}.zy"));
        fs::write(&file, source).unwrap();

        let output =
            Command::new(env!("CARGO_BIN_EXE_zydeco")).arg("fmt").arg(&file).output().unwrap();

        assert!(
            output.status.success(),
            "formatting failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        assert_eq!(fs::read_to_string(&file).unwrap(), expected, "case: {name}");
    }
}

#[test]
fn fmt_format_annotations_scope_width_and_indentation() {
    let directory = tempfile::tempdir().unwrap();
    let source = concat!(
        "begin\n",
        "  @[format(indent(4))] begin\n",
        "  let x = f\n",
        "  in ret x\n",
        "  end\n",
        "end\n",
    );

    let indented = directory.path().join("indented.zy");
    fs::write(&indented, source).unwrap();
    let output =
        Command::new(env!("CARGO_BIN_EXE_zydeco")).arg("fmt").arg(&indented).output().unwrap();
    assert!(output.status.success());
    assert_eq!(
        fs::read_to_string(indented).unwrap(),
        concat!(
            "begin\n",
            "  @[format(indent(4))] begin\n",
            "      let x = f in\n",
            "      ret x\n",
            "  end\n",
            "end\n",
        )
    );

    let narrow = directory.path().join("narrow.zy");
    fs::write(
        &narrow,
        "@[format(width(24))] ! (bool/if) (Ret Int64) greater { ret left } { ret right }\n",
    )
    .unwrap();
    let output =
        Command::new(env!("CARGO_BIN_EXE_zydeco")).arg("fmt").arg(&narrow).output().unwrap();
    assert!(output.status.success());
    assert_eq!(
        fs::read_to_string(narrow).unwrap(),
        concat!(
            "@[format(width(24))]\n",
            "! (bool/if) (Ret Int64)\n",
            "  greater { ret left } {\n",
            "  ret right\n",
            "}\n",
        )
    );
}

#[test]
fn fmt_format_verbatim_preserves_long_payload() {
    let directory = tempfile::tempdir().unwrap();
    let file = directory.path().join("verbatim.zy");
    let source = concat!(
        "@[format(verbatim)] (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, ",
        "16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, ",
        "35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, ",
        "54, 55, 56, 57, 58, 59, 60, 61, 62, 63)\n",
    );
    fs::write(&file, source).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_zydeco")).arg("fmt").arg(&file).output().unwrap();

    assert!(
        output.status.success(),
        "formatting failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(fs::read_to_string(file).unwrap(), source);
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
