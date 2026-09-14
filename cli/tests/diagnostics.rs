use std::{fs, process::Command};

#[test]
fn desugaring_reports_each_independent_error_and_rejects_compilation() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("lowering.zy");
    fs::write(&path, "(\n  @[typeof(extra)] 1,\n  @[typeof(extra)] 2\n)\n").unwrap();
    for command in ["check", "run", "build"] {
        let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .current_dir(directory.path())
            .arg(command)
            .arg(&path)
            .output()
            .unwrap();
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success(), "{command}");
        assert!(error.contains("lowering.zy:2:"), "{command}: {error}");
        assert!(error.contains("lowering.zy:3:"), "{command}: {error}");
        assert!(!error.contains("panicked"), "{command}: {error}");
    }
    fs::write(&path, "(@[typeof] 1, @[typeof] 2)\n").unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .current_dir(directory.path())
        .arg("check")
        .arg(&path)
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
}

#[test]
fn parse_failures_render_source_snippets_for_check_run_build_and_format() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("syntax.zy");
    for (source, line, issue) in [
        ("let value = in value\n", "let value =", "Unrecognized token"),
        ("begin\n  ret 0\n", "ret 0", "Unrecognized EOF"),
        ("let value = \"λ\" in ?\n", "let value =", "unrecognized source token"),
        ("\"\\q\"\n", "\\q", "unknown escape"),
    ] {
        fs::write(&path, source).unwrap();
        for command in ["check", "run", "build", "fmt"] {
            let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
                .arg(command)
                .arg(&path)
                .output()
                .unwrap();
            let error = String::from_utf8_lossy(&output.stderr);
            assert!(!output.status.success(), "{command}: {source}");
            assert!(error.contains(issue), "{command}: {error}");
            assert!(error.contains(line), "{command}: missing source snippet: {error}");
            assert!(error.contains("syntax.zy:"), "{command}: missing source location: {error}");
            if source.contains('λ') {
                assert!(error.contains("syntax.zy:1:20"), "{command}: {error}");
            }
            assert!(!error.contains("panicked"), "{command}: {error}");
            assert_eq!(fs::read_to_string(&path).unwrap(), source);
        }
    }
    fs::write(&path, "ret 0\n").unwrap();
    for command in ["check", "fmt"] {
        let output =
            Command::new(env!("CARGO_BIN_EXE_zydeco")).arg(command).arg(&path).output().unwrap();
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        assert!(output.stderr.is_empty());
    }
}

#[test]
fn source_scanning_reports_multiple_directive_categories() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("directives.zy");
    fs::write(&path, "(@(import), @(import(0)), @[literal(extra)] _)").unwrap();
    for command in ["check", "run", "build"] {
        let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .current_dir(directory.path())
            .arg(command)
            .arg(&path)
            .output()
            .unwrap();
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(error.contains("expects one source argument"), "{error}");
        assert!(error.contains("must be positive"), "{error}");
        assert!(error.contains("invalid literal directive"), "{error}");
        assert!(!error.contains("panicked"), "{error}");
    }
    fs::write(&path, "(1, 2, 3)").unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .current_dir(directory.path())
        .arg("check")
        .arg(&path)
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
}

#[test]
fn resolution_reports_every_unbound_reference_at_its_own_location() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("resolution.zy");
    fs::write(&path, "(\n  missing,\n  absent\n)\n").unwrap();
    for command in ["check", "run", "build"] {
        let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .current_dir(directory.path())
            .arg(command)
            .arg(&path)
            .output()
            .unwrap();
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(error.contains("resolution.zy:2:"), "{error}");
        assert!(error.contains("resolution.zy:3:"), "{error}");
        assert_eq!(error.matches("Unbound variable").count(), 2, "{error}");
        assert!(!error.contains("panicked"), "{error}");
    }
    fs::write(&path, "(1, 2)").unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .current_dir(directory.path())
        .arg("check")
        .arg(&path)
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
}

#[test]
fn parser_reports_recovery_and_later_eof_as_separate_errors() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("parser-issues.zy");
    fs::write(&path, "let first = in\nlet second =").unwrap();
    for command in ["check", "run", "build", "fmt"] {
        let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .current_dir(directory.path())
            .arg(command)
            .arg(&path)
            .output()
            .unwrap();
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(error.contains("Unrecognized token"), "{error}");
        assert!(error.contains("Unrecognized EOF"), "{error}");
        assert!(error.contains("parser-issues.zy:1:"), "{error}");
        assert!(error.contains("parser-issues.zy:2:"), "{error}");
        assert!(!error.contains("additional parse"), "{error}");
        assert!(!error.contains("panicked"), "{error}");
    }
    fs::write(&path, "let first = 1 in\nlet second = 2 in second").unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .current_dir(directory.path())
        .arg("check")
        .arg(&path)
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
}

#[test]
fn imported_parse_failures_keep_each_file_and_do_not_replay_shared_providers() {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path().join("main.zy");
    fs::write(&root, r#"(@(import("first.zy")), @(import("second.zy")), @(import("first.zy")))"#)
        .unwrap();
    for name in ["first.zy", "second.zy"] {
        fs::write(directory.path().join(name), "let value = in value").unwrap();
    }
    for command in ["check", "run", "build"] {
        let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .current_dir(directory.path())
            .arg(command)
            .arg(&root)
            .output()
            .unwrap();
        let error = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert_eq!(error.matches("Unrecognized token").count(), 2, "{error}");
        assert!(error.contains("first.zy:1:"), "{error}");
        assert!(error.contains("second.zy:1:"), "{error}");
    }
    for name in ["first.zy", "second.zy"] {
        fs::write(directory.path().join(name), "()").unwrap();
    }
    assert!(
        Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .current_dir(directory.path())
            .arg("check")
            .arg(&root)
            .output()
            .unwrap()
            .status
            .success()
    );
}
