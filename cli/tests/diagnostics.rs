use std::{fs, process::Command};

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
