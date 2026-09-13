use clap::{Parser, error::ErrorKind};
use std::{
    io::{BufRead, BufReader, Write},
    path::{Path, PathBuf},
    process::{Child, Command, Output, Stdio},
    sync::mpsc,
    time::Duration,
};
use zydeco_cli::{Cli, Commands, ExecutionTarget, TestTarget};

struct Fixture {
    directory: tempfile::TempDir,
}

impl Fixture {
    fn new() -> Self {
        Self { directory: tempfile::tempdir().unwrap() }
    }

    fn write(&self, name: &str, source: &str) {
        std::fs::write(self.directory.path().join(name), source).unwrap();
    }

    fn program(body: &str) -> String {
        let builtin = Path::new(env!("CARGO_MANIFEST_DIR")).join("../lib/std/builtin.zy");
        format!(
            "param (/OS; /Unit; /Int64; /String; /process; /system) : @(import({:?})) in {body}",
            builtin.to_str().unwrap()
        )
    }

    fn file_effect() -> String {
        Self::program(
            r#"! system/fs/create_writer "executed.marker"
                { fn _ _ => ! process/exit 8 }
                { fn writer => ! system/io/close_writer writer
                    { fn _ _ => ! process/exit 9 }
                    { ! process/exit 0 }
                }"#,
        )
    }

    fn suite(&self, first: &str, second: &str) {
        self.write(
            "package.zy",
            r#"@[discover(include("tests.zy"))] @[package(library, name(suite))] ()"#,
        );
        self.write(
            "tests.zy",
            &format!(
                "(#a = {{ @[package(test(of(suite)), name(a))] ({first}) }},
                  #b = {{ @[package(test(of(suite)), name(b))] ({second}) }})"
            ),
        );
    }

    fn command(&self, arguments: &[&str]) -> Command {
        let mut command = Command::new(env!("CARGO_BIN_EXE_zydeco"));
        command.current_dir(self.directory.path()).args(arguments);
        command
    }

    fn stdout(output: &Output, code: i32) -> String {
        assert_eq!(output.status.code(), Some(code), "{}", String::from_utf8_lossy(&output.stderr));
        String::from_utf8(output.stdout.clone()).unwrap()
    }

    fn line(child: &mut Child, lines: &mpsc::Receiver<String>) -> String {
        match lines.recv_timeout(Duration::from_secs(30)) {
            | Ok(line) => line,
            | Err(error) => {
                child.kill().unwrap();
                let mut stderr = String::new();
                std::io::Read::read_to_string(child.stderr.as_mut().unwrap(), &mut stderr).unwrap();
                child.wait().unwrap();
                panic!("program must respond before stdin closes: {error}: {stderr}");
            }
        }
    }
}

#[test]
fn execution_flags_default_to_interpreter_and_only_test_accepts_multiple_targets() {
    for command in ["run", "test"] {
        let cli = Cli::try_parse_from(["zydeco", command, "example"]).unwrap();
        match cli.command {
            | Commands::Run { target, .. } => assert_eq!(target, ExecutionTarget::Interpreter),
            | Commands::Test { targets, .. } => {
                assert_eq!(targets, [TestTarget::One(ExecutionTarget::Interpreter)]);
            }
            | _ => unreachable!(),
        }
        for target in ["zir", "zasm", "asm", "unknown", ""] {
            let error = Cli::try_parse_from(["zydeco", command, "example", "-t", target])
                .err()
                .expect("non-executable targets are rejected");
            assert_eq!(error.kind(), ErrorKind::InvalidValue);
        }
    }
    let cli =
        Cli::try_parse_from(["zydeco", "test", "example", "-t", "wasm-sps", "--target", "exe"])
            .unwrap();
    let Commands::Test { targets, .. } = cli.command else { unreachable!() };
    assert_eq!(
        targets,
        [TestTarget::One(ExecutionTarget::WasmSps), TestTarget::One(ExecutionTarget::Exe)]
    );
    let cli = Cli::try_parse_from(["zydeco", "test", "example", "-t", "all"]).unwrap();
    let Commands::Test { targets, .. } = cli.command else { unreachable!() };
    assert_eq!(targets, [TestTarget::All]);
    assert_eq!(
        targets[0].expand(),
        [
            ExecutionTarget::Interpreter,
            ExecutionTarget::Exe,
            ExecutionTarget::WasmAm,
            ExecutionTarget::WasmSps
        ]
    );
    for command in ["run", "build"] {
        let error = Cli::try_parse_from(["zydeco", command, "example", "-t", "all"])
            .err()
            .expect("all is a test-only selection");
        assert_eq!(error.kind(), ErrorKind::InvalidValue);
    }
    let error =
        Cli::try_parse_from(["zydeco", "run", "example", "-t", "wasm-am", "-t", "wasm-sps"])
            .err()
            .expect("run selects one backend");
    assert_eq!(error.kind(), ErrorKind::ArgumentConflict);
}

#[test]
fn test_backends_replace_the_default_and_deduplicate_in_request_order() {
    let fixture = Fixture::new();
    fixture.suite(&Fixture::program("! process/exit 7"), &Fixture::program("! process/exit 0"));
    let output = fixture
        .command(&["test", "suite", "-t", "wasm-sps", "-t", "wasm-am", "-t", "wasm-sps"])
        .output()
        .unwrap();
    let stdout = Fixture::stdout(&output, 1);
    let results = stdout
        .lines()
        .filter(|line| line.starts_with("PASS") || line.starts_with("FAIL"))
        .map(|line| line.split(" (").next().unwrap())
        .collect::<Vec<_>>();
    assert_eq!(
        results,
        ["FAIL [wasm-sps] a", "FAIL [wasm-am] a", "PASS [wasm-sps] b", "PASS [wasm-am] b"]
    );
    assert!(stdout.contains("2 passed; 2 failed."), "{stdout}");
    assert!(!stdout.contains("interpreter"));
    assert!(!fixture.directory.path().join("build").exists());
}

#[test]
fn runtime_failures_report_each_backend_and_do_not_stop_the_suite() {
    let fixture = Fixture::new();
    fixture.suite(
        &Fixture::program("@[partial] let 0 = 1 in ! process/exit 0"),
        &Fixture::program("! process/exit 0"),
    );
    let output = fixture
        .command(&["test", "suite", "-t", "interpreter", "-t", "wasm-am", "-t", "wasm-sps"])
        .output()
        .unwrap();
    let stdout = Fixture::stdout(&output, 1);
    for target in ["interpreter", "wasm-am", "wasm-sps"] {
        assert!(stdout.contains(&format!("FAIL [{target}] a")), "{stdout}");
        assert!(stdout.contains(&format!("PASS [{target}] b")), "{stdout}");
    }
    assert!(stdout.contains("3 passed; 3 failed."), "{stdout}");
    assert!(String::from_utf8_lossy(&output.stderr).contains("pattern match failed"));
}

#[test]
fn test_uses_empty_input_and_arguments_and_displays_output_on_failure() {
    let fixture = Fixture::new();
    fixture.write(
        "case.zy",
        &format!(
            "@[package(test)] {}",
            Fixture::program(
                r#"! system/args/at OS 0 {
            ! system/stdio/read_all { fn input =>
                ! system/stdio/write input {
                    ! system/stdio/write_line "captured" { ! process/exit 7 }
                }
            }
        } { fn _ => ! process/exit 8 }"#,
            )
        ),
    );
    for target in ["interpreter", "wasm-am", "wasm-sps"] {
        let mut child = fixture
            .command(&["test", "case.zy", "-t", target])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();
        child.stdin.as_mut().unwrap().write_all(b"must not reach the test").unwrap();
        let output = child.wait_with_output().unwrap();
        let stdout = Fixture::stdout(&output, 1);
        assert!(stdout.contains("(exit 7)\ncaptured\n0 passed; 1 failed."), "{stdout}");
    }
}

#[test]
fn invalid_sources_and_missing_hosts_fail_before_any_test_runs() {
    let fixture = Fixture::new();
    for (second, diagnostic) in [
        ("42".to_owned(), "classified as a value"),
        (Fixture::program("! process/exit 0"), "cannot start"),
    ] {
        fixture.suite(
            &Fixture::program("! system/stdio/write_line \"effect\" { ! process/exit 0 }"),
            &second,
        );
        let output = fixture
            .command(&["test", "suite", "-t", "all"])
            .env("NODE", fixture.directory.path().join("missing-node"))
            .output()
            .unwrap();
        assert!(Fixture::stdout(&output, 1).is_empty());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains(diagnostic), "{stderr}");
        assert!(!fixture.directory.path().join("build").exists());
    }
}

#[test]
fn test_preflight_failures_leave_filesystem_effects_unexecuted() {
    let fixture = Fixture::new();
    let marker = fixture.directory.path().join("executed.marker");
    let valid = Fixture::program("! process/exit 0");
    for (second, diagnostic) in [("42", "classified as a value"), (valid.as_str(), "cannot start")]
    {
        fixture.suite(&Fixture::file_effect(), second);
        let output = fixture
            .command(&["test", "suite", "-t", "interpreter", "-t", "wasm-sps"])
            .env("NODE", fixture.directory.path().join("missing-node"))
            .output()
            .unwrap();
        assert!(Fixture::stdout(&output, 1).is_empty());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains(diagnostic), "{stderr}");
        assert!(!marker.exists(), "a test executed before preparation completed");
    }

    let output = fixture.command(&["test", "suite"]).output().unwrap();
    assert!(Fixture::stdout(&output, 0).contains("2 passed; 0 failed."));
    assert!(marker.is_file(), "the successful control must actually perform the effect");
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn native_preparation_failure_does_not_execute_already_prepared_interpreter_programs() {
    let fixture = Fixture::new();
    fixture.suite(&Fixture::file_effect(), &Fixture::program("! process/exit 0"));
    let output = fixture
        .command(&["test", "suite", "-t", "interpreter", "-t", "exe", "-r", "missing-runtime"])
        .output()
        .unwrap();
    assert!(Fixture::stdout(&output, 1).is_empty());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("runtime directory"), "{stderr}");
    assert!(!fixture.directory.path().join("executed.marker").exists());
    assert!(!fixture.directory.path().join("build").exists());
}

#[test]
fn dry_run_needs_no_backend_tools_and_does_not_execute() {
    let fixture = Fixture::new();
    fixture.write(
        "main.zy",
        &Fixture::program("! system/stdio/write_line \"effect\" { ! process/exit 7 }"),
    );
    for target in ["interpreter", "exe", "wasm-am", "wasm-sps"] {
        let output = fixture
            .command(&["run", "main.zy", "--dry", "-t", target, "-r", "missing-runtime"])
            .env("NODE", fixture.directory.path().join("missing-node"))
            .output()
            .unwrap();
        assert!(Fixture::stdout(&output, 0).is_empty());
        assert!(output.stderr.is_empty());
        assert!(!fixture.directory.path().join("build").exists());
    }
    fixture.write("main.zy", "42");
    let output = fixture.command(&["run", "main.zy", "--dry", "-t", "wasm-am"]).output().unwrap();
    assert!(Fixture::stdout(&output, 1).is_empty());
    assert!(String::from_utf8_lossy(&output.stderr).contains("classified as a value"));
}

#[test]
fn run_forwards_arguments_and_line_input_without_waiting_for_stdin_eof() {
    let fixture = Fixture::new();
    fixture.write(
        "main.zy",
        &Fixture::program(
            r#"
        ! system/args/at OS 0 { ! process/exit 8 } { fn argument =>
            ! system/stdio/write_line argument {
                ! system/stdio/read_line { fn line =>
                    ! system/stdio/write_line line { ! process/exit 7 }
                }
            }
        }
    "#,
        ),
    );
    for target in ["interpreter", "wasm-am", "wasm-sps"] {
        let mut child = fixture
            .command(&["run", "main.zy", "-t", target, "--", "--argument"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();
        let stdin = child.stdin.take().unwrap();
        let stdout = child.stdout.take().unwrap();
        let (sender, lines) = mpsc::channel();
        let reader = std::thread::spawn(move || {
            for line in BufReader::new(stdout).lines() {
                sender.send(line.unwrap()).unwrap();
            }
        });
        assert_eq!(Fixture::line(&mut child, &lines), "--argument", "{target}");
        let mut stdin = stdin;
        stdin.write_all(b"hello\r\n").unwrap();
        assert_eq!(Fixture::line(&mut child, &lines), "hello", "{target}");
        // Keep stdin open until the child exits: neither printing nor line input needs EOF.
        let output = child.wait_with_output().unwrap();
        Fixture::stdout(&output, 7);
        assert!(output.stderr.is_empty(), "{}", String::from_utf8_lossy(&output.stderr));
        drop(stdin);
        reader.join().unwrap();
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn native_run_and_all_backend_tests_share_the_runner() {
    let fixture = Fixture::new();
    let runtime = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../runtime");
    fixture.write(
        "main.zy",
        &Fixture::program("! system/stdio/write_line \"native\" { ! process/exit 7 }"),
    );
    let output =
        fixture.command(&["run", "main.zy", "-t", "exe", "-r"]).arg(&runtime).output().unwrap();
    assert_eq!(Fixture::stdout(&output, 7), "native\n");
    fixture.write("case.zy", &format!("@[package(test)] {}", Fixture::program("! process/exit 0")));
    for (targets, expected) in [
        (vec!["-t", "all"], ["[interpreter]", "[exe]", "[wasm-am]", "[wasm-sps]"]),
        (
            vec!["-t", "wasm-sps", "-t", "all", "--target", "interpreter", "-t", "all"],
            ["[wasm-sps]", "[interpreter]", "[exe]", "[wasm-am]"],
        ),
    ] {
        let output = fixture
            .command(&["test", "case.zy"])
            .args(targets)
            .arg("-r")
            .arg(&runtime)
            .output()
            .unwrap();
        let stdout = Fixture::stdout(&output, 0);
        let backends = stdout
            .lines()
            .filter(|line| line.starts_with("PASS"))
            .map(|line| line.split_whitespace().nth(1).unwrap())
            .collect::<Vec<_>>();
        assert_eq!(backends, expected, "{stdout}");
        assert!(stdout.contains("4 passed; 0 failed."), "{stdout}");
    }
    assert!(!fixture.directory.path().join("build").exists());
}

#[test]
fn wasm_random_integers_use_the_host_generator_and_preserve_full_width() {
    let fixture = Fixture::new();
    fixture.write("main.zy", &Fixture::program(
        "! system/random/generate { fn value => ! system/stdio/write_int value { ! process/exit 0 } }",
    ));
    for value in [i64::MIN, i64::MAX] {
        fixture.write(
            "random.cjs",
            &format!(
                r#"
            require('node:crypto').randomBytes = size => {{
                if (size !== 8) throw new Error('random integers require eight bytes');
                const bytes = Buffer.alloc(size);
                bytes.writeBigInt64LE({value}n);
                return bytes;
            }};
            require('node:module').syncBuiltinESMExports();
        "#
            ),
        );
        for target in ["wasm-am", "wasm-sps"] {
            let output = fixture
                .command(&["run", "main.zy", "-t", target])
                .env(
                    "NODE_OPTIONS",
                    format!("--require={:?}", fixture.directory.path().join("random.cjs")),
                )
                .output()
                .unwrap();
            assert_eq!(Fixture::stdout(&output, 0), value.to_string(), "{target}");
        }
    }
}
