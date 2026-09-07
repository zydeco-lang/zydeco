use std::{
    path::PathBuf,
    process::{Command, Output},
};

struct RuntimeFixture {
    workspace: PathBuf,
    directory: tempfile::TempDir,
}

impl RuntimeFixture {
    fn new() -> Self {
        Self {
            workspace: PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(".."),
            directory: tempfile::tempdir().unwrap(),
        }
    }

    fn run(&self, body: &str, backend: &str) -> Output {
        let source = self.directory.path().join("runtime.zy");
        let build = self.directory.path().join("build");
        std::fs::write(
            &source,
            format!(
            "param (/OS; /Ret; /Int64; /String; /numeric; /process; /system; /text) : @(import(\"{}\")) in {body}\n",
                self.workspace.join("lib/std/builtin.zy").display(),
            ),
        )
        .unwrap();
        let mut command = Command::new(env!("CARGO_BIN_EXE_zydeco"));
        if backend == "interpreter" {
            return command.arg("run").arg(&source).output().unwrap();
        }
        command.arg("build").arg(&source).args(["--target", backend, "--build-dir"]).arg(&build);
        if backend == "exe" {
            command
                .args(["--target-arch", "x86-64", "--runtime-dir"])
                .arg(self.workspace.join("runtime"))
                .arg("--execute");
            return command.output().unwrap();
        }
        let output = command.output().unwrap();
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        let extension = if backend == "wasm-am" { "am" } else { "sps" };
        Command::new(std::env::var_os("NODE").unwrap_or_else(|| "node".into()))
            .arg(self.workspace.join("lang/tests/wasm-host.mjs"))
            .arg(build.join(format!("runtime.{extension}.wasm")))
            .output()
            .unwrap()
    }

    fn assert_failure(output: Output, expected: &str, context: &str) {
        let diagnostic = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success(), "{context}: failure must stop execution");
        assert!(output.status.code().is_some(), "{context}: must not terminate by signal");
        assert!(diagnostic.contains(expected), "{context}: {diagnostic}");
        assert!(!diagnostic.contains("panicked"), "{context}: {diagnostic}");
        assert!(output.stdout.is_empty(), "{context}: must not produce stdout");
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn float_text_agrees_across_runtime_backends() {
    let fixture = RuntimeFixture::new();
    let cases = [
        ("float32", "add -0.0 -0.0", (-0.0_f32).to_string()),
        ("float64", "add -0.0 -0.0", (-0.0_f64).to_string()),
        ("float32", "add 0.1 0.2", (0.1_f32 + 0.2).to_string()),
        ("float64", "add 0.1 0.2", (0.1_f64 + 0.2).to_string()),
        ("float32", "add 1e21 0.0", 1e21_f32.to_string()),
        ("float64", "add 1e21 0.0", 1e21_f64.to_string()),
        ("float32", "div 1.0 0.0", f32::INFINITY.to_string()),
        ("float64", "div -1.0 0.0", f64::NEG_INFINITY.to_string()),
        ("float64", "div 0.0 0.0", f64::NAN.to_string()),
        ("float32", "add 1e-45 0.0", f32::from_bits(1).to_string()),
        ("float64", "add 5e-324 0.0", f64::from_bits(1).to_string()),
        ("float64", "add 1.7976931348623157e308 0.0", f64::MAX.to_string()),
    ];
    let body = cases.iter().rev().fold("! process/exit 0".to_owned(), |tail, (group, operation, _)| {
        format!(
            "do value <- ! numeric/{group}/{operation}; do text <- ! numeric/{group}/to_string value; ! system/stdio/write_line text {{ {tail} }}"
        )
    });
    let expected = cases.iter().map(|(_, _, text)| format!("{text}\n")).collect::<String>();
    for backend in ["interpreter", "exe", "wasm-am", "wasm-sps"] {
        let output = fixture.run(&body, backend);
        assert!(output.status.success(), "{backend}: {}", String::from_utf8_lossy(&output.stderr));
        assert_eq!(String::from_utf8_lossy(&output.stdout), expected, "{backend}");
        assert!(output.stderr.is_empty(), "{backend}: {}", String::from_utf8_lossy(&output.stderr));
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn standard_error_uses_its_own_injected_stream() {
    let fixture = RuntimeFixture::new();
    let body = r#"
do writer <- ! system/stdio/stderr;
do bytes <- ! text/bytes/from_string "stderr only\n";
let failed = { fn (_ : Int64) (_ : String) => ! process/exit 42 } in
! system/io/write_all writer bytes failed {
  ! system/io/flush writer failed {
    ! system/io/close_writer writer failed {
      ! system/stdio/write "stdout only\n" { ! process/exit 0 }
    }
  }
}
"#;
    for backend in ["interpreter", "exe", "wasm-am", "wasm-sps"] {
        let output = fixture.run(body, backend);
        assert!(output.status.success(), "{backend}: {}", String::from_utf8_lossy(&output.stderr));
        assert_eq!(String::from_utf8_lossy(&output.stdout), "stdout only\n", "{backend}");
        assert_eq!(String::from_utf8_lossy(&output.stderr), "stderr only\n", "{backend}");
    }
    let captured = zydeco_cli::CommandCompiler::default()
        .test_io(&fixture.directory.path().join("runtime.zy"), &[], "")
        .unwrap();
    assert_eq!(captured.code, 0);
    assert_eq!(captured.output, "stdout only\n");
    assert_eq!(captured.stderr, "stderr only\n");
}

#[test]
fn wasm_machine_stack_overflow_has_a_runtime_diagnostic() {
    let fixture = RuntimeFixture::new();
    for depth in [200, 100_000] {
        let body = format!(
            "let fix count (n : Int64) : Ret Int64 = ! numeric/int64/eq (Ret Int64) n 0 {{ ret 0 }} {{ do next <- ! numeric/int64/sub n 1; do result <- ! count next; ! numeric/int64/add result 1 }} in do result <- ! count {depth}; ! process/exit 0"
        );
        let output = fixture.run(&body, "wasm-am");
        if depth == 200 {
            assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
        } else {
            RuntimeFixture::assert_failure(output, "operand/control stack overflow", "wasm-am");
        }
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn integer_zero_divisors_are_runtime_errors_at_every_width() {
    let fixture = RuntimeFixture::new();
    for integer in ["int8", "int16", "int32", "int64", "uint8", "uint16", "uint32", "uint64"] {
        for (operation, message) in
            [("div", "integer division by zero"), ("mod", "integer remainder by zero")]
        {
            let body =
                format!("do result <- ! numeric/{integer}/{operation} 7 0; ! process/exit 0");
            for backend in ["interpreter", "exe", "wasm-am", "wasm-sps"] {
                RuntimeFixture::assert_failure(
                    fixture.run(&body, backend),
                    message,
                    &format!("{integer}/{operation}: {backend}"),
                );
            }
        }
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn integer_division_and_remainder_preserve_wrapping_at_every_width() {
    let fixture = RuntimeFixture::new();
    let cases = [
        ("int8", "-128", "-1", "-128", "0"),
        ("int16", "-32768", "-1", "-32768", "0"),
        ("int32", "-2147483648", "-1", "-2147483648", "0"),
        ("int64", "-9223372036854775808", "-1", "-9223372036854775808", "0"),
        ("uint8", "7", "2", "3", "1"),
        ("uint16", "7", "2", "3", "1"),
        ("uint32", "7", "2", "3", "1"),
        ("uint64", "7", "2", "3", "1"),
    ];
    let body = cases.into_iter().fold("! process/exit 0".to_owned(), |tail, (integer, first, second, quotient, remainder)| {
        format!("do q <- ! numeric/{integer}/div {first} {second}; do r <- ! numeric/{integer}/mod {first} {second}; ! numeric/{integer}/eq OS q {quotient} {{ ! numeric/{integer}/eq OS r {remainder} {{ {tail} }} {{ ! process/exit 42 }} }} {{ ! process/exit 42 }}")
    });
    for backend in ["interpreter", "exe", "wasm-am", "wasm-sps"] {
        let output = fixture.run(&body, backend);
        assert!(output.status.success(), "{backend}: {}", String::from_utf8_lossy(&output.stderr));
        assert!(output.stdout.is_empty());
    }
}
