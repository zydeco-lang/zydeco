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

    fn source(&self, body: &str) -> PathBuf {
        let source = self.directory.path().join("runtime.zy");
        std::fs::write(
            &source,
            format!(
            "param (/OS; /Ret; /Int8; /Int16; /Int32; /Int64; /UInt8; /UInt16; /UInt32; /UInt64; /Float32; /Float64; /String; /numeric; /process; /system; /text) : @(import(\"{}\")) in {body}\n",
                self.workspace.join("lib/std/builtin.zy").display(),
            ),
        )
        .unwrap();
        source
    }

    fn run(&self, body: &str, backend: &str) -> Output {
        let source = self.source(body);
        let build = self.directory.path().join("build");
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

    // A fixpoint's parameters stay unknown during normalization, so these cases
    // execute residual instructions even when the caller supplies literals.
    fn arithmetic_case(ty: &str, operation: &str, first: &str, second: &str, tail: &str) -> String {
        let group = ty.to_lowercase();
        format!(
            "let fix calculate (x : {ty}) (y : {ty}) : Ret {ty} = do result <- ! numeric/{group}/{operation} x y; ret result in do result <- ! calculate {first} {second}; do text <- ! numeric/{group}/to_string result; ! system/stdio/write_line text {{ {tail} }}"
        )
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
fn inline_arithmetic_executes_at_every_numeric_width() {
    let fixture = RuntimeFixture::new();
    let mut cases = Vec::new();
    for ty in ["Int8", "Int16", "Int32", "Int64", "UInt8", "UInt16", "UInt32", "UInt64"] {
        for (operation, expected) in
            [("add", "23"), ("sub", "17"), ("mul", "60"), ("div", "6"), ("mod", "2")]
        {
            cases.push((ty, operation, "20", "3", expected));
        }
    }
    for ty in ["Float32", "Float64"] {
        for (operation, expected) in [("add", "23"), ("sub", "17"), ("mul", "60"), ("div", "4")] {
            cases.push((
                ty,
                operation,
                "20.0",
                if operation == "div" { "5.0" } else { "3.0" },
                expected,
            ));
        }
    }
    cases.extend([
        ("Int8", "add", "127", "1", "-128"),
        ("Int16", "add", "32767", "1", "-32768"),
        ("Int32", "mul", "1073741824", "4", "0"),
        ("Int64", "add", "9223372036854775807", "1", "-9223372036854775808"),
        ("UInt8", "sub", "0", "1", "255"),
        ("UInt16", "mul", "32768", "2", "0"),
        ("UInt32", "add", "4294967295", "1", "0"),
        ("UInt64", "add", "18446744073709551615", "1", "0"),
        ("Int64", "add", "4611686018427387903", "1", "4611686018427387904"),
        ("Int64", "sub", "-4611686018427387904", "1", "-4611686018427387905"),
        ("Int64", "sub", "4611686018427387904", "1", "4611686018427387903"),
        ("Int64", "div", "-9223372036854775808", "-1", "-9223372036854775808"),
        ("Int64", "mod", "-9223372036854775808", "-1", "0"),
        ("Int64", "div", "-20", "3", "-6"),
        ("Int64", "mod", "-20", "3", "-2"),
        ("UInt64", "add", "9223372036854775807", "1", "9223372036854775808"),
        ("UInt64", "sub", "9223372036854775808", "1", "9223372036854775807"),
        ("UInt64", "div", "18446744073709551615", "2", "9223372036854775807"),
        ("UInt64", "mod", "18446744073709551615", "2", "1"),
        ("Float32", "add", "0.1", "0.2", "0.3"),
        ("Float64", "add", "0.1", "0.2", "0.30000000000000004"),
        ("Float32", "add", "-0.0", "-0.0", "-0"),
        ("Float64", "add", "-0.0", "-0.0", "-0"),
        ("Float32", "div", "1.0", "0.0", "inf"),
        ("Float64", "div", "0.0", "0.0", "NaN"),
    ]);
    let body = cases.iter().rev().fold(
        "! process/exit 0".to_owned(),
        |tail, (ty, operation, first, second, _)| {
            RuntimeFixture::arithmetic_case(ty, operation, first, second, &tail)
        },
    );
    let expected = cases.iter().map(|(_, _, _, _, text)| format!("{text}\n")).collect::<String>();
    for backend in ["interpreter", "exe", "wasm-am", "wasm-sps"] {
        let output = fixture.run(&body, backend);
        assert!(output.status.success(), "{backend}: {}", String::from_utf8_lossy(&output.stderr));
        assert_eq!(String::from_utf8_lossy(&output.stdout), expected, "{backend}");
        assert!(output.stderr.is_empty(), "{backend}: {}", String::from_utf8_lossy(&output.stderr));
    }
}

#[test]
fn amd64_arithmetic_uses_machine_instructions_without_runtime_arithmetic_calls() {
    use zydeco_cli::{CommandCompiler, TargetOs};

    let fixture = RuntimeFixture::new();
    for (ty, operation, instruction) in [
        ("Int32", "add", "add rax, rcx"),
        ("Int64", "sub", "sub rax, rcx"),
        ("UInt64", "mul", "imul rax, rcx"),
        ("Int64", "div", "idiv rcx"),
        ("UInt64", "mod", "div rcx"),
        ("Float32", "add", "addss xmm0, xmm1"),
        ("Float64", "add", "addsd xmm0, xmm1"),
    ] {
        let float = ty.starts_with("Float");
        let body = RuntimeFixture::arithmetic_case(
            ty,
            operation,
            if float { "20.0" } else { "20" },
            if float { "3.0" } else { "3" },
            "! process/exit 0",
        );
        let backend = CommandCompiler::default().lower(&fixture.source(&body)).unwrap();
        let low = backend.render_sps_low();
        let host_call = format!("<extern:{}_{operation}/2>", ty.to_lowercase());
        assert!(!low.contains(&host_call), "{low}");
        for target in [TargetOs::Linux, TargetOs::Macos] {
            let assembly = backend.emit_amd64(target).assembly;
            let count = assembly.lines().filter(|line| line.trim() == instruction).count();
            assert_eq!(count, 1, "{ty}/{operation}: {assembly}");
            let host_call = format!("call zydeco_{}_{operation}", ty.to_lowercase());
            assert!(!assembly.contains(&host_call), "{assembly}");
            let declaration = format!("extern zydeco_{}_{operation}", ty.to_lowercase());
            assert!(!assembly.contains(&declaration), "{assembly}");
            if ty == "Int32" {
                let primitive = assembly.split("; primitive: ").nth(1).unwrap();
                let primitive = primitive.split("push rax").next().unwrap();
                assert!(
                    !primitive.contains("call "),
                    "immediate arithmetic must not allocate or call:\n{primitive}"
                );
            }
        }
    }
}

#[test]
fn arithmetic_modules_import_only_their_remaining_host_calls() {
    let fixture = RuntimeFixture::new();
    let source = fixture.source(
        "let fix add (x : Int64) (y : Int64) : Ret Int64 = ! numeric/int64/add x y in do result <- ! add 20 22; ! process/exit result",
    );
    let backend = zydeco_cli::CommandCompiler::default().lower(&source).unwrap();
    for module in [backend.emit_wasm_am().unwrap(), backend.emit_wasm_sps().unwrap()] {
        let imports = wasmparser::Parser::new(0)
            .parse_all(&module)
            .filter_map(|payload| match payload.unwrap() {
                | wasmparser::Payload::ImportSection(imports) => Some(imports),
                | _ => None,
            })
            .flat_map(|imports| imports.into_imports())
            .map(|import| import.unwrap().name)
            .collect::<std::collections::BTreeSet<_>>();
        assert_eq!(imports, ["exit", "runtime_error"].into_iter().collect());
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
fn inline_division_errors_keep_their_position_between_effects() {
    let fixture = RuntimeFixture::new();
    let body = r#"
let fix divide (x : Int64) (y : Int64) : Ret Int64 = ! numeric/int64/div x y in
! system/stdio/write_line "before" {
  do unused <- ! divide 7 0;
  ! system/stdio/write_line "after" { ! process/exit 0 }
}
"#;
    for backend in ["interpreter", "exe", "wasm-am", "wasm-sps"] {
        let mut output = fixture.run(body, backend);
        assert_eq!(String::from_utf8_lossy(&output.stdout), "before\n", "{backend}");
        output.stdout.clear();
        RuntimeFixture::assert_failure(output, "integer division by zero", backend);
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn inline_boxed_results_survive_native_collection() {
    let fixture = RuntimeFixture::new();
    // Allocate more than a semispace of boxed results. The raw arithmetic result
    // must survive allocation without being mistaken for a tagged GC root.
    let body = r#"
let fix churn (n : Int64) (value : Int64) : Ret Int64 =
  ! numeric/int64/eq (Ret Int64) n 0 { ret value } {
    do next <- ! numeric/int64/add value 1;
    do remaining <- ! numeric/int64/sub n 1;
    ! churn remaining next
  }
in
do result <- ! churn 100000 4611686018427387904;
! numeric/int64/eq OS result 4611686018427487904 { ! process/exit 0 } { ! process/exit 42 }
"#;
    let output = fixture.run(body, "exe");
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    assert!(output.stdout.is_empty());
    assert!(output.stderr.is_empty());
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
