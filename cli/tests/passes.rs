use std::{
    path::PathBuf,
    process::{Command, Output},
};
use zydeco_cli::{CommandCompiler, HighSpsPlan};

struct Fixture {
    directory: tempfile::TempDir,
    workspace: PathBuf,
}

impl Fixture {
    fn new(body: &str) -> Self {
        let fixture = Self {
            directory: tempfile::tempdir().unwrap(),
            workspace: PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(".."),
        };
        std::fs::write(fixture.source(), format!(
            "param (/system; /process; /numeric; /OS; /Ret; /Int8; /Int16; /Int32; /Int64; /Int; /UInt8; /UInt16; /UInt32; /UInt64; /UInt; /Float32; /Float64) : @(import(\"{}\")) in {body}\n",
            fixture.workspace.join("lib/std/builtin.zy").display(),
        )).unwrap();
        fixture
    }

    fn source(&self) -> PathBuf {
        self.directory.path().join("program.zy")
    }
    fn build_dir(&self) -> PathBuf {
        self.directory.path().join("build")
    }

    fn build(&self, selection: &str, target: &str, extra: &[&str]) -> Output {
        Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .arg("build")
            .arg(self.source())
            .args([
                "--target",
                target,
                "--sps-passes",
                selection,
                "--target-arch",
                "x86-64",
                "--runtime-dir",
            ])
            .arg(self.workspace.join("runtime"))
            .arg("--build-dir")
            .arg(self.build_dir())
            .args(extra)
            .output()
            .unwrap()
    }

    fn execute(&self, selection: &str, target: &str) -> Output {
        if target == "exe" || target == "zasm" {
            return self.build(selection, target, &["--execute"]);
        }
        let compiled = self.build(selection, target, &[]);
        Self::assert_success(&compiled);
        let extension = if target == "wasm-am" { "am" } else { "sps" };
        Command::new(std::env::var_os("NODE").unwrap_or_else(|| "node".into()))
            .arg(self.workspace.join("cli/wasm/wasm-host.mjs"))
            .arg(self.build_dir().join(format!("program.{extension}.wasm")))
            .output()
            .unwrap()
    }

    fn assert_success(output: &Output) {
        assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    }

    fn arithmetic() -> Self {
        Self::new("do value <- ! numeric/int/add 20 22; ! process/exit 0")
    }
}

#[test]
fn discovery_and_expansion_need_no_source_file() {
    let output = Command::new(env!("CARGO_BIN_EXE_zydeco")).arg("passes").output().unwrap();
    Fixture::assert_success(&output);
    let text = String::from_utf8(output.stdout).unwrap();
    assert!(text.contains("normalize:"));
    for plan in ["default", "none", "normalize,normalize"] {
        let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .args(["passes", "--sps-passes", plan])
            .output()
            .unwrap();
        Fixture::assert_success(&output);
        let text = String::from_utf8(output.stdout).unwrap();
        assert_eq!(text, plan.parse::<HighSpsPlan>().unwrap().explain());
    }
}

#[test]
fn rejected_selections_stop_before_source_loading_or_artifact_creation() {
    let fixture = Fixture::arithmetic();
    std::fs::remove_file(fixture.source()).unwrap();
    for (plan, diagnostic) in [
        ("", "empty high-SPS pass selection"),
        ("normalize,missing", "position 2"),
        ("normalize,", "empty high-SPS pass at position 2"),
        ("normalize(limit=2)", "unknown high-SPS pass"),
        ("normalize,none", "selects a complete plan"),
        ("stack-analysis", "unknown high-SPS pass"),
    ] {
        let output = fixture.build(plan, "exe", &[]);
        assert!(!output.status.success());
        let text = String::from_utf8_lossy(&output.stderr);
        assert!(text.contains(diagnostic), "{plan}: {text}");
        assert!(!text.contains("panicked"));
        assert!(!text.contains("No such file"));
        assert!(!fixture.build_dir().exists());
    }
    for command in ["run", "check"] {
        let output = Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .arg(command)
            .arg(fixture.source())
            .args(["--sps-passes", "none"])
            .output()
            .unwrap();
        assert!(!output.status.success());
        assert!(
            String::from_utf8_lossy(&output.stderr).contains("unexpected argument '--sps-passes'")
        );
    }
}

#[test]
fn inspection_preserves_output_and_identifies_every_selected_occurrence() {
    let fixture = Fixture::arithmetic();
    let baseline = fixture.build("default", "zir", &[]);
    Fixture::assert_success(&baseline);
    let inspected =
        fixture.build("normalize", "zir", &["--trace-passes", "--verify-passes", "--dump-passes"]);
    Fixture::assert_success(&inspected);
    assert_eq!(inspected.stdout, baseline.stdout);
    let trace = String::from_utf8_lossy(&inspected.stderr);
    assert!(trace.contains("before high-SPS normalize[1] (invocation 1)"));
    assert!(trace.contains("after high-SPS normalize[1] (invocation 1)"));
    assert!(trace.lines().count() > 2, "IR dumps accompany trace boundaries");
    let repeated =
        fixture.build("normalize,normalize", "zir", &["--trace-passes", "--verify-passes"]);
    Fixture::assert_success(&repeated);
    let trace = String::from_utf8_lossy(&repeated.stderr);
    assert_eq!(trace.matches("before high-SPS").count(), 2);
    assert!(trace.contains("normalize[2] (invocation 1)"));
    let empty = fixture.build("none", "zir", &["--trace-passes", "--verify-passes"]);
    Fixture::assert_success(&empty);
    assert!(empty.stderr.is_empty());
}

#[test]
fn changing_command_selection_produces_independent_frozen_backend_products() {
    let fixture = Fixture::arithmetic();
    let mut compiler = CommandCompiler::default();
    let original = compiler.lower(&fixture.source()).unwrap();
    let original_size = original.assembly().arena().programs.len();
    compiler = compiler.with_sps_passes(HighSpsPlan::None);
    let unoptimized = compiler.lower(&fixture.source()).unwrap();
    assert_eq!(unoptimized.sps_passes(), &HighSpsPlan::None);
    assert!(unoptimized.assembly().arena().programs.len() > original_size);
    compiler = compiler.with_sps_passes(HighSpsPlan::Default);
    let again = compiler.lower(&fixture.source()).unwrap();
    assert_eq!(again.sps_passes(), &HighSpsPlan::Default);
    assert_eq!(again.assembly().arena().programs.len(), original_size);
    assert_eq!(original.assembly().arena().programs.len(), original_size);
}

#[test]
fn every_build_target_accepts_the_high_sps_selection() {
    let fixture = Fixture::arithmetic();
    for target in ["zir", "zasm", "asm", "wasm-am", "wasm-sps"] {
        Fixture::assert_success(&fixture.build("none", target, &[]));
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn selected_sequences_preserve_sharing_effects_and_results_across_backends() {
    let fixture = Fixture::new(
        r#"
! system/stdio/write_line "before" {
  do value <- ! numeric/int/add 20 22;
  do first <- ! numeric/int/to_string value;
  ! system/stdio/write_line first {
    do second <- ! numeric/int/to_string value;
    ! system/stdio/write_line second { ! process/exit 0 }
  }
}
"#,
    );
    let reference = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .arg("run")
        .arg(fixture.source())
        .output()
        .unwrap();
    Fixture::assert_success(&reference);
    assert_eq!(reference.stdout, b"before\n42\n42\n");
    for plan in ["default", "none", "normalize,normalize"] {
        // ZASM's interpreter has no external-call dispatch; the target-selection
        // test covers its lowering, while these backends execute host effects.
        for target in ["wasm-am", "wasm-sps", "exe"] {
            let output = fixture.execute(plan, target);
            Fixture::assert_success(&output);
            assert_eq!(output.stdout, reference.stdout, "{plan}/{target}");
        }
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
#[test]
fn selected_sequences_keep_unused_traps_between_effects() {
    let fixture = Fixture::new(
        r#"
let fix divide (x : Int) (y : Int) : Ret Int = ! numeric/int/div x y in
! system/stdio/write_line "before" {
  do unused <- ! divide 7 0;
  ! system/stdio/write_line "after" { ! process/exit 0 }
}

"#,
    );
    for plan in ["default", "none", "normalize,normalize"] {
        for target in ["wasm-am", "wasm-sps", "exe"] {
            let output = fixture.execute(plan, target);
            assert!(!output.status.success(), "{plan}/{target}");
            assert_eq!(output.stdout, b"before\n", "{plan}/{target}");
            let error = String::from_utf8_lossy(&output.stderr);
            assert!(error.contains("integer division by zero"), "{plan}/{target}: {error}");
            assert!(!error.contains("panicked"), "{plan}/{target}: {error}");
        }
    }
}

#[test]
fn unoptimized_numeric_imports_match_primitive_execution() {
    let mut cases = Vec::new();
    for ty in
        ["Int8", "Int16", "Int32", "Int64", "Int", "UInt8", "UInt16", "UInt32", "UInt64", "UInt"]
    {
        for operation in ["add", "sub", "mul", "div", "mod"] {
            cases.push((ty, operation, "20", "3"));
        }
    }
    for ty in ["Float32", "Float64"] {
        for operation in ["add", "sub", "mul", "div"] {
            cases.push((ty, operation, "20.0", "3.0"));
        }
    }
    cases.extend([
        ("Int64", "add", "9223372036854775807", "1"),
        ("Int64", "div", "-9223372036854775808", "-1"),
        ("Int64", "mod", "-9223372036854775808", "-1"),
        ("UInt64", "sub", "0", "1"),
        ("UInt64", "add", "18446744073709551615", "1"),
        ("UInt64", "div", "18446744073709551615", "2"),
        ("Int8", "add", "127", "1"),
        ("Int16", "sub", "-32768", "1"),
        ("Int32", "mul", "1073741824", "4"),
        ("Int", "add", "4611686018427387903", "1"),
        ("Int", "div", "-4611686018427387904", "-1"),
        ("Int", "mod", "-4611686018427387904", "-1"),
        ("Int", "div", "-20", "3"),
        ("Int", "mod", "-20", "3"),
        ("UInt8", "sub", "0", "1"),
        ("UInt16", "mul", "32768", "2"),
        ("UInt32", "add", "4294967295", "1"),
        ("UInt", "add", "9223372036854775807", "1"),
        ("UInt", "div", "9223372036854775807", "2"),
        ("UInt", "sub", "0", "1"),
        ("Float32", "add", "0.1", "0.2"),
        ("Float64", "add", "0.1", "0.2"),
        ("Float32", "add", "-0.0", "-0.0"),
        ("Float64", "add", "-0.0", "-0.0"),
        ("Float32", "div", "1.0", "0.0"),
        ("Float64", "div", "0.0", "0.0"),
    ]);
    for cases in cases.chunks(24) {
        let body = cases.iter().rev().fold(
            "! process/exit 0".to_owned(),
            |tail, (ty, operation, first, second)| {
                let group = ty.to_lowercase();
                format!("do value <- ! numeric/{group}/{operation} ({first} : {ty}) ({second} : {ty}); do text <- ! numeric/{group}/to_string value; ! system/stdio/write_line text {{ {tail} }}")
            },
        );
        let fixture = Fixture::new(&body);
        let reference = Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .arg("run")
            .arg(fixture.source())
            .output()
            .unwrap();
        Fixture::assert_success(&reference);
        for target in ["wasm-am", "wasm-sps", "exe"] {
            if target == "exe" && !cfg!(any(target_os = "linux", target_os = "macos")) {
                continue;
            }
            for plan in ["default", "none"] {
                let output = fixture.execute(plan, target);
                Fixture::assert_success(&output);
                assert_eq!(output.stdout, reference.stdout, "{plan}/{target}");
            }
        }
    }
}

#[test]
fn unoptimized_integer_zero_divisors_fail_before_the_continuation() {
    for target in ["wasm-am", "wasm-sps", "exe"] {
        if target == "exe" && !cfg!(any(target_os = "linux", target_os = "macos")) {
            continue;
        }
        for ty in [
            "Int8", "Int16", "Int32", "Int64", "Int", "UInt8", "UInt16", "UInt32", "UInt64", "UInt",
        ] {
            for (operation, diagnostic) in
                [("div", "integer division by zero"), ("mod", "integer remainder by zero")]
            {
                let group = ty.to_lowercase();
                let fixture = Fixture::new(&format!(
                    "do unused <- ! numeric/{group}/{operation} (7 : {ty}) (0 : {ty}); ! process/exit 0"
                ));
                let output = fixture.execute("none", target);
                assert!(!output.status.success(), "{ty}/{operation}/{target}");
                assert!(output.stdout.is_empty());
                assert!(String::from_utf8_lossy(&output.stderr).contains(diagnostic));
            }
        }
    }
}
