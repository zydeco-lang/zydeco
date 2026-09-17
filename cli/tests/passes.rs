use std::{
    path::PathBuf,
    process::{Command, Output},
};
use zydeco_cli::{CommandCompiler, HighSpsPlan};

struct Fixture {
    directory: tempfile::TempDir,
    workspace: PathBuf,
    source: PathBuf,
}

impl Fixture {
    fn new(body: &str) -> Self {
        let directory = tempfile::tempdir().unwrap();
        let source = directory.path().join("program.zy");
        let fixture = Self {
            directory,
            workspace: PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(".."),
            source,
        };
        std::fs::write(fixture.source(), format!(
            "param (/system; /process; /numeric; /OS; /Ret; /Thk; /Addr; /Int8; /Int16; /Int32; /Int64; /Int; /UInt8; /UInt16; /UInt32; /UInt64; /UInt; /Float32; /Float64) : @(import(\"{}\")) in {body}\n",
            fixture.workspace.join("lib/std/builtin.zy").display(),
        )).unwrap();
        fixture
    }

    fn source(&self) -> PathBuf {
        self.source.clone()
    }

    fn with_source(relative: &str) -> Self {
        let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
        Self {
            directory: tempfile::tempdir().unwrap(),
            source: workspace.join(relative),
            workspace,
        }
    }
    fn build_dir(&self) -> PathBuf {
        self.directory.path().join("build")
    }

    fn build(&self, selection: &str, target: &str, extra: &[&str]) -> Output {
        Command::new(env!("CARGO_BIN_EXE_zydeco"))
            .current_dir(&self.workspace)
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
        self.execute_options(selection, target, &[])
    }

    fn execute_options(&self, selection: &str, target: &str, extra: &[&str]) -> Output {
        if target == "exe" || target == "zasm" {
            let extra = extra.iter().copied().chain(["--execute"]).collect::<Vec<_>>();
            return self.build(selection, target, &extra);
        }
        let compiled = self.build(selection, target, extra);
        Self::assert_success(&compiled);
        let extension = if target == "wasm-am" { "am" } else { "sps" };
        Command::new(std::env::var_os("NODE").unwrap_or_else(|| "node".into()))
            .arg(self.workspace.join("cli/wasm/wasm-host.mjs"))
            .arg(self.build_dir().join(format!(
                "{}.{extension}.wasm",
                self.source.file_stem().unwrap().to_string_lossy()
            )))
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
fn runtime_constructor_bindings_work_with_and_without_normalization() {
    let fixture = Fixture::new(
        r#"
let Wrapped = data | +Wrap : Int end in
let val unwrap (wrapped : Wrapped) : Int =
  let +Wrap(value) = wrapped in value
in
do wrapped <- ret (+Wrap(37) : Wrapped);
! process/exit (unwrap wrapped)
"#,
    );
    for plan in ["none", "default", "normalize,normalize"] {
        for target in ["wasm-am", "wasm-sps", "exe"] {
            let output = fixture.execute_options(plan, target, &["--verify-passes"]);
            assert_eq!(
                output.status.code(),
                Some(37),
                "{plan}/{target}: {}",
                String::from_utf8_lossy(&output.stderr)
            );
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

#[test]
fn address_offsets_lower_without_host_calls_and_preserve_wrapping() {
    use zydeco_assembly::syntax::{Extern, Instruction, Program, Terminator};
    use zydeco_syntax::BuiltinValueRole;

    let fixture = Fixture::new(
        r#"
let fail = { fn (_ : Int) => ! process/exit 41 } in
! system/memory/allocate OS 16 8 fail {
  fn base =>
    do null <- ! system/memory/null;
    do before_null <- ! system/memory/offset null (-1);
    do wrapped <- ! system/memory/offset before_null 1;
    ! system/memory/store_addr OS base wrapped {
      ! numeric/uint64/load_le OS base {
        fn bits =>
          ! numeric/uint64/eq OS bits (0 : UInt64) {
            do after <- ! system/memory/offset base 9;
            do inside <- ! system/memory/offset after (-8);
            ! numeric/uint8/store_le OS inside (42 : UInt8) {
              do expected <- ! system/memory/offset base 1;
              ! numeric/uint8/load_le OS expected {
                fn value =>
                  ! system/memory/free OS base 16 8 fail {
                    ! numeric/uint8/eq OS value (42 : UInt8)
                      { ! process/exit 0 } { ! process/exit 42 }
                  }
              }
            }
          } { ! process/exit 43 }
      }
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
    for selection in ["none", "default", "normalize,normalize"] {
        let compiler = CommandCompiler::default().with_sps_passes(selection.parse().unwrap());
        let backend = compiler.lower(&fixture.source()).unwrap();
        let programs = &backend.assembly().arena().programs;
        assert!(programs.iter().any(|(_, program)| matches!(
            program,
            Program::Instruction(Instruction::AddrOffset, _)
        )));
        assert!(!programs.iter().any(|(_, program)| matches!(
            program,
            Program::Terminator(Terminator::Extern(Extern::Host {
                role: BuiltinValueRole::MemoryOffset,
                ..
            }))
        )));
        for target in ["exe", "wasm-am", "wasm-sps"] {
            Fixture::assert_success(&fixture.execute(selection, target));
        }
    }
}

#[test]
fn known_zero_address_offset_disappears() {
    use zydeco_assembly::syntax::{Instruction, Program};
    let fixture = Fixture::new(
        r#"
let fail = { fn (_ : Int) => ! process/exit 41 } in
! system/memory/allocate OS 8 8 fail {
  fn base =>
    let offset = system/memory/offset in
    do same <- ! offset base 0;
    do same <- ! offset same 0;
    ! numeric/uint64/store_le OS same (42 : UInt64) {
      ! system/memory/free OS base 8 8 fail { ! process/exit 0 }
    }
}
"#,
    );
    let backend = CommandCompiler::default().lower(&fixture.source()).unwrap();
    assert!(
        !backend.assembly().arena().programs.iter().any(|(_, program)| matches!(
            program,
            Program::Instruction(Instruction::AddrOffset, _)
        ))
    );
}

#[test]
fn scalar_memory_repeated_builtin_loads_keep_independent_observations() {
    let fixture = Fixture::new(
        r#"
let fail = { ! process/exit 41 } in
let no = { fn (_ : Int) => ! fail } in
! system/memory/allocate OS 8 8 no { fn address =>
  let load = numeric/int/load_le in
  ! numeric/int/store_le OS address 4 {
    ! load OS address { fn saved =>
      ! numeric/int/store_le OS address 0 {
        ! load OS address { fn current =>
          ! numeric/int/eq OS saved 4 {
            ! numeric/int/eq OS current 0 {
              ! system/memory/free OS address 8 8 no { ! process/exit 0 }
            } fail
          } fail
        }
      }
    }
  }
}
"#,
    );
    for selection in ["none", "default"] {
        for target in ["exe", "wasm-am", "wasm-sps"] {
            Fixture::assert_success(&fixture.execute(selection, target));
        }
    }
}

#[test]
fn scalar_memory_accesses_preserve_width_endianness_and_unaligned_extremes() {
    use zydeco_assembly::syntax::{Extern, Program, Terminator};
    use zydeco_syntax::memory::{MemoryAccess, MemoryScalar};
    use zydeco_syntax::{IntegerLiteral, Literal};
    let values = [
        IntegerLiteral::Int8(i8::MIN),
        IntegerLiteral::Int8(i8::MAX),
        IntegerLiteral::Int16(i16::MIN),
        IntegerLiteral::Int16(i16::MAX),
        IntegerLiteral::Int32(i32::MIN),
        IntegerLiteral::Int32(i32::MAX),
        IntegerLiteral::Int64(i64::MIN),
        IntegerLiteral::Int64(i64::MAX),
        IntegerLiteral::Int(-(1 << 62)),
        IntegerLiteral::Int((1 << 62) - 1),
        IntegerLiteral::UInt8(u8::MAX),
        IntegerLiteral::UInt16(u16::MAX),
        IntegerLiteral::UInt32(u32::MAX),
        IntegerLiteral::UInt64(u64::MAX),
        IntegerLiteral::UInt((1 << 63) - 1),
    ];
    for value in values {
        let next = "! system/memory/free OS base 16 8 fail { ! process/exit 0 }".to_owned();
        let ty = value.integer_type().unwrap();
        let group = format!("{ty}").to_lowercase();
        let scalar = MemoryScalar::Integer(ty);
        let bytes = scalar.bits(&Literal::Integer(value)).unwrap().to_le_bytes();
        let checked = bytes[..scalar.bytes() as usize].iter().enumerate().rev().fold(next, |next, (index, byte)| format!(
            "do byte_address <- ! system/memory/offset slot {index}; ! numeric/uint8/load_le OS byte_address {{ fn byte => ! numeric/uint8/eq OS byte {byte} {{ {next} }} bad }}"
        ));
        let after = scalar.bytes() + 1;
        let body = format!(
            r#"
! system/memory/fill OS base 16 (165 : UInt8) {{
  ! numeric/{group}/store_le OS slot ({number} : {ty}) {{
    ! numeric/{group}/load_le OS slot {{ fn loaded =>
      ! numeric/{group}/eq OS loaded ({number} : {ty}) {{
        ! numeric/uint8/load_le OS base {{ fn before =>
          ! numeric/uint8/eq OS before (165 : UInt8) {{
            do after <- ! system/memory/offset base {after};
            ! numeric/uint8/load_le OS after {{ fn after =>
              ! numeric/uint8/eq OS after (165 : UInt8) {{ {checked} }} bad
            }}
          }} bad
        }}
      }} bad
    }}
  }}
}}
"#,
            number = value.value()
        );
        let fixture = Fixture::new(&format!(
            r#"
let fail = {{ fn (_ : Int) => ! process/exit 41 }} in
let bad = {{ ! process/exit 42 }} in
! system/memory/allocate OS 16 8 fail {{ fn base =>
  do slot <- ! system/memory/offset base 1;
  {body}
}}
"#
        ));
        let backend = CommandCompiler::default().lower(&fixture.source()).unwrap();
        assert!(!backend.assembly().arena().programs.iter().any(|(_, program)| matches!(program,
        Program::Terminator(Terminator::Extern(Extern::Host { role, .. })) if MemoryAccess::from_builtin(*role).is_some())));
        Fixture::assert_success(
            &Command::new(env!("CARGO_BIN_EXE_zydeco"))
                .arg("run")
                .arg(fixture.source())
                .output()
                .unwrap(),
        );
        for selection in ["none", "default"] {
            for target in ["exe", "wasm-am", "wasm-sps"] {
                Fixture::assert_success(&fixture.execute(selection, target));
            }
        }
    }
}

#[test]
fn scalar_memory_load_boxing_preserves_live_roots_during_collection() {
    let fixture = Fixture::new(
        r#"
let fail = { fn (_ : Int) => ! process/exit 41 } in
let fix churn (address : Addr) (keep : Int64) (count : Int) : OS =
  ! numeric/int/eq OS count 0 {
    ! system/memory/free OS address 8 8 fail {
      ! numeric/int64/eq OS keep (4096 : Int64) { ! process/exit 0 } { ! process/exit 42 }
    }
  } {
    ! numeric/uint64/load_le OS address { fn value =>
      do copied <- ! numeric/uint64/add value (0 : UInt64);
      ! numeric/uint64/store_le OS address copied {
        do next <- ! numeric/int/sub count 1;
        ! churn address keep next
      }
    }
  }
in
do keep <- ! numeric/int64/from_int 4096;
! system/memory/allocate OS 8 8 fail { fn address =>
  ! numeric/uint64/store_le OS address (18446744073709551615 : UInt64) {
    ! churn address keep 200000
  }
}

"#,
    );
    for representation in ["boxed", "local"] {
        Fixture::assert_success(&fixture.execute_options(
            "default",
            "exe",
            &["--representation", representation],
        ));
    }
}

#[test]
fn scalar_memory_keeps_float_bits_through_unknown_callbacks() {
    for (float, integer, bits) in [
        ("Float32", "UInt32", 0x8000_0000_u64),
        ("Float32", "UInt32", 0x7fc1_2345),
        ("Float64", "UInt64", 0x8000_0000_0000_0000),
        ("Float64", "UInt64", 0x7ff8_1234_5678_9abc),
    ] {
        let floats = float.to_lowercase();
        let integers = integer.to_lowercase();
        let fixture = Fixture::new(&format!(
            r#"
let fail = {{ fn (_ : Int) => ! process/exit 41 }} in
let fix read (address : Addr) (next : Thk ({float} -> OS)) : OS =
  ! numeric/{floats}/load_le OS address next
in
! system/memory/allocate OS 24 8 fail {{ fn base =>
  do source <- ! system/memory/offset base 1;
  do destination <- ! system/memory/offset base 11;
  ! numeric/{integers}/store_le OS source ({bits} : {integer}) {{
    ! read source {{ fn value =>
      ! numeric/{floats}/store_le OS destination value {{
        ! numeric/{integers}/load_le OS destination {{ fn result =>
          ! system/memory/free OS base 24 8 fail {{
            ! numeric/{integers}/eq OS result ({bits} : {integer})
              {{ ! process/exit 0 }} {{ ! process/exit 42 }}
          }}
        }}
      }}
    }}
  }}
}}
"#
        ));
        Fixture::assert_success(
            &Command::new(env!("CARGO_BIN_EXE_zydeco"))
                .arg("run")
                .arg(fixture.source())
                .output()
                .unwrap(),
        );
        for selection in ["none", "default"] {
            for target in ["exe", "wasm-am", "wasm-sps"] {
                Fixture::assert_success(&fixture.execute(selection, target));
            }
        }
    }
}

#[test]
fn scalar_memory_checks_invalid_carriers_before_resuming_or_later_effects() {
    for (group, bits) in [("int", 1_u64 << 62), ("int", (3_u64 << 62) - 1), ("uint", 1_u64 << 63)] {
        let fixture = Fixture::new(&format!(
            r#"
let fail = {{ fn (_ : Int) => ! process/exit 41 }} in
! system/memory/allocate OS 8 8 fail {{ fn address =>
  ! numeric/uint64/store_le OS address ({bits} : UInt64) {{
    ! system/stdio/write_line "before" {{
      ! numeric/{group}/load_le OS address {{ fn _ =>
        ! numeric/uint64/store_le OS address (0 : UInt64) {{
          ! system/stdio/write_line "after" {{ ! process/exit 0 }}
        }}
      }}
    }}
  }}
}}
"#
        ));
        for selection in ["none", "default"] {
            for target in ["exe", "wasm-am", "wasm-sps"] {
                let output = fixture.execute(selection, target);
                assert_eq!(output.status.code(), Some(1));
                assert_eq!(output.stdout, b"before\n");
                let error = String::from_utf8_lossy(&output.stderr);
                assert!(
                    error.contains("integer exceeds the tagged payload range"),
                    "{selection}/{target}: {error}"
                );
                assert!(!error.contains("panicked"));
            }
        }
    }
}

#[test]
fn scalar_regions_remove_boxes_and_preserve_results_across_backends() {
    use zydeco_assembly::syntax::{Instruction, Program};
    use zydeco_cli::RepresentationStrategy;
    let fixture = Fixture::new(
        r#"
let fix floating (x : Float64) (n : Int) : Ret Float64 =
  ! numeric/int/eq (Ret Float64) n 0 { ret x } {
    do y <- ! numeric/float64/add x 1.0;
    do z <- ! numeric/float64/mul y 2.0;
    do m <- ! numeric/int/sub n 1;
    ! floating z m
  }
in
do x <- ! numeric/int64/from_int 4096;
do y <- ! numeric/int64/add x (9223372036854775807 : Int64);
do z <- ! numeric/int64/sub y (9223372036854775807 : Int64);
do text <- ! numeric/int64/to_string z;
! system/stdio/write_line text {
  do x <- ! numeric/uint64/from_uint 4096;
  do y <- ! numeric/uint64/add x (18446744073709551615 : UInt64);
  do z <- ! numeric/uint64/mul y (2 : UInt64);
  do text <- ! numeric/uint64/to_string z;
  ! system/stdio/write_line text {
    do z <- ! floating 2.0 4;
    do text <- ! numeric/float64/to_string z;
    ! system/stdio/write_line text { ! process/exit 0 }
  }
}

"#,
    );
    let counts = [RepresentationStrategy::Boxed, RepresentationStrategy::Local].map(|policy| {
        let compiler = CommandCompiler::default().with_representation(policy);
        let backend = compiler.lower(&fixture.source()).unwrap();
        backend
            .assembly()
            .arena()
            .programs
            .iter()
            .map(|(_, program)| match program {
                | Program::Instruction(Instruction::Scalar(region), _) => region.allocation_count(),
                | _ => 0,
            })
            .sum::<usize>()
    });
    assert_eq!(
        counts[0] - counts[1],
        3,
        "one intermediate box removed from each of three scalar chains"
    );
    let reference = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .arg("run")
        .arg(fixture.source())
        .output()
        .unwrap();
    Fixture::assert_success(&reference);
    assert_eq!(reference.stdout, b"4096\n8190\n62\n");
    for policy in ["boxed", "local"] {
        for target in ["exe", "wasm-am", "wasm-sps"] {
            let output = fixture.execute_options("default", target, &["--representation", policy]);
            Fixture::assert_success(&output);
            assert_eq!(output.stdout, reference.stdout, "{policy}/{target}");
        }
    }
}

#[test]
fn scalar_regions_survive_collection_and_keep_failure_order() {
    for (ty, group, initial, increment, expected) in [
        ("Int64", "int64", "9223372036854775807", "1", "-9223372036854575809"),
        ("UInt64", "uint64", "18446744073709551615", "1", "199999"),
        ("Float64", "float64", "1.5", "1.0", "200001.5"),
    ] {
        let fixture = Fixture::new(&format!(
            r#"
let fix churn (n : Int) (value : {ty}) : Ret {ty} =
  ! numeric/int/eq (Ret {ty}) n 0 {{ ret value }} {{
    do first <- ! numeric/{group}/add value ({increment} : {ty});
    do second <- ! numeric/{group}/add first ({increment} : {ty});
    do remaining <- ! numeric/int/sub n 1;
    ! churn remaining second
  }}
in
do result <- ! churn 100000 ({initial} : {ty});
! numeric/{group}/eq OS result ({expected} : {ty}) {{ ! process/exit 0 }} {{ ! process/exit 42 }}
"#
        ));
        for policy in ["boxed", "local"] {
            Fixture::assert_success(&fixture.execute_options(
                "default",
                "exe",
                &["--representation", policy],
            ));
        }
    }
    // Both divisions remain runtime operations. Fusion must keep the first
    // failure, even when the later operation also fails and neither returns.
    let fixture = Fixture::new(
        r#"
! system/stdio/write_line "before" {
  do first <- ! numeric/int64/mod (7 : Int64) (0 : Int64);
  do second <- ! numeric/int64/div first (0 : Int64);
  ! system/stdio/write_line "after" { ! process/exit 0 }
}
"#,
    );
    for policy in ["boxed", "local"] {
        for target in ["exe", "wasm-am", "wasm-sps"] {
            let output = fixture.execute_options("default", target, &["--representation", policy]);
            assert!(!output.status.success());
            assert_eq!(output.stdout, b"before\n", "{policy}/{target}");
            let error = String::from_utf8_lossy(&output.stderr);
            assert!(error.contains("integer remainder by zero"), "{policy}/{target}: {error}");
            assert!(!error.contains("panicked"));
        }
    }
}

#[test]
fn scalar_memory_kernels_keep_values_raw_and_preserve_ordinary_fallbacks() {
    use zydeco_assembly::syntax::{Instruction, Program};
    use zydeco_cli::RepresentationStrategy;
    for (ty, group, initial, amount, expected) in [
        ("Int64", "int64", "9223372036854775807", "1", "-9223372036854775808"),
        ("UInt64", "uint64", "18446744073709551615", "1", "0"),
        ("Float64", "float64", "1.25", "0.5", "1.75"),
    ] {
        let one = if ty == "Float64" { "1.0" } else { "1" };
        let fixture = Fixture::new(&format!(
            r#"
let fail = {{ fn (_ : Int) => ! process/exit 41 }} in
let fix update (source : Addr) (destination : Addr) (amount : {ty}) : OS =
  ! numeric/{group}/load_le OS source {{ fn value =>
    do first <- ! numeric/{group}/add value amount;
    do result <- ! numeric/{group}/mul first ({one} : {ty});
    ! numeric/{group}/store_le OS destination result {{
      ! numeric/{group}/load_le OS destination {{ fn observed =>
        ! system/memory/free OS source 16 8 fail {{
          ! numeric/{group}/eq OS observed ({expected} : {ty}) {{ ! process/exit 0 }} {{ ! process/exit 42 }}
        }}
      }}
    }}
  }}
in
! system/memory/allocate OS 16 8 fail {{ fn source =>
  do destination <- ! system/memory/offset source 8;
  ! numeric/{group}/store_le OS source ({initial} : {ty}) {{
    ! update source destination ({amount} : {ty})
  }}
}}
"#
        ));
        for (policy, expected_kernels) in
            [(RepresentationStrategy::Boxed, 0), (RepresentationStrategy::Local, 1)]
        {
            let backend = CommandCompiler::default()
                .with_representation(policy)
                .lower(&fixture.source())
                .unwrap();
            let kernels = backend
                .assembly()
                .arena()
                .programs
                .iter()
                .filter_map(|(_, program)| match program {
                    | Program::Instruction(Instruction::MemoryKernel(kernel), _) => Some(kernel),
                    | _ => None,
                })
                .collect::<Vec<_>>();
            assert_eq!(kernels.len(), expected_kernels, "{group}");
            if let Some(kernel) = kernels.first() {
                assert_eq!(kernel.region().inputs.len(), 2, "load plus the runtime amount");
            }
        }
        let assembly = fixture.build("default", "asm", &["--representation", "local"]);
        Fixture::assert_success(&assembly);
        let text = String::from_utf8(assembly.stdout).unwrap();
        let kernel = text
            .split("raw memory kernel: begin")
            .nth(1)
            .unwrap()
            .split("raw memory kernel: end")
            .next()
            .unwrap();
        assert!(!kernel.contains("call"), "successful arithmetic has no calls: {kernel}");
        assert!(!kernel.contains("alloc"));
        assert!(kernel.contains("mov QWORD [rcx], rax"), "one final exact-width store: {kernel}");
        for policy in ["boxed", "local"] {
            for target in ["exe", "wasm-am", "wasm-sps"] {
                Fixture::assert_success(&fixture.execute_options(
                    "default",
                    target,
                    &["--representation", policy],
                ));
            }
        }
    }
}

#[test]
fn scalar_memory_kernel_failure_precedes_the_store_and_later_arithmetic() {
    use zydeco_assembly::syntax::{Instruction, Program};
    let fixture = Fixture::new(
        r#"
let fail = { fn (_ : Int) => ! process/exit 41 } in
let fix update (address : Addr) (zero : Int64) : OS =
  ! system/stdio/write_line "before" {
    ! numeric/int64/load_le OS address { fn value =>
      do quotient <- ! numeric/int64/div value zero;
      do remainder <- ! numeric/int64/mod quotient zero;
      ! numeric/int64/store_le OS address remainder {
        ! system/stdio/write_line "after" { ! process/exit 42 }
      }
    }
  }
in
! system/memory/allocate OS 8 8 fail { fn address =>
  ! numeric/int64/store_le OS address (7 : Int64) {
    ! update address (0 : Int64)
  }
}
"#,
    );
    let backend = CommandCompiler::default().lower(&fixture.source()).unwrap();
    assert_eq!(
        backend
            .assembly()
            .arena()
            .programs
            .iter()
            .filter(|(_, program)| matches!(
                program,
                Program::Instruction(Instruction::MemoryKernel(_), _)
            ))
            .count(),
        1
    );
    for policy in ["boxed", "local"] {
        for target in ["exe", "wasm-am", "wasm-sps"] {
            let output = fixture.execute_options("default", target, &["--representation", policy]);
            assert_eq!(output.status.code(), Some(1));
            assert_eq!(output.stdout, b"before\n");
            let message = String::from_utf8_lossy(&output.stderr);
            assert!(message.contains("integer division by zero"), "{message}");
            assert!(!message.contains("remainder by zero"));
        }
    }
}

#[test]
fn scalar_memory_kernel_stops_at_shared_values_and_unknown_callbacks() {
    use zydeco_assembly::syntax::{Instruction, Program};
    for (reader, continuation) in [
        (
            "numeric/int64/load_le OS",
            "! numeric/int64/eq OS result (8 : Int64) { ! process/exit 0 } { ! process/exit 42 }",
        ),
        ("read", "! process/exit 0"),
    ] {
        let fixture = Fixture::new(&format!(
            r#"
let fail = {{ fn (_ : Int) => ! process/exit 41 }} in
let fix read (address : Addr) (next : Thk (Int64 -> OS)) : OS =
  ! numeric/int64/load_le OS address next
in
! system/memory/allocate OS 8 8 fail {{ fn address =>
  ! numeric/int64/store_le OS address (7 : Int64) {{
    ! {reader} address {{ fn value =>
      do result <- ! numeric/int64/add value (1 : Int64);
      ! numeric/int64/store_le OS address result {{
        ! system/memory/free OS address 8 8 fail {{ {continuation} }}
      }}
    }}
  }}
}}
"#
        ));
        let backend = CommandCompiler::default().lower(&fixture.source()).unwrap();
        assert!(!backend.assembly().arena().programs.iter().any(|(_, program)| matches!(
            program,
            Program::Instruction(Instruction::MemoryKernel(_), _)
        )));
        for target in ["exe", "wasm-am", "wasm-sps"] {
            Fixture::assert_success(&fixture.execute("default", target));
        }
    }
}

#[test]
fn scalar_memory_kernel_bounds_preserve_fallback_and_aliasing() {
    use zydeco_assembly::syntax::{Instruction, Program};
    for (operations, expected_kernels) in [(32, 1), (33, 0)] {
        let arithmetic = (0..operations)
            .map(|index| {
                format!("do v{} <- ! numeric/int64/add v{} (1 : Int64);\n", index + 1, index)
            })
            .collect::<String>();
        let fixture = Fixture::new(&format!(
            r#"
let fail = {{ fn (_ : Int) => ! process/exit 41 }} in
! system/memory/allocate OS 16 8 fail {{ fn address =>
  ! numeric/int64/store_le OS address (7 : Int64) {{
    do destination <- ! system/memory/offset address 1;
    ! numeric/int64/load_le OS address {{ fn v0 =>
      {arithmetic}
      ! numeric/int64/store_le OS destination v{operations} {{
        ! numeric/int64/load_le OS destination {{ fn result =>
          ! system/memory/free OS address 16 8 fail {{
            ! numeric/int64/eq OS result ({expected} : Int64) {{ ! process/exit 0 }} {{ ! process/exit 42 }}
          }}
        }}
      }}
    }}
  }}
}}
"#,
            expected = 7 + operations
        ));
        let backend = CommandCompiler::default().lower(&fixture.source()).unwrap();
        assert_eq!(
            backend
                .assembly()
                .arena()
                .programs
                .iter()
                .filter(|(_, program)| matches!(
                    program,
                    Program::Instruction(Instruction::MemoryKernel(_), _)
                ))
                .count(),
            expected_kernels
        );
        for target in ["exe", "wasm-am", "wasm-sps"] {
            Fixture::assert_success(&fixture.execute("default", target));
        }
    }
}

#[test]
fn scalar_memory_kernel_moves_only_total_independent_address_bindings() {
    use zydeco_assembly::syntax::{Instruction, Program};
    for (displacement, expected_kernels, fails) in
        [("8", 1, false), ("calculated", 0, false), ("calculated", 0, true)]
    {
        let calculate = if displacement == "calculated" {
            "do calculated <- ! numeric/int/div 8 divisor;"
        } else {
            ""
        };
        let arithmetic = if fails { "mod value (0 : Int64)" } else { "add value (1 : Int64)" };
        let divisor = if fails { 0 } else { 1 };
        let fixture = Fixture::new(&format!(
            r#"
let fail = {{ fn (_ : Int) => ! process/exit 41 }} in
let fix run (divisor : Int) : OS =
! system/memory/allocate OS 16 8 fail {{ fn address =>
  ! numeric/int64/store_le OS address (7 : Int64) {{
    ! numeric/int64/load_le OS address {{ fn value =>
      do updated <- ! numeric/int64/{arithmetic};
      {calculate}
      do destination <- ! system/memory/offset address {displacement};
      ! numeric/int64/store_le OS destination updated {{
        ! numeric/int64/load_le OS destination {{ fn observed =>
          ! numeric/int64/eq OS observed (8 : Int64) {{
            ! system/memory/free OS address 16 8 fail {{ ! process/exit 0 }}
          }} {{ ! process/exit 42 }}
        }}
      }}
    }}
  }}
}}
in ! run {divisor}
"#
        ));
        let backend = CommandCompiler::default().lower(&fixture.source()).unwrap();
        assert_eq!(
            backend
                .assembly()
                .arena()
                .programs
                .iter()
                .filter(|(_, program)| matches!(
                    program,
                    Program::Instruction(Instruction::MemoryKernel(_), _)
                ))
                .count(),
            expected_kernels
        );
        for policy in ["boxed", "local"] {
            for target in ["exe", "wasm-am", "wasm-sps"] {
                let output =
                    fixture.execute_options("default", target, &["--representation", policy]);
                if fails {
                    assert_eq!(output.status.code(), Some(1));
                    let error = String::from_utf8_lossy(&output.stderr);
                    assert!(
                        error.contains("integer remainder by zero"),
                        "{policy}/{target}: {error}"
                    );
                    assert!(!error.contains("panicked"));
                } else {
                    Fixture::assert_success(&output);
                }
            }
        }
    }
}

#[test]
fn typed_memory_kernels_preserve_checked_header_updates() {
    for source in [
        "docs/evaluations/2026-09-15-typed-memory/header-raw.zy",
        "docs/evaluations/2026-09-15-typed-memory/header-typed.zy",
        "lib/tests/std/typed-memory-kernel.zy",
    ] {
        let fixture = Fixture::with_source(source);
        let mut opaque_sites = Vec::new();
        for (policy, expected_kernels) in [("boxed", 0), ("local", 1)] {
            let assembly = fixture.build("default", "asm", &["--representation", policy]);
            Fixture::assert_success(&assembly);
            let assembly = String::from_utf8(assembly.stdout).unwrap();
            opaque_sites
                .push(assembly.matches("allocate opaque block in the copying heap").count());
            let kernels = assembly.split("raw memory kernel: begin").skip(1).collect::<Vec<_>>();
            assert_eq!(kernels.len(), expected_kernels, "{source}/{policy}");
            for kernel in kernels {
                let kernel = kernel.split("raw memory kernel: end").next().unwrap();
                assert!(!kernel.contains("call"), "{source}: {kernel}");
                assert!(!kernel.contains("alloc"), "{source}: {kernel}");
                assert_eq!(kernel.matches("mov QWORD [rcx], rax").count(), 1);
            }
            for target in ["exe", "wasm-am", "wasm-sps"] {
                Fixture::assert_success(&fixture.execute_options(
                    "default",
                    target,
                    &["--representation", policy],
                ));
            }
        }
        assert_eq!(
            opaque_sites[0] - opaque_sites[1],
            3,
            "load, literal, and arithmetic result: {source}"
        );
    }
}
