use std::{
    path::PathBuf,
    process::{Command, Output},
};
use zydeco_cli::{CommandCompiler, HighSpsPlan, TargetOs};

struct Fixture {
    directory: tempfile::TempDir,
    workspace: PathBuf,
    source: PathBuf,
}

impl Fixture {
    fn new(body: &str) -> Self {
        let directory = tempfile::tempdir().unwrap();
        let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
        let source = directory.path().join("comparison.zy");
        std::fs::write(&source, format!(
            "param (/CType; /OS; /Ret; /Thk; /Int8; /Int16; /Int32; /Int64; /Int; /UInt8; /UInt16; /UInt32; /UInt64; /UInt; /Float32; /Float64; /numeric; /process) : @(import(\"{}\")) in {body}\n",
            workspace.join("lib/std/builtin.zy").display(),
        )).unwrap();
        Self { directory, workspace, source }
    }

    fn run(&self, target: &str, plan: &str) -> Output {
        let mut command = Command::new(env!("CARGO_BIN_EXE_zydeco"));
        if target == "interpreter" {
            return command.arg("run").arg(&self.source).output().unwrap();
        }
        let build = self.directory.path().join("build");
        command
            .arg("build")
            .arg(&self.source)
            .args(["--target", target, "--sps-passes", plan, "--verify-passes", "--build-dir"])
            .arg(&build);
        if target == "exe" {
            return command
                .args(["--target-arch", "x86-64", "--runtime-dir"])
                .arg(self.workspace.join("runtime"))
                .arg("--execute")
                .output()
                .unwrap();
        }
        let output = command.output().unwrap();
        assert!(output.status.success(), "{target}: {}", String::from_utf8_lossy(&output.stderr));
        let extension = if target == "wasm-am" { "am" } else { "sps" };
        Command::new(std::env::var_os("NODE").unwrap_or_else(|| "node".into()))
            .arg(self.workspace.join("cli/wasm/wasm-host.mjs"))
            .arg(build.join(format!("comparison.{extension}.wasm")))
            .output()
            .unwrap()
    }

    fn success(output: Output, context: &str) {
        assert!(output.status.success(), "{context}: {}", String::from_utf8_lossy(&output.stderr));
        assert!(output.stdout.is_empty(), "{context}: unexpected output");
    }

    fn execute_all(&self) {
        Self::success(self.run("interpreter", "default"), "interpreter");
        for target in ["wasm-am", "wasm-sps", "exe"] {
            if target == "exe" && !cfg!(any(target_os = "linux", target_os = "macos")) {
                continue;
            }
            for plan in ["default", "none"] {
                Self::success(self.run(target, plan), &format!("{target}/{plan}"));
            }
        }
    }

    fn is_comparison_import(name: &str) -> bool {
        [
            "int8", "int16", "int32", "int64", "int", "uint8", "uint16", "uint32", "uint64",
            "uint", "float32", "float64",
        ]
        .into_iter()
        .any(|ty| {
            ["eq", "lt", "gt"]
                .into_iter()
                .any(|predicate| name.contains(&format!("{ty}_{predicate}_branch")))
        })
    }

    /// All operands remain parameters of a residual function, so every predicate
    /// executes even though the caller supplies constants.
    fn probe(ty: &str, cases: &[(&str, &str, i32)], tail: String) -> String {
        let group = ty.to_lowercase();
        let checks = cases.iter().rev().fold(tail, |tail, (left, right, expected)| format!(
            "do actual <- ! probe {left} {right}; ! numeric/int/eq OS actual {expected} {{ {tail} }} {{ ! process/exit 91 }}"
        ));
        format!(
            "let fix probe (left : {ty}) (right : {ty}) : Ret Int =
            do equal <- ! numeric/{group}/eq (Ret Int) left right {{ ret 1 }} {{ ret 0 }};
            do less <- ! numeric/{group}/lt (Ret Int) left right {{ ret 2 }} {{ ret 0 }};
            do greater <- ! numeric/{group}/gt (Ret Int) left right {{ ret 4 }} {{ ret 0 }};
            do partial <- ! numeric/int/add equal less;
            ! numeric/int/add partial greater
            in {checks}"
        )
    }
}

#[test]
fn integer_comparisons_execute_at_every_width_with_both_lowering_plans() {
    for types in [
        &[
            ("Int8", "-128", "127"),
            ("Int16", "-32768", "32767"),
            ("Int32", "-2147483648", "2147483647"),
            ("Int64", "-9223372036854775808", "9223372036854775807"),
            ("Int", "-4611686018427387904", "4611686018427387903"),
        ][..],
        &[
            ("UInt8", "0", "255"),
            ("UInt16", "0", "65535"),
            ("UInt32", "0", "4294967295"),
            ("UInt64", "0", "18446744073709551615"),
            ("UInt", "0", "9223372036854775807"),
        ][..],
    ] {
        let body =
            types.iter().rev().fold("! process/exit 0".to_owned(), |tail, (ty, min, max)| {
                Fixture::probe(
                    ty,
                    &[(min, min, 1), (max, max, 1), (min, max, 2), (max, min, 4)],
                    tail,
                )
            });
        Fixture::new(&body).execute_all();
    }
}

#[test]
fn float_comparisons_preserve_unordered_values_and_signed_zero() {
    for ty in ["Float32", "Float64"] {
        let group = ty.to_lowercase();
        let body = Fixture::probe(
            ty,
            &[
                ("0.0", "-0.0", 1),
                ("-0.0", "0.0", 1),
                ("-2.0", "3.0", 2),
                ("3.0", "-2.0", 4),
                ("negative_infinity", "infinity", 2),
                ("infinity", "negative_infinity", 4),
                ("infinity", "infinity", 1),
                ("nan", "0.0", 0),
                ("0.0", "nan", 0),
                ("nan", "nan", 0),
            ],
            "! process/exit 0".into(),
        );
        Fixture::new(&format!(
            "do nan <- ! numeric/{group}/div 0.0 0.0;
            do infinity <- ! numeric/{group}/div 1.0 0.0;
            do negative_infinity <- ! numeric/{group}/div -1.0 0.0;
            {body}"
        ))
        .execute_all();
    }
}

#[test]
fn comparisons_emit_instructions_without_host_imports() {
    for (ty, instruction) in
        [("Int64", "cmp"), ("UInt64", "cmp"), ("Float32", "ucomiss"), ("Float64", "ucomisd")]
    {
        let zero = if ty.starts_with("Float") { "0.0" } else { "0" };
        let fixture =
            Fixture::new(&Fixture::probe(ty, &[(zero, zero, 1)], "! process/exit 0".into()));
        for plan in
            [HighSpsPlan::Default, HighSpsPlan::None, "normalize,normalize".parse().unwrap()]
        {
            let backend =
                CommandCompiler::default().with_sps_passes(plan).lower(&fixture.source).unwrap();
            let low = backend.render_sps_low();
            assert!(low.contains(&format!("compare {}_eq", ty.to_lowercase())), "{low}");
            assert!(!Fixture::is_comparison_import(&low));
            for target in [TargetOs::Linux, TargetOs::Macos] {
                let assembly = backend.emit_amd64(target).assembly;
                assert!(
                    assembly
                        .lines()
                        .any(|line| line.split_whitespace().next() == Some(instruction)),
                    "{assembly}"
                );
                assert!(!Fixture::is_comparison_import(&assembly));
            }
            for module in [backend.emit_wasm_am().unwrap(), backend.emit_wasm_sps().unwrap()] {
                wasmparser::Validator::new().validate_all(&module).unwrap();
                let imports = wasmparser::Parser::new(0)
                    .parse_all(&module)
                    .filter_map(|payload| match payload.unwrap() {
                        | wasmparser::Payload::ImportSection(imports) => Some(imports),
                        | _ => None,
                    })
                    .flat_map(|imports| imports.into_imports())
                    .map(|import| import.unwrap().name)
                    .collect::<std::collections::BTreeSet<_>>();
                assert!(
                    imports.iter().all(|name| !Fixture::is_comparison_import(name)),
                    "{imports:?}"
                );
            }
        }
    }
}

#[test]
fn constant_comparisons_and_literal_patterns_eliminate_the_unused_branch() {
    for body in [
        "! numeric/int/eq OS 10 10 { ! process/exit 0 } { ! process/exit 91 }",
        "! numeric/float64/eq OS -0.0 0.0 { ! process/exit 0 } { ! process/exit 91 }",
        "do nan <- ! numeric/float32/div 0.0 0.0; ! numeric/float32/lt OS nan 1.0 { ! process/exit 91 } { ! process/exit 0 }",
        "match (7 : Int64) | 6 => ! process/exit 91 | 7 => ! process/exit 0 | _ => ! process/exit 92 end",
    ] {
        let fixture = Fixture::new(body);
        let low = CommandCompiler::default().lower(&fixture.source).unwrap().render_sps_low();
        assert!(!low.contains("compare "), "{low}");
        assert!(!low.contains("pack-closure("), "{low}");
        assert_eq!(low.matches("<extern:exit/1>").count(), 1, "{low}");
        assert!(low.contains("arg(0)"), "{low}");
    }
}

#[test]
fn direct_comparisons_inline_branches_and_keep_one_shared_continuation() {
    let fixture = Fixture::new(
        "let fix probe (x : Int) : OS =
        do result <- ! numeric/int/lt (Ret Int) x 4 { ret 17 } { ret 23 };
        ! process/exit result in ! probe 3",
    );
    let low = CommandCompiler::default().lower(&fixture.source).unwrap().render_sps_low();
    assert!(low.contains("compare int_lt"), "{low}");
    assert!(
        !low.contains("open-closure "),
        "known comparison branches must not dispatch through thunks: {low}"
    );
    assert_eq!(low.matches("pack-continuation(").count(), 1, "one shared continuation: {low}");
}

#[test]
fn runtime_selected_comparisons_keep_indirect_dispatch_with_primitive_bodies() {
    let fixture = Fixture::new("let Compare = Thk (forall (R : CType) . Int -> Int -> Thk R -> Thk R -> R) in
        let fix choose (x : Int) : Ret Compare =
          ! numeric/int/eq (Ret Compare) x 0 { ret numeric/int/lt } { ret numeric/int/gt }
        in do less <- ! choose 0;
        do greater <- ! choose 1;
        ! less OS 1 2 { ! greater OS 2 1 { ! process/exit 0 } { ! process/exit 91 } } { ! process/exit 92 }");
    let low = CommandCompiler::default().lower(&fixture.source).unwrap().render_sps_low();
    assert!(low.contains("compare int_lt") && low.contains("compare int_gt"), "{low}");
    assert!(low.contains("pack-closure(") && low.contains("open-closure "), "{low}");
    assert!(!low.contains("_branch/4>"), "{low}");
    fixture.execute_all();
}

#[test]
fn comparison_aliases_preserve_captures_and_argument_stacks_across_calls() {
    let fixture = Fixture::new(
        "let less = numeric/int/lt in
        let equal = numeric/int/eq in
        let fix choose (x : Int) : Int -> Ret Int =
          ! less (Int -> Ret Int) x 0
            { fn (value : Int) => ! numeric/int/add value x }
            { fn (value : Int) => ! numeric/int/sub value x }
        in do first <- ! choose -1 18;
        do second <- ! choose 1 24;
        ! equal OS first 17
          { ! equal OS second 23 { ! process/exit 0 } { ! process/exit 91 } }
          { ! process/exit 92 }",
    );
    fixture.execute_all();
}
