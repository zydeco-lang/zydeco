use std::path::PathBuf;
use zydeco_cli::CommandCompiler;
use zydeco_tests::e2e_sources;
use zydeco_tests::utils::{ExecutionTarget, SourceProgram};

e2e_sources!({
    add => "tests/builtin/add.zy",
    sub => "tests/builtin/sub.zy",
    mul => "tests/builtin/mul.zy",
    cmp => "tests/builtin/cmp.zy",
    env => "tests/builtin/env.zy",
    host_return => "tests/builtin/host-return.zy",
    host_runtime => "tests/builtin/host-runtime.zy",
    numeric_widths => "tests/builtin/numeric-widths.zy",
    primitive_aliases => "tests/builtin/primitive-aliases.zy",
    primitive_selection => "tests/builtin/primitive-selection.zy",
});

struct PrimitiveFixture;

impl PrimitiveFixture {
    fn sps_low(name: &str) -> String {
        let path =
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/tests/builtin").join(name);
        CommandCompiler::default()
            .lower(&path)
            .expect("primitive fixture must lower")
            .render_sps_low()
    }
}

#[test]
fn known_primitive_calls_do_not_allocate_thunks() {
    for fixture in ["add.zy", "primitive-aliases.zy"] {
        let sps = PrimitiveFixture::sps_low(fixture);
        assert!(!sps.contains("<extern:int_add/2>"), "{sps}");
        assert!(!sps.contains("pack-continuation("), "arithmetic returns must be inlined:\n{sps}");
        assert_eq!(sps.matches("<extern:exit/1>").count(), 1, "{sps}");
        assert!(!sps.contains("pack-closure("), "known calls must not allocate thunks:\n{sps}");
        assert!(
            !sps.contains("open-closure "),
            "known calls must not dispatch through thunks:\n{sps}"
        );
    }
}

#[test]
fn constant_primitive_selection_eliminates_dispatch() {
    let sps = PrimitiveFixture::sps_low("primitive-selection.zy");
    assert!(!sps.contains("<extern:int_eq_branch/4>"), "{sps}");
    assert!(!sps.contains("pack-closure("), "constant selection must fold:\n{sps}");
    assert!(!sps.contains("open-closure "), "the selected primitive is known:\n{sps}");
}

// `exit.zy` is driven by the CLI build test and the TUI engine; the `echo*`
// programs are run by the session crate with exact-output checks on the
// interpreter.

mod host_arguments {
    use super::*;

    fn program() -> SourceProgram {
        SourceProgram::setup("tests/builtin/host-arguments.zy").with_args(["alpha", "beta"])
    }

    #[test]
    fn interpreter() {
        program().test(ExecutionTarget::Interpreter);
    }

    #[test]
    fn exe() {
        program().test(ExecutionTarget::Exe);
    }

    #[test]
    fn wasm_sps() {
        program().test(ExecutionTarget::WasmSps);
    }
}

mod argument_contract {
    use super::*;

    fn program() -> SourceProgram {
        SourceProgram::setup("tests/builtin/argument-contract.zy").with_args(["alpha", "beta"])
    }

    #[test]
    fn interpreter() {
        program().test(ExecutionTarget::Interpreter);
    }

    #[test]
    fn exe() {
        program().test(ExecutionTarget::Exe);
    }

    #[test]
    fn wasm_sps() {
        program().test(ExecutionTarget::WasmSps);
    }
}

#[test]
fn intrinsic_primitive_operations_execute_across_all_backends() {
    let dir = tempfile::tempdir().unwrap();
    let std =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/std").canonicalize().unwrap();
    let path = dir.path().join("intrinsics.zy");
    std::fs::write(
        &path,
        format!(
            r#"
param (/process; /stdio) : @(import("{std}/builtin.zy")) in
let add = @(intrinsic(int/add)) in
let eq = @(intrinsic(int/eq)) in
let append = @(intrinsic(string/append)) in
let codepoint = @(intrinsic(char/codepoint)) in
let widen = @(intrinsic(int64/from_int)) in
let eq64 = @(intrinsic(int64/eq)) in
let vadd = @(intrinsic(val/int/add)) in
let val_sum = vadd 3 4 in
do dyn_sum <- ! add val_sum 5;
! eq OS dyn_sum 12 {{
  do greeting <- ! append "ok-" "yes";
  do cp <- ! codepoint 'A';
  ! eq OS cp 65 {{
    do w <- ! widen 9;
    ! eq64 OS w (9 : Int64) {{
      ! stdio/write_line greeting {{ ! process/exit 0 }}
    }} {{ ! process/exit 3 }}
  }} {{ ! process/exit 2 }}
}} {{ ! process/exit 1 }}
"#,
            std = std.display()
        ),
    )
    .unwrap();
    SourceProgram::setup(path).test_io_all("ok-yes\n", 0);
}

#[test]
fn intrinsic_primitive_operations_inline_without_closure_allocation() {
    let dir = tempfile::tempdir().unwrap();
    let std =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/std").canonicalize().unwrap();
    let path = dir.path().join("inline_intrinsics.zy");
    std::fs::write(
        &path,
        format!(
            r#"
param (/process) : @(import("{std}/builtin.zy")) in
do sum <- ! (@(intrinsic(int/add))) 20 22;
! (@(intrinsic(int/eq))) OS sum 42 {{ ! process/exit 0 }} {{ ! process/exit 1 }}
"#,
            std = std.display()
        ),
    )
    .unwrap();
    let sps = CommandCompiler::default().lower(&path).unwrap().render_sps_low();
    assert!(!sps.contains("<extern:int_add/2>"), "{sps}");
    assert!(!sps.contains("<extern:int_eq_branch/4>"), "{sps}");
    assert!(
        !sps.contains("pack-closure("),
        "known intrinsic calls must not allocate thunks:\n{sps}"
    );
    assert!(!sps.contains("open-closure "), "known intrinsic calls must fold directly:\n{sps}");
    assert_eq!(sps.matches("<extern:exit/1>").count(), 1, "{sps}");
}

#[test]
fn retired_intrinsic_int_add_name_is_rejected() {
    let err = zydeco_tests::utils::SourceCase::check("let _ = @(intrinsic(int_add)) in ! exit 0")
        .unwrap_err();
    let msg = format!("{err:?}");
    assert!(
        msg.contains("UnknownRole(\"int_add\")"),
        "expected unknown intrinsic role `int_add`, got: {msg}"
    );
}
