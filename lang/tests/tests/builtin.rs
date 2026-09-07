use std::path::PathBuf;
use zydeco_cli::CommandCompiler;
use zydeco_tests::e2e_sources;
use zydeco_tests::utils::{SourceProgram, TestBackend};

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
    for (fixture, additions) in [("add.zy", 1), ("primitive-aliases.zy", 2)] {
        let sps = PrimitiveFixture::sps_low(fixture);
        assert_eq!(sps.matches("<extern:int64_add/2>").count(), additions, "{sps}");
        assert_eq!(sps.matches("<extern:exit/1>").count(), 1, "{sps}");
        assert!(!sps.contains("pack-closure("), "known calls must not allocate thunks:\n{sps}");
        assert!(
            !sps.contains("open-closure "),
            "known calls must not dispatch through thunks:\n{sps}"
        );
    }
}

#[test]
fn dynamically_selected_primitives_keep_their_thunks() {
    let sps = PrimitiveFixture::sps_low("primitive-selection.zy");
    assert!(sps.contains("<extern:int64_eq_branch/4>"), "the branch primitive is known:\n{sps}");
    assert!(sps.contains("pack-closure("), "escaping primitives must remain values:\n{sps}");
    assert!(sps.contains("open-closure "), "the selected primitive needs runtime dispatch:\n{sps}");
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
        program().test(TestBackend::Interpreter);
    }

    #[test]
    fn amd64() {
        program().test(TestBackend::Amd64);
    }
}
