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
});

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
