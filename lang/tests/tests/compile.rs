use zydeco_tests::e2e_sources;

e2e_sources!({
    exit => "tests/source/compile-exit.zy",
    env => "tests/source/compile-env.zy",
    add0 => "tests/source/compile-add0.zy",
    add => "tests/source/compile-add.zy",
    sub => "tests/source/compile-sub.zy",
    mul => "tests/source/compile-mul.zy",
    nested => "tests/source/compile-nested.zy",
    nested_out => "tests/source/compile-nested-out.zy",
    r#match => "tests/source/compile-match.zy",
    cmp => "tests/source/compile-cmp.zy",
    label => "tests/source/compile-label.zy",
    id => "tests/source/compile-id.zy",
    r#const => "tests/source/compile-const.zy",
    hof => "tests/source/compile-hof.zy",
    comatch => "tests/source/compile-comatch.zy",
    fn_cmp_ret => "tests/source/compile-fn-cmp-ret.zy",
    tuple => "tests/source/compile-tuple.zy",
    tuple_do => "tests/source/compile-tuple-do.zy",
    triple => "tests/source/compile-triple.zy",
    sum => "tests/source/compile-sum.zy",
    fact => "tests/source/compile-fact.zy",
    let_stack => "tests/source/compile-let-stack.zy",
    kont_clone => "tests/source/compile-kont-clone.zy",
    uniform => "tests/source/compile-uniform.zy",
    named_mixed => "tests/source/compile-named-mixed.zy",
    named => "tests/source/compile-named.zy",
    host_return => "tests/source/compile-host-return.zy",
    host_runtime => "tests/source/compile-host-runtime.zy",
    string_literal => "tests/source/compile-string-literal.zy",
});

mod host_arguments {
    use zydeco_tests::utils::{SourceProgram, TestBackend};

    fn program() -> SourceProgram {
        SourceProgram::setup("tests/source/compile-host-arguments.zy").with_args(["alpha", "beta"])
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
