use zydeco_tests::e2e_sources;

e2e_sources!({
    exit => "tests/compile/exit.zy",
    env => "tests/compile/env.zy",
    add0 => "tests/compile/add0.zy",
    add => "tests/compile/add.zy",
    sub => "tests/compile/sub.zy",
    mul => "tests/compile/mul.zy",
    nested => "tests/compile/nested.zy",
    nested_out => "tests/compile/nested-out.zy",
    r#match => "tests/compile/match.zy",
    cmp => "tests/compile/cmp.zy",
    label => "tests/compile/label.zy",
    id => "tests/compile/id.zy",
    r#const => "tests/compile/const.zy",
    hof => "tests/compile/hof.zy",
    comatch => "tests/compile/comatch.zy",
    fn_cmp_ret => "tests/compile/fn-cmp-ret.zy",
    tuple => "tests/compile/tuple.zy",
    tuple_do => "tests/compile/tuple-do.zy",
    direct_tuple => "tests/compile/direct-tuple.zy",
    direct_closure => "tests/compile/direct-closure.zy",
    triple => "tests/compile/triple.zy",
    sum => "tests/compile/sum.zy",
    fact => "tests/compile/fact.zy",
    let_stack => "tests/compile/let-stack.zy",
    kont_clone => "tests/compile/kont-clone.zy",
    uniform => "tests/compile/uniform.zy",
    named_mixed => "tests/compile/named-mixed.zy",
    named => "tests/compile/named.zy",
    named_nested => "tests/exec/named-nested.zy",
    pattern_alias => "tests/exec/pattern-alias.zy",
    host_return => "tests/compile/host-return.zy",
    host_runtime => "tests/compile/host-runtime.zy",
    string_literal => "tests/compile/string-literal.zy",
    pure_functions => "tests/compile/pure-functions.zy",
    numeric_widths => "tests/builtin/numeric-widths.zy",
    tagged_values => "tests/compile/tagged-values.zy",
});

mod host_arguments {
    use zydeco_tests::utils::{SourceProgram, TestBackend};

    fn program() -> SourceProgram {
        SourceProgram::setup("tests/compile/host-arguments.zy").with_args(["alpha", "beta"])
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

mod gc_stress {
    use zydeco_tests::utils::{SourceProgram, TestBackend};

    fn program() -> SourceProgram {
        SourceProgram::setup("tests/compile/gc-stress.zy")
    }

    #[test]
    fn amd64() {
        program().test(TestBackend::Amd64);
    }

    #[test]
    fn wasm_am() {
        program().test(TestBackend::WasmAm);
    }

    #[test]
    fn wasm_sps() {
        program().test(TestBackend::WasmSps);
    }
}
