use zydeco_tests::{check_source, e2e_sources};

e2e_sources!({
    comatch => "tests/core/comatch.zy",
    r#const => "tests/core/const.zy",
    direct_closure => "tests/core/direct-closure.zy",
    direct_tuple => "tests/core/direct-tuple.zy",
    explosion => "tests/core/explosion.zy",
    fact => "tests/core/fact.zy",
    fn_cmp_ret => "tests/core/fn-cmp-ret.zy",
    hof => "tests/core/hof.zy",
    id => "tests/core/id.zy",
    kont_clone => "tests/core/kont-clone.zy",
    label => "tests/core/label.zy",
    let_stack => "tests/core/let-stack.zy",
    literal_pattern => "tests/core/literal-pattern.zy",
    loop_ => "tests/core/loop.zydeco",
    loopy => "tests/core/loopy.zy",
    r#match => "tests/core/match.zy",
    named => "tests/core/named.zy",
    named_mixed => "tests/core/named-mixed.zy",
    named_nested => "tests/core/named-nested.zy",
    named_pattern => "tests/core/named-pattern.zy",
    named_data => "tests/core/named-data.zy",
    named_function => "tests/core/named-function.zy",
    named_codata => "tests/core/named-codata.zy",
    named_pun => "tests/core/named-pun.zy",
    named_tuple => "tests/core/named-tuple.zy",
    nested => "tests/core/nested.zy",
    nested_out => "tests/core/nested-out.zy",
    partial_annotation => "tests/core/partial-annotation.zy",
    pattern_alias => "tests/core/pattern-alias.zy",
    string_literal => "tests/core/string-literal.zy",
    sum => "tests/core/sum.zy",
    tagged_values => "tests/core/tagged-values.zy",
    triple => "tests/core/triple.zy",
    tuple => "tests/core/tuple.zy",
    tuple_do => "tests/core/tuple-do.zy",
    uniform => "tests/core/uniform.zy",
    value_views => "tests/core/value-views.zy",
});

// `iota` exports a lazy value rather than exiting, so it is checked, not run.
check_source!(iota, "tests/core/iota.zy");

// `loop.zy` and `looping.zy` never terminate when run, so the session crate
// only checks them and emits native assembly.
//
// `fail-annotation.zy`, `fail-unbound.zy`, and `warn-unattached-text.zy` are
// negative fixtures driven by the CLI diagnostic-rendering tests.

mod gc_stress {
    use zydeco_tests::utils::{SourceProgram, TestBackend};

    fn program() -> SourceProgram {
        SourceProgram::setup("tests/core/gc-stress.zy")
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
