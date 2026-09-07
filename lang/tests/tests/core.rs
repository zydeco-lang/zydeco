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
    local_normalization => "tests/core/local-normalization.zy",
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
    value_views_runtime => "tests/core/value-views-runtime.zy",
    static_composition => "tests/core/static-composition.zy",
    runtime_codata_contract => "tests/core/runtime-codata-contract.zy",
    runtime_package_adapters => "tests/core/runtime-package-adapters.zy",
    runtime_package_callback => "tests/core/runtime-package-callback.zy",
    runtime_package_payload => "tests/core/runtime-package-payload.zy",
});

mod normalization {
    use std::path::PathBuf;
    use zydeco_cli::CommandCompiler;

    struct Fixture;

    impl Fixture {
        fn sps(name: &str) -> String {
            let path =
                PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/tests/core").join(name);
            CommandCompiler::default()
                .lower(&path)
                .unwrap_or_else(|error| panic!("fixture {name}: {error}"))
                .render_sps_low()
                .lines()
                .filter(|line| !line.starts_with("[function:"))
                .collect::<Vec<_>>()
                .join("\n")
        }
    }

    #[test]
    fn direct_eliminations_remove_closure_and_continuation_packages() {
        for name in ["direct-closure.zy", "direct-tuple.zy", "comatch.zy", "match.zy"] {
            let sps = Fixture::sps(name);
            assert_eq!(sps.matches("<extern:exit/1>").count(), 1, "{name}: {sps}");
            if name == "direct-closure.zy" {
                assert!(sps.contains("<extern:exit/1> arg(0) :: •"), "{name}: {sps}");
            }
            assert!(!sps.contains("pack-closure("), "{name}: {sps}");
            assert!(!sps.contains("pack-continuation("), "{name}: {sps}");
        }
    }

    #[test]
    fn local_reductions_preserve_shared_bodies_and_remove_dead_branch_dependencies() {
        let sps = Fixture::sps("local-normalization.zy");
        assert_eq!(sps.matches("pack-closure(").count(), 1, "{sps}");
        assert_eq!(sps.matches("open-closure ").count(), 2, "{sps}");
        assert_eq!(sps.matches("<extern:int64_add/2>").count(), 3, "{sps}");
        assert!(!sps.contains("<extern:int64_mul/2>"), "{sps}");
    }
}

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
