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
    zir_names => "tests/core/zir-names.zy",
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
        assert_eq!(sps.matches("<primitive:int_add>").count(), 1, "{sps}");
        assert!(!sps.contains("<extern:int_add/2>"), "{sps}");
        assert_eq!(sps.matches("pack-continuation(").count(), 2, "{sps}");
        assert!(!sps.contains("<extern:int_mul/2>"), "{sps}");
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
    use zydeco_tests::utils::{ExecutionTarget, SourceProgram};

    fn program() -> SourceProgram {
        SourceProgram::setup("tests/core/gc-stress.zy")
    }

    #[test]
    fn amd64() {
        program().test(ExecutionTarget::Exe);
    }

    #[test]
    fn wasm_am() {
        program().test(ExecutionTarget::WasmAm);
    }

    #[test]
    fn wasm_sps() {
        program().test(ExecutionTarget::WasmSps);
    }
}

mod listing_names {
    use std::path::PathBuf;
    use zydeco_cli::{CommandCompiler, HighSpsInspection, NameStyle, TargetOs};

    fn fixture() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/tests/core/zir-names.zy")
    }

    fn sps(names: NameStyle) -> String {
        CommandCompiler::default()
            .with_pass_inspection(HighSpsInspection { names, ..Default::default() })
            .lower(&fixture())
            .expect("zir-names.zy must lower")
            .render_sps_low()
    }

    #[test]
    fn listings_spell_source_names_and_prime_rebindings() {
        let sps = sps(NameStyle::Readable);
        assert!(!sps.contains("__static_value__"), "{sps}");
        assert!(!sps.contains("_code__"), "{sps}");
        // The fix block and the continuation receiving its result carry their binders.
        assert!(sps.contains("[block:loop'/code]"), "{sps}");
        assert!(sps.contains("[block:total/kont]"), "{sps}");
        assert!(sps.contains("let loop' = pack-closure("), "{sps}");
        // Parameters keep their names; the two rebindings of `acc` in the loop body and
        // the one of `i` are told apart by primes, in order of appearance.
        assert!(sps.contains("let arg(i) :: • = • in"), "{sps}");
        assert!(sps.contains("let arg(acc) :: • = • in"), "{sps}");
        assert!(sps.contains("let acc'1 = <primitive:int_add>(acc, i) in"), "{sps}");
        assert!(sps.contains("let acc'2 = <primitive:int_add>(acc'1, i) in"), "{sps}");
        assert!(sps.contains("let i'1 = <primitive:int_sub>(i, 1) in"), "{sps}");
        assert!(sps.contains("arg(i'1) :: arg(acc'2) :: •"), "{sps}");
        // Openings bind `env`, `code`, and `kont`; no name carries an arena id.
        assert!(sps.contains("open-closure loop' as (env, code) in"), "{sps}");
        assert!(sps.contains("open-continuation • as kont :: • in"), "{sps}");
        assert!(!sps.contains("/code["), "{sps}");
        assert!(!sps.contains("/kont["), "{sps}");
    }

    #[test]
    fn print_ids_keeps_arena_ids_on_every_name() {
        let sps = sps(NameStyle::Identified);
        assert!(sps.contains("[block:loop'/code["), "{sps}");
        assert!(!sps.contains("acc'1"), "{sps}");
        let identified = sps.lines().filter(|line| line.contains("arg(acc[")).count();
        assert!(identified >= 1, "{sps}");
    }

    #[test]
    fn primed_names_assemble_to_sanitized_symbols() {
        let assembly = CommandCompiler::default()
            .lower(&fixture())
            .expect("zir-names.zy must lower")
            .emit_amd64(TargetOs::Linux)
            .assembly;
        assert!(assembly.contains("loop__code_"), "{assembly}");
        let bad_label = assembly
            .lines()
            .filter(|line| line.ends_with(':') && !line.starts_with(';'))
            .find(|label| {
                !label.trim_end_matches(':').chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
            });
        assert_eq!(bad_label, None, "{assembly}");
    }
}
