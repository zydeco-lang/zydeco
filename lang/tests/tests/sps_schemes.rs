//! Every stack-passing scheme compiles the same programs to the same observable behavior,
//! with and without the optional passes, on each compiled backend.

use std::path::PathBuf;
use zydeco_cli::{CommandCompiler, SpsPass, SpsPlan, SpsScheme};
use zydeco_tests::utils::{ExecutionTarget, SourceProgram};

struct Fixture;

impl Fixture {
    fn agree(name: &str) {
        // The default plan localizes after normalizing; the reverse order hands procedures to the
        // normalizer.
        let plans = [
            SpsPlan::Default,
            SpsPlan::None,
            SpsPlan::Custom(vec![SpsPass::Localize, SpsPass::Normalize]),
        ];
        for &scheme in SpsScheme::ALL {
            for plan in plans.clone() {
                for backend in [ExecutionTarget::Exe, ExecutionTarget::WasmSps] {
                    SourceProgram::setup(format!("tests/core/{name}"))
                        .with_sps_scheme(scheme)
                        .with_sps_passes(plan.clone())
                        .test(backend);
                }
            }
        }
    }

    /// SPSLow of a core program lowered under `scheme` without optional passes.
    fn unnormalized(name: &str, scheme: SpsScheme) -> String {
        let path =
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/tests/core").join(name);
        let program = CommandCompiler::default()
            .with_sps_scheme(scheme)
            .with_sps_passes(SpsPlan::None)
            .lower(&path)
            .unwrap_or_else(|error| panic!("fixture {name}: {error}"));
        assert_eq!(program.sps_scheme(), scheme);
        program.render_sps_low()
    }
}

macro_rules! schemes_agree {
    ($($name:ident => $file:literal),* $(,)?) => {
        $(
            #[test]
            fn $name() {
                Fixture::agree($file);
            }
        )*
    };
}

schemes_agree! {
    codata_order => "codata-order.zy",
    comatch => "comatch.zy",
    constructor_fallback_sharing => "constructor-fallback-sharing.zy",
    constructor_fallthrough => "constructor-fallthrough.zy",
    dispatch => "dispatch.zy",
    fact => "fact.zy",
    fn_cmp_ret => "fn-cmp-ret.zy",
    kont_clone => "kont-clone.zy",
    let_stack => "let-stack.zy",
    literal_pattern => "literal-pattern.zy",
    local_normalization => "local-normalization.zy",
    nested_constructor_patterns => "nested-constructor-patterns.zy",
    procedures => "procedures.zy",
    representation_policies => "representation-policies.zy",
    tagged_values => "tagged-values.zy",
}

#[test]
fn full_compilation_names_every_frame_and_consumes_only_the_ambient_stack() {
    let branch_join = Fixture::unnormalized("dispatch.zy", SpsScheme::BranchJoin);
    let full = Fixture::unnormalized("dispatch.zy", SpsScheme::Full);
    let continuations = |low: &str| low.matches("pack-continuation(").count();
    assert_eq!(continuations(&full), continuations(&branch_join), "{full}");
    // Branch-join guards tail-position branches with `let • = • in`; full names frames instead.
    let frame_lets =
        |low: &str| low.matches("let • = ").count() - low.matches("let • = • in").count();
    assert!(!full.contains("let • = • in"), "{full}");
    assert!(frame_lets(&full) > frame_lets(&branch_join), "{full}");
    let ambient_jumps = |low: &str| (low.matches("jump ").count(), low.matches(") ! •").count());
    let (jumps, ambient) = ambient_jumps(&full);
    assert_eq!(jumps, ambient, "{full}");
    let (jumps, ambient) = ambient_jumps(&branch_join);
    assert!(ambient < jumps, "{branch_join}");
}

#[test]
fn duplicating_compilation_copies_the_consumer_that_branch_join_names_once() {
    let branch_join = Fixture::unnormalized("dispatch.zy", SpsScheme::BranchJoin);
    let duplicating = Fixture::unnormalized("dispatch.zy", SpsScheme::Duplicating);
    assert!(branch_join.contains("let • = "), "{branch_join}");
    assert!(!duplicating.contains("let • = "), "{duplicating}");
    let continuations = |low: &str| low.matches("pack-continuation(").count();
    assert_eq!(continuations(&duplicating), continuations(&branch_join) + 1, "{duplicating}");
}
