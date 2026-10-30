use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::{e2e_sources, utils::SourceCase};

e2e_sources!({
    exists => "tests/pack/exists.zy",
    manifest => "tests/pack/manifest.zy",
    interleaved => "tests/pack/interleaved.zy",
    named => "tests/pack/named.zy",
    monadic => "tests/pack/monadic.zy",
    uniform => "tests/pack/uniform.zy",
    deep_selection => "tests/pack/deep-selection.zy",
});

#[test]
fn inferred_named_witnesses_preserve_typed_arena_and_lowering_invariants() {
    for fixture in [
        include_str!("../cases/existential/pack_infers_named_witness_classifiers.zy"),
        include_str!("../cases/existential/manifest_exists_infers_named_witness_classifiers.zy"),
        include_str!("../cases/existential/pack_infers_nested_named_witness_classifiers.zy"),
        include_str!(
            "../cases/existential/manifest_exists_infers_nested_named_witness_classifiers.zy"
        ),
        include_str!("../cases/existential/sealed_pack_infers_named_witness_classifiers.zy"),
    ] {
        let source = format!("let case_value = ({fixture}) in ! exit 0");
        SourceCase::assert_accepted(SourceCase::check_invariants(&source));
        SourceCase::assert_accepted(SourceCase::lower(&source));
    }
}

#[test]
fn named_witness_inference_rejections_precede_lowering() {
    for (fixture, code) in [
        (
            include_str!(
                "../cases/existential/pack_rejects_an_incompatible_named_witness_classifier.zy"
            ),
            TyckDiagnosticCode::KindMismatch,
        ),
        (
            include_str!(
                "../cases/existential/manifest_exists_rejects_an_incompatible_named_witness_classifier.zy"
            ),
            TyckDiagnosticCode::KindMismatch,
        ),
        (
            include_str!("../cases/existential/sealed_pack_inference_preserves_witness_opacity.zy"),
            TyckDiagnosticCode::TypeMismatch,
        ),
    ] {
        let source = format!("let case_value = ({fixture}) in ! exit 0");
        SourceCase::assert_rejected(SourceCase::check(&source), code);
        SourceCase::assert_rejected(SourceCase::lower(&source), code);
    }
}
