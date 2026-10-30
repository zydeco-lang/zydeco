//! Source cases live in `cases/monadic-value-functions/`; Rust retains compiler assertions and backend coverage.

use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

zydeco_tests::runtime_source!(state_effect_order, "tests/monadic/value-functions.zy");

struct MonadicCase;

impl MonadicCase {
    fn source(body: &str) -> String {
        let basis =
            std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib/std/effect/monad.zy");
        format!(
            r#"
let monadic_basis = @(import("{}")) in
let (/Monad; /Algebra) = monadic_basis in
{body}
"#,
            basis.display()
        )
    }

    fn assert_linted_and_lowered(source: &str) {
        let source = Self::source(source);
        SourceCase::assert_accepted(SourceCase::check_invariants(&source));
        SourceCase::assert_accepted(SourceCase::lower(&source));
    }
}

#[test]
fn computation_kind_factory_translates_with_its_structure() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-value-functions/computation_kind_factory_translates_with_its_structure.zy"
    ));
}

#[test]
fn annotated_partial_and_higher_order_value_functions_translate() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-value-functions/annotated_partial_and_higher_order_value_functions_translate.zy"
    ));
}

#[test]
fn generated_classifiers_have_one_structure_argument_per_type_parameter() {
    let source = MonadicCase::source(
        r#"
let Translated = @[typeof] @[monadic]
  fn (factory : val pi (A : VType) (B : CType) . Thk (A -> B)) => ret ()
in
let Implementation = @[typeof] ({
  fn (M : VType -> CType) (mo : Thk (Monad M))
    (factory : val pi (X : VType) (_ : Thk Top)
      (Y : CType) (_ : Thk (Algebra M Y)) . Thk (X -> Y)) =>
      ! mo .return Unit ()
} : Thk Translated) in
ret ()
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(&source));
    SourceCase::assert_rejected(
        SourceCase::check(&source.replace("(_ : Thk (Algebra M Y))", "(_ : Int)")),
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn value_and_higher_kinds_and_alternating_arguments_translate() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-value-functions/value_and_higher_kinds_and_alternating_arguments_translate.zy"
    ));
}

#[test]
fn named_type_patterns_preserve_their_payload_witness() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-value-functions/named_type_patterns_preserve_their_payload_witness.zy"
    ));
}

#[test]
fn package_parameters_reuse_carried_structures() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-value-functions/package_parameters_reuse_carried_structures.zy"
    ));
}

#[test]
fn product_package_parameters_preserve_witness_order() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-value-functions/product_package_parameters_preserve_witness_order.zy"
    ));
}

#[test]
fn multiple_package_witnesses_translate() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-value-functions/multiple_package_witnesses_translate.zy"
    ));
}

#[test]
fn factories_export_thunks_through_packed_fields() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-value-functions/factories_export_thunks_through_packed_fields.zy"
    ));
}

#[test]
fn manifest_and_abstract_witnesses_in_product_parameters_translate() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-value-functions/manifest_and_abstract_witnesses_in_product_parameters_translate.zy"
    ));
}

#[test]
fn source_annotations_match_translated_package_routes() {
    let source = MonadicCase::source(include_str!(
        "../cases/monadic-value-functions/source_annotations_match_translated_package_routes.zy"
    ));
    SourceCase::assert_accepted(SourceCase::check_invariants(&source));
}

#[test]
fn sibling_factories_and_nested_monadic_blocks_keep_distinct_structures() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-value-functions/sibling_factories_and_nested_monadic_blocks_keep_distinct_structures.zy"
    ));
}

#[test]
fn wrong_kind_label_and_argument_order_keep_source_diagnostics() {
    for (source, expected) in [
        (
            r#"@[monadic] let val f (B : CType) = () in ret (f Int)"#,
            TyckDiagnosticCode::KindMismatch,
        ),
        (
            r#"@[monadic] let val f ((#a = B) : (#a :: CType)) = () in ret (f (#b = Ret Int))"#,
            TyckDiagnosticCode::NamedLabelMismatch,
        ),
        (
            r#"@[monadic] let val f (B : CType) = () in ret (f ())"#,
            TyckDiagnosticCode::SortMismatch,
        ),
    ] {
        SourceCase::assert_rejected(SourceCase::check_monadic(source), expected);
    }
}
