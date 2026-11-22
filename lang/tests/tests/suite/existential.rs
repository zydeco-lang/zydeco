//! Rust tests retain desugared error variants; source cases live in `cases/existential/`.

use zydeco_session::DesugarError;
use zydeco_tests::utils::SourceCase;

#[test]
fn rejects_a_pack_parameter_without_evidence() {
    SourceCase::assert_desugar_error(
        SourceCase::check_value(
            r#"
begin
  let packed = pack (X : VType) where (42 : X) end in
  packed
end
"#,
        ),
        |error| matches!(error, DesugarError::PackParameterNeedsEvidence(_)),
    );
}

#[test]
fn rejects_redundant_evidence_on_a_manifest_parameter() {
    SourceCase::assert_desugar_error(
        SourceCase::check_value(
            r#"
begin
  let packed = pack (X as Int : VType) is Char where (42 : X) end in
  packed
end
"#,
        ),
        |error| matches!(error, DesugarError::PackParameterRedundantEvidence(_)),
    );
}
