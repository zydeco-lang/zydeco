//! Rust tests retain resolved surface error variants; source cases live in `cases/existential/`.

use zydeco_session::ResolveError;
use zydeco_tests::utils::SourceCase;

#[test]
fn rejects_a_pack_parameter_without_evidence() {
    SourceCase::assert_resolve_error_matching(
        SourceCase::check_value(
            r#"
begin
  let packed = pack (X : VType) where (42 : X) end in
  packed
end
"#,
        ),
        |error| matches!(error, ResolveError::PackParameterNeedsEvidence(_)),
    );
}

#[test]
fn rejects_redundant_evidence_on_a_manifest_parameter() {
    SourceCase::assert_resolve_error_matching(
        SourceCase::check_value(
            r#"
begin
  let packed = pack (X as Int : VType) is Char where (42 : X) end in
  packed
end
"#,
        ),
        |error| matches!(error, ResolveError::PackParameterRedundantEvidence(_)),
    );
}
