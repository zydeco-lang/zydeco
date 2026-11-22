//! Single-source cases live in `lang/tests/cases/inference/`.
//! Rust tests retain import boundaries and generated numeric matrices.

use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

#[test]
fn rejects_call_site_inference_across_an_imported_source_boundary() {
    SourceCase::assert_rejected(
        SourceCase::check_with_import(
            r#"
let identity = @(import("imported.zy")) in
! identity ()
"#,
            r#"{ fn value => ret value }"#,
        ),
        TyckDiagnosticCode::UnconstrainedInference,
    );
}

#[test]
fn integer_literals_enforce_the_selected_payload_range() {
    for (ty, accepted, rejected) in [
        ("Int64", "-9223372036854775808", "-9223372036854775809"),
        ("Int64", "9223372036854775807", "9223372036854775808"),
        ("UInt64", "0", "-1"),
        ("UInt64", "18446744073709551615", "18446744073709551616"),
        ("Int", "-4611686018427387904", "-4611686018427387905"),
        ("Int", "4611686018427387903", "4611686018427387904"),
        ("UInt", "0", "-1"),
        ("UInt", "9223372036854775807", "9223372036854775808"),
    ] {
        SourceCase::assert_accepted(SourceCase::check(&format!(
            "let value : {ty} = {accepted} in ret value"
        )));
        SourceCase::assert_rejected(
            SourceCase::check(&format!("let value : {ty} = {rejected} in ret value")),
            TyckDiagnosticCode::IntegerLiteralOutOfRange,
        );
    }
    SourceCase::assert_accepted(SourceCase::check("ret 4611686018427387903"));
    SourceCase::assert_rejected(
        SourceCase::check("ret 4611686018427387904"),
        TyckDiagnosticCode::IntegerLiteralOutOfRange,
    );
}

#[test]
fn machine_and_exact_width_integers_require_explicit_conversion() {
    for (source, target) in
        [("Int", "Int64"), ("Int64", "Int"), ("UInt", "UInt64"), ("UInt64", "UInt")]
    {
        SourceCase::assert_accepted(SourceCase::check(&format!("let x : {source} = 0 in ret x")));
        let result =
            SourceCase::check(&format!("let x : {source} = 0 in let y : {target} = x in ret y"));
        SourceCase::assert_rejected(result, TyckDiagnosticCode::TypeMismatch);
    }
}
