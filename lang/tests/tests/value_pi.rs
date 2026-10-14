//! Source cases live in `cases/value-pi/`; Rust retains typed-arena lint and lowering checks.

use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

#[test]
fn backward_application_accepts_bare_type_and_value_abstractions() {
    let source = include_str!(
        "../cases/value-pi/backward_application_accepts_bare_type_and_value_abstractions.zy"
    );
    SourceCase::assert_accepted(SourceCase::check_linted(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn backward_application_rejects_a_type_function_with_the_wrong_result_kind() {
    let source = include_str!(
        "../cases/value-pi/backward_application_rejects_a_type_function_with_the_wrong_result_kind.zy"
    );
    SourceCase::assert_rejected(SourceCase::lower(source), TyckDiagnosticCode::KindMismatch);
}
