use zydeco_session::ResolveError;
use zydeco_tests::utils::SourceCase;

#[test]
fn codata_parameters_must_be_patterns() {
    SourceCase::assert_accepted(SourceCase::check(
        "let C = codata | .foo (A : VType) (value : A) : Ret A end in ret ()",
    ));
    SourceCase::assert_resolve_error_matching(
        SourceCase::check("let C = codata | .foo .bar : OS end in ret ()"),
        |error| matches!(error, ResolveError::QuantifierParameterNotPattern(_)),
    );
}
