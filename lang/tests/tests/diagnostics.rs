use zydeco_cli::CompileError;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::{CaseError, SourceCase};

#[test]
fn type_mismatches_preserve_expected_and_found_direction() {
    for (body, expected, found) in [
        ("let x : Int64 = \"s\" in ret x", "Int64", "String"),
        ("let s = \"s\" in let x : Int64 = s in ret x", "Int64", "String"),
        ("let x : Int64 = (\"s\" : String) in ret x", "Int64", "String"),
        ("let x : Int64 * Unit = (\"s\", ()) in ret x", "Int64", "String"),
        ("let x = (#field = \"s\") in let y : Int64 = x/field in ret y", "Int64", "String"),
        (
            "let f : Thk (Int64 -> Ret Int64) = {fn x => ret x} in (! f 0 : Ret String)",
            "String",
            "Int64",
        ),
        (
            "let f : Thk (String -> Ret Int64) = {fn (x : Int64) => ret x} in ! exit 0",
            "String",
            "Int64",
        ),
        ("let x : Int64 = () in ret x", "Int64", "Unit"),
    ] {
        let result = SourceCase::check(body);
        let Err(CaseError::Compile(CompileError::Rejected(analysis))) = result else {
            panic!("expected a type error for {body}: {result:?}");
        };
        let diagnostics = analysis.outcome().diagnostics().unwrap();
        let error = diagnostics
            .iter()
            .find(|error| error.code == TyckDiagnosticCode::TypeMismatch)
            .unwrap_or_else(|| panic!("expected a type mismatch for {body}: {diagnostics:?}"));
        assert!(error.primary.is_some());
        assert!(
            error
                .message
                .split_whitespace()
                .collect::<Vec<_>>()
                .join(" ")
                .contains(&format!("expected {expected}, found {found}")),
            "{body}: {}",
            error.message
        );
    }
}
