use zydeco_cli::CompileError;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::{CaseError, SourceCase};

#[test]
fn malformed_monadic_operation_contracts_are_diagnosed() {
    for (definition, code) in [
        ("OS", TyckDiagnosticCode::TypeExpected),
        ("codata end", TyckDiagnosticCode::UnknownCoDataDestructor),
        ("codata | .return : OS end", TyckDiagnosticCode::TypeExpected),
        ("codata | .return (A : VType) : OS end", TyckDiagnosticCode::TypeExpected),
        ("codata | .return (A : VType) : String -> M A end", TyckDiagnosticCode::TypeMismatch),
    ] {
        let body =
            format!("let Monad (M : VType -> CType) : CType = {definition} in @[monadic] ret 0");
        SourceCase::assert_rejected(SourceCase::check_monadic(&body), code);
    }
    SourceCase::assert_accepted(SourceCase::check_monadic("@[monadic] ret 0"));
}

#[test]
fn binding_cycles_fail_before_dependent_annotations_are_checked() {
    for body in [
        "begin def ! (M : VType) (x : M) : Ret M = ret x that ! exit 0 end",
        "begin let x : Int64 = y that let y : Int64 = x that ! exit 0 end",
        "begin let x = y that let y = x that ! exit 0 end",
        "begin def ! loop (x : Int64) : Ret Int64 = ! loop x that ! exit 0 end",
    ] {
        let result = SourceCase::check(body);
        SourceCase::assert_rejected(result, TyckDiagnosticCode::InvalidBindingCycle);
    }
    for body in [
        "begin def ! identity (M : VType) (x : M) : Ret M = ret x that ! exit 0 end",
        "begin def ! _ (M : VType) (x : M) : Ret M = ret x that ! exit 0 end",
        "begin def fix loop (x : Int64) : Ret Int64 = ! loop x that ! exit 0 end",
    ] {
        SourceCase::assert_accepted(SourceCase::check_linted(body));
    }
}

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
