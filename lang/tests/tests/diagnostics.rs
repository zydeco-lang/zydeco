use zydeco_cli::CompileError;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::{CaseError, SourceCase};

#[test]
fn nominal_type_mismatches_identify_their_distinct_source_bindings() {
    for (body, origin) in [
        (
            "let Package = exists (X : VType) . X in fn (p : Package) (q : Package) => let (X, x) = p in let (X, y) = q in do _ <- (ret x : Ret X); ret ()",
            "opened here",
        ),
        (
            "let A = (def X : VType = Int64 in X) in let B = (def X : VType = Int64 in X) in fn (x : A) => (ret x : Ret B)",
            "sealed here",
        ),
    ] {
        let result = SourceCase::check(body);
        let Err(CaseError::Compile(CompileError::Rejected(analysis))) = result else {
            panic!("expected a type error for {body}: {result:?}");
        };
        let errors = analysis.outcome().diagnostics().unwrap();
        let error =
            errors.iter().find(|error| error.code == TyckDiagnosticCode::TypeMismatch).unwrap();
        assert!(error.message.contains("X (identity 1)"), "{error:?}");
        assert!(error.message.contains("X (identity 2)"), "{error:?}");
        assert!(
            error.help.iter().any(|help| help.contains("distinct abstract types")),
            "{error:?}"
        );
        assert_eq!(error.related.len(), 2, "{error:?}");
        assert_ne!(error.related[0].span, error.related[1].span);
        for label in &error.related {
            assert!(label.message.contains(origin), "{label:?}");
            let (file, range) = analysis.spans().source_map().unwrap().range(label.span).unwrap();
            assert_eq!(&file.source()[range], "X");
        }
    }
    for body in [
        "let Package = exists (X : VType) . X in fn (p : Package) => let (X, x) = p in do _ <- (ret x : Ret X); ret ()",
        "let A = (def X : VType = Int64 in X) in let B = A in fn (x : A) => (ret x : Ret B)",
    ] {
        SourceCase::assert_accepted(SourceCase::check_linted(body));
    }
}

#[test]
fn incomplete_type_diagnostics_keep_independent_holes_without_cascades() {
    for (body, primary) in [
        ("let x : Int64 = { _ } in ret x", TyckDiagnosticCode::TypeMismatch),
        ("fn x => ret x", TyckDiagnosticCode::UnconstrainedInference),
        ("ret _", TyckDiagnosticCode::SortMismatch),
        ("let x = { ret _ } in ret x", TyckDiagnosticCode::SortMismatch),
    ] {
        let result = SourceCase::check(body);
        let Err(CaseError::Compile(CompileError::Rejected(analysis))) = result else {
            panic!("expected a type error for {body}: {result:?}");
        };
        let errors = analysis.outcome().diagnostics().unwrap();
        assert_eq!(errors.len(), 1, "{body}: {errors:?}");
        let error = errors.iter().next().unwrap();
        assert_eq!(error.code, primary, "{body}: {error:?}");
        assert!(error.primary.is_some());
        assert!(!error.message.contains("fill-"), "{body}: {}", error.message);
        if primary == TyckDiagnosticCode::TypeMismatch {
            assert!(error.message.contains("Thk _"), "{}", error.message);
        }
    }
    let body = "let waiting = { ! _ } in let wrong : Int64 = { _ } in ret 0";
    let result = SourceCase::check(body);
    let Err(CaseError::Compile(CompileError::Rejected(analysis))) = result else {
        panic!("expected a type error for {body}: {result:?}");
    };
    let errors = analysis.outcome().diagnostics().unwrap();
    assert_eq!(errors.len(), 2, "{errors:?}");
    assert!(errors.iter().any(|error| error.code == TyckDiagnosticCode::TypeMismatch));
    let missing =
        errors.iter().find(|error| error.code == TyckDiagnosticCode::MissingSolution).unwrap();
    let span = missing.primary.as_ref().unwrap().span;
    let (file, range) = analysis.spans().source_map().unwrap().range(span).unwrap();
    assert_eq!(&file.source()[range], "_");
    assert!(missing.related.is_empty(), "the failed expression's hole must be omitted");
    SourceCase::assert_rejected(SourceCase::check("! _"), TyckDiagnosticCode::MissingSolution);
    SourceCase::assert_accepted(SourceCase::check("let x : Int64 = 0 in ret x"));
}

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
