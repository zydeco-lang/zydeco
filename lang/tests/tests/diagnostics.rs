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
            "let A = (def X : VType = Int in X) in let B = (def X : VType = Int in X) in fn (x : A) => (ret x : Ret B)",
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
        "let A = (def X : VType = Int in X) in let B = A in fn (x : A) => (ret x : Ret B)",
    ] {
        SourceCase::assert_accepted(SourceCase::check_linted(body));
    }
}

#[test]
fn incomplete_type_diagnostics_keep_independent_holes_without_cascades() {
    for (body, primary) in [
        ("let x : Int = { _ } in ret x", TyckDiagnosticCode::TypeMismatch),
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
    let body = "let waiting = { ! _ } in let wrong : Int = { _ } in ret 0";
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
    SourceCase::assert_accepted(SourceCase::check("let x : Int = 0 in ret x"));
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
        "begin let x : Int = y that let y : Int = x that ! exit 0 end",
        "begin let x = y that let y = x that ! exit 0 end",
        "begin def ! loop (x : Int) : Ret Int = ! loop x that ! exit 0 end",
    ] {
        let result = SourceCase::check(body);
        SourceCase::assert_rejected(result, TyckDiagnosticCode::InvalidBindingCycle);
    }
    for body in [
        "begin def ! identity (M : VType) (x : M) : Ret M = ret x that ! exit 0 end",
        "begin def ! _ (M : VType) (x : M) : Ret M = ret x that ! exit 0 end",
        "begin def fix loop (x : Int) : Ret Int = ! loop x that ! exit 0 end",
    ] {
        SourceCase::assert_accepted(SourceCase::check_linted(body));
    }
}

#[test]
fn type_mismatches_preserve_expected_and_found_direction() {
    for (body, expected, found) in [
        ("let x : Int = \"s\" in ret x", "Int", "String"),
        ("let s = \"s\" in let x : Int = s in ret x", "Int", "String"),
        ("let x : Int = (\"s\" : String) in ret x", "Int", "String"),
        ("let x : Int * Unit = (\"s\", ()) in ret x", "Int", "String"),
        ("let x = (#field = \"s\") in let y : Int = x/field in ret y", "Int", "String"),
        ("let f : Thk (Int -> Ret Int) = {fn x => ret x} in (! f 0 : Ret String)", "String", "Int"),
        ("let f : Thk (String -> Ret Int) = {fn (x : Int) => ret x} in ! exit 0", "String", "Int"),
        ("let x : Int = () in ret x", "Int", "Unit"),
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

#[test]
fn independent_components_and_arms_report_all_type_errors() {
    use TyckDiagnosticCode::{KindMismatch, TypeMismatch};
    for (rejected, accepted, code) in [
        (r#"(("left" : Int), ("right" : Int))"#, "((1 : Int), (2 : Int))", TypeMismatch),
        (r#"(("left", "right") : Int * Int)"#, "((1, 2) : Int * Int)", TypeMismatch),
        (
            r#"((Int, "left", "right") : exists (A : VType) . A * A)"#,
            "((Int, 1, 2) : exists (A : VType) . A * A)",
            TypeMismatch,
        ),
        ("(Ret Unit, Ret Unit, Int)", "(Unit, Unit, Int)", KindMismatch),
        (
            r#"match 0 | 0 => 1 | 1 => "left" | _ => "right" end"#,
            "match 0 | 0 => 1 | 1 => 2 | _ => 3 end",
            TypeMismatch,
        ),
        (
            "data | +Left : Ret Unit | +Right : Ret Int end",
            "data | +Left : Unit | +Right : Int end",
            KindMismatch,
        ),
        (
            "codata | .left : Unit | .right : Int end",
            "codata | .left : Ret Unit | .right : Ret Int end",
            KindMismatch,
        ),
        (
            r#"match 0 | 0 => ("left" : Int) | _ => ("right" : Int) end"#,
            "match 0 | 0 => (1 : Int) | _ => (2 : Int) end",
            TypeMismatch,
        ),
        (
            r#"let C = codata | .left : Ret Int | .right : Ret Int end in
            (comatch | .left => ret "left" | .right => ret "right" end : C)"#,
            "let C = codata | .left : Ret Int | .right : Ret Int end in
            (comatch | .left => ret 1 | .right => ret 2 end : C)",
            TypeMismatch,
        ),
    ] {
        let rejected = format!("let classifier = @[typeof] ({rejected}) in ret ()");
        let accepted = format!("let classifier = @[typeof] ({accepted}) in ret ()");
        let result = SourceCase::check(&rejected);
        let Err(CaseError::Compile(CompileError::Rejected(analysis))) = result else {
            panic!("expected checking rejection for {rejected}: {result:?}");
        };
        assert!(analysis.outcome().root().is_none());
        let diagnostics = analysis.outcome().diagnostics().unwrap();
        assert_eq!(diagnostics.len(), 2, "{rejected}: {diagnostics:?}");
        assert!(
            diagnostics.iter().all(|diagnostic| diagnostic.code == code),
            "{rejected}: {diagnostics:?}"
        );
        let spans = diagnostics
            .iter()
            .map(|diagnostic| diagnostic.primary.as_ref().unwrap().span)
            .collect::<Vec<_>>();
        assert_ne!(spans[0], spans[1], "independent errors retain their own sites");
        SourceCase::assert_accepted(SourceCase::check_linted(&accepted));
    }
}

#[test]
fn failed_patterns_do_not_invent_bindings_for_their_bodies() {
    let source = "match 0 | +Bad(x) => (x : String) | _ => (() : Int) end";
    let result = SourceCase::check(source);
    let Err(CaseError::Compile(CompileError::Rejected(analysis))) = result else {
        panic!("expected checking rejection: {result:?}");
    };
    let diagnostics = analysis.outcome().diagnostics().unwrap();
    assert_eq!(diagnostics.len(), 2, "{diagnostics:?}");
    assert!(
        diagnostics.iter().any(|diagnostic| diagnostic.code == TyckDiagnosticCode::TypeExpected)
    );
    assert!(
        diagnostics.iter().any(|diagnostic| diagnostic.code == TyckDiagnosticCode::TypeMismatch)
    );
    SourceCase::assert_accepted(SourceCase::check_linted("ret (match 0 | 0 => 1 | _ => 2 end)"));
}

#[test]
fn an_oversized_package_payload_is_rejected_before_indexing_its_expected_components() {
    SourceCase::assert_rejected(
        SourceCase::check_value("((Int, 1, 2, 3, 4) : exists (A : VType) . A * A)"),
        TyckDiagnosticCode::TypeExpected,
    );
    SourceCase::assert_accepted(SourceCase::check_value(
        "((Int, 1, 2) : exists (A : VType) . A * A)",
    ));
}
