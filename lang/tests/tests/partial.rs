use zydeco_cli::CompileError;
use zydeco_session::DesugarError;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::{CaseError, SourceCase};

#[test]
fn single_constructor_decomposition_is_irrefutable() {
    for body in [
        "let W = data | +W : Int64 end in let +W x = (+W 0 : W) in ! exit x",
        "let W = data | +W : Int64 end in let (whole; +W x) = (+W 0 : W) in ! exit x",
        "let W = data | +W : Int64 end in let (/field = +W x) = (#field = (+W 0 : W)) in ! exit x",
    ] {
        SourceCase::assert_accepted(SourceCase::check_linted(body));
        SourceCase::assert_accepted(SourceCase::run(body));
        SourceCase::assert_accepted(SourceCase::lower(body));
    }
}

#[test]
fn refutable_computation_binders_require_an_explicit_opt_in() {
    for body in [
        "let 0 = 0 in ! exit 0",
        "do (0, x) <- ret (0, 3); ! exit 0",
        "do r <- (fn (0 : Int64) => ret 0) 0; ! exit r",
        "let f : Thk (Int64 -> Ret Int64) = { fn 0 => ret 0 } in do r <- ! f 0; ! exit r",
        "begin param (0 : Int64) that ! exit 0 end",
        "begin let 0 = 0 that ! exit 0 end",
        "let B = data | +T : Unit | +F : Unit end in let +T() = (+T() : B) in ! exit 0",
    ] {
        SourceCase::assert_rejected(SourceCase::check(body), TyckDiagnosticCode::RefutableBinding);
    }
}

#[test]
fn partial_headers_allow_all_their_own_binders() {
    for body in [
        "@[partial] let 0 = 0 in ! exit 0",
        "@[partial] do (0, x) <- ret (0, 3); ! exit 0",
        "do r <- (@[partial] fn (0 : Int64) (1 : Int64) => ret 0) 0 1; ! exit r",
        "let f : Thk (Int64 -> Ret Int64) = { @[partial] fn 0 => ret 0 } in do r <- ! f 0; ! exit r",
        "do r <- (begin @[partial] param (0 : Int64) that ret 0 end) 0; ! exit r",
        "begin @[partial] let 0 = 0 that ! exit 0 end",
        "@[partial] let ! f (0 : Int64) (1 : Int64) : Ret Int64 = ret 0 in do r <- ! f 0 1; ! exit r",
        "let B = data | +T : Unit | +F : Unit end in @[partial] let +T() = (+T() : B) in ! exit 0",
    ] {
        SourceCase::assert_accepted(SourceCase::check_linted(body));
        SourceCase::assert_accepted(SourceCase::run(body));
        SourceCase::assert_accepted(SourceCase::lower(body));
    }
}

#[test]
fn partial_annotations_do_not_cover_nested_or_later_binders() {
    for body in [
        "@[partial] let 0 = 0 in let 1 = 1 in ret 0",
        "@[partial] fn (0 : Int64) => let 1 = 1 in ret 0",
        "@[partial] fn (0 : Int64) => fn (1 : Int64) => ret 0",
        "begin @[partial] param (0 : Int64) that let 1 = 1 that ret 0 end",
    ] {
        SourceCase::assert_rejected(SourceCase::check(body), TyckDiagnosticCode::RefutableBinding);
    }
}

#[test]
fn partial_annotations_cannot_make_value_functions_partial() {
    for body in [
        "let f = (@[partial] val (0 : Int64) => 0) in ret 0",
        "let val f (x : Int64) : Int64 = @[partial] let 0 = x in 1 in ret (f 0)",
        "let value = (@[partial] let 0 = 0 in 1) in ret value",
    ] {
        SourceCase::assert_rejected(SourceCase::check(body), TyckDiagnosticCode::Expressivity);
    }
}

#[test]
fn partial_annotations_validate_their_arguments_and_target() {
    SourceCase::assert_desugar_error(
        SourceCase::check("@[partial(1)] let x = 0 in ret x"),
        |error| matches!(error, DesugarError::InvalidPartialMeta { .. }),
    );
    SourceCase::assert_desugar_error(
        SourceCase::check("@[partial] begin let x = 0 in ret x end"),
        |error| matches!(error, DesugarError::PartialPayloadNotBinding(_)),
    );
}

#[test]
fn failed_partial_patterns_are_runtime_errors() {
    for body in [
        "@[partial] let 0 = 1 in ! exit 0",
        "@[partial] do 0 <- ret 1; ! exit 0",
        "do r <- (@[partial] fn (0 : Int64) => ret 0) 1; ! exit r",
        "let B = data | +T : Unit | +F : Unit end in @[partial] let +T() = (+F() : B) in ! exit 0",
    ] {
        SourceCase::assert_accepted(SourceCase::lower(body));
        let error =
            SourceCase::run(body).expect_err("a failed partial pattern must stop evaluation");
        assert!(matches!(error, CaseError::Compile(CompileError::Runtime(_))));
        assert_eq!(error.to_string(), "pattern match failed");
    }
}
