use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::{SourceCase, SourceTemplate};

zydeco_tests::check_source!(nondet_module, "std/effect/nondet.zy");
zydeco_tests::e2e_io_source!(operations, "tests/effects/nondet-operations.zy", "", "");
zydeco_tests::e2e_io_source!(laws, "tests/effects/nondet-laws.zy", "", "");
zydeco_tests::e2e_io_source!(
    eager_list_counterexample,
    "tests/effects/nondet-law-counterexamples.zy",
    "",
    ""
);
zydeco_tests::e2e_io_source!(transformers, "tests/effects/nondet-transformers.zy", "", "");
zydeco_tests::e2e_io_source!(translated_clients, "tests/effects/nondet-monadic.zy", "", "");

struct NonDetCase;

impl NonDetCase {
    fn source(body: &str) -> String {
        let library = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/std")
            .display()
            .to_string();
        SourceTemplate::parse(include_str!("../templates/nondet.zy.in"))
            .and_then(|template| template.render([("std", library.as_str()), ("body", body)]))
            .expect("the nondeterminism template and its arguments must agree")
    }
}

#[test]
fn shared_list_and_carrier_witnesses_survive_composition() {
    let source = NonDetCase::source(
        r#"
let _ : WithMonad INonDet = (N, n) in
let _ : WithMonad INonDet = (NO, #monad = no/monad, #nondet = no/nondet) in
let _ : Thk (forall (A : VType) . Thk (O A) -> NO A) = {
  fn A m => ! no/lift A m
} in
let algebra : Thk (Algebra Ret (Ret Int)) = {
  fn A m k => do a <- ! m; ! k a
} in
do result <- ! n/monad .return Int 7 (Ret Int) algebra { fn value rest => ret value } { ret 0 };
do values <- ! nondet/observe_many Int 2 { ! n/monad .return Int result };
let _ : List Int = values in
let _ : Thk (N Int) = { ! nondet/choose (N, n) Int values } in
let _ : Thk (NO Int) = { ! nondet/choose (NO, #monad = no/monad, #nondet = no/nondet) Int values } in
! exit 0
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_linted(&source));
    SourceCase::assert_accepted(SourceCase::lower(&source));
}

#[test]
fn search_rejects_mixed_carriers_and_invalid_observers() {
    for (body, code) in [
        (
            "let _ : WithMonad INonDet = (N, #monad = o/monad, #nondet = n/nondet) in ! exit 0",
            TyckDiagnosticCode::TypeMismatch,
        ),
        (
            "let _ : WithMonad INonDet = (NO, #monad = no/monad, #nondet = n/nondet) in ! exit 0",
            TyckDiagnosticCode::TypeMismatch,
        ),
        ("let _ : Thk (Monad List) = n/monad in ! exit 0", TyckDiagnosticCode::KindMismatch),
        ("let _ : Thk (N Int) = { ret 1 } in ! exit 0", TyckDiagnosticCode::TypeMismatch),
        (
            "do _ <- ! nondet/observe_many Int \"two\" { ! n/nondet .empty Int }; ! exit 0",
            TyckDiagnosticCode::TypeMismatch,
        ),
        (
            r#"
let algebra : Thk (Algebra Ret (Ret Int)) = {
  fn A m k => do a <- ! m; ! k a
} in
do _ <- ! no/monad .return Int 7 (Ret Int) algebra { fn value rest => ret value } { ret 0 };
! exit 0
"#,
            TyckDiagnosticCode::TypeMismatch,
        ),
    ] {
        let source = NonDetCase::source(body);
        SourceCase::assert_rejected(SourceCase::check(&source), code);
        SourceCase::assert_rejected(SourceCase::lower(&source), code);
    }
}
