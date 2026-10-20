use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

struct EitherCase;

impl EitherCase {
    fn source(body: &str) -> String {
        let library = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib/std");
        format!(
            r#"
let DataPackage = @(import("{library}/data/package.type.zy")) in
let data_package : DataPackage = builtin |> (@(import("{library}/data/package.zy"))) in
let (/Either; /either) = data_package in
{body}
"#,
            library = library.display(),
        )
    }

    fn reject(body: &str, code: TyckDiagnosticCode) {
        let source = Self::source(body);
        SourceCase::assert_rejected(SourceCase::check(&source), code);
        SourceCase::assert_rejected(SourceCase::lower(&source), code);
    }
}

#[test]
fn either_shares_its_carrier_with_exception_and_the_data_signature() {
    let library = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib/std");
    let source = EitherCase::source(&format!(
        r#"
let DirectEither = @(import("{library}/data/either.zy")) in
let (/ExceptionT) = builtin |> (@(import("{library}/effect/exception.zy"))) in
let _ = val (L : VType) (R : VType) (value : Either L R) =>
  (
    value : DirectEither L R
  ) in
let _ = val (E : VType) (A : VType) (value : Thk (ExceptionT E Ret A)) =>
  (
    value : Thk (Ret (Either E A))
  ) in
! exit 0
"#,
        library = library.display(),
    ));
    SourceCase::assert_accepted(SourceCase::check_linted(&source));
    SourceCase::assert_accepted(SourceCase::lower(&source));
}

#[test]
fn either_rejects_result_exports_and_old_constructor_labels() {
    for body in [
        "let (/Result) = data_package in ! exit 0",
        "let _ = data_package/result in ! exit 0",
        "let _ = either/ok in ! exit 0",
        "let _ = either/err in ! exit 0",
        "let _ = either/map_err in ! exit 0",
        "let _ = either/is_ok in ! exit 0",
        "let _ = either/is_err in ! exit 0",
    ] {
        EitherCase::reject(body, TyckDiagnosticCode::MissingNamedField);
    }
    for body in [
        "let _ : Either String Int = +Ok(1) in ! exit 0",
        "let _ : Either String Int = +Err(\"error\") in ! exit 0",
    ] {
        EitherCase::reject(body, TyckDiagnosticCode::UnknownDataConstructor);
    }
}

#[test]
fn either_rejects_reversed_payloads_and_fold_handlers() {
    for body in [
        r#"
let _ : Thk (Ret (Either String Int)) = { ! either/right Int String "value" } in
! exit 0
"#,
        r#"
do _ <- ! either/fold String Int (Ret Int) (+Left("error") : Either String Int)
  { fn (value : Int) => ret value } { fn (_ : String) => ret 0 };
! exit 0
"#,
    ] {
        EitherCase::reject(body, TyckDiagnosticCode::TypeMismatch);
    }
}
