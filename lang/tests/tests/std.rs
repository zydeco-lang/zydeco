use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::e2e_sources;
use zydeco_tests::utils::{ExecutionTarget, SourceCase, SourceProgram};

e2e_sources!({
    bool => "tests/std/bool.zy",
    option => "tests/std/option.zy",
    either => "tests/std/either.zy",
    utf8 => "tests/std/utf8.zy",
    bytes => "tests/std/bytes.zy",
    collections => "tests/std/collections.zy",
    float => "tests/std/float.zy",
    numeric_capabilities => "tests/std/numeric-capabilities.zy",
});

struct FilesystemCase;

impl FilesystemCase {
    fn run(backend: ExecutionTarget) {
        let directory = tempfile::tempdir().expect("filesystem fixture directory");
        let path = directory.path().join("roundtrip.bin");
        std::fs::write(path.with_extension("bin.invalid"), [0xff, 0xfe])
            .expect("invalid UTF-8 fixture");
        SourceProgram::setup("tests/std/support/filesystem.zy")
            .with_args([path.to_string_lossy().into_owned()])
            .test(backend);
    }
}

#[test]
fn filesystem_interpreter() {
    FilesystemCase::run(ExecutionTarget::Interpreter);
}

#[test]
fn filesystem_amd64() {
    FilesystemCase::run(ExecutionTarget::Exe);
}

#[test]
fn filesystem_wasm_am() {
    FilesystemCase::run(ExecutionTarget::WasmAm);
}

#[test]
fn filesystem_wasm_sps() {
    FilesystemCase::run(ExecutionTarget::WasmSps);
}

struct ArgumentListCase;

impl ArgumentListCase {
    fn run(backend: ExecutionTarget) {
        SourceProgram::setup("tests/std/support/arg-list.zy").with_args(["alpha"]).test(backend);
    }
}

#[test]
fn argument_list_interpreter() {
    ArgumentListCase::run(ExecutionTarget::Interpreter);
}

#[test]
fn argument_list_wasm_am() {
    ArgumentListCase::run(ExecutionTarget::WasmAm);
}

#[test]
fn argument_list_wasm_sps() {
    ArgumentListCase::run(ExecutionTarget::WasmSps);
}

zydeco_tests::e2e_io_source!(
    read_line_as_int,
    "tests/std/support/read-line-as-int.zy",
    "123\n",
    ""
);

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
