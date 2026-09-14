//! Executable examples and gap witnesses for the next FFI design.

use std::path::PathBuf;
use zydeco_cli::CompileError;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::{CaseError, SourceCase};

struct ForeignExamples;

impl ForeignExamples {
    fn library() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib").canonicalize().unwrap()
    }

    fn declaration(symbol: &str, classifier: &str) -> String {
        format!(
            "let foreign : {classifier} = \
             @(ffi(c, library(\"zyffi_examples\"), symbol(\"{symbol}\"))) in ! exit 0"
        )
    }

    fn rejects(source: &str, message: &str) {
        let error = SourceCase::check(source).unwrap_err();
        let CaseError::Compile(CompileError::Rejected(analysis)) = error else {
            panic!("expected a foreign classifier rejection, found {error}");
        };
        assert!(analysis.outcome().root().is_none());
        let diagnostics = analysis.outcome().diagnostics().unwrap();
        assert!(
            diagnostics.iter().any(|diagnostic| {
                diagnostic.code == TyckDiagnosticCode::InvalidForeignClassifier
                    && diagnostic.primary.is_some()
                    && diagnostic.message.contains(message)
            }),
            "{diagnostics:?}"
        );
    }

    fn record(body: &str) -> String {
        let library = Self::library();
        format!(
            r#"
let (/Bytes; /bytes; byte_package) = builtin |> (@(import("{library}/std/text/bytes.zy"))) in
let (/Uninit; /Init; /Ptr; /fixed; /allocation; /pointer) = builtin |> (@(import("{library}/std/memory/package.zy"))) in
let (= Plan, = Layout, memory) = fixed in
let (/Alloc; /heap) = allocation in
let decode = (builtin |> (@(import("{library}/tests/ffi/record-input.zy")))) byte_package in
let Record = UInt8 * UInt32 in
let fail = {{ ! exit 1 }} in
match memory/product UInt8 UInt32 memory/uint8 memory/uint32
| +Err(_) => ! fail
| +Ok(plan) =>
  let (= L, storage) = memory/realize Record plan in
  {body}
end
"#,
            library = library.display(),
        )
    }
}

#[test]
fn checksum_has_a_direct_foreign_classifier() {
    SourceCase::assert_accepted(SourceCase::check_linted(&ForeignExamples::declaration(
        "sample_checksum",
        "Thk (Addr -> Int64 -> UInt64 -> Ret UInt64)",
    )));
}

#[test]
fn pointer_only_record_imports_have_no_implicit_length_argument() {
    SourceCase::assert_accepted(SourceCase::check_linted(&ForeignExamples::declaration(
        "sample_inspect_with_options",
        "Thk (Addr -> UInt64 -> UInt64 -> UInt64 -> UInt64 -> UInt64 -> Ret UInt64)",
    )));
}

#[test]
fn typed_pointers_require_explicit_exposure_at_the_c_boundary() {
    let wrapper = ForeignExamples::record(
        r#"
let raw : Thk (Addr -> Ret UInt64) =
  @(ffi(c, library("zyffi_examples"), symbol("sample_inspect"))) in
let inspect : Thk (Ptr L Init -> Ret UInt64) = {
  fn value => ! raw (pointer/unsafe/address L Init value)
} in ! exit 0
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_linted(&wrapper));
    SourceCase::assert_accepted(SourceCase::lower(&wrapper));
    ForeignExamples::rejects(
        &ForeignExamples::record(&ForeignExamples::declaration(
            "sample_inspect",
            "Thk (Ptr L Init -> Ret UInt64)",
        )),
        "argument 1",
    );
}

#[test]
fn a_logical_product_does_not_select_the_c_record_value_abi() {
    ForeignExamples::rejects(
        &ForeignExamples::declaration(
            "sample_inspect_value",
            "Thk ((UInt8 * UInt32) -> Ret UInt64)",
        ),
        "argument 1",
    );
}

#[test]
fn raw_mutable_destinations_are_supported_and_capturing_callbacks_remain_unsupported() {
    SourceCase::assert_accepted(SourceCase::check_linted(&ForeignExamples::declaration(
        "sample_write",
        "Thk (Addr -> UInt8 -> UInt32 -> Ret Int32)",
    )));
    ForeignExamples::rejects(
        &ForeignExamples::declaration(
            "sample_visit",
            "Thk (Addr -> Thk (Int64 -> Ret Int64) -> Ret Int64)",
        ),
        "argument 2",
    );
}

#[test]
#[cfg(unix)]
fn c_output_decodes_without_reading_or_constraining_padding() {
    use std::process::Command;
    use zydeco_tests::utils::{ExecutionTarget, SourceProgram};

    let directory = tempfile::tempdir().unwrap();
    let executable = directory.path().join("ffi_examples");
    let output = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror"])
        .arg(ForeignExamples::library().join("tests/ffi/contracts.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .expect("C compiler is required for FFI example tests");
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    let output = Command::new(executable).output().unwrap();
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
    // This fixture deliberately targets the same little-endian layout as its source plan.
    assert_eq!(output.stdout, [7, 0x58, 0x58, 0x58, 4, 3, 2, 1]);
    let literal = output.stdout.iter().map(|byte| format!("\\u{{{byte:x}}}")).collect::<String>();
    let source = ForeignExamples::record(&format!(
        r#"
do foreign <- ! bytes/from_string "{literal}";
! decode OS foreign fail {{ fn (tag, payload) =>
  ! numeric/uint8/eq OS tag 7 {{
    ! numeric/uint32/eq OS payload 16909060 {{ ! exit 0 }} fail
  }} fail
}}

"#
    ));
    SourceCase::assert_accepted(SourceCase::check_linted(&source));
    let program = directory.path().join("record-input.zy");
    std::fs::write(
        &program,
        format!(
            "param (/VType; /CType; /Thk; /Ret; /OS; /UInt8; /UInt32; \
             /numeric; /process; builtin) : @(import(\"{}/std/builtin.zy\")) in \
             let exit = process/exit in {source}",
            ForeignExamples::library().display()
        ),
    )
    .unwrap();
    for backend in [
        ExecutionTarget::Interpreter,
        ExecutionTarget::Exe,
        ExecutionTarget::WasmAm,
        ExecutionTarget::WasmSps,
    ] {
        SourceProgram::setup(&program).test(backend);
    }
}

#[test]
fn the_foreign_decoder_checks_extent_before_exposing_fields() {
    for literal in ["", "abcdefg", "abcdefghi"] {
        let source = ForeignExamples::record(&format!(
            "do source <- ! bytes/from_string \"{literal}\"; \
             ! decode OS source {{ ! exit 0 }} {{ fn _ => ! fail }}"
        ));
        SourceCase::assert_accepted(SourceCase::run(&source));
    }
}
