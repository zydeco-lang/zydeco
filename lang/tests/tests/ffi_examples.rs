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
let make_memory = @(import("{library}/std/memory/static-layout.zy")) in
let (= Plan, = Layout, memory) = (builtin |> make_memory) byte_package in
let decode = (builtin |> (@(import("{library}/tests/ffi/record-input.zy")))) byte_package in
let Record = UInt8 * UInt32 in
let fail = {{ ! exit 1 }} in
match memory/product UInt8 UInt32 memory/uint8 memory/uint32
| +Err(_) => ! fail
| +Ok(plan) =>
  let (= Stored, storage) = memory/realize Record plan in
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
        "Thk ((Access * Addr * Int64) -> Int64 -> UInt64 -> Ret UInt64)",
    )));
}

#[test]
fn pointer_only_record_imports_have_no_implicit_length_argument() {
    SourceCase::assert_accepted(SourceCase::check_linted(&ForeignExamples::declaration(
        "sample_inspect_with_options",
        "Thk ((Access * Addr * Int64) -> UInt64 -> UInt64 -> UInt64 -> UInt64 -> UInt64 -> Ret UInt64)",
    )));
}

#[test]
fn storage_can_supply_a_window_but_cannot_directly_classify_the_foreign_import() {
    let wrapper = ForeignExamples::record(
        r#"
let raw : Thk ((Access * Addr * Int64) -> Int64 -> Ret UInt64) =
  @(ffi(c, library("zyffi_examples"), symbol("sample_inspect_bytes"))) in
let inspect : Thk (Stored -> Ret UInt64) = {
  fn value => do encoded <- ! storage/bytes value;
    ! bytes/with_window (Ret UInt64) encoded
      { fn _ => @[partial] let 0 = 1 in ret (0 : UInt64) }
      { fn access address length => ! raw (access, address, length) length }
} in
! storage/store OS (7 : UInt8, 16909060 : UInt32) fail {
  fn value => do _ <- ! inspect value; ! exit 0
}
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_linted(&wrapper));
    SourceCase::assert_accepted(SourceCase::lower(&wrapper));
    ForeignExamples::rejects(
        &ForeignExamples::record(&ForeignExamples::declaration(
            "sample_inspect",
            "Thk (Stored -> Ret UInt64)",
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
fn mutable_destinations_and_capturing_callbacks_need_new_contracts() {
    ForeignExamples::rejects(
        &format!(
            "let (/Buffer; _) = builtin in {}",
            ForeignExamples::declaration(
                "sample_write",
                "Thk (Buffer -> UInt8 -> UInt32 -> Ret Int32)",
            )
        ),
        "argument 1",
    );
    ForeignExamples::rejects(
        &ForeignExamples::declaration(
            "sample_visit",
            "Thk ((Access * Addr * Int64) -> Thk (Int64 -> Ret Int64) -> Ret Int64)",
        ),
        "argument 2",
    );
}

#[test]
#[cfg(unix)]
fn c_examples_execute_and_their_output_can_be_canonicalized_in_source() {
    use std::process::Command;
    use zydeco_tests::utils::{SourceProgram, TestBackend};

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
! storage/from_bytes OS foreign {{
  ! decode OS foreign fail {{ fn (tag, payload) =>
    ! numeric/uint8/eq OS tag 7 {{
      ! numeric/uint32/eq OS payload 16909060 {{
        ! storage/store OS (tag, payload) fail {{ fn canonical =>
          do encoded <- ! storage/bytes canonical;
          do expected <- ! bytes/from_string "\u{{7}}\0\0\0\u{{4}}\u{{3}}\u{{2}}\u{{1}}";
          ! bytes/eq OS encoded expected {{
            ! storage/from_bytes OS encoded fail {{ fn _ => ! exit 0 }}
          }} fail
        }}
      }} fail
    }} fail
  }}
}} {{ fn _ => ! fail }}
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
    for backend in
        [TestBackend::Interpreter, TestBackend::Amd64, TestBackend::WasmAm, TestBackend::WasmSps]
    {
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
