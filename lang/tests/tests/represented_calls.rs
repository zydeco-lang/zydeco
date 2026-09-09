use std::path::PathBuf;
use zydeco_cli::RepresentationStrategy;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::{
    e2e_sources,
    utils::{SourceCase, SourceProgram, TestBackend},
};

e2e_sources!({
    represented_call => "tests/std/represented-call/main.zy",
});

#[test]
fn dynamic_calls_and_conversion_agree_under_every_word_policy() {
    for backend in [TestBackend::Interpreter, TestBackend::WasmSps] {
        SourceProgram::setup("tests/std/represented-call/main.zy")
            .with_args(["convert"])
            .test(backend);
    }
    for backend in [TestBackend::Amd64, TestBackend::WasmAm] {
        for &policy in RepresentationStrategy::ALL {
            SourceProgram::setup("tests/std/represented-call/main.zy")
                .with_args(["convert"])
                .with_representation(policy)
                .test(backend);
        }
    }
}

struct CallCase;

impl CallCase {
    fn source(body: &str) -> String {
        let directory = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/std/memory")
            .canonicalize()
            .unwrap();
        format!(
            r#"
let make_memory = @(import("{directory}/static-layout.zy")) in
let (= Plan, = Layout, memory) = builtin |> make_memory in
let Storage = @(import("{directory}/storage.type.zy")) in
let (/Function; calls) = @(import("{directory}/call.zy")) in
let Record = UInt8 * UInt32 in
let natural = memory/product UInt8 UInt32 memory/uint8 memory/uint32 in
let fail = {{ ! exit 1 }} in
match memory/align Record 16 natural
| +Err(_) => ! fail
| +Ok(plan) =>
  let (= Stored, repr) = memory/realize Record plan in
  let boundary = calls/between Record Record Stored Stored repr repr in
  let identity = boundary/encode OS {{ fn value no yes => ! yes value }} in
  {body}
end
"#,
            directory = directory.display()
        )
    }

    fn rejected(body: &str) {
        SourceCase::assert_rejected(
            SourceCase::check(&Self::source(body)),
            TyckDiagnosticCode::TypeMismatch,
        );
    }
}

#[test]
fn matching_interfaces_pass_stored_arguments_and_results() {
    let source = CallCase::source(
        "! repr/store OS (7 : UInt8, 42 : UInt32) fail { fn value => \
         ! identity value fail { fn result => \
         ! repr/load OS result fail { fn _ => ! exit 0 } } }",
    );
    SourceCase::assert_accepted(SourceCase::check_linted(&source));
    SourceCase::assert_accepted(SourceCase::run(&source));
    SourceCase::assert_accepted(SourceCase::lower(&source));
}

#[test]
fn logical_values_and_bytes_cannot_bypass_the_call_contract() {
    for body in [
        "let logical : Record = (7 : UInt8, 42 : UInt32) in \
         ! identity logical fail { fn _ => ! exit 0 }",
        "do bytes <- ! numeric/uint8/to_le_bytes 7; ! identity bytes fail { fn _ => ! exit 0 }",
        "let broken : Thk (Function Stored Stored OS) = { \
         fn value no yes => let logical : Record = (7 : UInt8, 42 : UInt32) in \
         ! yes logical } in ! exit 0",
    ] {
        CallCase::rejected(body);
    }
}

#[test]
fn different_alignment_or_width_cannot_supply_an_argument_or_continuation() {
    for (logical, layout, value) in [
        ("Record", "memory/align Record 64 natural", "(7 : UInt8, 42 : UInt32)"),
        (
            "UInt8 * UInt16",
            "memory/align (UInt8 * UInt16) 16 \
             (memory/product UInt8 UInt16 memory/uint8 memory/uint16)",
            "(7 : UInt8, 42 : UInt16)",
        ),
        // Even identical placement needs a shared opening, not a metadata comparison.
        ("Record", "memory/align Record 16 natural", "(7 : UInt8, 42 : UInt32)"),
    ] {
        for use_other in [
            format!(
                "! other/store OS {value} fail {{ fn value => \
                     ! identity value fail {{ fn _ => ! exit 0 }} }}"
            ),
            "! repr/store OS (7 : UInt8, 42 : UInt32) fail { fn value => \
             ! identity value fail { fn (result : Other) => ! exit 0 } }"
                .to_owned(),
            format!(
                "let other_boundary = calls/between ({logical}) ({logical}) Other Other other other in \
                     let other_identity = other_boundary/encode OS {{ fn value no yes => ! yes value }} in \
                     let broken = calls/compose Stored Stored Other OS identity other_identity in ! exit 0"
            ),
        ] {
            CallCase::rejected(&format!(
                "match {layout} | +Err(_) => ! fail | +Ok(other_plan) => \
                 let (#Stored = Other, other) = memory/realize ({logical}) other_plan in \
                 {use_other} end"
            ));
        }
    }
}

#[test]
fn adapters_preserve_failure_continuations_in_a_returning_protocol() {
    let cases = [
        "let boundary = calls/between Record Record Stored Stored failing repr in \
         let encoded = boundary/encode R unexpected_logical in ! encoded stored no yes_stored",
        "let encoded = boundary/encode R reject_logical in ! encoded stored no yes_stored",
        "let boundary = calls/between Record Record Stored Stored repr failing in \
         let encoded = boundary/encode R keep_logical in ! encoded stored no yes_stored",
        "let boundary = calls/between Record Record Stored Stored failing repr in \
         let decoded = boundary/decode R unexpected_stored in ! decoded value no yes_logical",
        "let decoded = boundary/decode R reject_stored in ! decoded value no yes_logical",
        "let boundary = calls/between Record Record Stored Stored repr failing in \
         let decoded = boundary/decode R keep_stored in ! decoded value no yes_logical",
        "let convert = calls/convert Record Stored Stored failing repr R in \
         ! convert stored no yes_stored",
        "let convert = calls/convert Record Stored Stored repr failing R in \
         ! convert stored no yes_stored",
        "let composed = calls/compose Stored Stored Stored R reject_stored unexpected_stored in \
         ! composed stored no yes_stored",
        "let composed = calls/compose Stored Stored Stored R keep_stored reject_stored in \
         ! composed stored no yes_stored",
    ];
    for body in cases {
        let source = CallCase::source(&format!(
            r#"
let R = Ret Int64 in
let no = {{ ret 0 }} in
let yes_stored = {{ fn (_ : Stored) => ret 99 }} in
let yes_logical = {{ fn (_ : Record) => ret 99 }} in
let unexpected_logical : Thk (Function Record Record R) = {{ fn _ _ _ => ret 99 }} in
let unexpected_stored : Thk (Function Stored Stored R) = {{ fn _ _ _ => ret 99 }} in
let reject_logical : Thk (Function Record Record R) = {{ fn _ no _ => ! no }} in
let reject_stored : Thk (Function Stored Stored R) = {{ fn _ no _ => ! no }} in
let keep_logical : Thk (Function Record Record R) = {{ fn value _ yes => ! yes value }} in
let keep_stored : Thk (Function Stored Stored R) = {{ fn value _ yes => ! yes value }} in
let failing : Storage Record Stored =
  (#size = repr/size, #alignment = repr/alignment,
   #store = {{ fn R _ no _ => ! no }}, #load = {{ fn R _ no _ => ! no }},
   #bytes = repr/bytes, #from_bytes = repr/from_bytes)
in
let value : Record = (7 : UInt8, 42 : UInt32) in
! repr/store OS value fail {{ fn stored =>
  do code <- ({body});
  ! exit code
}}
"#
        ));
        SourceCase::assert_accepted(SourceCase::check_linted(&source));
        SourceCase::assert_accepted(SourceCase::run(&source));
    }
}
