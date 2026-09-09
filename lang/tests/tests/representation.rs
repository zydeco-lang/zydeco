use std::path::PathBuf;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::{e2e_sources, utils::SourceCase};

e2e_sources!({
    scalar_bytes => "tests/builtin/scalar-bytes.zy",
    representation => "tests/std/representation.zy",
    representation_invalid => "tests/std/representation-invalid.zy",
});

struct RepresentationCase;

impl RepresentationCase {
    fn source(body: &str) -> String {
        let library = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/std/memory/package.zy")
            .canonicalize()
            .unwrap();
        format!(
            r#"
let make_memory = @(import("{}")) in
let (= Layout, = Representation, memory) = builtin |> make_memory in
let fail = {{ ! exit 1 }} in
! memory/realize UInt8 OS memory/uint8 fail {{ fn (= Stored, repr) =>
  {body}
}}
"#,
            library.display()
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
fn a_storage_contract_accepts_its_own_values() {
    SourceCase::assert_accepted(SourceCase::lower(&RepresentationCase::source(
        "! repr/store OS 7 fail { fn stored => ! repr/load OS stored fail { fn _ => ! exit 0 } }",
    )));
}

#[test]
fn storage_rejects_a_value_with_the_wrong_logical_type() {
    RepresentationCase::rejected("! repr/store OS (7 : UInt32) fail { fn _ => ! exit 0 }");
}

#[test]
fn bytes_cannot_bypass_storage_validation() {
    RepresentationCase::rejected(
        "do bytes <- ! numeric/uint8/to_le_bytes 7; \
         ! repr/load OS bytes fail { fn _ => ! exit 0 }",
    );
}

#[test]
fn separate_openings_cannot_exchange_stored_values() {
    RepresentationCase::rejected(
        "! repr/store OS 7 fail { fn stored => \
         ! memory/realize UInt8 OS (memory/align UInt8 16 memory/uint8) fail { \
         fn (#Stored = OtherStored, other) => ! other/load OS stored fail { fn _ => ! exit 0 } } }",
    );
}
