#[path = "support/memory.rs"]
mod memory;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::{e2e_sources, utils::SourceCase};

e2e_sources!({
    exact_integers => "tests/std/exact-integers.zy",
    scalar_bytes => "tests/std/scalar-bytes.zy",
    representation => "tests/std/representation.zy",
    representation_invalid => "tests/std/representation-invalid.zy",
    storage_codecs => "tests/std/storage-codecs.zy",
});

fn source(body: &str) -> String {
    memory::source(&format!(
        "match memory/uint8 | +Err(_) => ! fail | +Ok(plan) => \
        let (= L, repr) = memory/realize UInt8 plan in \
        ! (allocation/reserve L Unit allocation/static_heap () repr/storage) OS no {{ fn vacant => {body} }} end"
    ))
}

#[test]
fn initialized_pointers_can_be_read_taken_and_freed() {
    let body = "! (repr/codec/init vacant 7) OS { fn value => \
        ! (codecs/unsafe/take L UInt8 repr/codec value) OS { fn vacant _ => \
        ! (allocation/unsafe/release L Unit allocation/static_heap () repr/storage vacant) OS no { ! exit 0 } } }";
    SourceCase::assert_accepted(SourceCase::check_linted(&source(body)));
    SourceCase::assert_accepted(SourceCase::run(&source(body)));
}

#[test]
fn pointer_states_and_logical_values_must_match_the_operation() {
    for body in [
        "! (repr/codec/read vacant) OS { fn _ => ! exit 0 }",
        "! (repr/codec/init vacant (7 : UInt32)) OS { fn _ => ! exit 0 }",
        "! (repr/codec/init vacant 7) OS { fn initialized => \
         ! (allocation/unsafe/release L Unit allocation/static_heap () repr/storage initialized) OS no { ! exit 0 } }",
        "! (repr/codec/init vacant 7) OS { fn initialized => \
         ! (repr/codec/init initialized 8) OS { fn _ => ! exit 0 } }",
        "let forged : Ptr L Init = 0 in ! exit 0",
    ] {
        SourceCase::assert_rejected(
            SourceCase::check(&source(body)),
            TyckDiagnosticCode::TypeMismatch,
        );
    }
}

#[test]
fn independent_layout_openings_cannot_exchange_typed_pointers() {
    SourceCase::assert_rejected(
        SourceCase::check(&source(
            "let (#L = Other, other) = memory/realize UInt8 plan in \
         ! (other/codec/init vacant 7) OS { fn _ => ! exit 0 }",
        )),
        TyckDiagnosticCode::TypeMismatch,
    );
}
