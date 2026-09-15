#[path = "support/memory.rs"]
mod memory;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::{e2e_sources, utils::SourceCase};

e2e_sources!({
    exact_integers => "tests/std/exact-integers.zy",
    scalar_bytes => "tests/std/scalar-bytes.zy",
    representation => "tests/std/representation.zy",
    representation_invalid => "tests/std/representation-invalid.zy",
});

fn source(body: &str) -> String {
    memory::source(&format!(
        "match memory/uint8 | +Err(_) => ! fail | +Ok(plan) => \
        let (= L, repr) = memory/realize UInt8 plan in \
        ! repr/allocate OS heap no {{ fn vacant => {body} }} end"
    ))
}

#[test]
fn initialized_pointers_can_be_read_taken_and_freed() {
    let body = "! repr/unsafe/init OS vacant 7 { fn value => \
        ! repr/unsafe/take OS value { fn vacant _ => \
        ! repr/unsafe/free OS heap vacant no { ! exit 0 } } }";
    SourceCase::assert_accepted(SourceCase::check_linted(&source(body)));
    SourceCase::assert_accepted(SourceCase::run(&source(body)));
}

#[test]
fn pointer_states_and_logical_values_must_match_the_operation() {
    for body in [
        "! repr/unsafe/read OS vacant { fn _ => ! exit 0 }",
        "! repr/unsafe/init OS vacant (7 : UInt32) { fn _ => ! exit 0 }",
        "! repr/unsafe/init OS vacant 7 { fn initialized => \
         ! repr/unsafe/free OS heap initialized no { ! exit 0 } }",
        "! repr/unsafe/init OS vacant 7 { fn initialized => \
         ! repr/unsafe/init OS initialized 8 { fn _ => ! exit 0 } }",
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
         ! other/unsafe/init OS vacant 7 { fn _ => ! exit 0 }",
        )),
        TyckDiagnosticCode::TypeMismatch,
    );
}
