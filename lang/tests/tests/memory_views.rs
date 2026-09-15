#[path = "support/memory.rs"]
mod memory;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

struct SliceCase;

impl SliceCase {
    fn source(body: &str) -> String {
        memory::source(&format!(
            "match memory/int | +Err(_) => ! fail | +Ok(plan) => \
             let (= L, repr) = memory/realize Int plan in \
             let access = slices/for_layout L repr/storage in {body} end"
        ))
    }
}

#[test]
fn slice_validation_rejects_bounds_and_overflow_before_pointer_arithmetic() {
    for (count, index, fault) in [
        (3_i64, -1_i64, "+Bounds()"),
        (3, 3, "+Bounds()"),
        (0, 0, "+Bounds()"),
        (
            zydeco_machine::word::RuntimeWord::SIGNED_MAX,
            zydeco_machine::word::RuntimeWord::SIGNED_MAX - 1,
            "+Overflow()",
        ),
    ] {
        let body = format!(
            r#"
do address <- ! raw/unsafe/null;
let p = pointer/unsafe/from_address L Uninit address in
! slices/unsafe/from_parts L Uninit OS p {count} no {{ fn slice =>
  ! access/unsafe/at Uninit OS slice {index} {{ fn fault =>
    match fault | {fault} => ! exit 0 | _ => ! fail end
  }} {{ fn _ => ! fail }}
}}
"#
        );
        SourceCase::assert_accepted(SourceCase::run(&SliceCase::source(&body)));
    }
}

#[test]
fn slice_state_and_selected_element_layout_must_match() {
    for body in [
        r#"
do address <- ! raw/unsafe/null;
let p = pointer/unsafe/from_address L Uninit address in
! slices/unsafe/from_parts L Uninit OS p 0 no { fn slice =>
  ! access/unsafe/at Init OS slice 0 no { fn _ => ! exit 0 }
}
"#,
        r#"
let (#L = Other, other) = memory/realize Int plan in
let wrong = slices/for_layout Other other/storage in
do address <- ! raw/unsafe/null;
! slices/unsafe/from_parts L Uninit OS (pointer/unsafe/from_address L Uninit address) 0 no { fn slice =>
  ! wrong/unsafe/at Uninit OS slice 0 no { fn _ => ! exit 0 }
}
"#,
    ] {
        SourceCase::assert_rejected(
            SourceCase::check(&SliceCase::source(body)),
            TyckDiagnosticCode::TypeMismatch,
        );
    }
}

#[test]
fn negative_slice_lengths_are_rejected() {
    SourceCase::assert_accepted(SourceCase::run(&SliceCase::source(
        r#"
do address <- ! raw/unsafe/null;
! slices/unsafe/from_parts L Uninit OS (pointer/unsafe/from_address L Uninit address) -1
  { fn fault => match fault | +Bounds() => ! exit 0 | _ => ! fail end }
  { fn _ => ! fail }
"#,
    )));
}
