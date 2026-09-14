#[path = "support/memory.rs"]
mod memory;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

#[test]
fn slice_validation_rejects_bounds_and_overflow_before_pointer_arithmetic() {
    for (count, index, stride, fault) in [
        (3_i64, -1_i64, 8_i64, "+Bounds()"),
        (3, 3, 8, "+Bounds()"),
        (0, 0, 8, "+Bounds()"),
        (i64::MAX, i64::MAX - 1, 8, "+Overflow()"),
        (3, 1, -1, "+InvalidLayout()"),
    ] {
        let body = format!(
            r#"
do address <- ! raw/unsafe/null;
let pointer = pointer/unsafe/from_address Unit Uninit address in
! slices/unsafe/from_parts Unit Uninit OS pointer {count} no {{ fn slice =>
  ! slices/unsafe/at Unit Uninit OS {stride} slice {index} {{ fn fault =>
    match fault | {fault} => ! exit 0 | _ => ! fail end
  }} {{ fn _ => ! fail }}
}}
"#
        );
        SourceCase::assert_accepted(SourceCase::run(&memory::source(&body)));
    }
}

#[test]
fn slice_state_is_preserved_by_indexing() {
    SourceCase::assert_rejected(
        SourceCase::check(&memory::source(
            r#"
do address <- ! raw/unsafe/null;
let p = pointer/unsafe/from_address Unit Uninit address in
! slices/unsafe/from_parts Unit Uninit OS p 0 no { fn slice =>
  ! slices/unsafe/at Unit Init OS 8 slice 0 no { fn _ => ! exit 0 }
}
"#,
        )),
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn negative_slice_lengths_are_rejected() {
    SourceCase::assert_accepted(SourceCase::run(&memory::source(
        r#"
do address <- ! raw/unsafe/null;
! slices/unsafe/from_parts Unit Uninit OS (pointer/unsafe/from_address Unit Uninit address) -1
  { fn fault => match fault | +Bounds() => ! exit 0 | _ => ! fail end }
  { fn _ => ! fail }
"#,
    )));
}
