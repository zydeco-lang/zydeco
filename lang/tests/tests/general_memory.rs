#[path = "support/memory.rs"]
mod memory;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::{e2e_sources, utils::SourceCase};

e2e_sources!({
    general_views => "tests/std/general-views.zy",
    array_memory => "tests/std/array-memory.zy",
    header_array => "tests/std/header-array.zy",
    runtime_memory => "tests/std/runtime-memory.zy",
});

struct LayoutCase;

impl LayoutCase {
    fn source(body: &str) -> String {
        memory::source(&format!(
            "match memory/int64 | +Err(_) => ! fail | +Ok(plan) => \
             let (#L = Left, left) = memory/realize Int64 plan in \
             let (#L = Right, right) = memory/realize Int64 plan in {body} end"
        ))
    }

    fn record(body: &str) -> String {
        Self::source(&format!(
            "let run = match records/product Left Right Int64 Int64 left right \
             | +Err(_) => fail | +Ok(record) => \
             let (/L; /value; /states; /left = first; /right = second) = record in \
             {{ {body} }} end in ! run"
        ))
    }
}

#[test]
fn explicit_record_placement_checks_alignment_overlap_and_extent() {
    for (first, second, count, alignment, pattern) in [
        (0_i64, 8_i64, 16_i64, 8_i64, "+Ok(_)"),
        (8, 0, 16, 8, "+Ok(_)"),
        (0, 16, 32, 16, "+Ok(_)"),
        (-1, 8, 16, 8, "+Err(+NegativeSize())"),
        (0, 4, 16, 8, "+Err(+MisalignedField())"),
        (0, 0, 16, 8, "+Err(+OverlappingFields())"),
        (0, 8, 8, 8, "+Err(+InvalidExtent())"),
        (0, 8, 16, 3, "+Err(+InvalidAlignment())"),
        (0, i64::MAX, i64::MAX, 8, "+Err(+SizeOverflow())"),
    ] {
        let body = format!(
            "let code = match records/at Left Right Int64 Int64 left right \
             {first} {second} {count} {alignment} | {pattern} => 0 | _ => 1 end in ! exit code"
        );
        SourceCase::assert_accepted(SourceCase::run(&LayoutCase::source(&body)));
    }
}

#[test]
fn checked_static_multiplication_handles_overflow_without_runtime_primitives() {
    for (left, right, pattern) in [
        (0_i64, i64::MAX, "+Ok(0)".to_owned()),
        (i64::MAX, 0, "+Ok(0)".to_owned()),
        (23, 45, "+Ok(1035)".to_owned()),
        (i64::MAX, 1, format!("+Ok({})", i64::MAX)),
        (i64::MAX, 2, "+Err(+SizeOverflow())".to_owned()),
        (-1, 0, "+Err(+NegativeSize())".to_owned()),
    ] {
        let body = format!(
            "let code = match size/multiply {left} {right} | {pattern} => 0 | _ => 1 end in ! exit code"
        );
        SourceCase::assert_accepted(SourceCase::run(&memory::source(&body)));
    }
}

#[test]
fn partial_field_states_reject_uninitialized_reads_and_incomplete_finish() {
    for body in [
        "! value/allocate OS heap no { fn p => \
         ! first/read Uninit OS (states/empty p) { fn _ => ! exit 0 } }",
        "! value/allocate OS heap no { fn p => \
         let live = states/finish (states/empty p) in ! exit 0 }",
        "! value/allocate OS heap no { fn p => \
         ! first/init Uninit OS (states/empty p) 7 { fn partial => \
         ! first/init Uninit OS partial 8 { fn _ => ! exit 0 } } }",
        "! value/allocate OS heap no { fn p => \
         do child <- ! (fields/uninitialized L Right first/path p); ! exit 0 }",
    ] {
        SourceCase::assert_rejected(
            SourceCase::check(&LayoutCase::record(body)),
            TyckDiagnosticCode::TypeMismatch,
        );
    }
}

#[test]
fn fixed_views_and_geometry_cannot_be_unknown_runtime_arguments() {
    for body in [
        "let (/View) = views in \
         let use : Thk (View Ret Int64 Int64 -> Int64 -> Ret Int64) = \
         { fn view value => ! (view value) } in \
         do value <- ! use (val value => { ret value }) 1; ! exit 0",
        "let use : Thk (Int64 -> OS) = { fn count => \
         match arrays/make Left Int64 left count 8 \
         | +Err(_) => ! fail | +Ok(_) => ! exit 0 end } in ! use 4",
        "let use : Thk (Int64 -> OS) = { fn offset => \
         match records/at Left Right Int64 Int64 left right 0 offset 16 8 \
         | +Err(_) => ! fail | +Ok(_) => ! exit 0 end } in ! use 8",
    ] {
        SourceCase::assert_rejected(
            SourceCase::check(&LayoutCase::source(body)),
            TyckDiagnosticCode::StaticElimination,
        );
    }
}

#[test]
fn dynamic_views_and_array_geometry_accept_runtime_selection() {
    let body = r#"
let (/DynamicView; /Cps; /materialize; /identity) = views in
let choose : Thk (Int64 -> Ret (DynamicView Ret Int64 Int64)) = {
  fn select => ! int64/eq (Ret (DynamicView Ret Int64 Int64)) select 0
    { ret (materialize Ret Int64 Int64 (identity Int64)) }
    { ret { fn value => ! int64/add value 1 } }
} in
do view <- ! choose 1;
do answer <- ! view 7;
! int64/eq OS answer 8 {
  let make : Thk (Int64 -> OS) = { fn count =>
    ! arrays/realize Left Int64 OS left count 16 no { fn array =>
      let (/L; /contents; /elements; /buffer) = array in
      ! contents/allocate OS heap no { fn p =>
        ! elements/init_each OS p { fn _ slot _ yes => ! left/unsafe/init OS slot 9 yes }
          { fn _ _ => ! fail } { fn initialized =>
            ! contents/unsafe/take OS initialized { fn vacant _ =>
              ! contents/unsafe/free OS heap vacant no { ! exit 0 }
            }
          }
      }
    }
  } in ! make 3
} fail
"#;
    SourceCase::assert_accepted(SourceCase::run(&LayoutCase::source(body)));
}

#[test]
fn dynamic_array_rejection_precedes_allocation() {
    for (count, alignment, fault) in [
        (-1_i64, 8_i64, "+InvalidLayout()"),
        (1, 3, "+InvalidLayout()"),
        (i64::MAX, 8, "+Overflow()"),
    ] {
        let body = format!(
            "! arrays/realize Left Int64 OS left {count} {alignment} \
             {{ fn fault => match fault | {fault} => ! exit 0 | _ => ! fail end }} \
             {{ fn _ => ! fail }}"
        );
        SourceCase::assert_accepted(SourceCase::run(&LayoutCase::source(&body)));
    }
}

#[test]
fn dynamic_fields_retain_only_explicit_offsets_and_preserve_pointer_types() {
    let body = r#"
let retained = fields/materialize L Right second/path in
let use : Thk (DynamicField L Right -> Ptr L Init -> Thk (Int64 -> OS) -> OS) = {
  fn path live yes =>
    do member <- ! runtime_fields/initialized L Right path live;
    ! right/unsafe/read OS member yes
} in
! value/allocate OS heap no { fn p =>
  ! value/unsafe/init OS p (11, 22) { fn live =>
    -- The read occurs before releasing the allocation.
    do path_offset <- ! runtime_fields/offset L Right retained;
    ! int64/eq OS path_offset 8 {
      ! use retained live { fn read =>
        ! int64/eq OS read 22 {
          ! value/unsafe/take OS live { fn p _ => ! value/unsafe/free OS heap p no { ! exit 0 } }
        } fail
      }
    } fail
  }
}
"#;
    SourceCase::assert_accepted(SourceCase::run(&LayoutCase::record(body)));
    SourceCase::assert_rejected(
        SourceCase::check(&LayoutCase::record(
            "let retained = fields/materialize L Right second/path in \
             ! value/allocate OS heap no { fn p => \
             do member <- ! runtime_fields/initialized L Right retained p; ! exit 0 }",
        )),
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn zero_sized_arrays_and_empty_builders_do_not_access_storage() {
    let body = r#"
match memory/unit
| +Err(_) => ! fail
| +Ok(plan) =>
  let (#L = Element, element) = memory/realize Unit plan in
  ! arrays/realize Element Unit OS element 3 16 no { fn array =>
    let (/L; /contents; /elements; /buffer) = array in
    ! contents/allocate OS heap no { fn p =>
      ! elements/init_each OS p { fn _ slot _ yes => ! element/unsafe/init OS slot () yes }
        { fn _ _ => ! fail } { fn live =>
          ! elements/at Init OS live 2 no { fn last =>
            ! element/unsafe/read OS last { fn _ =>
              ! contents/unsafe/take OS live { fn p _ => ! contents/unsafe/free OS heap p no { ! exit 0 } }
            }
          }
        }
    }
  }
end
"#;
    SourceCase::assert_accepted(SourceCase::run(&memory::source(body)));
}

#[test]
fn builder_capacity_failures_preserve_the_prefix_and_storage() {
    let body = r#"
! arrays/realize Left Int64 OS left 1 8 no { fn array =>
  let (/L; /contents; /buffer) = array in
  ! contents/allocate OS heap no { fn p =>
    do empty <- ! buffer/start p;
    ! buffer/finish OS empty { fn fault =>
      match fault | +Bounds() =>
        ! buffer/push OS empty 17 no { fn full =>
          ! buffer/push OS full 99 { fn fault =>
            match fault | +Bounds() =>
              do count <- ! buffer/length full;
              ! int64/eq OS count 1 {
                ! buffer/pop OS full no { fn empty value =>
                  ! int64/eq OS value 17 {
                    ! buffer/free OS heap empty no { ! exit 0 }
                  } fail
                }
              } fail
            | _ => ! fail end
          } { fn _ => ! fail }
        }
      | _ => ! fail end
    } { fn _ => ! fail }
  }
}
"#;
    SourceCase::assert_accepted(SourceCase::run(&LayoutCase::source(body)));
}

#[test]
fn checked_view_composition_preserves_failure_and_skips_the_successor() {
    let body = r#"
let (/View; /Checked; /Cps; /as_cps; /as_checked; /identity; /compose_checked; /map_checked) = views in
let first : View (Checked Fault) Int64 Int64 = val value => {
  fn R no yes => ! int64/lt R value 0 { ! no +Bounds() } { ! yes value }
} in
! raw/allocate OS 8 8 no { fn marker =>
! int64/store_le OS marker 0 {
let second : View (Checked Fault) Int64 Int64 = val value => {
  fn R _ yes => ! int64/store_le R marker 1 { ! yes value }
} in
let bad = compose_checked Fault Int64 Int64 Int64 first second in
! (bad -1) OS { fn fault =>
  match fault | +Bounds() =>
    let pure = as_checked Fault Int64 Int64 (as_cps Int64 Int64 (identity Int64)) in
    let mapped = map_checked Fault Int64 Int64 (Int64 * Int64) pure (val value => (value, value)) in
    ! (mapped 7) OS no { fn (a, b) =>
      ! int64/eq OS a b {
        ! int64/load_le OS marker { fn observed =>
          ! int64/eq OS observed 0 {
            ! raw/unsafe/free OS marker 8 8 no { ! exit 0 }
          } fail
        }
      } fail
    }
  | _ => ! fail end
} { fn _ => ! fail }
}}
"#;
    SourceCase::assert_accepted(SourceCase::run(&memory::source(body)));
}

#[test]
fn header_metadata_can_be_opened_before_payload_initialization() {
    let body = r#"
let forms = headers/from_fields L Left Right Int64 left first/path second/path in
let (/H; /unsafe = view) = forms/inline in
! value/allocate OS heap no { fn vacant =>
  ! first/init Uninit OS (states/empty vacant) 1 { fn partial =>
    let base = pointer/unsafe/address L (Fields Init Uninit) partial in
    ! (view/open Uninit (view/from_address Uninit base)) OS { fn (destination, count) =>
      ! int64/eq OS count 1 {
        ! right/unsafe/init OS destination 7 { fn initialized =>
          let completed = second/replace Uninit Init Init partial initialized in
          ! value/unsafe/take OS (states/finish completed) { fn vacant (_, item) =>
            ! int64/eq OS item 7 {
              ! value/unsafe/free OS heap vacant no { ! exit 0 }
            } fail
          }
        }
      } fail
    }
  }
}
"#;
    SourceCase::assert_accepted(SourceCase::run(&LayoutCase::record(body)));
    SourceCase::assert_rejected(
        SourceCase::check(&LayoutCase::record(
            "let forms = headers/from_fields L Left Right Int64 left first/path second/path in \
             let (/H; /unsafe = view) = forms/inline in \
             let read : Thk (H Uninit -> OS) = { fn handle => \
             ! (view/open Init handle) OS { fn _ => ! exit 0 } } in ! exit 0",
        )),
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn nested_field_paths_and_partial_updates_preserve_sibling_states() {
    let body = r#"
let next = match records/product Left L Int64 (Int64 * Int64) left value
| +Err(_) => fail
| +Ok(outer) =>
  let (/L = Outer; /value = outer_value; /states = outer_states; /left = head; /right = tail) = outer in
  let path = fields/compose Outer L Right tail/path second/path in
  { ! outer_value/allocate OS heap no { fn p =>
    let building = outer_states/empty p in
    do child <- ! (tail/project Uninit Uninit building);
    ! first/init Uninit OS (states/empty child) 11 { fn partial =>
      ! second/init Init OS partial 22 { fn complete =>
        let child = states/finish complete in
        let building = tail/replace Uninit Uninit Init building child in
        ! head/init Init OS building 7 { fn complete =>
          let live = outer_states/finish complete in
          do member <- ! (fields/initialized Outer Right path live);
          ! right/unsafe/read OS member { fn observed =>
            ! int64/eq OS observed 22 {
              ! outer_value/unsafe/take OS live { fn p _ =>
                ! outer_value/unsafe/free OS heap p no { ! exit 0 }
              }
            } fail
          }
        }
      }
    }
  } }
end in ! next
"#;
    SourceCase::assert_accepted(SourceCase::run(&LayoutCase::record(body)));
}
