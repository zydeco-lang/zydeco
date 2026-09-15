#[path = "support/memory.rs"]
mod memory;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::{e2e_sources, utils::SourceCase};

e2e_sources!({
    general_views => "tests/std/general-views.zy",
    array_memory => "tests/std/array-memory.zy",
    header_array => "tests/std/header-array.zy",
    runtime_memory => "tests/std/runtime-memory.zy",
    storage_arrays => "tests/std/storage-arrays.zy",
    typed_memory_kernel => "tests/std/typed-memory-kernel.zy",
});

struct LayoutCase;

impl LayoutCase {
    fn source(body: &str) -> String {
        memory::source(&format!(
            "match memory/int | +Err(_) => ! fail | +Ok(plan) => \
             let (#L = Left, left) = memory/realize Int plan in \
             let (#L = Right, right) = memory/realize Int plan in {body} end"
        ))
    }

    fn record(body: &str) -> String {
        Self::source(&format!(
            "let run = match records/product Left Right left/storage right/storage \
             | +Err(_) => fail | +Ok(record) => \
             let (/L; /storage = object_storage; /states; /left = first; /right = second) = record in
             let value = (#storage = object_storage, #codec = codecs/product L Left Right Int Int first/path second/path left/codec right/codec) in \
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
        (
            0,
            zydeco_machine::word::RuntimeWord::SIGNED_MAX,
            zydeco_machine::word::RuntimeWord::SIGNED_MAX,
            8,
            "+Err(+SizeOverflow())",
        ),
    ] {
        let body = format!(
            "let code = match records/at Left Right left/storage right/storage {first} {second} {count} {alignment} | {pattern} => 0 | _ => 1 end in ! exit code"
        );
        SourceCase::assert_accepted(SourceCase::run(&LayoutCase::source(&body)));
    }
}

#[test]
fn checked_static_multiplication_handles_overflow_without_runtime_primitives() {
    for (left, right, pattern) in [
        (0_i64, zydeco_machine::word::RuntimeWord::SIGNED_MAX, "+Ok(0)".to_owned()),
        (zydeco_machine::word::RuntimeWord::SIGNED_MAX, 0, "+Ok(0)".to_owned()),
        (23, 45, "+Ok(1035)".to_owned()),
        (
            zydeco_machine::word::RuntimeWord::SIGNED_MAX,
            1,
            format!("+Ok({})", zydeco_machine::word::RuntimeWord::SIGNED_MAX),
        ),
        (zydeco_machine::word::RuntimeWord::SIGNED_MAX, 2, "+Err(+SizeOverflow())".to_owned()),
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
        "! (allocation/reserve L Unit allocation/static_heap () value/storage) OS no { fn p => \
         ! (first/read Int left/codec Uninit (states/empty p)) OS { fn _ => ! exit 0 } }",
        "! (allocation/reserve L Unit allocation/static_heap () value/storage) OS no { fn p => \
         let live = states/finish (states/empty p) in ! exit 0 }",
        "! (allocation/reserve L Unit allocation/static_heap () value/storage) OS no { fn p => \
         ! (first/init Int left/codec Uninit (states/empty p) 7) OS { fn partial => \
         ! (first/init Int left/codec Uninit partial 8) OS { fn _ => ! exit 0 } } }",
        "! (allocation/reserve L Unit allocation/static_heap () value/storage) OS no { fn p => \
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
         let use : Thk (View Ret Int Int -> Int -> Ret Int) = \
         { fn view value => ! (view value) } in \
         do value <- ! use (val value => { ret value }) 1; ! exit 0",
        "let use : Thk (Int -> OS) = { fn count => \
         match arrays/make Left left/storage count 8 \
         | +Err(_) => ! fail | +Ok(_) => ! exit 0 end } in ! use 4",
        "let use : Thk (Int -> OS) = { fn offset => \
         match records/at Left Right left/storage right/storage 0 offset 16 8 \
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
let choose : Thk (Int -> Ret (DynamicView Ret Int Int)) = {
  fn select => ! int/eq (Ret (DynamicView Ret Int Int)) select 0
    { ret (materialize Ret Int Int (identity Int)) }
    { ret { fn value => ! int/add value 1 } }
} in
do view <- ! choose 1;
do answer <- ! view 7;
! int/eq OS answer 8 {
  let make : Thk (Int -> OS) = { fn count =>
    ! arrays/realize Left OS (storage/constant/materialize Left left/storage) count 16 no { fn array =>
      let (/L; /storage = array_storage; /elements; /buffer; /Values = Sequence; /codecs = array_codecs) = array in
let Values = Sequence Int in
      let element_codec = codecs/materialize Left Int left/codec in
      do contents_codec <- ! array_codecs Int element_codec;
      let contents = (#storage = array_storage, #codec = contents_codec) in
      ! allocation/dynamic_reserve L OS heap contents/storage no { fn p =>
        ! elements/init_each OS p { fn _ slot _ yes => ! (left/codec/init slot 9) OS yes }
          { fn _ _ => ! fail } { fn initialized =>
            ! codecs/unsafe/dynamic_take L Values contents/codec initialized OS { fn vacant _ =>
              ! allocation/unsafe/dynamic_release L OS heap contents/storage vacant no { ! exit 0 }
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
        (zydeco_machine::word::RuntimeWord::SIGNED_MAX, 8, "+Overflow()"),
    ] {
        let body = format!(
            "! arrays/realize Left OS (storage/constant/materialize Left left/storage) {count} {alignment} \
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
let use : Thk (DynamicField L Right -> Ptr L Init -> Thk (Int -> OS) -> OS) = {
  fn path live yes =>
    do member <- ! runtime_fields/initialized L Right path live;
    ! (right/codec/read member) OS yes
} in
! (allocation/reserve L Unit allocation/static_heap () value/storage) OS no { fn p =>
  ! (value/codec/init p (11, 22)) OS { fn live =>
    -- The read occurs before releasing the allocation.
    do path_offset <- ! runtime_fields/offset L Right retained;
    ! int/eq OS path_offset 8 {
      ! use retained live { fn read =>
        ! int/eq OS read 22 {
          ! (codecs/unsafe/take L (Int * Int) value/codec live) OS { fn p _ => ! (allocation/unsafe/release L Unit allocation/static_heap () value/storage p) OS no { ! exit 0 } }
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
             ! (allocation/reserve L Unit allocation/static_heap () value/storage) OS no { fn p => \
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
  ! arrays/realize Element OS (storage/constant/materialize Element element/storage) 3 16 no { fn array =>
    let (/L; /storage = array_storage; /elements; /buffer; /Values = Sequence; /codecs = array_codecs) = array in
let Values = Sequence Unit in
      let element_codec = codecs/materialize Element Unit element/codec in
      do contents_codec <- ! array_codecs Unit element_codec;
      let contents = (#storage = array_storage, #codec = contents_codec) in
    ! allocation/dynamic_reserve L OS heap contents/storage no { fn p =>
      ! elements/init_each OS p { fn _ slot _ yes => ! (element/codec/init slot ()) OS yes }
        { fn _ _ => ! fail } { fn live =>
          ! elements/at Init OS live 2 no { fn last =>
            ! (element/codec/read last) OS { fn _ =>
              ! codecs/unsafe/dynamic_take L Values contents/codec live OS { fn p _ => ! allocation/unsafe/dynamic_release L OS heap contents/storage p no { ! exit 0 } }
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
! arrays/realize Left OS (storage/constant/materialize Left left/storage) 1 8 no { fn array =>
  let (/L; /storage = array_storage; /buffer; /Values = Sequence; /codecs = array_codecs) = array in
let Values = Sequence Int in
      let element_codec = codecs/materialize Left Int left/codec in
      do contents_codec <- ! array_codecs Int element_codec;
      let contents = (#storage = array_storage, #codec = contents_codec) in
  ! allocation/dynamic_reserve L OS heap contents/storage no { fn p =>
    do empty <- ! buffer/start p;
    ! buffer/finish OS empty { fn fault =>
      match fault | +Bounds() =>
        ! buffer/push Int OS element_codec empty 17 no { fn full =>
          ! buffer/push Int OS element_codec full 99 { fn fault =>
            match fault | +Bounds() =>
              do count <- ! buffer/length full;
              ! int/eq OS count 1 {
                ! buffer/pop Int OS element_codec full no { fn empty value =>
                  ! int/eq OS value 17 {
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
let first : View (Checked Fault) Int Int = val value => {
  fn R no yes => ! int/lt R value 0 { ! no +Bounds() } { ! yes value }
} in
! raw/allocate OS 8 8 no { fn marker =>
! int/store_le OS marker 0 {
let second : View (Checked Fault) Int Int = val value => {
  fn R _ yes => ! int/store_le R marker 1 { ! yes value }
} in
let bad = compose_checked Fault Int Int Int first second in
! (bad -1) OS { fn fault =>
  match fault | +Bounds() =>
    let pure = as_checked Fault Int Int (as_cps Int Int (identity Int)) in
    let mapped = map_checked Fault Int Int (Int * Int) pure (val value => (value, value)) in
    ! (mapped 7) OS no { fn (a, b) =>
      ! int/eq OS a b {
        ! int/load_le OS marker { fn observed =>
          ! int/eq OS observed 0 {
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
let forms = headers/from_fields L Left Right Int left/codec first/path second/path in
let (/H; /unsafe = view) = forms/inline in
! (allocation/reserve L Unit allocation/static_heap () value/storage) OS no { fn vacant =>
  ! (first/init Int left/codec Uninit (states/empty vacant) 1) OS { fn partial =>
    let base = pointer/unsafe/address L (Fields Init Uninit) partial in
    ! (view/open Uninit (view/from_address Uninit base)) OS { fn (destination, count) =>
      ! int/eq OS count 1 {
        ! (right/codec/init destination 7) OS { fn initialized =>
          let completed = second/replace Uninit Init Init partial initialized in
          ! (codecs/unsafe/take L (Int * Int) value/codec (states/finish completed)) OS { fn vacant (_, item) =>
            ! int/eq OS item 7 {
              ! (allocation/unsafe/release L Unit allocation/static_heap () value/storage vacant) OS no { ! exit 0 }
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
            "let forms = headers/from_fields L Left Right Int left/codec first/path second/path in \
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
let next = match records/product Left L left/storage value/storage
| +Err(_) => fail
| +Ok(outer) =>
  let (/L = Outer; /storage = outer_storage; /states = outer_states; /left = head; /right = tail) = outer in
  let outer_value = (#storage = outer_storage, #codec = codecs/product Outer Left L Int (Int * Int) head/path tail/path left/codec value/codec) in
  let path = fields/compose Outer L Right tail/path second/path in
  { ! (allocation/reserve Outer Unit allocation/static_heap () outer_value/storage) OS no { fn p =>
    let building = outer_states/empty p in
    do child <- ! (tail/project Uninit Uninit building);
    ! (first/init Int left/codec Uninit (states/empty child) 11) OS { fn partial =>
      ! (second/init Int right/codec Init partial 22) OS { fn complete =>
        let child = states/finish complete in
        let building = tail/replace Uninit Uninit Init building child in
        ! (head/init Int left/codec Init building 7) OS { fn complete =>
          let live = outer_states/finish complete in
          do member <- ! (fields/initialized Outer Right path live);
          ! (right/codec/read member) OS { fn observed =>
            ! int/eq OS observed 22 {
              ! (codecs/unsafe/take Outer (Int * (Int * Int)) outer_value/codec live) OS { fn p _ =>
                ! (allocation/unsafe/release Outer Unit allocation/static_heap () outer_value/storage p) OS no { ! exit 0 }
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

#[test]
fn storage_only_array_stride_is_checked_before_reservation() {
    for (count, alignment, capacity, expected) in [
        (8_i64, 64_i64, 2_i64, "+Ok(_)"),
        (0, 64, 0, "+Ok(_)"),
        (8, 64, -1, "+Err(+NegativeSize())"),
        (4611686018427387903, 8, 1, "+Err(+SizeOverflow())"),
    ] {
        let source = memory::source(&format!(
            r#"
match storage/constant/create {count} {alignment}
| +Err(_) => ! fail
| +Ok(#L = Element, element) =>
  match arrays/make Element element {capacity} 1
  | {expected} => ! exit 0 | _ => ! fail end
end
"#
        ));
        SourceCase::assert_accepted(SourceCase::check_linted(&source));
        SourceCase::assert_accepted(SourceCase::run(&source));
    }
}
