//! Source-defined view composition and phase boundaries over a deterministic provider.

use std::path::PathBuf;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::{SourceCase, SourceProgram, TestBackend};

struct ViewCase;

impl ViewCase {
    fn source(body: &str) -> String {
        let model = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/tests/ffi/views/model.zy")
            .canonicalize()
            .unwrap();
        format!(
            r#"
let (/Access; /Fault; /Memory; /Mem; /Cell; /Fat; /View; /views; /memory; /unavailable) =
  builtin |> (@(import("{}"))) in
let pointer = views/address 8 8 in
let fail = {{ ! exit 1 }} in
let no = {{ fn (_ : Fault) => ! fail }} in
{body}
"#,
            model.display(),
        )
    }

    fn runs(body: &str) {
        SourceCase::assert_accepted(SourceCase::run(&Self::source(body)));
    }
}

#[test]
fn source_view_interfaces_check_with_explicit_address_and_access_types() {
    SourceCase::assert_accepted(SourceCase::check_linted(&ViewCase::source(
        "let view : View Int64 Unit = views/thin in ! exit 0",
    )));
}

#[test]
fn view_carriers_and_runtime_metadata_types_must_match() {
    for body in [
        "let view : View (Fat Int64) Unit = views/thin in ! exit 0",
        "let handle : Fat Unit = views/with_runtime_metadata Int64 8 3 in ! exit 0",
        "let view : View Int64 Unit = views/indirect Int64 views/int64 -8 0 in ! exit 0",
    ] {
        SourceCase::assert_rejected(
            SourceCase::check(&ViewCase::source(body)),
            TyckDiagnosticCode::TypeMismatch,
        );
    }
}

#[test]
fn runtime_payloads_can_be_captured_and_projected_by_value_functions() {
    let source = ViewCase::source(
        r#"
let copy : Thk (Int64 -> Int64 -> Ret Int64) = {
  fn address length =>
    let handle = views/with_runtime_metadata Int64 address length in
    ret (views/runtime_metadata Int64 handle)
} in
do result <- ! copy 8 3;
! int64/eq OS result 3 { ! exit 0 } fail
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_linted(&source));
    SourceCase::assert_accepted(SourceCase::run(&source));
}

#[test]
fn fat_views_open_closure_metadata_without_a_storage_cell() {
    ViewCase::runs(
        r#"
let handle = views/with_runtime_metadata (Thk (Ret Int64)) 8 { ret 3 } in
let view = views/fat (Thk (Ret Int64)) in
! view/open OS +Live() handle { ! unavailable OS } no {
  fn (address, metadata) =>
    do length <- ! metadata;
    match (address, length) | (8, 3) => ! exit 0 | _ => ! fail end
}
"#,
    );
}

#[test]
fn runtime_sizes_cannot_drive_static_cell_placement() {
    SourceCase::assert_rejected(
        SourceCase::check(&ViewCase::source(
            r#"
let build : Thk (Int64 -> OS) = {
  fn width =>
    match views/product Int64 Int64 (views/address width 8) views/int64
    | +Err(_) => ! exit 1
    | +Ok(_) => ! exit 0
    end
} in
! build 8
"#,
        )),
        TyckDiagnosticCode::StaticElimination,
    );
}

#[test]
fn reading_a_header_does_not_make_its_runtime_length_static() {
    SourceCase::assert_rejected(
        SourceCase::check(&ViewCase::source(
            r#"
let prefix = views/indirect Int64 views/int64 -8 0 in
! prefix/open OS +Live() 8 { ! memory OS } no {
  fn (_, length) =>
    match views/product Int64 Int64 (views/address length 8) views/int64
    | +Err(_) => ! exit 1
    | +Ok(_) => ! exit 0
    end
}
"#,
        )),
        TyckDiagnosticCode::StaticElimination,
    );
}

#[test]
fn runtime_offsets_remain_captured_in_view_computations() {
    let source = ViewCase::source(
        r#"
let read : Thk (Int64 -> OS) = {
  fn displacement =>
    let view = views/indirect Int64 views/int64 displacement 0 in
    ! view/open OS +Live() 8 { ! memory OS } no {
      fn (address, length) =>
        match (address, length) | (8, 3) => ! exit 0 | _ => ! fail end
    }
} in
! read -8
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_linted(&source));
    SourceCase::assert_accepted(SourceCase::run(&source));
}

#[test]
fn carrier_layouts_allow_optional_and_arbitrary_runtime_metadata() {
    ViewCase::runs(
        r#"
match views/fat_cell Int64 pointer views/int64
| +Err(_) => ! fail
| +Ok(length_cell) =>
  match views/product Int64 Int64 views/int64 views/int64
  | +Err(_) => ! fail
  | +Ok(pair_cell) =>
    match views/fat_cell (Int64 * Int64) pointer pair_cell
    | +Err(_) => ! fail
    | +Ok(pair_fat) =>
      let sizes = (pointer/size, length_cell/size, pair_fat/size) in
      match sizes | (8, 16, 24) => ! exit 0 | _ => ! fail end
    end
  end
end
"#,
    );
}

#[test]
fn cell_composition_rejects_invalid_layouts_and_reads_typed_fields() {
    for (width, alignment, fault) in [
        (-1_i64, 8, "+NegativeSize()"),
        (8, 3, "+InvalidAlignment()"),
        (i64::MAX, 8, "+SizeOverflow()"),
    ] {
        ViewCase::runs(&format!(
            "match views/product Int64 Int64 (views/address {width} {alignment}) views/int64 \
             | +Err({fault}) => ! exit 0 | _ => ! fail end"
        ));
    }
    ViewCase::runs(
        r#"
match views/fat_cell Int64 pointer views/int64
| +Err(_) => ! fail
| +Ok(cell) =>
  ! cell/read OS +Live() 16 { ! memory OS } no {
    fn handle =>
      match (handle/address, handle/runtime_metadata)
      | (24, 7) => ! exit 0
      | _ => ! fail
      end
  }
end
"#,
    );
}

#[test]
fn thin_and_fat_open_do_not_access_memory() {
    ViewCase::runs(
        r#"
let thin = views/thin in
! thin/open OS +Closed() 8 { ! unavailable OS } no {
  fn (address, ()) =>
    ! int64/eq OS address 8 {
      match views/fat_cell Int64 pointer views/int64
      | +Err(_) => ! fail
      | +Ok(carrier) =>
        let fat = views/fat Int64 in
        ! fat/open OS +Closed() (views/with_runtime_metadata Int64 address 3)
          { ! unavailable OS } no {
          fn (address, length) =>
            match (address, length) | (8, 3) => ! exit 0 | _ => ! fail end
        }
      end
    } fail
}
"#,
    );
}

#[test]
fn indirect_views_report_provider_faults_before_exposing_a_result() {
    for (origin, access, delta, provider, fault) in [
        (8_i64, "+Closed()", -8_i64, "memory", "+Closed()"),
        (8, "+WriteOnly()", -8, "memory", "+Permission()"),
        (0, "+Live()", -8, "memory", "+Bounds()"),
        (8, "+Live()", i64::MAX, "memory", "+Bounds()"),
        (8, "+Live()", -8, "unavailable", "+Unavailable()"),
    ] {
        ViewCase::runs(&format!(
            "let view = views/indirect Int64 views/int64 {delta} 0 in \
             ! view/open OS {access} {origin} {{ ! {provider} OS }} \
             {{ fn fault => match fault | {fault} => ! exit 0 | _ => ! fail end }} \
             {{ fn _ => ! fail }}"
        ));
    }
}

#[test]
fn runtime_selected_prefix_inline_and_object_views_execute_on_every_backend() {
    let source = ViewCase::source(
        r#"
let prefix = views/indirect Int64 views/int64 -8 0 in
let inline = views/indirect Int64 views/int64 0 8 in
let object = views/indirect Int64 pointer 0 0 in
let Opened = Int64 * Int64 in
let open : Thk (Int64 -> Ret Opened) = {
  fn choice =>
    let accept : Thk (View Int64 Int64 -> Int64 -> Ret Opened) = { fn view origin =>
      ! view/open (Ret Opened) +Live() origin
        { ! memory (Ret Opened) } { fn _ => ret (-1, -1) } { fn value => ret value }
    } in
    match choice
    | 0 => ! accept prefix 8
    | _ => ! accept inline 0
    end
} in
do prefixed <- ! open 0;
do inlined <- ! open 1;
match (prefixed, inlined)
| ((8, 3), (8, 3)) =>
  ! object/open OS +Live() 16 { ! memory OS } no {
    fn (origin, vtable) =>
      match (origin, vtable) | (16, 24) => ! exit 0 | _ => ! fail end
  }
| _ => ! fail
end
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_linted(&source));
    let directory = tempfile::tempdir().unwrap();
    let program = directory.path().join("views.zy");
    let builtin = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../lib/std/builtin.zy")
        .canonicalize()
        .unwrap();
    std::fs::write(
        &program,
        format!(
            "param (/VType; /CType; /Thk; /Ret; /Unit; /Int64; /OS; /numeric; /process; builtin) \
             : @(import(\"{}\")) in \
             let int64 = numeric/int64 in let exit = process/exit in {source}",
            builtin.display()
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
fn padding_alignment_and_full_footprint_checks_precede_field_reads() {
    for (expression, expected) in [
        ("views/padding -1", "+Err(+NegativeSize())"),
        ("views/align Int64 3 views/int64", "+Err(+InvalidAlignment())"),
        ("views/align Int64 0 views/int64", "+Err(+InvalidAlignment())"),
    ] {
        ViewCase::runs(&format!("match {expression} | {expected} => ! exit 0 | _ => ! fail end"));
    }
    // The model only implements loads at 0, 8, and 24. At origin 4 a child load
    // would report Bounds, so Alignment demonstrates that the outer check ran first.
    for (boundary, origin, fault) in [(16, 4, "+Alignment()"), (64, 0, "+Bounds()")] {
        ViewCase::runs(&format!(
            "match views/align Int64 {boundary} views/int64 \
             | +Err(_) => ! fail | +Ok(cell) => \
             ! cell/read OS +Live() {origin} {{ ! memory OS }} \
             {{ fn fault => match fault | {fault} => ! exit 0 | _ => ! fail end }} \
             {{ fn _ => ! fail }} end"
        ));
    }
    ViewCase::runs(
        "match views/padding 32 | +Err(_) => ! fail | +Ok(cell) => \
         ! cell/read OS +Live() 0 { ! memory OS } no { fn () => ! exit 0 } end",
    );
    ViewCase::runs(
        "match views/align Int64 16 views/int64 | +Err(_) => ! fail | +Ok(cell) => \
         match (cell/size, cell/alignment) | (16, 16) => \
         ! cell/read OS +Live() 0 { ! memory OS } no { fn value => \
         ! int64/eq OS value 3 { ! exit 0 } fail } | _ => ! fail end end",
    );
}

#[test]
fn typed_slice_indexing_checks_lengths_indices_stride_overflow_and_extent() {
    ViewCase::runs(
        "let view = views/indirect Int64 views/int64 0 8 in \
         ! views/index Int64 Int64 OS view views/int64 +Live() 0 0 \
         { ! memory OS } no { fn value => ! int64/eq OS value 5 { ! exit 0 } fail }",
    );
    for (length, index, stride, fault) in [
        (3_i64, -1_i64, 8_i64, "+Bounds()"),
        (3, 3, 8, "+Bounds()"),
        (-1, 0, 8, "+InvalidValue()"),
        (0, 0, 8, "+Bounds()"),
        (i64::MAX, i64::MAX - 1, 8, "+Overflow()"),
        (3, 1, -1, "+InvalidValue()"),
        (8, 5, 8, "+Bounds()"),
    ] {
        ViewCase::runs(&format!(
            "match views/fat_cell Int64 pointer views/int64 \
             | +Err(_) => ! fail | +Ok(carrier) => \
             let view = views/fat Int64 in \
             ! views/index (Fat Int64) Int64 OS view (views/address {stride} 8) \
               +Live() (views/with_runtime_metadata Int64 0 {length}) {index} \
               {{ ! memory OS }} \
               {{ fn fault => match fault | {fault} => ! exit 0 | _ => ! fail end }} \
               {{ fn _ => ! fail }} end"
        ));
    }
}

#[test]
fn pointer_and_slice_factories_bind_an_abstract_handle_to_its_element_operations() {
    ViewCase::runs(
        "let (= Ptr, pointers) = views/pointer Int64 pointer views/int64 in \
         let pointer : Ptr = pointers/from_address 0 in \
         ! pointers/get OS +Live() pointer { ! memory OS } no { fn value => \
           ! int64/eq OS value 3 { \
             let view = views/indirect Int64 views/int64 0 8 in \
             let (= Slice, slices) = views/slice Int64 Int64 (views/address 8 8) view views/int64 in \
             let slice : Slice = slices/from_handle 0 in \
             ! slices/get OS +Live() slice 0 { ! memory OS } no { fn value => \
               ! int64/eq OS value 5 { ! exit 0 } fail } \
           } fail }",
    );
    SourceCase::assert_rejected(
        SourceCase::check(&ViewCase::source(
            "let (= Ptr, pointers) = views/pointer Int64 pointer views/int64 in \
             let forged : Ptr = 0 in ! exit 0",
        )),
        TyckDiagnosticCode::TypeMismatch,
    );
}
