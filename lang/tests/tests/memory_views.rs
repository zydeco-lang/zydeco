//! The view proposal's source types and phase boundaries; no native pointer ABI is implied.

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
        "let view : View Int64 Unit = views/thin pointer in ! exit 0",
    )));
}

#[test]
fn view_carriers_and_runtime_metadata_types_must_match() {
    for body in [
        "let view : View (Fat Int64) Unit = views/thin pointer in ! exit 0",
        "let handle : Fat Unit = views/with_runtime_metadata Int64 8 3 in ! exit 0",
        "let view : View Int64 Unit = views/indirect Int64 pointer views/int64 -8 0 in ! exit 0",
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
let prefix = views/indirect Int64 pointer views/int64 -8 0 in
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
    let view = views/indirect Int64 pointer views/int64 displacement 0 in
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
let thin = views/thin pointer in
! thin/open OS +Closed() 8 { ! unavailable OS } no {
  fn (address, ()) =>
    ! int64/eq OS address 8 {
      match views/fat_cell Int64 pointer views/int64
      | +Err(_) => ! fail
      | +Ok(carrier) =>
        let fat = views/fat Int64 carrier in
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
            "let view = views/indirect Int64 pointer views/int64 {delta} 0 in \
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
let prefix = views/indirect Int64 pointer views/int64 -8 0 in
let inline = views/indirect Int64 pointer views/int64 0 8 in
let object = views/indirect Int64 pointer pointer 0 0 in
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
