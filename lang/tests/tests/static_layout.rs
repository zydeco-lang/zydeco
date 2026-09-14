#[path = "support/memory.rs"]
mod memory;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::{e2e_sources, utils::SourceCase};

e2e_sources!({
    static_layout => "tests/std/static-layout.zy",
});

struct LayoutCase;

impl LayoutCase {
    fn source(body: &str) -> String {
        memory::source(body)
    }

    fn accepts(expression: &str, pattern: &str) {
        let source = Self::source(&format!(
            "let code = match {expression} | {pattern} => 0 | _ => 1 end in ! exit code"
        ));
        SourceCase::assert_accepted(SourceCase::check_linted(&source));
        SourceCase::assert_accepted(SourceCase::run(&source));
    }
}

#[test]
fn size_arithmetic_checks_signed_bounds_before_wrapping_operations() {
    for (expression, pattern) in [
        ("size/size 0", "+Ok(0)"),
        ("size/size -1", "+Err(+NegativeSize())"),
        ("size/add 9223372036854775807 0", "+Ok(9223372036854775807)"),
        ("size/add 9223372036854775807 1", "+Err(+SizeOverflow())"),
        ("size/add 0 -1", "+Err(+NegativeSize())"),
        ("size/round_up 1 16", "+Ok(16)"),
        ("size/round_up 16 16", "+Ok(16)"),
        ("size/round_up 0 4611686018427387904", "+Ok(0)"),
        ("size/round_up 9223372036854775807 1", "+Ok(9223372036854775807)"),
        ("size/round_up 9223372036854775792 16", "+Ok(9223372036854775792)"),
        ("size/round_up 9223372036854775793 16", "+Err(+SizeOverflow())"),
        ("size/round_up -1 16", "+Err(+NegativeSize())"),
    ] {
        LayoutCase::accepts(expression, pattern);
    }
}

#[test]
fn layouts_propagate_typed_validation_errors_without_realizing_storage() {
    for boundary in [-9223372036854775808_i64, -1, 0, 3, 9223372036854775807] {
        LayoutCase::accepts(
            &format!("memory/align UInt8 {boundary} memory/uint8"),
            "+Err(+InvalidAlignment())",
        );
    }
    for (expression, pattern) in [
        ("memory/padding -1", "+Err(+NegativeSize())"),
        ("memory/product Unit UInt8 (memory/padding -1) memory/uint8", "+Err(+NegativeSize())"),
        ("memory/product UInt8 Unit memory/uint8 (memory/padding -1)", "+Err(+NegativeSize())"),
        (
            "memory/product Unit Unit (memory/padding 9223372036854775807) (memory/padding 1)",
            "+Err(+SizeOverflow())",
        ),
        ("memory/align Unit 16 (memory/padding 9223372036854775807)", "+Err(+SizeOverflow())"),
        ("memory/padding 9223372036854775807", "+Ok(_)"),
        ("memory/align Unit 4611686018427387904 memory/unit", "+Ok(_)"),
    ] {
        LayoutCase::accepts(expression, pattern);
    }
}

#[test]
fn static_construction_rejects_runtime_sizes_with_a_source_diagnostic() {
    SourceCase::assert_rejected(
        SourceCase::check(&LayoutCase::source(
            r#"
let build : Thk (Int64 -> Ret (Layout Unit)) = {
  fn count => ret (memory/padding count)
} in
! exit 0
"#,
        )),
        TyckDiagnosticCode::StaticElimination,
    );
}

#[test]
fn callers_cannot_forge_a_successful_plan_or_change_its_logical_type() {
    for body in [
        "let forged : Layout UInt8 = +Ok(()) in ! exit 0",
        "match memory/uint8 | +Err(_) => ! exit 1 | +Ok(plan) => \
         let repr = memory/realize UInt32 plan in ! exit 0 end",
    ] {
        SourceCase::assert_rejected(
            SourceCase::check(&LayoutCase::source(body)),
            TyckDiagnosticCode::TypeMismatch,
        );
    }
}

#[test]
fn equivalent_alignment_compositions_keep_offsets_and_normalize_metadata() {
    LayoutCase::accepts(
        r#"
match memory/align (UInt8 * UInt32) 8
  (memory/align (UInt8 * UInt32) 16
    (memory/product UInt8 UInt32 memory/uint8 memory/uint32))
| +Err(_) => (-1, -1, -1)
| +Ok(plan) =>
  let shape = memory/inspect (UInt8 * UInt32) plan in
  match shape/form
  | +Product(fields) => (shape/size, shape/alignment, fields/offset)
  | _ => (-1, -1, -1)
  end
end
"#,
        "(16, 16, 4)",
    );
}

#[test]
fn scalar_width_survives_explicit_tail_padding() {
    LayoutCase::accepts(
        r#"
match memory/align UInt8 16 memory/uint8
| +Err(_) => (-1, -1, -1)
| +Ok(plan) =>
  let shape = memory/inspect UInt8 plan in
  match shape/form
  | +Scalar(leaf) => (shape/size, shape/alignment, leaf/width)
  | _ => (-1, -1, -1)
  end
end
"#,
        "(16, 16, 1)",
    );
}

#[test]
fn runtime_plans_cannot_drive_fixed_realization_or_placement() {
    SourceCase::assert_rejected(
        SourceCase::check(&LayoutCase::source(
            "let use : Thk (Plan UInt8 -> OS) = { fn plan => \
             let (= L, repr) = memory/realize UInt8 plan in ! exit 0 } in ! exit 0",
        )),
        TyckDiagnosticCode::StaticElimination,
    );
    SourceCase::assert_rejected(
        SourceCase::check(&LayoutCase::source(
            r#"
let change : Thk (Plan UInt8 -> Ret (Layout UInt8)) = { fn plan =>
  ret (memory/align UInt8 16 (+Ok(plan) : Layout UInt8))
} in ! exit 0
"#,
        )),
        TyckDiagnosticCode::StaticElimination,
    );
}
