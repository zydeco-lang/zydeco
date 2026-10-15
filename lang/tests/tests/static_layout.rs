#[path = "support/memory.rs"]
mod memory;
use memory::MemoryCase;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::{e2e_sources, utils::SourceCase};

e2e_sources!({
    static_layout => "tests/std/static-layout.zy",
});

struct LayoutCase;

impl LayoutCase {
    fn source(body: &str) -> String {
        MemoryCase::source(body)
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
        ("size/size 0", "+Right(0)"),
        ("size/size -1", "+Left(+NegativeSize())"),
        ("size/add 4611686018427387903 0", "+Right(4611686018427387903)"),
        ("size/add 4611686018427387903 1", "+Left(+SizeOverflow())"),
        ("size/add 0 -1", "+Left(+NegativeSize())"),
        ("size/round_up 1 16", "+Right(16)"),
        ("size/round_up 16 16", "+Right(16)"),
        ("size/round_up 0 2305843009213693952", "+Right(0)"),
        ("size/round_up 4611686018427387903 1", "+Right(4611686018427387903)"),
        ("size/round_up 4611686018427387888 16", "+Right(4611686018427387888)"),
        ("size/round_up 4611686018427387889 16", "+Left(+SizeOverflow())"),
        ("size/round_up -1 16", "+Left(+NegativeSize())"),
    ] {
        LayoutCase::accepts(expression, pattern);
    }
}

#[test]
fn layouts_propagate_typed_validation_errors_without_realizing_storage() {
    for boundary in [-4611686018427387904_i64, -1, 0, 3, 4611686018427387903] {
        LayoutCase::accepts(
            &format!("memory/align UInt8 {boundary} memory/uint8"),
            "+Left(+InvalidAlignment())",
        );
    }
    for (expression, pattern) in [
        ("memory/padding -1", "+Left(+NegativeSize())"),
        ("memory/product Unit UInt8 (memory/padding -1) memory/uint8", "+Left(+NegativeSize())"),
        ("memory/product UInt8 Unit memory/uint8 (memory/padding -1)", "+Left(+NegativeSize())"),
        (
            "memory/product Unit Unit (memory/padding 4611686018427387903) (memory/padding 1)",
            "+Left(+SizeOverflow())",
        ),
        ("memory/align Unit 16 (memory/padding 4611686018427387903)", "+Left(+SizeOverflow())"),
        ("memory/padding 4611686018427387903", "+Right(_)"),
        ("memory/align Unit 2305843009213693952 memory/unit", "+Right(_)"),
    ] {
        LayoutCase::accepts(expression, pattern);
    }
}

#[test]
fn static_construction_rejects_runtime_sizes_with_a_source_diagnostic() {
    SourceCase::assert_rejected(
        SourceCase::check(&LayoutCase::source(
            r#"
let build : Thk (Int -> Ret (Layout Unit)) = {
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
        "let forged : Layout UInt8 = +Right(()) in ! exit 0",
        "match memory/uint8 | +Left(_) => ! exit 1 | +Right(plan) => \
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
| +Left(_) => (-1, -1, -1)
| +Right(plan) =>
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
| +Left(_) => (-1, -1, -1)
| +Right(plan) =>
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
  ret (memory/align UInt8 16 (+Right(plan) : Layout UInt8))
} in ! exit 0
"#,
        )),
        TyckDiagnosticCode::StaticElimination,
    );
}
