use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

#[test]
fn value_matches_and_arithmetic_reduce_through_higher_order_application() {
    let source = r#"
let add = @(intrinsic(i64_add)) in
let compare = @(intrinsic(i64_compare)) in
let sub = @(intrinsic(i64_sub)) in
let val maximum (a : Int64) (b : Int64) : Int64 =
  match compare a b | -1 => b | _ => a end
in
let val apply (f : val pi (x : Int64) . Int64) (x : Int64) : Int64 = f x in
let answer = apply (maximum 16) (add 5 11) in
! exit (sub answer 16)
"#;
    SourceCase::assert_accepted(SourceCase::check_linted(source));
    SourceCase::assert_accepted(SourceCase::run(source));
    SourceCase::assert_accepted(SourceCase::run_monadic(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn value_matches_require_exhaustive_patterns() {
    SourceCase::assert_rejected(
        SourceCase::check("let selected = match (0 : Int64) | 0 => 1 end in ! exit 0"),
        TyckDiagnosticCode::Coverage,
    );
}

#[test]
fn runtime_data_cannot_supply_static_choices_or_integer_operands() {
    for body in ["let add = @(intrinsic(i64_add)) in add x 1", "match x | 0 => 1 | _ => 2 end"] {
        SourceCase::assert_rejected(
            SourceCase::check(&format!(
                "let f : Thk (Int64 -> Ret Int64) = {{ fn x => ret ({body}) }} in ! exit 0"
            )),
            TyckDiagnosticCode::StaticElimination,
        );
    }
}

#[test]
fn value_integer_leaves_have_total_wrapping_and_signed_comparison_semantics() {
    for (expression, expected) in [
        ("add 9223372036854775807 1", "-9223372036854775808"),
        ("sub -9223372036854775808 1", "9223372036854775807"),
        ("bit_and 31 -16", "16"),
        ("compare -9223372036854775808 9223372036854775807", "-1"),
    ] {
        SourceCase::assert_accepted(SourceCase::run(&format!(
            "let add = @(intrinsic(i64_add)) in let sub = @(intrinsic(i64_sub)) in \
             let bit_and = @(intrinsic(i64_and)) in let compare = @(intrinsic(i64_compare)) in \
             let result = match {expression} | {expected} => 0 | _ => 1 end in ! exit result"
        )));
    }
}

#[test]
fn known_choices_can_forward_runtime_payloads_through_views_and_nested_patterns() {
    let source = r#"
let Choice = data | +Left : Int64 * Int64 | +Right : Unit end in
let val first ((left, _) : Int64 * Int64) : Int64 = left in
let f : Thk (Int64 -> Ret Int64) = { fn x =>
  let selected = match (+Left(x, 3) : Choice)
    | +Right() => 1
    | +Left(first ~> result) => result
    end
  in ret selected
} in
do result <- ! f 0;
! exit result
"#;
    SourceCase::assert_accepted(SourceCase::check_linted(source));
    SourceCase::assert_accepted(SourceCase::run(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn an_unknown_earlier_row_cannot_be_skipped_for_a_known_later_match() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
let f : Thk (Int64 -> Ret Int64) = { fn x =>
  ret (match (x, 0) | (0, _) => 1 | (_, 0) => 2 | _ => 3 end)
} in ! exit 0
"#,
        ),
        TyckDiagnosticCode::StaticElimination,
    );
}

#[test]
fn value_matching_does_not_execute_suspended_effects() {
    let source = r#"
let run = match (0 : Int64)
  | 0 => { ! exit 0 }
  | _ => { ! exit 1 }
  end
in ! run
"#;
    SourceCase::assert_accepted(SourceCase::run(source));
    SourceCase::assert_accepted(SourceCase::run_monadic(source));
    SourceCase::assert_rejected(
        SourceCase::check("let selected = match (0 : Int64) | 0 => 1 | _ => ret 2 end in ! exit 0"),
        TyckDiagnosticCode::TypeMismatch,
    );
}
