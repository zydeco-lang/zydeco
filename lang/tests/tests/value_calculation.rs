//! Source cases live in `cases/value-calculation/`; Rust retains generated matrices and cross-stage checks.

use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

#[test]
fn value_matches_and_arithmetic_reduce_through_higher_order_application() {
    let source = include_str!(
        "../cases/value-calculation/value_matches_and_arithmetic_reduce_through_higher_order_application.zy"
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(source));
    SourceCase::assert_accepted(SourceCase::run_monadic(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn runtime_data_cannot_supply_static_choices_or_integer_operands() {
    for body in ["let add = @(intrinsic(int_add)) in add x 1", "match x | 0 => 1 | _ => 2 end"] {
        SourceCase::assert_rejected(
            SourceCase::check(&format!(
                "let f : Thk (Int -> Ret Int) = {{ fn x => ret ({body}) }} in ! exit 0"
            )),
            TyckDiagnosticCode::StaticElimination,
        );
    }
}

#[test]
fn value_integer_leaves_have_total_wrapping_and_signed_comparison_semantics() {
    for (expression, expected) in [
        ("add 4611686018427387903 1", "-4611686018427387904"),
        ("sub -4611686018427387904 1", "4611686018427387903"),
        ("bit_and 31 -16", "16"),
        ("compare -4611686018427387904 4611686018427387903", "-1"),
    ] {
        SourceCase::assert_accepted(SourceCase::run(&format!(
            "let add = @(intrinsic(int_add)) in let sub = @(intrinsic(int_sub)) in \
             let bit_and = @(intrinsic(int_and)) in let compare = @(intrinsic(int_compare)) in \
             let result = match {expression} | {expected} => 0 | _ => 1 end in ! exit result"
        )));
    }
}

#[test]
fn known_choices_can_forward_runtime_payloads_through_views_and_nested_patterns() {
    let source = include_str!(
        "../cases/value-calculation/known_choices_can_forward_runtime_payloads_through_views_and_nested_patterns.zy"
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn value_matching_does_not_execute_suspended_effects() {
    SourceCase::assert_accepted(SourceCase::run_monadic(include_str!(
        "../cases/value-calculation/value_matching_does_not_execute_suspended_effects.zy"
    )));
}
