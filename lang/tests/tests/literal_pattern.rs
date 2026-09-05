use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

#[test]
fn checks_integer_literal_arms() {
    SourceCase::assert_accepted(SourceCase::check(
        r#"
begin
  let n : Int64 = 3 that
  match n
  | 0 => ret ()
  | -7 => ret ()
  | _ => ret ()
  end
end
"#,
    ));
}

#[test]
fn lowers_integer_literal_arms_through_the_compiled_pipeline() {
    SourceCase::assert_accepted(SourceCase::lower(
        r#"
begin
  let n : Int64 = 3 that
  match n
  | 0 => ! exit 1
  | 42 => ! exit 0
  | _ => ! exit 2
  end
end
"#,
    ));
}

#[test]
fn accepts_literals_nested_in_refutable_patterns() {
    SourceCase::assert_accepted(SourceCase::check(
        r#"
begin
  let Tagged =
    data
    | +Tagged : Int64
    | +Other : Unit
    end
  that
  let value : Tagged = +Tagged(0) that
  match value
  | +Tagged(0) => ret ()
  | +Tagged(_) => ret ()
  | +Other(_) => ret ()
  end
end
"#,
    ));
}

#[test]
fn rejects_a_float_literal_pattern_on_a_float_type() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  let x : Float64 = 1.5 that
  match x
  | 1.5 => ret ()
  | _ => ret ()
  end
end
"#,
        ),
        TyckDiagnosticCode::TypeExpected,
    );
}

#[test]
fn rejects_a_float_literal_pattern_on_an_integer_type() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  let n : Int64 = 0 that
  match n
  | 1.5 => ret ()
  | _ => ret ()
  end
end
"#,
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn rejects_a_literal_pattern_on_a_non_primitive_type() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  match ()
  | 0 => ret ()
  | _ => ret ()
  end
end
"#,
        ),
        TyckDiagnosticCode::TypeExpected,
    );
}

#[test]
fn rejects_a_literal_outside_the_scrutinee_range() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  let n : Int8 = 0 that
  match n
  | 300 => ret ()
  | _ => ret ()
  end
end
"#,
        ),
        TyckDiagnosticCode::IntegerLiteralOutOfRange,
    );
}

#[test]
fn rejects_a_literal_only_match_as_non_exhaustive() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  let n : Int64 = 0 that
  match n
  | 0 => ret ()
  | 1 => ret ()
  end
end
"#,
        ),
        TyckDiagnosticCode::Coverage,
    );
}

#[test]
fn rejects_a_literal_binder_on_a_value_function_parameter() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  let val zero (0 : Int64) : Int64 = 0 that
  ret (0 |> zero)
end
"#,
        ),
        TyckDiagnosticCode::Expressivity,
    );
}
