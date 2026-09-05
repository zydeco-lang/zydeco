use zydeco_tests::utils::SourceCase;

struct LiteralPatternCase;

impl LiteralPatternCase {
    fn check(source: &str) {
        SourceCase::check(source).unwrap();
    }

    fn lower(source: &str) {
        SourceCase::lower(source).unwrap();
    }

    fn assert_rejection(source: &str, expected: &str) {
        match SourceCase::check(source) {
            | Err(error) if error.is_type_error() => {
                let found = format!("{error:?}");
                assert!(
                    found.contains(expected),
                    "expected a rejection mentioning `{expected}`, found: {found}"
                );
            }
            | Ok(()) => panic!("expected `{expected}`, but the program was accepted"),
            | Err(error) => panic!("expected a type error, found: {error:?}"),
        }
    }
}

#[test]
fn checks_integer_literal_arms() {
    LiteralPatternCase::check(
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
    );
}

#[test]
fn lowers_integer_literal_arms_through_the_compiled_pipeline() {
    LiteralPatternCase::lower(
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
    );
}

#[test]
fn accepts_literals_nested_in_refutable_patterns() {
    LiteralPatternCase::check(
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
    );
}

#[test]
fn rejects_a_float_literal_pattern_on_a_float_type() {
    LiteralPatternCase::assert_rejection(
        r#"
begin
  let x : Float64 = 1.5 that
  match x
  | 1.5 => ret ()
  | _ => ret ()
  end
end
"#,
        "an integer primitive type",
    );
}

#[test]
fn rejects_a_float_literal_pattern_on_an_integer_type() {
    LiteralPatternCase::assert_rejection(
        r#"
begin
  let n : Int64 = 0 that
  match n
  | 1.5 => ret ()
  | _ => ret ()
  end
end
"#,
        "integer literals only",
    );
}

#[test]
fn rejects_a_literal_pattern_on_a_non_primitive_type() {
    LiteralPatternCase::assert_rejection(
        r#"
begin
  match ()
  | 0 => ret ()
  | _ => ret ()
  end
end
"#,
        "an integer primitive type",
    );
}

#[test]
fn rejects_a_literal_outside_the_scrutinee_range() {
    LiteralPatternCase::assert_rejection(
        r#"
begin
  let n : Int8 = 0 that
  match n
  | 300 => ret ()
  | _ => ret ()
  end
end
"#,
        "IntegerLiteralOutOfRange",
    );
}

#[test]
fn rejects_a_literal_only_match_as_non_exhaustive() {
    LiteralPatternCase::assert_rejection(
        r#"
begin
  let n : Int64 = 0 that
  match n
  | 0 => ret ()
  | 1 => ret ()
  end
end
"#,
        "Coverage",
    );
}

#[test]
fn rejects_a_literal_binder_on_a_value_function_parameter() {
    LiteralPatternCase::assert_rejection(
        r#"
begin
  let val zero (0 : Int64) : Int64 = 0 that
  ret (0 |> zero)
end
"#,
        "irrefutable",
    );
}
