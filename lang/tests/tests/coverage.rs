use zydeco_tests::utils::SourceCase;

struct CoverageCase;

impl CoverageCase {
    fn check(source: &str) {
        SourceCase::check(source).unwrap();
    }

    fn assert_type_error(source: &str) {
        match SourceCase::check(source) {
            | Err(error) if error.is_type_error() => {}
            | Ok(()) => panic!("expected a coverage error, but the program was accepted"),
            | Err(error) => panic!("expected a coverage error, found: {error:?}"),
        }
    }
}

const BOOL_DECLARATION: &str = r#"
let Bool =
  data
  | +False : Unit
  | +True : Unit
  end
that
"#;

#[test]
fn accepts_an_exhaustive_data_match() {
    CoverageCase::check(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  let value : Bool = +True() that
  match value
  | +False(_) -> ret ()
  | +True(_) -> ret ()
  end
end
"#,
    ));
}

#[test]
fn rejects_a_data_match_with_a_missing_constructor() {
    CoverageCase::assert_type_error(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  let value : Bool = +True() that
  match value
  | +True(_) -> ret ()
  end
end
"#,
    ));
}

#[test]
fn accepts_a_wildcard_data_match() {
    CoverageCase::check(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  let value : Bool = +True() that
  match value
  | _ -> ret ()
  end
end
"#,
    ));
}

#[test]
fn accepts_nested_constructor_coverage() {
    CoverageCase::check(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  let Pair = data | +Pair : Bool * Bool end that
  let value : Pair = +Pair(+True(), +False()) that
  match value
  | +Pair(+False(_), _) -> ret ()
  | +Pair(+True(_), _) -> ret ()
  end
end
"#,
    ));
}

#[test]
fn treats_nary_and_nested_product_patterns_as_the_same_shape() {
    CoverageCase::check(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  let Triple = data | +Triple : Bool * Bool * Bool end that
  let value : Triple = +Triple(+True(), +False(), +True()) that
  match value
  | +Triple(+False(_), _, _) -> ret ()
  | +Triple(+True(_), (_, _)) -> ret ()
  end
end
"#,
    ));
}

#[test]
fn rejects_a_correlated_gap_in_nested_product_patterns() {
    CoverageCase::assert_type_error(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  let Pair = data | +Pair : Bool * Bool end that
  let value : Pair = +Pair(+True(), +False()) that
  match value
  | +Pair(+True(_), _) -> ret ()
  | +Pair(_, +False(_)) -> ret ()
  end
end
"#,
    ));
}

#[test]
fn accepts_elimination_from_an_empty_data_type() {
    CoverageCase::check(
        r#"
begin
  let Void = data end that
  let absurd : Thk (Void -> Ret Unit) = {
    fn value -> match value end
  } that
  ret ()
end
"#,
    );
}

#[test]
fn accepts_an_exhaustive_codata_comatch() {
    CoverageCase::check(
        r#"
begin
  let Choice =
    codata
    | .left : Ret Unit
    | .right : Ret Unit
    end
  that
  (comatch
  | .left -> ret ()
  | .right -> ret ()
  end : Choice)
end
"#,
    );
}

#[test]
fn rejects_a_codata_comatch_with_a_missing_destructor() {
    CoverageCase::assert_type_error(
        r#"
begin
  let Choice =
    codata
    | .left : Ret Unit
    | .right : Ret Unit
    end
  that
  (comatch
  | .left -> ret ()
  end : Choice)
end
"#,
    );
}

#[test]
fn rejects_duplicate_codata_destructor_arms() {
    CoverageCase::assert_type_error(
        r#"
begin
  let Choice =
    codata
    | .left : Ret Unit
    | .right : Ret Unit
    end
  that
  (comatch
  | .left -> ret ()
  | .left -> ret ()
  | .right -> ret ()
  end : Choice)
end
"#,
    );
}
