use zydeco_tests::utils::SourceCase;

struct CoverageCase;

impl CoverageCase {
    fn check(source: &str) {
        SourceCase::check(source).unwrap();
    }

    fn run(source: &str) {
        SourceCase::run(source).unwrap();
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
  | +False(_) => ret ()
  | +True(_) => ret ()
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
  | +True(_) => ret ()
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
  | _ => ret ()
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
  | +Pair(+False(_), _) => ret ()
  | +Pair(+True(_), _) => ret ()
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
  | +Triple(+False(_), _, _) => ret ()
  | +Triple(+True(_), (_, _)) => ret ()
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
  | +Pair(+True(_), _) => ret ()
  | +Pair(_, +False(_)) => ret ()
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
    fn value => match value end
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
  | .left => ret ()
  | .right => ret ()
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
  | .left => ret ()
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
  | .left => ret ()
  | .left => ret ()
  | .right => ret ()
  end : Choice)
end
"#,
    );
}

#[test]
fn accepts_function_copattern_clauses() {
    CoverageCase::check(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  (comatch
  | +False(_) => ret ()
  | +True(_) => ret ()
  end : Bool -> Ret Unit)
end
"#,
    ));
}

#[test]
fn checks_empty_function_comatches_against_their_argument_type() {
    CoverageCase::check(
        r#"
begin
  let Void = data end that
  (comatch end : Void -> Ret Unit)
end
"#,
    );

    CoverageCase::assert_type_error(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  (comatch end : Bool -> Ret Unit)
end
"#,
    ));
}

#[test]
fn accepts_repeated_destructors_split_by_argument_patterns() {
    CoverageCase::check(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  let Observer =
    codata
    | .choose : Bool -> Ret Unit
    | .reset : Ret Unit
    end
  that
  (comatch
  | .choose +False(_) => ret ()
  | .choose +True(_) => ret ()
  | .reset => ret ()
  end : Observer)
end
"#,
    ));
}

#[test]
fn rejects_a_missing_argument_case_below_a_destructor() {
    CoverageCase::assert_type_error(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  let Observer =
    codata
    | .choose : Bool -> Ret Unit
    end
  that
  (comatch
  | .choose +True(_) => ret ()
  end : Observer)
end
"#,
    ));
}

#[test]
fn accepts_nested_destructor_copatterns() {
    CoverageCase::check(
        r#"
begin
  let Inner =
    codata
    | .left : Ret Unit
    | .right : Ret Unit
    end
  that
  let Outer =
    codata
    | .open : Inner
    end
  that
  (comatch
  | .open .left => ret ()
  | .open .right => ret ()
  end : Outer)
end
"#,
    );
}

#[test]
fn rejects_a_missing_nested_destructor_copattern() {
    CoverageCase::assert_type_error(
        r#"
begin
  let Inner =
    codata
    | .left : Ret Unit
    | .right : Ret Unit
    end
  that
  let Outer =
    codata
    | .open : Inner
    end
  that
  (comatch
  | .open .left => ret ()
  end : Outer)
end
"#,
    );
}

#[test]
fn checks_correlated_coverage_across_multiple_arguments() {
    CoverageCase::check(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  let Observer =
    codata
    | .choose : Bool -> Bool -> Ret Unit
    end
  that
  (comatch
  | .choose +False(_) _ => ret ()
  | .choose +True(_) _ => ret ()
  end : Observer)
end
"#,
    ));

    CoverageCase::assert_type_error(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  let Observer =
    codata
    | .choose : Bool -> Bool -> Ret Unit
    end
  that
  (comatch
  | .choose +True(_) _ => ret ()
  | .choose _ +False(_) => ret ()
  end : Observer)
end
"#,
    ));
}

#[test]
fn accepts_type_arguments_mixed_with_value_and_destructor_copatterns() {
    CoverageCase::check(
        r#"
begin
  let Inner (A : VType) =
    codata
    | .get : Ret A
    | .ignore : Ret Unit
    end
  that
  let Poly =
    codata
    | .open : forall (A : VType) . A -> Inner A
    end
  that
  (comatch
  | .open A (value : A) .get => ret value
  | .open A _ .ignore => ret ()
  end : Poly)
end
"#,
    );
}

#[test]
fn carries_argument_coverage_through_a_later_type_abstraction() {
    CoverageCase::run(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  let Poly =
    codata
    | .open : Bool -> (forall (A : VType) . Ret Unit)
    end
  that
  do _ <-
    (comatch
    | .open +False(_) A => ret ()
    | .open +True(_) A => ret ()
    end : Poly) .open +True() Unit;
  ! (api/exit) 0
end
"#,
    ));
}

#[test]
fn accepts_a_package_dependent_pattern_in_a_copattern_spine() {
    CoverageCase::run(
        r#"
begin
  let Box =
    exists (A : VType) . A
  that
  let Service =
    codata
    | .unbox : pi ((A, _) : Box) . Ret A
    end
  that
  do status <-
    (comatch
    | .unbox ((A, value) : Box) => ret value
    end : Service) .unbox (Int64, 0);
  ! (api/exit) status
end
"#,
    );
}

#[test]
fn rejects_a_refutable_package_dependent_copattern() {
    CoverageCase::assert_type_error(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  let Box =
    exists (A : VType) . Bool
  that
  let Service =
    codata
    | .inspect : pi ((A, _) : Box) . Ret Unit
    end
  that
  (comatch
  | .inspect ((A, +True(_)) : Box) => ret ()
  end : Service)
end
"#,
    ));
}

#[test]
fn executes_a_mixed_copattern_observation_path() {
    CoverageCase::run(
        r#"
begin
  let Input =
    data
    | +First : Int64
    | +Second : Int64
    end
  that
  let Choice =
    codata
    | .left : Ret Int64
    | .right : Ret Int64
    end
  that
  let Router =
    codata
    | .route : Input -> Choice
    end
  that
  do status <-
    (comatch
    | .route +First(value) .left => ret 1
    | .route +First(value) .right => ret value
    | .route +Second(_) .left => ret 1
    | .route +Second(_) .right => ret 1
    end : Router) .route +First(0) .right;
  ! (api/exit) status
end
"#,
    );
}
