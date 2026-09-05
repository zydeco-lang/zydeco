use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

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
    SourceCase::assert_accepted(SourceCase::check(&format!(
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
    )));
}

#[test]
fn rejects_a_data_match_with_a_missing_constructor() {
    SourceCase::assert_rejected(
        SourceCase::check(&format!(
            r#"
begin
  {BOOL_DECLARATION}
  let value : Bool = +True() that
  match value
  | +True(_) => ret ()
  end
end
"#,
        )),
        TyckDiagnosticCode::Coverage,
    );
}

#[test]
fn accepts_a_wildcard_data_match() {
    SourceCase::assert_accepted(SourceCase::check(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  let value : Bool = +True() that
  match value
  | _ => ret ()
  end
end
"#,
    )));
}

#[test]
fn accepts_nested_constructor_coverage() {
    SourceCase::assert_accepted(SourceCase::check(&format!(
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
    )));
}

#[test]
fn covers_flat_and_nested_product_patterns_at_matching_arities() {
    SourceCase::assert_accepted(SourceCase::check(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  let Nested = data | +Nested : Bool * (Bool * Bool) end that
  let value : Nested = +Nested(+True(), (+False(), +True())) that
  match value
  | +Nested(+False(_), _) => ret ()
  | +Nested(+True(_), (_, _)) => ret ()
  end
end
"#,
    )));
}

#[test]
fn rejects_a_correlated_gap_in_nested_product_patterns() {
    SourceCase::assert_rejected(
        SourceCase::check(&format!(
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
        )),
        TyckDiagnosticCode::Coverage,
    );
}

#[test]
fn accepts_elimination_from_an_empty_data_type() {
    SourceCase::assert_accepted(SourceCase::check(
        r#"
begin
  let Void = data end that
  let absurd : Thk (Void -> Ret Unit) = {
    fn value => match value end
  } that
  ret ()
end
"#,
    ));
}

#[test]
fn accepts_an_exhaustive_codata_comatch() {
    SourceCase::assert_accepted(SourceCase::check(
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
    ));
}

#[test]
fn rejects_a_codata_comatch_with_a_missing_destructor() {
    SourceCase::assert_rejected(
        SourceCase::check(
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
        ),
        TyckDiagnosticCode::Coverage,
    );
}

#[test]
fn rejects_duplicate_codata_destructor_arms() {
    SourceCase::assert_rejected(
        SourceCase::check(
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
        ),
        TyckDiagnosticCode::OverlappingCopatternClauses,
    );
}

#[test]
fn accepts_function_copattern_clauses() {
    SourceCase::assert_accepted(SourceCase::check(&format!(
        r#"
begin
  {BOOL_DECLARATION}
  (comatch
  | +False(_) => ret ()
  | +True(_) => ret ()
  end : Bool -> Ret Unit)
end
"#,
    )));
}

#[test]
fn checks_empty_function_comatches_against_their_argument_type() {
    SourceCase::assert_accepted(SourceCase::check(
        r#"
begin
  let Void = data end that
  (comatch end : Void -> Ret Unit)
end
"#,
    ));

    SourceCase::assert_rejected(
        SourceCase::check(&format!(
            r#"
begin
  {BOOL_DECLARATION}
  (comatch end : Bool -> Ret Unit)
end
"#,
        )),
        TyckDiagnosticCode::Coverage,
    );
}

#[test]
fn accepts_repeated_destructors_split_by_argument_patterns() {
    SourceCase::assert_accepted(SourceCase::check(&format!(
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
    )));
}

#[test]
fn rejects_a_missing_argument_case_below_a_destructor() {
    SourceCase::assert_rejected(
        SourceCase::check(&format!(
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
        )),
        TyckDiagnosticCode::Coverage,
    );
}

#[test]
fn accepts_nested_destructor_copatterns() {
    SourceCase::assert_accepted(SourceCase::check(
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
    ));
}

#[test]
fn rejects_a_missing_nested_destructor_copattern() {
    SourceCase::assert_rejected(
        SourceCase::check(
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
        ),
        TyckDiagnosticCode::Coverage,
    );
}

#[test]
fn checks_correlated_coverage_across_multiple_arguments() {
    SourceCase::assert_accepted(SourceCase::check(&format!(
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
    )));

    SourceCase::assert_rejected(
        SourceCase::check(&format!(
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
        )),
        TyckDiagnosticCode::Coverage,
    );
}

#[test]
fn accepts_type_arguments_mixed_with_value_and_destructor_copatterns() {
    SourceCase::assert_accepted(SourceCase::check(
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
    ));
}

#[test]
fn carries_argument_coverage_through_a_later_type_abstraction() {
    SourceCase::assert_accepted(SourceCase::run(&format!(
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
  ! api/exit 0
end
"#,
    )));
}

#[test]
fn accepts_a_package_dependent_pattern_in_a_copattern_spine() {
    SourceCase::assert_accepted(SourceCase::run(
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
  ! api/exit status
end
"#,
    ));
}

#[test]
fn rejects_a_refutable_package_dependent_copattern() {
    SourceCase::assert_rejected(
        SourceCase::check(&format!(
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
        )),
        TyckDiagnosticCode::Coverage,
    );
}

#[test]
fn executes_a_mixed_copattern_observation_path() {
    SourceCase::assert_accepted(SourceCase::run(
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
  ! api/exit status
end
"#,
    ));
}
