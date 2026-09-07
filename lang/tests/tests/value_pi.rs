use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

#[test]
fn pipelines_apply_value_functions_in_both_directions() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
begin
  let val keep (A : VType) (value : A) : A = value that
  let forward : Int64 = 0 |> keep Int64 that
  let backward : Int64 = keep Int64 <| forward that
  ! exit backward
end
"#,
    ));
}

#[test]
fn explicit_value_pi_classifies_a_value_abstraction() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
begin
  let identity : val pi (A : VType) (value : A) . A =
    val (A : VType) (value : A) => value
  that
  let recovered : Unit = () |> identity Unit that
  ! exit 0
end
"#,
    ));
}

#[test]
fn param_val_introduces_lexical_and_block_value_functions() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
begin
  let lexical : val pi (value : Unit) . Unit =
    param val (value : Unit) in value
  that
  let mobile : val pi (value : Unit) . Unit = begin
    param val (value : Unit) that
    value
  end that
  let first : Unit = () |> lexical that
  let second : Unit = first |> mobile that
  ! exit 0
end
"#,
    ));
}

#[test]
fn plain_param_does_not_infer_a_value_function() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  let invalid = param (value : Unit) in value that
  ! exit 0
end
"#,
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn value_functions_apply_through_partial_type_instantiation() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
begin
  let val keep (A : VType) (value : A) : A = value that
  let keep_unit : val pi (value : Unit) . Unit = keep Unit that
  let recovered : Unit = () |> keep_unit that
  ! exit 0
end
"#,
    ));
}

#[test]
fn value_functions_flow_through_products() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
begin
  let val keep (A : VType) (value : A) : A = value that
  let keep_unit : val pi (value : Unit) . Unit = keep Unit that
  let functions = (keep_unit, ()) that
  let (stored, _) = functions that
  let recovered : Unit = () |> stored that
  ! exit 0
end
"#,
    ));
}

#[test]
fn value_functions_reject_being_returned_by_computations() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  let val keep (value : Unit) : Unit = value that
  do escaped <- ret keep;
  ! exit 0
end
"#,
        ),
        TyckDiagnosticCode::StaticElimination,
    );
}

#[test]
fn value_functions_accept_higher_order_domains() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
begin
  let val apply_twice (function : val pi (_ : Unit) . Unit) : Unit =
    () |> function
  that
  ! exit 0
end
"#,
    ));
}

#[test]
fn value_functions_accept_static_constructor_payload_types() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
begin
  let Stored =
    data
    | +Wrap : (val pi (_ : Unit) . Unit)
    end
  that
  ! exit 0
end
"#,
    ));
}

#[test]
fn let_wrapped_value_function_bodies_still_unfold() {
    // A lexical `let` in the right-hand side must not block unfolding: the
    // elaborated definition stays reducible and lowering succeeds.
    SourceCase::lower(
        r#"
begin
  let wrapped : val pi (value : Unit) . Unit =
    let ignored : Unit = () in
    param val (value : Unit) in value
  that
  let recovered : Unit = () |> wrapped that
  ! exit 0
end
"#,
    )
    .expect("the let-wrapped definition must lower");
}

#[test]
fn value_function_bodies_reject_computations() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  let invalid : val pi (_ : Unit) . Unit =
    val (_ : Unit) => ret ()
  that
  ! exit 0
end
"#,
        ),
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn package_witness_instantiation_follows_the_parameter_pattern() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
begin
  let Box = exists (A : VType) . A that
  let val take_second
    ((_ : Box), ((B, value) : Box))
  : B =
    value
  that
  let status : Int64 = ((Unit, ()), (Int64, 0)) |> take_second that
  ! exit status
end
"#,
    ));
}

#[test]
fn value_pi_composes_multiple_package_openings_in_product_order() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
begin
  let Box = exists (A : VType) . A that
  let val unpack_both
    (((A, left) : Box), ((B, right) : Box))
  : A * B =
    (left, right)
  that
  let (_ : Unit, status : Int64) =
    ((Unit, ()), (Int64, 0)) |> unpack_both
  that
  ! exit status
end
"#,
    ));
}

#[test]
fn value_functions_capture_runtime_values() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
begin
  do captured <- ret 0;
  let val constant (_ : Unit) : Int64 = captured in
  let status = () |> constant in
  ! exit status
end
"#,
    ));
}

#[test]
fn value_functions_share_the_value_namespace() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
begin
  let val keep (value : Int64) : Int64 = value in
  let stored = keep in
  let keep : Unit = () in
  let transformed : Int64 = 0 |> stored in
  let _ : Unit = keep in
  ! exit transformed
end
"#,
    ));
}

#[test]
fn value_function_bindings_are_non_recursive() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  let val loop (value : Unit) : Unit = value |> loop that
  ! exit 0
end
"#,
        ),
        TyckDiagnosticCode::InvalidBindingCycle,
    );
}

#[test]
fn value_function_parameters_must_be_irrefutable() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  let Maybe =
    data
    | +None : Unit
    | +Some : Int64
    end
  that
  let val invalid ((+Some(value)) : Maybe) : Int64 = value that
  ! exit 0
end
"#,
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn pipelines_respect_curried_binder_order() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  let val keep (A : VType) (value : A) : A = value that
  let invalid = () |> keep that
  ! exit 0
end
"#,
        ),
        TyckDiagnosticCode::SortMismatch,
    );
}

#[test]
fn computation_abstraction_does_not_stand_in_for_val() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  let identity = fn (value : Unit) => value that
  ! exit 0
end
"#,
        ),
        TyckDiagnosticCode::Expressivity,
    );
}
