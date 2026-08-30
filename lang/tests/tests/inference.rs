use zydeco_tests::utils::{CaseError, SourceCase};

struct InferenceCase;

impl InferenceCase {
    fn check(source: &str) {
        SourceCase::check(source).unwrap();
    }

    fn assert_type_error(source: &str) {
        Self::assert_type_result(SourceCase::check(source));
    }

    fn assert_type_result(result: Result<(), CaseError>) {
        match result {
            | Err(error) if error.is_type_error() => {}
            | Ok(()) => panic!("expected a type error, but the program was accepted"),
            | Err(error) => panic!("expected a type error, found: {error:?}"),
        }
    }
}

#[test]
fn infers_an_unannotated_parameter_from_its_body() {
    InferenceCase::check(
        r#"
begin
  let consume = { fn value => ! exit value } that
  ret ()
end
"#,
    );
}

#[test]
fn accepts_compatible_constraints_from_several_body_uses() {
    InferenceCase::check(
        r#"
begin
  let duplicate = { fn value => ret (value, value) } that
  do _ <- ! duplicate ();
  ret ()
end
"#,
    );
}

#[test]
fn rejects_incompatible_constraints_from_the_body() {
    InferenceCase::assert_type_error(
        r#"
begin
  let impossible = {
    fn value =>
      let (_ : Unit) = value in
      ! exit value
  } that
  ret ()
end
"#,
    );
}

#[test]
fn infers_an_unannotated_parameter_from_a_call_site() {
    InferenceCase::check(
        r#"
begin
  let identity = { fn value => ret value } that
  do result <- ! identity ();
  ret result
end
"#,
    );
}

#[test]
fn accepts_compatible_call_site_constraints() {
    InferenceCase::check(
        r#"
begin
  let identity = { fn value => ret value } that
  do _ <- ! identity ();
  do result <- ! identity ();
  ret result
end
"#,
    );
}

#[test]
fn rejects_incompatible_call_site_constraints() {
    InferenceCase::assert_type_error(
        r#"
begin
  let identity = { fn value => ret value } that
  do _ <- ! identity ();
  do _ <- ! identity 0;
  ret ()
end
"#,
    );
}

#[test]
fn synthesizes_ordinary_tuple_patterns_componentwise() {
    InferenceCase::check(
        r#"
begin
  let swap = { fn (first, second) => ret (second, first) } that
  do _ <- ! swap ((), ());
  ret ()
end
"#,
    );
}

#[test]
fn synthesizes_named_patterns_from_their_payload() {
    InferenceCase::check(
        r#"
begin
  let unwrap = { fn (#field = value) => ret value } that
  do result <- ! unwrap (#field = ());
  ret result
end
"#,
    );
}

#[test]
fn refines_an_inferred_parameter_through_thunk_and_return_shapes() {
    InferenceCase::check(
        r#"
begin
  let run = {
    fn thunk =>
      do result <- ! thunk;
      ret result
  } that
  do result <- ! run { ret () };
  ret result
end
"#,
    );
}

#[test]
fn refines_an_inferred_computation_into_an_arrow() {
    InferenceCase::check(
        r#"
begin
  let apply = { fn thunk => ! thunk () } that
  do result <- ! apply { fn (_ : Unit) => ret () };
  ret result
end
"#,
    );
}

#[test]
fn refines_an_inferred_value_into_a_product() {
    InferenceCase::check(
        r#"
begin
  let first = {
    fn pair =>
      let (head, _) = pair in
      ret head
  } that
  do result <- ! first ((), ());
  ret result
end
"#,
    );
}

#[test]
fn permits_an_inner_inference_variable_to_alias_an_outer_one() {
    InferenceCase::check(
        r#"
begin
  let outer = {
    fn value =>
      begin
        let identity = { fn inner => ret inner } that
        ! identity value
      end
  } that
  do _ <- ! outer ();
  ret ()
end
"#,
    );
}

#[test]
fn rejects_call_site_inference_across_a_block_boundary() {
    InferenceCase::assert_type_error(
        r#"
let identity =
  begin
    { fn value => ret value }
  end
in
! identity ()
"#,
    );
}

#[test]
fn rejects_call_site_inference_across_an_imported_source_boundary() {
    InferenceCase::assert_type_result(SourceCase::check_with_import(
        r#"
let identity = @[import("imported.zy")] _ in
! identity ()
"#,
        r#"{ fn value => ret value }"#,
    ));
}

#[test]
fn rejects_an_unconstrained_parameter_at_its_block_boundary() {
    InferenceCase::assert_type_error(
        r#"
begin
  let ignore = { fn value => ret () } that
  ret ()
end
"#,
    );
}

#[test]
fn retains_explicit_parameter_annotations() {
    InferenceCase::check(
        r#"
begin
  let ignore = { fn (value : Unit) => ret () } that
  ret ()
end
"#,
    );
}

#[test]
fn keeps_constructor_patterns_annotation_directed() {
    InferenceCase::assert_type_error(
        r#"
begin
  let Optional =
    data
    | +None : Unit
    | +Some : Unit
    end
  that
  let unwrap = { fn +Some value => ret value } that
  ret ()
end
"#,
    );
}

#[test]
fn rejects_self_application_during_the_occurs_check() {
    InferenceCase::assert_type_error(
        r#"
begin
  let identity = { fn value => ret value } that
  ! identity identity
end
"#,
    );
}

#[test]
fn rejects_an_existential_witness_escaping_through_an_inferred_domain() {
    InferenceCase::assert_type_error(
        r#"
begin
  let Box = exists (X : VType) . X that
  def boxed : Box = (Int64, 0) that
  let identity = { fn value => ret value } that

  match boxed
  | (X, value) => ! identity value
  end
end
"#,
    );
}

#[test]
fn checks_literals_in_each_rust_numeric_domain() {
    InferenceCase::check(
        r#"
begin
  let int8_value : Int8 = -128 that
  let int16_value : Int16 = -32768 that
  let int32_value : Int32 = -2147483648 that
  let int64_value : Int64 = -9223372036854775808 that
  let uint8_value : UInt8 = 255 that
  let uint16_value : UInt16 = 65535 that
  let uint32_value : UInt32 = 4294967295 that
  let uint64_value : UInt64 = 18446744073709551615 that
  let float32_value : Float32 = 1.5 that
  let float64_value : Float64 = 1.5 that
  ret (
    int8_value, int16_value, int32_value, int64_value,
    uint8_value, uint16_value, uint32_value, uint64_value,
    float32_value, float64_value
  )
end
"#,
    );
}

#[test]
fn rejects_signed_literals_outside_the_selected_rust_domain() {
    InferenceCase::assert_type_error("let value : Int8 = 128 in ret value");
}

#[test]
fn rejects_negative_literals_in_unsigned_rust_domains() {
    InferenceCase::assert_type_error("let value : UInt8 = -1 in ret value");
}

#[test]
fn rejects_finite_literals_that_overflow_float32() {
    InferenceCase::assert_type_error("let value : Float32 = 3.5e38 in ret value");
}
