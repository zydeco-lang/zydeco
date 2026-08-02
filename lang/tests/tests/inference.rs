use zydeco_driver::{BuildError, check::err::CompileError};
use zydeco_tests::utils::SourceCase;

struct InferenceCase;

impl InferenceCase {
    fn check(source: &str) {
        SourceCase::check(source).unwrap();
    }

    fn assert_type_error(source: &str) {
        Self::assert_type_result(SourceCase::check(source));
    }

    fn assert_type_result(result: Result<(), BuildError>) {
        match result {
            | Err(BuildError::CompileError(
                CompileError::TyckErrorReports { .. } | CompileError::TyckErrors(_),
            )) => {}
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
  let consume = { fn value -> ! exit value } that
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
  let duplicate = { fn value -> ret (value, value) } that
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
    fn value ->
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
  let identity = { fn value -> ret value } that
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
  let identity = { fn value -> ret value } that
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
  let identity = { fn value -> ret value } that
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
  let swap = { fn (first, second) -> ret (second, first) } that
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
  let unwrap = { fn (field = value) -> ret value } that
  do result <- ! unwrap (field = ());
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
    fn thunk ->
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
  let apply = { fn thunk -> ! thunk () } that
  do result <- ! apply { fn (_ : Unit) -> ret () };
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
    fn pair ->
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
    fn value ->
      begin
        let identity = { fn inner -> ret inner } that
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
    { fn value -> ret value }
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
        r#"{ fn value -> ret value }"#,
    ));
}

#[test]
fn rejects_an_unconstrained_parameter_at_its_block_boundary() {
    InferenceCase::assert_type_error(
        r#"
begin
  let ignore = { fn value -> ret () } that
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
  let ignore = { fn (value : Unit) -> ret () } that
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
  let unwrap = { fn +Some value -> ret value } that
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
  let identity = { fn value -> ret value } that
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
  def boxed : Box = (Int, 0) that
  let identity = { fn value -> ret value } that

  match boxed
  | (X, value) -> ! identity value
  end
end
"#,
    );
}
