use zydeco_tests::utils::SourceCase;

struct PureFunctionCase;

impl PureFunctionCase {
    fn check(source: &str) {
        SourceCase::check(source).unwrap();
    }

    fn run(source: &str) {
        SourceCase::run(source).unwrap();
    }

    fn run_monadic(source: &str) {
        SourceCase::run_monadic(source).unwrap();
    }

    fn assert_type_error(source: &str) {
        match SourceCase::check(source) {
            | Err(error) if error.is_type_error() => {}
            | Ok(()) => panic!("expected a type error, but the program was accepted"),
            | Err(error) => panic!("expected a type error, found: {error:?}"),
        }
    }
}

#[test]
fn synthesizes_a_pure_identity_function_and_application() {
    PureFunctionCase::check(
        r#"
begin
  let identity = fn value => value that
  let result = identity () that
  result
end
"#,
    );
}

#[test]
fn checks_explicit_pure_function_types() {
    PureFunctionCase::check(
        r#"
begin
  let identity : Unit -> Unit = fn (value : Unit) => value that
  let result : Unit = identity () that
  result
end
"#,
    );
}

#[test]
fn infers_higher_order_pure_function_shapes() {
    PureFunctionCase::check(
        r#"
begin
  let apply = fn function => function () that
  let identity = fn value => value that
  let result = apply identity that
  result
end
"#,
    );
}

#[test]
fn refines_an_expected_metavariable_for_a_pure_abstraction() {
    PureFunctionCase::check(
        r#"
begin
  let forward = fn function => function that
  let identity = forward (fn value => value) that
  let result = identity () that
  result
end
"#,
    );
}

#[test]
fn pure_functions_capture_their_lexical_environment() {
    PureFunctionCase::run(
        r#"
begin
  let captured : Int64 = 0 that
  let constant : Unit -> Int64 = fn (_ : Unit) => captured that
  let result : Int64 = constant () that
  ! exit result
end
"#,
    );
}

#[test]
fn pure_function_bodies_reject_computations() {
    PureFunctionCase::assert_type_error(
        r#"
begin
  let invalid : Unit -> Unit = fn (_ : Unit) => ret () that
  invalid
end
"#,
    );
}

#[test]
fn rejects_incompatible_pure_call_site_constraints() {
    PureFunctionCase::assert_type_error(
        r#"
begin
  let identity = fn value => value that
  let first = identity () that
  let second = identity 0 that
  (first, second)
end
"#,
    );
}

#[test]
fn rejects_an_unconstrained_pure_parameter() {
    PureFunctionCase::assert_type_error(
        r#"
begin
  let ignore = fn value => () that
  ignore
end
"#,
    );
}

#[test]
fn rejects_pure_self_application_during_the_occurs_check() {
    PureFunctionCase::assert_type_error(
        r#"
begin
  let identity = fn value => value that
  let result = identity identity that
  result
end
"#,
    );
}

#[test]
fn synthesizes_a_pure_package_dependent_arrow() {
    PureFunctionCase::run(
        r#"
begin
  let Box = exists (X : VType) . X that
  let unpack = fn ((X, value) : Box) => value that
  let result : Int64 = unpack (Int64, 0) that
  ! exit result
end
"#,
    );
}

#[test]
fn checks_and_applies_a_pure_polymorphic_function() {
    PureFunctionCase::run(
        r#"
begin
  let identity : forall (A : VType) . A -> A =
    fn (A : VType) => fn (value : A) => value
  that
  let identity_thunk : forall (B : CType) . Thk B -> Thk B =
    fn (B : CType) => fn (value : Thk B) => value
  that
  let status : Int64 = identity Int64 0 that
  let top : Thk Top = identity_thunk Top triv that
  ! exit status
end
"#,
    );
}

#[test]
fn pure_parameterized_blocks_need_no_thunk_or_return_wrappers() {
    PureFunctionCase::run(
        r#"
begin
  let Input = exists (A : VType) . A that

  let make =
    param ((A, seed) : Input) in
    begin
      def identity (value : A) : A = value that
      let selected : A = identity seed that
      let Output =
        exists (B as A : VType) .
          (#identity :: B -> B) * B
      that
      (A, #identity = identity, selected) : Output
    end
  that

  let (B, #identity = identity, selected) = make (Int64, 0) that
  let result : B = identity selected that
  let status : Int64 = result that
  ! exit status
end
"#,
    );
}

#[test]
fn translates_pure_functions_inside_monadic_blocks() {
    PureFunctionCase::run_monadic(
        r#"
begin
  def ! ret_monad : Monad Ret =
    comatch
    | .return A value => ret value
    | .bind A B computation continuation =>
      do value <- ! computation;
      ! continuation value
    end
  that

  def ! translated =
    @[monadic] begin
      let identity : forall (A : VType) . A -> A =
        fn (A : VType) => fn (value : A) => value
      in
      let Box = exists (A : VType) . A in
      let unpack = fn ((A, value) : Box) => value in
      ret (identity Int64 (unpack (Int64, 0)))
    end
  that

  do status <- ! translated Ret { ! ret_monad };
  ! exit status
end
"#,
    );
}

#[test]
fn rejects_a_nested_existential_escape_from_a_pure_function() {
    PureFunctionCase::assert_type_error(
        r#"
begin
  let Nested = Unit * (exists (A : VType) . A) that
  let invalid = fn ((_, (A, value)) : Nested) => value that
  invalid
end
"#,
    );
}
