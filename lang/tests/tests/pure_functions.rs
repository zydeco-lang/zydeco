use zydeco_driver::{BuildError, check::err::CompileError};
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
            | Err(BuildError::CompileError(
                CompileError::TyckErrorReports { .. } | CompileError::TyckErrors(_),
            )) => {}
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
  let identity = fn value -> value that
  let result = identity () that
  ! exit 0
end
"#,
    );
}

#[test]
fn checks_explicit_pure_function_types() {
    PureFunctionCase::check(
        r#"
begin
  let identity : Unit -> Unit = fn (value : Unit) -> value that
  let result : Unit = identity () that
  ! exit 0
end
"#,
    );
}

#[test]
fn infers_higher_order_pure_function_shapes() {
    PureFunctionCase::check(
        r#"
begin
  let apply = fn function -> function () that
  let identity = fn value -> value that
  let result = apply identity that
  ! exit 0
end
"#,
    );
}

#[test]
fn refines_an_expected_metavariable_for_a_pure_abstraction() {
    PureFunctionCase::check(
        r#"
begin
  let forward = fn function -> function that
  let identity = forward (fn value -> value) that
  let result = identity () that
  ! exit 0
end
"#,
    );
}

#[test]
fn pure_functions_capture_their_lexical_environment() {
    PureFunctionCase::run(
        r#"
begin
  let captured : Unit = () that
  let constant : Unit -> Unit = fn (_ : Unit) -> captured that
  let result : Unit = constant () that
  ! exit 0
end
"#,
    );
}

#[test]
fn pure_function_bodies_reject_computations() {
    PureFunctionCase::assert_type_error(
        r#"
begin
  let invalid : Unit -> Unit = fn (_ : Unit) -> ret () that
  ! exit 0
end
"#,
    );
}

#[test]
fn rejects_incompatible_pure_call_site_constraints() {
    PureFunctionCase::assert_type_error(
        r#"
begin
  let identity = fn value -> value that
  let first = identity () that
  let second = identity 0 that
  ! exit 0
end
"#,
    );
}

#[test]
fn rejects_an_unconstrained_pure_parameter() {
    PureFunctionCase::assert_type_error(
        r#"
begin
  let ignore = fn value -> () that
  ! exit 0
end
"#,
    );
}

#[test]
fn rejects_pure_self_application_during_the_occurs_check() {
    PureFunctionCase::assert_type_error(
        r#"
begin
  let identity = fn value -> value that
  let result = identity identity that
  ! exit 0
end
"#,
    );
}

#[test]
fn rejects_an_existential_witness_escaping_a_pure_result() {
    PureFunctionCase::assert_type_error(
        r#"
begin
  let Box = exists (X : VType) . X that
  let unpack = fn ((X, value) : Box) -> value that
  ! exit 0
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
    | .return A value -> ret value
    | .bind A B computation continuation ->
      do value <- ! computation;
      ! continuation value
    end
  that

  def ! translated =
    monadic
      let identity : Unit -> Unit = fn value -> value that
      ret (identity ())
    end
  that

  do _ <- ! translated Ret { ! ret_monad };
  ! exit 0
end
"#,
    );
}
