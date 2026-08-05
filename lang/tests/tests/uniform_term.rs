use zydeco_tests::utils::{CaseError, SourceCase};

struct UniformTermCase;

impl UniformTermCase {
    fn check(source: &str) -> Result<(), CaseError> {
        SourceCase::check(source)
    }

    fn assert_resolve_error(source: &str) {
        match Self::check(source) {
            | Err(error) if error.is_resolve_error() => {}
            | Ok(()) => panic!("expected a resolution error, but the program was accepted"),
            | Err(error) => panic!("expected a resolution error, found: {error:?}"),
        }
    }

    fn assert_type_error(source: &str) {
        match Self::check(source) {
            | Err(error) if error.is_type_error() => {}
            | Ok(()) => panic!("expected a type error, but the program was accepted"),
            | Err(error) => panic!("expected a type error, found: {error:?}"),
        }
    }
}

#[test]
fn infers_the_result_kind_of_a_parameterized_alias() {
    UniformTermCase::check(
        r#"
begin
  let Option (A : VType) =
    data
    | +None : Unit
    | +Some : A
    end
  that
  let none : Option Int = +None() that
  ret ()
end
"#,
    )
    .unwrap();
}

#[test]
fn accepts_a_concise_recursive_type_definition() {
    UniformTermCase::check(
        r#"
begin
  def List (A : VType) : VType =
    data
    | +Nil : Unit
    | +Cons : A * List A
    end
  that
  let nil : List Unit = +Nil() that
  ret ()
end
"#,
    )
    .unwrap();
}

#[test]
fn rejects_that_without_an_enclosing_block() {
    UniformTermCase::assert_resolve_error(
        r#"
param A that A
"#,
    );
}

#[test]
fn rejects_a_recursive_parameter_component() {
    UniformTermCase::assert_resolve_error(
        r#"
begin
  param (A : A) that
  A
end
"#,
    );
}

#[test]
fn rejects_a_recursive_value_definition() {
    UniformTermCase::assert_type_error(
        r#"
begin
  def value : Int = value that
  ret ()
end
"#,
    );
}
