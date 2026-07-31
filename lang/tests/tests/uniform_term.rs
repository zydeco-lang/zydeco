use zydeco_driver::{BuildError, check::err::CompileError};
use zydeco_tests::utils::SourceCase;

struct UniformTermCase;

impl UniformTermCase {
    fn check(source: &str) -> Result<(), BuildError> {
        SourceCase::check(source)
    }

    fn assert_resolve_error(source: &str) {
        match Self::check(source) {
            | Err(BuildError::CompileError(
                CompileError::ResolveErrorReport { .. } | CompileError::ResolveError(_),
            )) => {}
            | Ok(()) => panic!("expected a resolution error, but the program was accepted"),
            | Err(error) => panic!("expected a resolution error, found: {error:?}"),
        }
    }

    fn assert_type_error(source: &str) {
        match Self::check(source) {
            | Err(BuildError::CompileError(
                CompileError::TyckErrorReports { .. } | CompileError::TyckErrors(_),
            )) => {}
            | Ok(()) => panic!("expected a type error, but the program was accepted"),
            | Err(error) => panic!("expected a type error, found: {error:?}"),
        }
    }
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
