use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

#[test]
fn infers_the_result_kind_of_a_parameterized_alias() {
    SourceCase::assert_accepted(SourceCase::check(
        r#"
begin
  let Option (A : VType) =
    data
    | +None : Unit
    | +Some : A
    end
  that
  let none : Option Int64 = +None() that
  ret ()
end
"#,
    ));
}

#[test]
fn accepts_a_concise_recursive_type_definition() {
    SourceCase::assert_accepted(SourceCase::check(
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
    ));
}

#[test]
fn rejects_that_without_an_enclosing_block() {
    SourceCase::assert_resolve_error(SourceCase::check(
        r#"
param A that A
"#,
    ));
}

#[test]
fn rejects_a_recursive_parameter_component() {
    SourceCase::assert_resolve_error(SourceCase::check(
        r#"
begin
  param (A : A) that
  A
end
"#,
    ));
}

#[test]
fn rejects_a_recursive_value_definition() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  def value : Int64 = value that
  ret ()
end
"#,
        ),
        TyckDiagnosticCode::SortMismatch,
    );
}
