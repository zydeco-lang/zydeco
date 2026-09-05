use zydeco_cli::compile::CompileError;
use zydeco_tests::utils::{CaseError, SourceCase};

struct ValuePiCase;

impl ValuePiCase {
    fn run(source: &str) {
        SourceCase::run(source).unwrap();
    }

    fn assert_type_error(source: &str) {
        match SourceCase::check(source) {
            | Err(error) if error.is_type_error() => {}
            | Ok(()) => panic!("expected a type error, but the program was accepted"),
            | Err(error) => panic!("expected a type error, found: {error:?}"),
        }
    }

    fn assert_first_class_error(source: &str) {
        match SourceCase::check(source) {
            | Err(CaseError::Compile(CompileError::Rejected(analysis))) => {
                let diagnostics = analysis
                    .outcome()
                    .diagnostics()
                    .expect("a rejected analysis carries diagnostics");
                assert!(
                    diagnostics.iter().any(|diagnostic| {
                        diagnostic.code.as_str() == "tyck.first-class-value-function"
                    }),
                    "expected a second-class value-function rejection, found: {:?}",
                    diagnostics.iter().map(|diagnostic| &diagnostic.message).collect::<Vec<_>>()
                );
            }
            | Err(error) => {
                panic!("expected a first-class value-function error, found: {error:?}")
            }
            | Ok(()) => {
                panic!("expected a first-class value-function error, but the program was accepted")
            }
        }
    }
}

#[test]
fn pipelines_apply_value_functions_in_both_directions() {
    ValuePiCase::run(
        r#"
begin
  let val keep (A : VType) (value : A) : A = value that
  let forward : Int64 = 0 |> keep Int64 that
  let backward : Int64 = keep Int64 <| forward that
  ! exit backward
end
"#,
    );
}

#[test]
fn explicit_value_pi_classifies_a_value_abstraction() {
    ValuePiCase::run(
        r#"
begin
  let identity : val pi (A : VType) (value : A) . A =
    val (A : VType) (value : A) => value
  that
  let recovered : Unit = () |> identity Unit that
  ! exit 0
end
"#,
    );
}

#[test]
fn param_val_introduces_lexical_and_block_value_functions() {
    ValuePiCase::run(
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
    );
}

#[test]
fn plain_param_does_not_infer_a_value_function() {
    ValuePiCase::assert_type_error(
        r#"
begin
  let invalid = param (value : Unit) in value that
  ! exit 0
end
"#,
    );
}

#[test]
fn value_functions_apply_through_partial_type_instantiation() {
    ValuePiCase::run(
        r#"
begin
  let val keep (A : VType) (value : A) : A = value that
  let keep_unit : val pi (value : Unit) . Unit = keep Unit that
  let recovered : Unit = () |> keep_unit that
  ! exit 0
end
"#,
    );
}

#[test]
fn value_functions_reject_storage_in_products() {
    ValuePiCase::assert_first_class_error(
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
    );
}

#[test]
fn value_functions_reject_being_returned_by_computations() {
    ValuePiCase::assert_first_class_error(
        r#"
begin
  let val keep (value : Unit) : Unit = value that
  do escaped <- ret keep;
  ! exit 0
end
"#,
    );
}

#[test]
fn value_functions_reject_higher_order_domains() {
    ValuePiCase::assert_first_class_error(
        r#"
begin
  let val apply_twice (function : val pi (_ : Unit) . Unit) : Unit =
    () |> function
  that
  ! exit 0
end
"#,
    );
}

#[test]
fn value_functions_reject_constructor_payload_types() {
    ValuePiCase::assert_first_class_error(
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
    );
}

#[test]
fn value_function_bodies_reject_computations() {
    ValuePiCase::assert_type_error(
        r#"
begin
  let invalid : val pi (_ : Unit) . Unit =
    val (_ : Unit) => ret ()
  that
  ! exit 0
end
"#,
    );
}

#[test]
fn package_witness_instantiation_follows_the_parameter_pattern() {
    ValuePiCase::run(
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
    );
}

#[test]
fn value_pi_composes_multiple_package_openings_in_product_order() {
    ValuePiCase::run(
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
    );
}

#[test]
fn value_functions_capture_runtime_values() {
    ValuePiCase::run(
        r#"
begin
  do captured <- ret 0;
  let val constant (_ : Unit) : Int64 = captured in
  let status = () |> constant in
  ! exit status
end
"#,
    );
}

#[test]
fn value_functions_share_the_value_namespace() {
    ValuePiCase::run(
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
    );
}

#[test]
fn value_function_bindings_are_non_recursive() {
    ValuePiCase::assert_type_error(
        r#"
begin
  let val loop (value : Unit) : Unit = value |> loop that
  ! exit 0
end
"#,
    );
}

#[test]
fn value_function_parameters_must_be_irrefutable() {
    ValuePiCase::assert_type_error(
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
    );
}

#[test]
fn pipelines_respect_curried_binder_order() {
    ValuePiCase::assert_type_error(
        r#"
begin
  let val keep (A : VType) (value : A) : A = value that
  let invalid = () |> keep that
  ! exit 0
end
"#,
    );
}

#[test]
fn computation_abstraction_does_not_stand_in_for_val() {
    ValuePiCase::assert_type_error(
        r#"
begin
  let identity = fn (value : Unit) => value that
  ! exit 0
end
"#,
    );
}
