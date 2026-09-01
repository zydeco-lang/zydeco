use zydeco_tests::utils::SourceCase;

struct ValueViewCase;

impl ValueViewCase {
    fn run(source: &str) {
        SourceCase::run(source).unwrap();
    }
}

#[test]
fn view_patterns_precompose_matching_with_the_same_value_cut() {
    ValueViewCase::run(
        r#"
begin
  let Tagged =
    data
    | +Tagged : Int64
    | +Other : Unit
    end
  that
  let val tag (value : Int64) : Tagged = +Tagged(value) that
  let val other (_ : Int64) : Tagged = +Other(()) that
  match 0
  | other ~> +Tagged(_) => ! exit 1
  | tag ~> +Tagged(status) => ! exit status
  | _ => ! exit 1
  end
end
"#,
    );
}

#[test]
fn view_patterns_bind_the_transformed_value() {
    ValueViewCase::run(
        r#"
begin
  let val second ((_, value) : Unit * Int64) : Int64 = value that
  let second ~> status = ((), 0) that
  ! exit status
end
"#,
    );
}

#[test]
fn view_patterns_may_use_functions_with_erased_type_parameters() {
    ValueViewCase::run(
        r#"
begin
  let val keep (A : VType) (value : A) : A = value that
  let unit : Unit = () |> keep Unit that
  let keep[Unit] ~> recovered = unit that
  let _ : Unit = recovered that
  ! exit 0
end
"#,
    );
}
