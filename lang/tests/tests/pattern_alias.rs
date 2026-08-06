use zydeco_tests::utils::SourceCase;

struct PatternAliasCase;

impl PatternAliasCase {
    fn assert_type_error(source: &str) {
        match SourceCase::check(source) {
            | Err(error) if error.is_type_error() => {}
            | Ok(()) => panic!("expected a type error, but the program was accepted"),
            | Err(error) => panic!("expected a type error, found: {error:?}"),
        }
    }
}

#[test]
fn rejects_refutable_alias_members() {
    PatternAliasCase::assert_type_error(
        r#"
begin
  def Bool : VType =
    data
    | +True : Unit
    | +False : Unit
    end
  that
  def truth : Bool = +True() that
  match truth
  | (whole; +True()) => ret ()
  | +False() => ret ()
  end
end
"#,
    );
}

#[test]
fn rejects_static_pattern_aliases() {
    PatternAliasCase::assert_type_error(
        r#"
begin
  let (First; Second) : VType = Int in
  ()
end
"#,
    );
}
