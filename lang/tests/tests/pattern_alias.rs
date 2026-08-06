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

#[test]
fn rejects_missing_field_projection_patterns() {
    PatternAliasCase::assert_type_error(
        r#"
begin
  let Point = (x :: Int) * (y :: Int) that
  def point : Point = (x = 1, y = 2) that
  let (/z = missing) = point in
  missing
end
"#,
    );
}

#[test]
fn rejects_ambiguous_field_projection_patterns() {
    PatternAliasCase::assert_type_error(
        r#"
begin
  let Ambiguous = (left :: (x :: Int)) * (right :: (x :: Int)) that
  def ambiguous : Ambiguous = (left = x = 1, right = x = 2) that
  let (/x = duplicate) = ambiguous in
  duplicate
end
"#,
    );
}

#[test]
fn rejects_refutable_field_projection_payloads() {
    PatternAliasCase::assert_type_error(
        r#"
begin
  def Maybe : VType =
    data
    | +Some : Int
    | +None : Unit
    end
  that
  let Box = (value :: Maybe) that
  def boxed : Box = (value = +Some(1)) that
  match boxed
  | (/value = +Some(payload)) => ret payload
  | (/value = +None()) => ret 0
  end
end
"#,
    );
}
