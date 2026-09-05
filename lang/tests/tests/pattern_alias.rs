use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

#[test]
fn rejects_refutable_alias_members() {
    SourceCase::assert_rejected(
        SourceCase::check(
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
        ),
        TyckDiagnosticCode::RefutablePatternAlias,
    );
}

#[test]
fn rejects_static_pattern_aliases() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  let (First; Second) : VType = Int64 in
  ()
end
"#,
        ),
        TyckDiagnosticCode::PatternAliasRequiresValue,
    );
}

#[test]
fn rejects_missing_field_projection_patterns() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  let Point = (#x :: Int64) * (#y :: Int64) that
  def point : Point = (#x = 1, #y = 2) that
  let (/z = missing) = point in
  missing
end
"#,
        ),
        TyckDiagnosticCode::MissingNamedField,
    );
}

#[test]
fn rejects_ambiguous_field_projection_patterns() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  let Ambiguous = (#left :: (#x :: Int64)) * (#right :: (#x :: Int64)) that
  def ambiguous : Ambiguous = (#left = #x = 1, #right = #x = 2) that
  let (/x = duplicate) = ambiguous in
  duplicate
end
"#,
        ),
        TyckDiagnosticCode::DuplicateNamedField,
    );
}

#[test]
fn rejects_refutable_field_projection_payloads() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
begin
  def Maybe : VType =
    data
    | +Some : Int64
    | +None : Unit
    end
  that
  let Box = (#value :: Maybe) that
  def boxed : Box = (#value = +Some(1)) that
  match boxed
  | (/value = +Some(payload)) => ret payload
  | (/value = +None()) => ret 0
  end
end
"#,
        ),
        TyckDiagnosticCode::RefutableFieldProjectionPattern,
    );
}
