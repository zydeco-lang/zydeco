use zydeco_tests::utils::SourceCase;

#[test]
fn accepts_multiple_parameters_for_dependent_quantifiers() {
    SourceCase::assert_accepted(SourceCase::check_value(
        r#"
begin
  let ExplicitFunction :
    CType =
    pi (A : VType) (value : A) .
      Ret A
  that
  let Universal :
    CType =
    forall (A : VType) (value : A) .
      Ret A
  that
  let ExplicitPair :
    VType =
    sigma (X : VType) (value : X) .
      X
  that
  let Existential :
    VType =
    exists (X : VType) (value : X) .
      X
  that
  let MixedPackage :
    VType =
    exists
      (X : VType)
      (Alias as X : VType)
      (value : Alias)
    .
      X
  that

  let Export =
    exists (MixedPackageType as MixedPackage : VType) . Unit
  that
  (MixedPackage, ()) : Export
end
"#,
    ));
}
