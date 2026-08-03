use zydeco_tests::utils::SourceCase;

struct QuantifierCase;

impl QuantifierCase {
    fn check(source: &str) {
        SourceCase::check(source).unwrap();
    }
}

#[test]
fn accepts_multiple_parameters_for_dependent_quantifiers() {
    QuantifierCase::check(
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
    exists (PublicMixedPackage as MixedPackage : VType) . Unit
  that
  (MixedPackage, ()) : Export
end
"#,
    );
}
