use zydeco_tests::utils::{CaseError, SourceCase};

struct NamedCase;

impl NamedCase {
    fn check(source: &str) -> Result<(), CaseError> {
        SourceCase::check(source)
    }

    fn check_monadic(source: &str) -> Result<(), CaseError> {
        SourceCase::check_monadic(source)
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
fn accepts_named_types_named_kinds_and_static_projection() {
    NamedCase::check(
        r#"
begin
  let Identity :
    (VType) -> (VType) =
    begin
      param (A : VType) that
      A
    end
  that

  let NamedIdentity :
    (constructor :: ((VType) -> (VType))) =
    (constructor = Identity)
  that

  let NamedInt : (item :: VType) =
    (item = (NamedIdentity/constructor Int))
  that

  let ProjectedInt : VType = NamedInt/item that
  let PayloadOf :
    (item :: VType) -> (VType) =
    begin
      param ((item = A) : (item :: VType)) that
      A
    end
  that
  let WholePayloadOf :
    (item :: VType) -> (VType) =
    begin
      param (Whole : (item :: VType)) that
      Whole/item
    end
  that
  let PatternProjectedInt : VType =
    PayloadOf NamedInt
  that
  let WholeProjectedInt : VType =
    WholePayloadOf NamedInt
  that

  let NamedOS : (operation :: CType) =
    (operation = OS)
  that
  let ProjectedOS : CType = NamedOS/operation that

  let PunnedNamedType :
    (VType) ->
    (Punned :: VType) =
    begin
      param (Punned : VType) that
      (= Punned)
    end
  that
  let PunnedNamedInt : (Punned :: VType) =
    PunnedNamedType Int
  that
  let PunnedProjectedInt : VType =
    PunnedNamedInt/Punned
  that

  let NestedNamedInt :
    (outer :: (inner :: VType)) =
    (outer = (inner = Int))
  that
  let NestedProjectedInt : VType =
    NestedNamedInt/outer/inner
  that

  def value : ProjectedInt = 0 that
  def pattern_value : PatternProjectedInt = 1 that
  def whole_value : WholeProjectedInt = 2 that
  def nested_value : NestedProjectedInt = 3 that
  def punned_value : PunnedProjectedInt = 4 that

  (
    value = value,
    pattern_value = pattern_value,
    whole_value = whole_value,
    nested_value = nested_value,
    punned_value = punned_value,
  )
end
"#,
    )
    .unwrap();
}

#[test]
fn accepts_named_type_patterns_in_polymorphic_functions() {
    NamedCase::check(
        r#"
begin
  def named_identity : Thk (
    forall ((item = A) : (item :: VType)) .
      A -> Ret A
  ) = {
    fn ((item = A) : (item :: VType))
       (value : A) =>
      ret value
  } that

  { ! named_identity (item = Int) 0 }
end
"#,
    )
    .unwrap();
}

#[test]
fn translates_named_type_applications_in_monadic_blocks() {
    NamedCase::check_monadic(
        r#"
begin
  def mo_ret : Thk (Monad Ret) = {
    comatch
    | .return A value => ret value
    | .bind A B computation continuation =>
      do value <- ! computation;
      ! continuation value
    end
  } that

  def translated : Thk (Ret Unit) = {
    (monadic
      let named_identity = {
        fn ((item = A) : (item :: VType))
           (value : A) =>
          ret value
      } in
      ! named_identity (item = Unit) ()
    end)
    Ret
    { ! mo_ret }
  } that

  def translated_polymorphic : Thk (
    forall ((item = A) : (item :: VType)) .
      Thk Top -> A -> Ret A
  ) = {
    (monadic
      do _ <- ret ();
      fn ((item = A) : (item :: VType))
         (value : A) =>
        ret value
    end)
    Ret
    { ! mo_ret }
  } that

  (
    translated = translated,
    translated_polymorphic = translated_polymorphic,
  )
end
"#,
    )
    .unwrap();
}

#[test]
fn distinguishes_payload_and_whole_named_existential_binders() {
    NamedCase::check(
        r#"
begin
  let PayloadBox =
    exists (
      (item = A) :
      (item :: VType)
    ) . A
  that

  let WholeBox =
    exists (
      Whole :
      (item :: VType)
    ) . Whole/item
  that

  def payload_box : PayloadBox = (item = Int, 41) that
  def whole_box : WholeBox = (item = Int, 42) that

  let (item = A, payload) = payload_box in
  let (Whole, whole) = whole_box in
  (payload_box, whole_box)
end
"#,
    )
    .unwrap();
}

#[test]
fn instantiates_package_dependent_results_from_named_witnesses() {
    NamedCase::check(
        r#"
begin
  let Box =
    exists (
      (item = A) :
      (item :: VType)
    ) . A
  that

  let Reveal =
    pi ((item = A, _) : Box) . Ret A
  that

  def reveal : Thk Reveal = {
    fn ((item = A, value) : Box) => ret value
  } that

  def boxed : Box = (item = Int, 41) that

  { ! reveal boxed }
end
"#,
    )
    .unwrap();
}

#[test]
fn rejects_named_term_with_mismatched_label() {
    NamedCase::assert_type_error(
        r#"
begin
  def bad : (x :: Int) = (y = 0) that
  bad
end
"#,
    );
}

#[test]
fn rejects_named_pattern_with_mismatched_label() {
    NamedCase::assert_type_error(
        r#"
begin
  def value : (x :: Int) = (x = 0) that
  let (y = inner) = value in
  inner
end
"#,
    );
}

#[test]
fn rejects_named_pattern_on_unnamed_mixed_component() {
    NamedCase::assert_type_error(
        r#"
begin
  let Mixed = (left :: Int) * (Int * (right :: Int)) that
  def value : Mixed = (left = 1, 2, right = 3) that
  let (
    left = left : Int,
    middle = middle : Int,
    right = right : Int
  ) = value in
  (left, middle, right)
end
"#,
    );
}

#[test]
fn rejects_mismatched_named_pattern_in_nested_mixed_product() {
    NamedCase::assert_type_error(
        r#"
begin
  let Nested = ((left :: Int) * Int) * (right :: Int) that
  def value : Nested = ((left = 1, 2), right = 3) that
  let (
    (wrong = left : Int, middle : Int),
    right = right : Int
  ) = value in
  (left, middle, right)
end
"#,
    );
}

#[test]
fn rejects_incompatible_named_payload_annotation_in_mixed_pattern() {
    NamedCase::assert_type_error(
        r#"
begin
  let Mixed = (left :: Int) * (Int * (right :: Int)) that
  def value : Mixed = (left = 1, 2, right = 3) that
  let (
    left = left : String,
    middle : Int,
    right = right : Int
  ) = value in
  (left, middle, right)
end
"#,
    );
}

#[test]
fn rejects_mismatched_named_type_label() {
    NamedCase::assert_type_error(
        r#"
begin
  let InvalidNamedType : (operation :: CType) =
    (other = OS)
  that
  ()
end
"#,
    );
}

#[test]
fn rejects_named_computation_classifiers_without_named_computations() {
    NamedCase::assert_type_error(
        r#"
begin
  let InvalidNamedComputation : CType =
    (operation :: OS)
  that
  ()
end
"#,
    );
}

#[test]
fn rejects_missing_named_type_projection() {
    NamedCase::assert_type_error(
        r#"
begin
  let NamedInt : (item :: VType) =
    (item = Int)
  that
  let InvalidProjection : VType =
    NamedInt/other
  that
  ()
end
"#,
    );
}

#[test]
fn rejects_missing_named_projection() {
    NamedCase::assert_type_error(
        r#"
begin
  let Point = (x :: Int) * (y :: Int) that
  def point : Point = (x = 0, y = 1) that
  point/z
end
"#,
    );
}

#[test]
fn rejects_ambiguous_named_projection() {
    NamedCase::assert_type_error(
        r#"
begin
  let DuplicateFields = (x :: Int) * (x :: Int) that
  def duplicate : DuplicateFields = (x = 0, x = 1) that
  duplicate/x
end
"#,
    );
}
