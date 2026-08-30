use zydeco_tests::utils::{CaseError, SourceCase};

struct ExistentialCase;

impl ExistentialCase {
    fn check(source: &str) -> Result<(), CaseError> {
        SourceCase::check(source)
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
fn opens_a_manifest_witness_as_its_disclosed_type() {
    ExistentialCase::check(
        r#"
begin
  let Transparent =
    exists (X as Int64 : VType) . X
  that
  def packed : Transparent = (Int64, 42) that
  def consume : Thk (Transparent -> Ret Int64) = {
    fn ((X, value) : Transparent) => ret value
  } that

  { ! consume packed }
end
"#,
    )
    .unwrap();
}

#[test]
fn rejects_a_witness_that_disagrees_with_the_manifest_definition() {
    ExistentialCase::assert_type_error(
        r#"
begin
  let Transparent =
    exists (X as Int64 : VType) . X
  that
  def packed : Transparent = (Char, 'x') that

  packed
end
"#,
    );
}

#[test]
fn composes_manifest_existentials_with_named_package_fields() {
    ExistentialCase::check(
        r#"
begin
  let CounterLibrary =
    exists (#Counter = ((Representation as Int64) : VType)) .
      (#zero :: Representation)
  that
  def library : CounterLibrary = (
    #Counter = Int64,
    #zero = 0,
  ) that
  def consume : Thk (CounterLibrary -> Ret Int64) = {
    fn ((= Counter, = zero) : CounterLibrary) => ret zero
  } that

  { ! consume library }
end
"#,
    )
    .unwrap();
}

#[test]
fn projection_patterns_select_types_and_values_from_one_package_opening() {
    ExistentialCase::check(
        r#"
begin
  let Box =
    exists (#Item = Hidden : VType) .
      (#value :: Hidden) *
      (#consume :: Thk (Hidden -> Ret Int64))
  that
  def boxed : Box = (
    #Item = Int64,
    #value = 41,
    #consume = { fn value => ret value },
  ) that

  {
    let (/Item; /value; /consume) = boxed in
    ! consume value
  }
end
"#,
    )
    .unwrap();
}

#[test]
fn projection_patterns_treat_plain_existential_binders_as_punned_fields() {
    ExistentialCase::check(
        r#"
begin
  let Box =
    exists (Item : VType) .
      (#value :: Item)
  that
  def boxed : Box = (Int64, #value = 42) that

  let (/Item; /value) = boxed in
  def selected : Item = value in
  ()
end
"#,
    )
    .unwrap();
}

#[test]
fn projection_patterns_can_name_one_type_field_twice() {
    ExistentialCase::check(
        r#"
begin
  let Box =
    exists (Item : VType) .
      (#value :: Item)
  that
  def boxed : Box = (Int64, #value = 42) that

  let (/Item = Left; /Item = Right; /value) = boxed in
  def left : Left = value in
  def right : Right = left in
  ()
end
"#,
    )
    .unwrap();
}

#[test]
fn projection_patterns_reject_a_missing_package_field() {
    ExistentialCase::assert_type_error(
        r#"
begin
  let Box =
    exists (Item : VType) .
      (#value :: Item)
  that
  def boxed : Box = (Int64, #value = 42) that

  let (/Missing) = boxed in
  ()
end
"#,
    );
}

#[test]
fn projection_patterns_reject_an_ambiguous_static_and_value_field() {
    ExistentialCase::assert_type_error(
        r#"
begin
  let Box =
    exists (Item : VType) .
      (#Item :: Item)
  that
  def boxed : Box = (Int64, #Item = 42) that

  let (/Item) = boxed in
  ()
end
"#,
    );
}

#[test]
fn substitutes_an_outer_abstract_witness_through_a_manifest_definition() {
    ExistentialCase::check(
        r#"
begin
  let Mixed =
    exists (X : VType) .
    exists (Y as X : VType) .
      Y
  that
  def packed : Mixed = (Int64, Int64, 7) that
  def unpack = {
    fn ((X, Y, value) : Mixed) => ret value
  } that

  { ! unpack packed }
end
"#,
    )
    .unwrap();
}

#[test]
fn skips_a_leading_manifest_component_when_instantiating_pack_pi() {
    ExistentialCase::check(
        r#"
begin
  let Mixed =
    exists (Y as Int64 : VType) .
    exists (X : VType) .
      X
  that
  def packed : Mixed = (Int64, Int64, 9) that
  def unpack = {
    fn ((Y, X, value) : Mixed) => ret value
  } that

  { ! unpack packed }
end
"#,
    )
    .unwrap();
}

#[test]
fn accepts_payload_at_its_fresh_witness() {
    ExistentialCase::check(
        r#"
begin
  let Box =
    exists (X : VType) . X * Thk (X -> Ret Int64)
  that
  def boxed : Box = (
    Int64,
    0,
    { fn (x : Int64) => ret x },
  ) that

  {
    match boxed
    | (X, value, consume) =>
      do status <- ! consume value;
      ret status
    end
  }
end
"#,
    )
    .unwrap();
}

#[test]
fn scopes_opened_witnesses_over_let_and_function_bodies() {
    ExistentialCase::check(
        r#"
begin
  let Box =
    exists (X : VType) . X * Thk (X -> Ret Int64)
  that
  def boxed : Box = (
    Int64,
    0,
    { fn (x : Int64) => ret x },
  ) that
  def consume_box : Thk (Box -> Ret Int64) = {
    fn ((X, value, consume) : Box) => ! consume value
  } that

  {
    let (Y, value, consume) = boxed in
    do from_let <- ! consume value;
    do from_function <- ! consume_box boxed;
    ret from_function
  }
end
"#,
    )
    .unwrap();
}

#[test]
fn scopes_an_opened_witness_over_a_do_tail() {
    ExistentialCase::check(
        r#"
begin
  let Box =
    exists (X : VType) . X * Thk (X -> Ret Int64)
  that
  def boxed : Box = (
    Int64,
    0,
    { fn (x : Int64) => ret x },
  ) that
  def yield_box : Thk (Ret Box) = {
    ret boxed
  } that

  {
    do (X, value, consume) <- ! yield_box;
    do status <- ! consume value;
    ret status
  }
end
"#,
    )
    .unwrap();
}

#[test]
fn rejects_mixing_payloads_from_distinct_openings() {
    ExistentialCase::assert_type_error(
        r#"
begin
  let Box =
    exists (X : VType) . X * Thk (X -> Ret Int64)
  that
  def ints : Box = (
    Int64,
    0,
    { fn (x : Int64) => ret x },
  ) that
  def chars : Box = (
    Char,
    'z',
    { fn (_ : Char) => ret 0 },
  ) that

  {
    match ints
    | (XI, xi, _) =>
      match chars
      | (XC, _, from_char) =>
        do status <- ! from_char xi;
        ret status
      end
    end
  }
end
"#,
    );
}

#[test]
fn rejects_an_opened_witness_in_the_result_type() {
    ExistentialCase::assert_type_error(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that
  def boxed : Box = (Int64, 0) that
  def leak = {
    match boxed
    | (X, value) => ret value
    end
  } that

  leak
end
"#,
    );
}

#[test]
fn synthesizes_a_package_dependent_function_result() {
    ExistentialCase::check(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that
  def unpack = {
    fn ((X, value) : Box) => ret value
  } that

  unpack
end
"#,
    )
    .unwrap();
}

#[test]
fn allows_repacking_an_opened_witness() {
    ExistentialCase::check(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that
  def repack : Thk (Box -> Ret Box) = {
    fn (boxed : Box) =>
      match boxed
      | (X, value) => ret (X, value)
      end
  } that

  repack
end
"#,
    )
    .unwrap();
}
