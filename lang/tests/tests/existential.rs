use zydeco_tests::utils::{CaseError, SourceCase};

struct ExistentialCase;

impl ExistentialCase {
    fn check(source: &str) -> Result<(), CaseError> {
        SourceCase::check_value(source)
    }

    fn run(source: &str) -> Result<(), CaseError> {
        SourceCase::run(source)
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

#[test]
fn pack_synthesizes_a_manifest_existential_package() {
    ExistentialCase::check(
        r#"
begin
  let Transparent =
    exists (X as Int64 : VType) . X
  that
  def consume : Thk (Transparent -> Ret Int64) = {
    fn ((X, value) : Transparent) => ret value
  } that

  let packed = pack (X as Int64 : VType) where (42 : X) end in
  { ! consume packed }
end
"#,
    )
    .unwrap();
}

#[test]
fn pack_infers_the_witness_classifier_from_the_definition() {
    ExistentialCase::check(
        r#"
begin
  let Transparent =
    exists (X as Int64 : VType) . X
  that
  def consume : Thk (Transparent -> Ret Int64) = {
    fn ((X, value) : Transparent) => ret value
  } that

  let packed = pack (X as Int64) where (42 : X) end in
  { ! consume packed }
end
"#,
    )
    .unwrap();
}

#[test]
fn pack_takes_the_payload_type_verbatim() {
    ExistentialCase::check(
        r#"
begin
  let Degenerate =
    exists (X as Int64 : VType) . Int64
  that
  def consume : Thk (Degenerate -> Ret Int64) = {
    fn ((X, value) : Degenerate) => ret value
  } that

  let packed = pack (X as Int64 : VType) where 42 end in
  { ! consume packed }
end
"#,
    )
    .unwrap();
}

#[test]
fn pack_supports_witness_telescopes() {
    ExistentialCase::check(
        r#"
begin
  let Mixed =
    exists (X as Int64 : VType) .
    exists (Y as Char : VType) .
      X
  that
  def unpack : Thk (Mixed -> Ret Int64) = {
    fn ((X, Y, value) : Mixed) => ret value
  } that

  let mixed = pack (X as Int64 : VType) (Y as Char : VType) where (7 : X), end in
  { ! unpack mixed }
end
"#,
    )
    .unwrap();
}

#[test]
fn pack_composes_named_witness_fields() {
    ExistentialCase::check(
        r#"
begin
  let CounterLibrary =
    exists (#Counter = ((Representation as Int64) : VType)) .
      (#zero :: Representation)
  that
  def consume : Thk (CounterLibrary -> Ret Int64) = {
    fn ((= Counter, = zero) : CounterLibrary) => ret zero
  } that

  let library =
    pack (#Counter = ((Representation as Int64) : VType))
    where #zero = (0 : Representation) end
  in
  { ! consume library }
end
"#,
    )
    .unwrap();
}

#[test]
fn pack_checks_against_an_expected_existential() {
    ExistentialCase::check(
        r#"
begin
  let Transparent =
    exists (X as Int64 : VType) . X
  that
  def packed : Transparent = pack (X as Int64 : VType) where (42 : X) end that
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
fn pack_elaborates_to_a_runtime_package() {
    ExistentialCase::run(
        r#"
begin
  let Box = exists (X as Int64 : VType) . X that
  let val unpack ((X, value) : Box) : X = value that
  let packed = pack (X as Int64 : VType) where (0 : X) end in
  let result : Int64 = unpack packed in
  ! exit result
end
"#,
    )
    .unwrap();
}

#[test]
fn rejects_a_pack_parameter_without_evidence() {
    match ExistentialCase::check(
        r#"
begin
  let packed = pack (X : VType) where (42 : X) end in
  packed
end
"#,
    ) {
        | Err(_) => {}
        | Ok(()) => panic!("expected an error, but the program was accepted"),
    }
}

#[test]
fn rejects_redundant_evidence_on_a_manifest_parameter() {
    match ExistentialCase::check(
        r#"
begin
  let packed = pack (X as Int64 : VType) is Char where (42 : X) end in
  packed
end
"#,
    ) {
        | Err(_) => {}
        | Ok(()) => panic!("expected an error, but the program was accepted"),
    }
}

#[test]
fn pack_synthesizes_a_sealed_dependent_existential_package() {
    ExistentialCase::check(
        r#"
begin
  def Switch =
    data
    | +Off : Unit
    | +On : Unit
    end
  that
  let Library =
    exists (S : VType) . (#state :: S)
  that
  def repack : Thk (Library -> Ret Library) = {
    fn ((S, state) : Library) => ret (S, state)
  } that

  let library =
    pack (S : VType) is Switch where #state = (+On () : Switch) end
  in
  { ! repack library }
end
"#,
    )
    .unwrap();
}

#[test]
fn sealed_pack_elaborates_to_a_runtime_package() {
    ExistentialCase::run(
        r#"
begin
  let Sealed = exists (X : VType) . Int64 that
  let packed = pack (X : VType) is Int64 where 0 end in
  match packed
  | (X, value) => ! exit 0
  end
end
"#,
    )
    .unwrap();
}

#[test]
fn sealed_pack_composes_with_a_disclosed_telescope() {
    ExistentialCase::check(
        r#"
begin
  let Mixed =
    exists (Y as Char : VType) (X : VType) . Y * Int64
  that
  def packed : Mixed =
    pack (Y as Char : VType) (X : VType) is Int64 where ('x' : Y, 42) end
  that

  packed
end
"#,
    )
    .unwrap();
}

#[test]
fn sealed_pack_composes_named_witness_fields() {
    ExistentialCase::check(
        r#"
begin
  def Switch =
    data
    | +Off : Unit
    | +On : Unit
    end
  that
  let CounterLibrary =
    exists (#Counter = Representation : VType) . (#zero :: Representation)
  that
  def library : CounterLibrary =
    pack (#Counter = Representation : VType) is Switch
      where #zero = (+Off () : Switch) end
  that

  library
end
"#,
    )
    .unwrap();
}

#[test]
fn sealed_pack_takes_a_dependent_payload_annotation() {
    ExistentialCase::check(
        r#"
begin
  def Switch =
    data
    | +Off : Unit
    | +On : Unit
    end
  that
  let Library =
    exists (B : VType) . (#value :: B)
  that
  def library : Library =
    pack (B : VType) is Switch where ((#value = +On ()) : (#value :: B)) end
  that

  library
end
"#,
    )
    .unwrap();
}

#[test]
fn rejects_a_computation_payload() {
    ExistentialCase::assert_type_error(
        r#"
begin
  let packed = pack (X as Int64 : VType) where ret 42 end in
  packed
end
"#,
    );
}
