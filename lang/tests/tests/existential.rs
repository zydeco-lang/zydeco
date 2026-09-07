use zydeco_session::DesugarError;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

#[test]
fn opens_a_manifest_witness_as_its_disclosed_type() {
    SourceCase::assert_accepted(SourceCase::check_value(
        r#"
begin
  let Transparent =
    exists (X as Int64 : VType) . X
  that
  def packed : Transparent = (Int64, 42) that
  let val disclose ((X, value) : Transparent) : Int64 = value that

  disclose packed
end
"#,
    ));
}

#[test]
fn rejects_a_witness_that_disagrees_with_the_manifest_definition() {
    SourceCase::assert_rejected(
        SourceCase::check_value(
            r#"
begin
  let Transparent =
    exists (X as Int64 : VType) . X
  that
  def packed : Transparent = (Char, 'x') that

  packed
end
"#,
        ),
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn composes_manifest_existentials_with_named_package_fields() {
    SourceCase::assert_accepted(SourceCase::check_value(
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
  let val disclose ((= Counter, = zero) : CounterLibrary) : Int64 = zero that

  disclose library
end
"#,
    ));
}

#[test]
fn projection_patterns_select_types_and_values_from_one_package_opening() {
    SourceCase::assert_accepted(SourceCase::check_value(
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
    ));
}

#[test]
fn projection_patterns_treat_plain_existential_binders_as_punned_fields() {
    SourceCase::assert_accepted(SourceCase::check_value(
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
    ));
}

#[test]
fn projection_patterns_can_name_one_type_field_twice() {
    SourceCase::assert_accepted(SourceCase::check_value(
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
    ));
}

#[test]
fn projection_patterns_reject_a_missing_package_field() {
    SourceCase::assert_rejected(
        SourceCase::check_value(
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
        ),
        TyckDiagnosticCode::MissingNamedField,
    );
}

#[test]
fn projection_patterns_reject_an_ambiguous_static_and_value_field() {
    SourceCase::assert_rejected(
        SourceCase::check_value(
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
        ),
        TyckDiagnosticCode::DuplicateNamedField,
    );
}

#[test]
fn substitutes_an_outer_abstract_witness_through_a_manifest_definition() {
    SourceCase::assert_accepted(SourceCase::check_value(
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
    ));
}

#[test]
fn skips_a_leading_manifest_component_when_instantiating_pack_pi() {
    SourceCase::assert_accepted(SourceCase::check_value(
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
    ));
}

#[test]
fn accepts_payload_at_its_fresh_witness() {
    SourceCase::assert_accepted(SourceCase::check_value(
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
    ));
}

#[test]
fn scopes_opened_witnesses_over_let_and_function_bodies() {
    SourceCase::assert_accepted(SourceCase::check_value(
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
  def consume_box = {
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
    ));
}

#[test]
fn scopes_an_opened_witness_over_a_do_tail() {
    SourceCase::assert_accepted(SourceCase::check_value(
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
  let val run_box ((X, value, consume) : Box) : Thk (Ret Int64) = {
    ! consume value
  } that

  {
    do status <- ! (boxed |> run_box);
    ret status
  }
end
"#,
    ));
}

#[test]
fn rejects_mixing_payloads_from_distinct_openings() {
    SourceCase::assert_rejected(
        SourceCase::check_value(
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
        ),
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn rejects_an_opened_witness_in_the_result_type() {
    SourceCase::assert_rejected(
        SourceCase::check_value(
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
        ),
        TyckDiagnosticCode::EscapingExistential,
    );
}

#[test]
fn synthesizes_a_package_dependent_function_result() {
    SourceCase::assert_accepted(SourceCase::check_value(
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
    ));
}

#[test]
fn allows_repacking_an_opened_witness() {
    SourceCase::assert_accepted(SourceCase::check_value(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that
  def boxed : Box = (Int64, 0) that
  let val repack ((X, value) : Box) : Box = (X, value) that

  boxed |> repack
end
"#,
    ));
}

#[test]
fn pack_synthesizes_a_manifest_existential_package() {
    SourceCase::assert_accepted(SourceCase::check_value(
        r#"
begin
  let Transparent =
    exists (X as Int64 : VType) . X
  that

  let val disclose ((X, value) : Transparent) : Int64 = value that

  let packed = pack (X as Int64 : VType) where (42 : X) end in
  disclose packed
end
"#,
    ));
}

#[test]
fn pack_infers_the_witness_classifier_from_the_definition() {
    SourceCase::assert_accepted(SourceCase::check_value(
        r#"
begin
  let Transparent =
    exists (X as Int64 : VType) . X
  that
  let val disclose ((X, value) : Transparent) : Int64 = value that

  let packed = pack (X as Int64) where (42 : X) end in
  disclose packed
end
"#,
    ));
}

#[test]
fn pack_takes_the_payload_type_verbatim() {
    SourceCase::assert_accepted(SourceCase::check_value(
        r#"
begin
  let Degenerate =
    exists (X as Int64 : VType) . Int64
  that
  let val disclose ((X, value) : Degenerate) : Int64 = value that

  let packed = pack (X as Int64 : VType) where 42 end in
  disclose packed
end
"#,
    ));
}

#[test]
fn pack_supports_witness_telescopes() {
    SourceCase::assert_accepted(SourceCase::check_value(
        r#"
begin
  let Mixed =
    exists (X as Int64 : VType) .
    exists (Y as Char : VType) .
      X
  that
  let val disclose ((X, Y, value) : Mixed) : Int64 = value that

  let mixed = pack (X as Int64 : VType) (Y as Char : VType) where (7 : X), end in
  disclose mixed
end
"#,
    ));
}

#[test]
fn pack_composes_named_witness_fields() {
    SourceCase::assert_accepted(SourceCase::check_value(
        r#"
begin
  let CounterLibrary =
    exists (#Counter = ((Representation as Int64) : VType)) .
      (#zero :: Representation)
  that
  let val disclose ((= Counter, = zero) : CounterLibrary) : Int64 = zero that

  let library =
    pack (#Counter = ((Representation as Int64) : VType))
    where #zero = (0 : Representation) end
  in
  disclose library
end
"#,
    ));
}

#[test]
fn pack_checks_against_an_expected_existential() {
    SourceCase::assert_accepted(SourceCase::check_value(
        r#"
begin
  let Transparent =
    exists (X as Int64 : VType) . X
  that
  def packed : Transparent = pack (X as Int64 : VType) where (42 : X) end that
  let val disclose ((X, value) : Transparent) : Int64 = value that

  disclose packed
end
"#,
    ));
}

#[test]
fn pack_elaborates_to_a_runtime_package() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
begin
  let Box = exists (X as Int64 : VType) . X that
  let val unpack ((X, value) : Box) : X = value that
  let packed = pack (X as Int64 : VType) where (0 : X) end in
  let result : Int64 = unpack packed in
  ! exit result
end
"#,
    ));
}

#[test]
fn rejects_a_pack_parameter_without_evidence() {
    SourceCase::assert_desugar_error(
        SourceCase::check_value(
            r#"
begin
  let packed = pack (X : VType) where (42 : X) end in
  packed
end
"#,
        ),
        |error| matches!(error, DesugarError::PackParameterNeedsEvidence(_)),
    );
}

#[test]
fn rejects_redundant_evidence_on_a_manifest_parameter() {
    SourceCase::assert_desugar_error(
        SourceCase::check_value(
            r#"
begin
  let packed = pack (X as Int64 : VType) is Char where (42 : X) end in
  packed
end
"#,
        ),
        |error| matches!(error, DesugarError::PackParameterRedundantEvidence(_)),
    );
}

#[test]
fn pack_synthesizes_a_sealed_dependent_existential_package() {
    SourceCase::assert_accepted(SourceCase::check_value(
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
  let val repack ((S, state) : Library) : Library = (S, state) that

  let library =
    pack (S : VType) is Switch where #state = (+On () : Switch) end
  in
  library |> repack
end
"#,
    ));
}

#[test]
fn sealed_pack_elaborates_to_a_runtime_package() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
begin
  let Sealed = exists (X : VType) . Int64 that
  let packed = pack (X : VType) is Int64 where 0 end in
  match packed
  | (X, value) => ! exit 0
  end
end
"#,
    ));
}

#[test]
fn sealed_pack_composes_with_a_disclosed_telescope() {
    SourceCase::assert_accepted(SourceCase::check_value(
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
    ));
}

#[test]
fn sealed_pack_composes_named_witness_fields() {
    SourceCase::assert_accepted(SourceCase::check_value(
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
    ));
}

#[test]
fn sealed_pack_takes_a_dependent_payload_annotation() {
    SourceCase::assert_accepted(SourceCase::check_value(
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
    ));
}

#[test]
fn rejects_a_computation_payload() {
    SourceCase::assert_rejected(
        SourceCase::check_value(
            r#"
begin
  let packed = pack (X as Int64 : VType) where ret 42 end in
  packed
end
"#,
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn nests_packages_in_products_and_named_components() {
    SourceCase::assert_accepted(SourceCase::check_value(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that
  def boxed : Box = (Int64, 0) that
  let Module = exists (M : VType) . (Box * Int64) * (#peer :: Box) that
  def module : Module = ((Int64, (boxed, 0), #peer = boxed)) that

  module
end
"#,
    ));
}

#[test]
fn accepts_a_plain_computation_arrow_over_a_package_domain() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that
  def consume : Thk (Box -> Ret Int64) = {
    fn (boxed : Box) => ret 0
  } that
  do value <- ! consume (Int64, 0);
  ! exit value
end
"#,
    ));
}

#[test]
fn accepts_returning_a_package_from_a_computation() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that
  def boxed : Box = (Int64, 0) that
  def producer : Thk (Ret Box) = {
    ret boxed
  } that
  do returned <- ! producer;
  let (X, value) = returned in
  ! exit 0
end
"#,
    ));
}

#[test]
fn accepts_a_package_in_a_constructor_payload() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that
  def boxed : Box = (Int64, 0) that
  def Holder = data | +Hold : Box end that
  do stored <- ret (+Hold(boxed) : Holder);
  match stored
  | +Hold(value) => ! exit 0
  end
end
"#,
    ));
}
