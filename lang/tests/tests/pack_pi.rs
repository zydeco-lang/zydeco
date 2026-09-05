use zydeco_tests::utils::{CaseError, SourceCase};

struct PackPiCase;

impl PackPiCase {
    const RET_MONAD: &str = r#"
def mo_ret : Thk (Monad Ret) = {
  comatch
  | .return A value =>
    ret value
  | .bind A B computation continuation =>
    do value <- ! computation;
    ! continuation value
  end
} that
"#;

    fn check(source: &str) -> Result<(), CaseError> {
        SourceCase::check_value(source)
    }

    fn check_monadic(source: &str) -> Result<(), CaseError> {
        SourceCase::check_monadic_value(source)
    }

    fn run(source: &str) -> Result<(), CaseError> {
        SourceCase::run_monadic(&format!("begin\n{}\n{}\nend", Self::RET_MONAD, source))
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
fn synthesizes_a_package_dependent_arrow() {
    PackPiCase::check(
        r#"
begin
  let Core =
    exists (OS : CType) . Unit
  that

  let Binary =
    pi ((OS, _) : Core) . OS
  that

  let Export =
    exists (BinaryType as Binary : CType) . Unit
  that
  (Binary, ()) : Export
end
"#,
    )
    .unwrap();
}

#[test]
fn analyzes_a_package_dependent_arrow_as_ctype() {
    PackPiCase::check(
        r#"
begin
  let Core =
    exists (OS : CType) . Unit
  that

  let Binary : CType =
    pi ((OS, _) : Core) . OS
  that

  let Export =
    exists (BinaryType as Binary : CType) . Unit
  that
  (Binary, ()) : Export
end
"#,
    )
    .unwrap();
}

#[test]
fn scopes_multiple_package_witnesses_over_the_codomain() {
    PackPiCase::check(
        r#"
begin
  let Core =
    exists (A : VType) .
    exists (OS : CType) .
      Unit
  that

  let Binary : CType =
    pi ((A, OS, _) : Core) . A -> OS
  that

  let Export =
    exists (BinaryType as Binary : CType) . Unit
  that
  (Binary, ()) : Export
end
"#,
    )
    .unwrap();
}

#[test]
fn rejects_dependency_on_the_package_payload() {
    PackPiCase::assert_type_error(
        r#"
begin
  let Core =
    exists (OS : CType) . Int64
  that

  let Bad : CType =
    pi ((OS, payload) : Core) . payload
  that

  ()
end
"#,
    );
}

#[test]
fn checks_and_applies_a_package_dependent_abstraction() {
    PackPiCase::check(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that

  let Unbox =
    pi ((X, _) : Box) . Ret X
  that

  def unbox : Thk Unbox = {
    fn ((X, value) : Box) => ret value
  } that

  def boxed : Box = (Int64, 41) that

  { ! unbox boxed }
end
"#,
    )
    .unwrap();
}

#[test]
fn synthesizes_a_package_dependent_abstraction() {
    PackPiCase::check(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that

  def unbox = {
    fn ((X, value) : Box) => ret value
  } that

  def boxed : Box = (Int64, 41) that

  { ! unbox boxed }
end
"#,
    )
    .unwrap();
}

#[test]
fn instantiates_multiple_package_witnesses() {
    PackPiCase::check(
        r#"
begin
  let PairBox =
    exists (X : VType) .
    exists (Y : VType) .
      X * Y
  that

  let UnboxPair =
    pi ((X, Y, _, _) : PairBox) . Ret (X * Y)
  that

  def unbox_pair : Thk UnboxPair = {
    fn ((X, Y, x, y) : PairBox) => ret (x, y)
  } that

  def boxed : PairBox = (Int64, Char, 41, 'z') that

  { ! unbox_pair boxed }
end
"#,
    )
    .unwrap();
}

#[test]
fn preserves_an_opened_witness_across_applications() {
    PackPiCase::check(
        r#"
begin
  let Box =
    exists (X : VType) .
      X * Thk (X -> Ret Int64)
  that

  let Reveal =
    pi ((X, _, _) : Box) . Ret X
  that

  def reveal : Thk Reveal = {
    fn ((X, value, _) : Box) => ret value
  } that

  def consume_twice = {
    fn ((X, value, consume) : Box) =>
      do first <- ! reveal (X, value, consume);
      do second <- ! reveal (X, first, consume);
      ! consume second
  } that

  def boxed : Box = (
    Int64,
    41,
    { fn (value : Int64) => ret value },
  ) that

  { ! consume_twice boxed }
end
"#,
    )
    .unwrap();
}

#[test]
fn checks_selective_package_patterns_against_canonical_witnesses() {
    PackPiCase::check(
        r#"
begin
  let Box =
    exists (#Item = ItemType : VType) .
      (#value :: ItemType)
  that

  let Reveal =
    pi ((#Item = ItemType, _) : Box) . Ret ItemType
  that

  def reveal : Thk Reveal = {
    fn ((#Item = ItemType, = value) : Box) => ret value
  } that

  def forward : Thk Reveal = {
    fn ((/Item; /value; whole) : Box) => ! reveal whole
  } that

  forward
end
"#,
    )
    .unwrap();
}

#[test]
fn selective_builtin_parameters_open_modular_groups() {
    PackPiCase::check(
        r#"
begin
  def selective = {
    fn ((
      /core;
      /representations;
      /text;
      builtin
    ) : Builtin) =>
      let (/VType = SelectedVType) = core in
      let (/Bytes = SelectedBytes) = representations/bytes in
      let bytes = text/bytes in
      let Selected : SelectedVType = SelectedBytes in
      ! bytes/empty
  } that

  selective
end
"#,
    )
    .unwrap();
}

#[test]
fn infers_a_hole_in_the_package_dependent_codomain() {
    PackPiCase::check(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that

  let Unbox : CType =
    pi ((X, _) : Box) . (_ : CType)
  that

  def unbox : Thk Unbox = {
    fn ((X, value) : Box) => ret value
  } that

  def boxed : Box = (Int64, 41) that

  { ! unbox boxed }
end
"#,
    )
    .unwrap();
}

#[test]
fn rejects_application_to_a_package_with_hidden_witnesses() {
    PackPiCase::assert_type_error(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that

  let Unbox =
    pi ((X, _) : Box) . Ret X
  that

  def unbox : Thk Unbox = {
    fn ((X, value) : Box) => ret value
  } that

  def hidden : Thk (Box -> Ret Int64) = {
    fn (boxed : Box) =>
      do _ <- ! unbox boxed;
      ret 0
  } that

  hidden
end
"#,
    );
}

#[test]
fn translates_package_dependent_functions_in_monadic_blocks() {
    PackPiCase::check_monadic(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that

  def translated = {
    @[monadic] begin
      let unbox = {
        do _ <- ret ();
        fn ((X, value) : Box) => ret value
      } in
      ! unbox (Unit, ())
    end
  } that

  translated
end
"#,
    )
    .unwrap();
}

#[test]
fn translates_multiple_package_witnesses_and_their_structures() {
    PackPiCase::check_monadic(
        r#"
begin
  let Core =
    exists (A : VType) .
    exists (OS : CType) .
      A * Thk (A -> OS)
  that

  def translated = {
    @[monadic] begin
      let run = {
        do _ <- ret ();
        fn ((A, OS, value, execute) : Core) =>
          ! execute value
      } in
      ! run (
        Unit,
        Ret Unit,
        (),
        { fn (_ : Unit) => ret () },
      )
    end
  } that

  translated
end
"#,
    )
    .unwrap();
}

#[test]
fn runs_package_dependent_destructors_after_a_monadic_bind() {
    PackPiCase::run(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that

  let Service : CType =
    codata
    | .unbox : pi ((X, _) : Box) . Ret X
    end
  that

  def translated = {
    @[monadic] begin
      do _ <- ret ();
      (comatch
      | .unbox =>
        fn ((X, value) : Box) => ret value
      end : Service)
    end
  } that

  do value <- ! translated Ret { ! mo_ret } .unbox (Int64, triv, 41);
  do status <- ! api/int64/sub value 41;
  ! exit status
end
"#,
    )
    .unwrap();
}

#[test]
fn runs_package_dependent_destructors_with_an_abstract_computation_witness() {
    PackPiCase::run(
        r#"
begin
  let Core =
    exists (OS : CType) . Thk OS
  that

  let Runner : CType =
    codata
    | .run : pi ((OS, _) : Core) . OS
    end
  that

  def ret_algebra : Thk (
    forall (R : CType) .
      Algebra Ret R
  ) = {
    fn (R : CType) =>
      comatch X computation continuation =>
        do value <- ! computation;
        ! continuation value
      end
  } that

  def translated = {
    @[monadic] begin
      do _ <- ret ();
      (comatch
      | .run =>
        fn ((OS, computation) : Core) => ! computation
      end : Runner)
    end
  } that

  do value <- ! translated Ret { ! mo_ret } .run (
    Ret Int64,
    { ! ret_algebra (Ret Int64) },
    { ret 41 },
  );
  do status <- ! api/int64/sub value 41;
  ! exit status
end
"#,
    )
    .unwrap();
}

#[test]
fn translates_only_the_existential_prefix_opened_by_a_package_arrow() {
    PackPiCase::check_monadic(
        r#"
begin
  let Inner =
    exists (Y : VType) . Y
  that

  let NestedBox =
    exists (X : VType) . X * Inner
  that

  def translated = {
    @[monadic] begin
      let first = {
        do _ <- ret ();
        fn ((X, value, _) : NestedBox) => ret value
      } in
      ! first (Unit, (), (Unit, ()))
    end
  } that

  translated
end
"#,
    )
    .unwrap();
}

#[test]
fn translates_a_manifest_component_before_a_pack_pi_witness() {
    PackPiCase::check_monadic(
        r#"
begin
  let Mixed =
    exists (Y as Unit : VType) .
    exists (X : VType) .
    exists (Z as X : VType) .
      Z
  that

  def translated = {
    @[monadic] begin
      let reveal = {
        do _ <- ret ();
        fn ((Y, X, Z, value) : Mixed) => ret value
      } in
      ! reveal (Unit, Unit, Unit, ())
    end
  } that

  translated
end
"#,
    )
    .unwrap();
}

#[test]
fn preserves_an_opened_witness_across_monadic_applications() {
    PackPiCase::check_monadic(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that

  def translated = {
    @[monadic] begin
      let reveal = {
        do _ <- ret ();
        fn ((X, value) : Box) => ret value
      } in
      fn ((X, value) : Box) =>
        do first <- ! reveal (X, value);
        ! reveal (X, first)
    end
  } that

  translated
end
"#,
    )
    .unwrap();
}

#[test]
fn runs_package_dependent_destructors_from_monadic_blocks() {
    PackPiCase::run(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that

  let Service : CType =
    codata
    | .unbox : pi ((X, _) : Box) . Ret X
    end
  that

  def translated = {
    @[monadic] begin
      (comatch
      | .unbox =>
        fn ((X, value) : Box) => ret value
      end : Service)
    end
  } that

  do value <- ! translated Ret { ! mo_ret } .unbox (Int64, triv, 41);
  do status <- ! api/int64/sub value 41;
  ! exit status
end
"#,
    )
    .unwrap();
}

#[test]
fn runs_multiple_package_witnesses_and_their_structures() {
    PackPiCase::run(
        r#"
begin
  let Core =
    exists (A : VType) .
    exists (OS : CType) .
      A * Thk (A -> OS)
  that

  def translated = {
    @[monadic] begin
      let run = {
        do _ <- ret ();
        fn ((A, OS, value, execute) : Core) =>
          ! execute value
      } in
      ! run (
        Int64,
        Ret Int64,
        0,
        { fn (value : Int64) => ret value },
      )
    end
  } that

  do status <- ! translated Ret { ! mo_ret };
  ! exit status
end
"#,
    )
    .unwrap();
}

#[test]
#[ignore = "n-ary products: the monadic translation of a package payload literal needs realignment"]
fn runs_with_an_unopened_existential_package_in_the_payload() {
    PackPiCase::run(
        r#"
begin
  let Inner =
    exists (Y : VType) . Y
  that

  let NestedBox =
    exists (X : VType) . X * Inner
  that

  def translated = {
    @[monadic] begin
      do _ <- ret ();
      fn ((X, value, _) : NestedBox) => ret value
    end
  } that

  do value <- ! translated Ret { ! mo_ret } (
    Int64,
    triv,
    41,
    (Unit, triv, ()),
  );
  do status <- ! api/int64/sub value 41;
  ! exit status
end
"#,
    )
    .unwrap();
}

#[test]
fn runs_repeated_package_applications_with_one_opened_witness() {
    PackPiCase::run(
        r#"
begin
  let Box =
    exists (X : VType) . X
  that

  def translated = {
    @[monadic] begin
      let reveal = {
        do _ <- ret ();
        fn ((X, value) : Box) => ret value
      } in
      fn ((X, value) : Box) =>
        do first <- ! reveal (X, value);
        ! reveal (X, first)
    end
  } that

  do value <- ! translated Ret { ! mo_ret } (Int64, triv, 41);
  do status <- ! api/int64/sub value 41;
  ! exit status
end
"#,
    )
    .unwrap();
}
