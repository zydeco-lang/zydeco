use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

zydeco_tests::runtime_source!(state_effect_order, "tests/monadic/value-functions.zy");

struct MonadicCase;

impl MonadicCase {
    fn source(body: &str) -> String {
        let basis =
            std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib/std/control/monad.zy");
        format!(
            r#"
let monadic_basis = @(import("{}")) in
let (/Monad; /Algebra) = builtin |> monadic_basis in
{body}
"#,
            basis.display()
        )
    }

    fn ret_program(body: &str) -> String {
        Self::source(&format!(
            r#"
let mo : Thk (Monad Ret) = {{
  comatch
  | .return A value => ret value
  | .bind A B computation continuation =>
      do value <- ! computation;
      ! continuation value
  end
}} in
do result <- (@[monadic] {body}) Ret mo;
do status <- ! int/sub result 1;
! exit status
"#
        ))
    }

    fn run(body: &str) {
        let source = Self::ret_program(body);
        SourceCase::assert_accepted(SourceCase::check_linted(&source));
        SourceCase::assert_accepted(SourceCase::run(&source));
        SourceCase::assert_accepted(SourceCase::lower(&source));
    }
}

const FACTORY: &str = r#"
begin
  let val make (B : CType) = {
    fn (k : Thk B) =>
      do _ <- ret ();
      ! k
  } in
  ! (make (Ret Int)) { ret 1 }
end
"#;

#[test]
fn computation_kind_factory_translates_with_its_structure() {
    SourceCase::assert_accepted(SourceCase::check_monadic(FACTORY));
    SourceCase::assert_accepted(SourceCase::check_monadic(&format!("@[monadic] {FACTORY}")));
    MonadicCase::run(FACTORY);
}

#[test]
fn annotated_partial_and_higher_order_value_functions_translate() {
    MonadicCase::run(
        r#"
begin
  let make : val pi (B : CType) . Thk (Thk B -> B) =
    val (Result : CType) => {
      fn (k : Thk Result) => do _ <- ret (); ! k
    }
  in
  let val apply (factory : val pi (B : CType) . Thk (Thk B -> B)) =
    factory (Ret Int)
  in
  let specialized = apply make in
  ! specialized { ret 1 }
end
"#,
    );
}

#[test]
fn generated_classifiers_have_one_structure_argument_per_type_parameter() {
    let source = MonadicCase::source(
        r#"
let Translated = @[typeof] @[monadic]
  fn (factory : val pi (A : VType) (B : CType) . Thk (A -> B)) => ret ()
in
let Implementation = @[typeof] ({
  fn (M : VType -> CType) (mo : Thk (Monad M))
    (factory : val pi (X : VType) (_ : Thk Top)
      (Y : CType) (_ : Thk (Algebra M Y)) . Thk (X -> Y)) =>
      ! mo .return Unit ()
} : Thk Translated) in
ret ()
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_linted(&source));
    SourceCase::assert_rejected(
        SourceCase::check(&source.replace("(_ : Thk (Algebra M Y))", "(_ : Int)")),
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn value_and_higher_kinds_and_alternating_arguments_translate() {
    MonadicCase::run(
        r#"
begin
  let val make (F : VType -> CType) (A : VType) (k : Thk (F A))
    (B : VType) (_ : B) = {
      do _ <- ret ();
      ! k
    }
  in
  ! (make Ret Int { ret 1 } Unit ())
end
"#,
    );
}

#[test]
fn named_type_patterns_preserve_their_payload_witness() {
    MonadicCase::run(
        r#"
begin
  let val make ((#result = B) : (#result :: CType)) = {
    fn (k : Thk B) => do _ <- ret (); ! k
  } in
  ! (make (#result = Ret Int)) { ret 1 }
end
"#,
    );
}

#[test]
fn package_parameters_reuse_carried_structures() {
    MonadicCase::run(
        r#"
begin
  let Box = exists (B : CType) . Thk B in
  let val make ((B, k) : Box) = {
    do _ <- ret ();
    ! k
  } in
  ! (make (Ret Int, { ret 1 }))
end
"#,
    );
}

#[test]
fn product_package_parameters_preserve_witness_order() {
    MonadicCase::run(
        r#"
begin
  let Box = exists (B : CType) . Thk B in
  let val make ((_ : Box), ((B, k) : Box)) = {
    do _ <- ret ();
    ! k
  } in
  ! (make ((Ret Unit, { ret () }), (Ret Int, { ret 1 })))
end
"#,
    );
}

#[test]
fn multiple_package_witnesses_translate() {
    MonadicCase::run(
        r#"
begin
  let Box = exists (A : VType) (B : CType) . A * Thk B in
  let val make ((A, B, (_, k)) : Box) = {
    do _ <- ret ();
    ! k
  } in
  ! (make (Unit, Ret Int, ((), { ret 1 })))
end
"#,
    );
}

#[test]
fn factories_export_thunks_through_packed_fields() {
    MonadicCase::run(
        r#"
begin
  let val make (B : CType) (k : Thk B)
    : exists (Result as B : CType) . (#run :: Thk Result) * Unit
    = (B, (#run = k, ())) in
  let exports = make (Ret Int) { ret 1 } in
  ! exports/run
end
"#,
    );
}

#[test]
fn hidden_nested_package_witnesses_are_rejected() {
    SourceCase::assert_rejected(
        SourceCase::check(
            r#"
let Box = exists (A : VType) . A * (exists (B : VType) . B) in
let val take ((A, (_, (B, value))) : Box) : B = value in
fn (hidden : Box) => let value = take hidden in ret ()
"#,
        ),
        TyckDiagnosticCode::PackageWitnessesUnavailable,
    );
}

#[test]
fn manifest_and_abstract_witnesses_in_product_parameters_translate() {
    MonadicCase::run(
        r#"
begin
  let Box = exists (A as Unit : VType) (B : CType) . A * Thk B in
  let val make ((_ : Unit), ((A, B, (_, k)) : Box)) = {
    do _ <- ret ();
    ! k
  } in
  ! (make ((), (Unit, Ret Int, ((), { ret 1 }))))
end
"#,
    );
}

#[test]
fn source_annotations_match_translated_package_routes() {
    let source = MonadicCase::source(
        r#"
let Box = exists (A : VType) (B : CType) . A * Thk B in
let Translated = @[typeof] @[monadic]
  fn (factory : val pi ((A, B, (_, k)) : Box) . Thk B) => ret ()
in
let Implementation = @[typeof] ({
  fn (M : VType -> CType) (mo : Thk (Monad M))
    (factory : val pi (
      (A, (_, (B, (_, (_, k)))) ) :
      exists (A : VType) . Thk Top *
        (exists (B : CType) . Thk (Algebra M B) * (A * Thk B))
    ) . Thk B) => ! mo .return Unit ()
} : Thk Translated) in
ret ()
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_linted(&source));
}

#[test]
fn nested_package_routes_instantiate_and_check_against_annotations() {
    SourceCase::assert_accepted(SourceCase::run(
        r#"
let Box = exists (A : VType) . A * (exists (B : VType) . B) in
let take : val pi ((A, (_, (B, value))) : Box) . B =
  val ((X, (_, (Y, value))) : Box) => value
in
let status : Int = take (Unit, ((), (Int, 0))) in
! exit status
"#,
    ));
}

#[test]
fn sibling_factories_and_nested_monadic_blocks_keep_distinct_structures() {
    MonadicCase::run(
        r#"
begin
  let val first (B : CType) = { fn (k : Thk B) => do _ <- ret (); ! k } in
  let val second (B : CType) = { fn (k : Thk B) => do _ <- ret (); ! k } in
  do _ <- ! (first (Ret Unit)) { ret () };
  ! (second (Ret Int)) { ret 1 }
end
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_monadic(&format!(
        "@[monadic] @[monadic] {FACTORY}"
    )));
}

#[test]
fn wrong_kind_label_and_argument_order_keep_source_diagnostics() {
    for (source, expected) in [
        (
            r#"@[monadic] let val f (B : CType) = () in ret (f Int)"#,
            TyckDiagnosticCode::KindMismatch,
        ),
        (
            r#"@[monadic] let val f ((#a = B) : (#a :: CType)) = () in ret (f (#b = Ret Int))"#,
            TyckDiagnosticCode::NamedLabelMismatch,
        ),
        (
            r#"@[monadic] let val f (B : CType) = () in ret (f ())"#,
            TyckDiagnosticCode::SortMismatch,
        ),
    ] {
        SourceCase::assert_rejected(SourceCase::check_monadic(source), expected);
    }
}

#[test]
fn free_runtime_capture_is_still_rejected() {
    SourceCase::assert_rejected(
        SourceCase::check_monadic(
            r#"
fn (captured : Int) => @[monadic]
  let val make (B : CType) = { fn (k : Thk B) => do _ <- ret captured; ! k } in
  ! (make (Ret Int)) { ret 1 }
"#,
        ),
        TyckDiagnosticCode::NotInlinable,
    );
}

#[test]
fn runtime_value_functions_still_fail_static_elimination() {
    SourceCase::assert_rejected(
        SourceCase::check_monadic(
            r#"
@[monadic] fn (unknown : val pi (B : CType) . Thk (Thk B -> B)) =>
  ! (unknown (Ret Int)) { ret 1 }
"#,
        ),
        TyckDiagnosticCode::StaticElimination,
    );
}
