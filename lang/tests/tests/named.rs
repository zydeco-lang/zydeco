use std::path::PathBuf;
use zydeco_driver::{BuildError, BuildSystem, check::err::CompileError};

struct NamedCase;

impl NamedCase {
    fn check(source: &str) -> Result<(), BuildError> {
        let case_dir = tempfile::tempdir().unwrap();
        let source_path = case_dir.path().join("named.zy");
        std::fs::write(&source_path, source).unwrap();

        let std_proj = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/std/proj.toml");
        let mut build_sys = BuildSystem::new();
        build_sys.add_local_package(std_proj).unwrap();
        let pack = build_sys.add_orphan_file(source_path).unwrap();
        build_sys.test_pack(pack, true)
    }

    fn assert_type_error(source: &str) {
        match Self::check(source) {
            | Err(BuildError::CompileError(
                CompileError::TyckErrorReports { .. } | CompileError::TyckErrors(_),
            )) => {}
            | Ok(()) => panic!("expected a type error, but the program was accepted"),
            | Err(error) => panic!("expected a type error, found: {error:?}"),
        }
    }
}

#[test]
fn accepts_named_types_named_kinds_and_static_projection() {
    NamedCase::check(
        r#"
alias Identity (A : VType) : VType = A end

alias NamedIdentity : (constructor :: (VType -> VType)) =
  (constructor = Identity)
end

alias NamedInt : (item :: VType) =
  (item = (NamedIdentity/constructor Int))
end

alias ProjectedInt : VType = NamedInt/item end
alias PayloadOf ((item = A) : (item :: VType)) : VType = A end
alias WholePayloadOf (Whole : (item :: VType)) : VType = Whole/item end
alias PatternProjectedInt : VType = PayloadOf NamedInt end
alias WholeProjectedInt : VType = WholePayloadOf NamedInt end

alias NamedOS : (operation :: CType) = (operation = OS) end
alias ProjectedOS : CType = NamedOS/operation end

alias PunnedNamedType (Punned : VType) : (Punned :: VType) =
  (= Punned)
end
alias PunnedNamedInt : (Punned :: VType) = PunnedNamedType Int end
alias PunnedProjectedInt : VType = PunnedNamedInt/Punned end

alias NestedNamedInt : (outer :: (inner :: VType)) =
  (outer = (inner = Int))
end
alias NestedProjectedInt : VType = NestedNamedInt/outer/inner end

def value : ProjectedInt = 0 end
def pattern_value : PatternProjectedInt = 1 end
def whole_value : WholeProjectedInt = 2 end
def nested_value : NestedProjectedInt = 3 end
def punned_value : PunnedProjectedInt = 4 end
main ! exit value end
"#,
    )
    .unwrap();
}

#[test]
fn accepts_named_type_patterns_in_polymorphic_functions() {
    NamedCase::check(
        r#"
def ! named_identity
  ((item = A) : (item :: VType))
  (value : A)
: Ret A =
  ret value
end

main
  do value <- ! named_identity (item = Int) 0;
  ! exit value
end
"#,
    )
    .unwrap();
}

#[test]
fn translates_named_type_applications_in_monadic_blocks() {
    NamedCase::check(
        r#"
def ! mo_ret : Monad Ret =
  comatch
  | .return A value -> ret value
  | .bind A B computation continuation ->
    do value <- ! computation;
    ! continuation value
  end
end

def ! translated : Ret Unit =
  (monadic
    let ! named_identity =
      fn ((item = A) : (item :: VType)) (value : A) ->
        ret value
    in
    ! named_identity (item = Unit) ()
  end)
  Ret
  { ! mo_ret }
end

def ! translated_polymorphic
: forall ((item = A) : (item :: VType)) .
    Thk Top -> A -> Ret A =
  (monadic
    do _ <- ret ();
    fn ((item = A) : (item :: VType)) (value : A) ->
      ret value
  end)
  Ret
  { ! mo_ret }
end

main
  do _ <- ! translated;
  do _ <- ! translated_polymorphic (item = Unit) triv ();
  ! exit 0
end
"#,
    )
    .unwrap();
}

#[test]
fn distinguishes_payload_and_whole_named_existential_binders() {
    NamedCase::check(
        r#"
alias PayloadBox =
  exists ((item = A) : (item :: VType)) . A
end

alias WholeBox =
  exists (Whole : (item :: VType)) . Whole/item
end

def payload_box : PayloadBox = (item = Int, 41) end
def whole_box : WholeBox = (item = Int, 42) end

main
  let (item = A, payload) = payload_box in
  let (Whole, whole) = whole_box in
  ! exit 0
end
"#,
    )
    .unwrap();
}

#[test]
fn instantiates_package_dependent_results_from_named_witnesses() {
    NamedCase::check(
        r#"
alias Box =
  exists ((item = A) : (item :: VType)) . A
end

alias Reveal =
  pi ((item = A, _) : Box) . Ret A
end

def reveal : Thk Reveal = {
  fn ((item = A, value) : Box) -> ret value
} end

def boxed : Box = (item = Int, 41) end

main
  do value <- ! reveal boxed;
  ! exit value
end
"#,
    )
    .unwrap();
}

#[test]
fn rejects_named_term_with_mismatched_label() {
    NamedCase::assert_type_error(
        r#"
def bad : (x :: Int) = (y = 0) end
main ! exit 0 end
"#,
    );
}

#[test]
fn rejects_named_pattern_with_mismatched_label() {
    NamedCase::assert_type_error(
        r#"
def value : (x :: Int) = (x = 0) end
main
  let (y = inner) = value in
  ! exit inner
end
"#,
    );
}

#[test]
fn rejects_named_pattern_on_unnamed_mixed_component() {
    NamedCase::assert_type_error(
        r#"
alias Mixed = (left :: Int) * (Int * (right :: Int)) end
def value : Mixed = (left = 1, 2, right = 3) end
main
  let (
    left = left : Int,
    middle = middle : Int,
    right = right : Int
  ) = value in
  ! exit 0
end
"#,
    );
}

#[test]
fn rejects_mismatched_named_pattern_in_nested_mixed_product() {
    NamedCase::assert_type_error(
        r#"
alias Nested = ((left :: Int) * Int) * (right :: Int) end
def value : Nested = ((left = 1, 2), right = 3) end
main
  let (
    (wrong = left : Int, middle : Int),
    right = right : Int
  ) = value in
  ! exit 0
end
"#,
    );
}

#[test]
fn rejects_incompatible_named_payload_annotation_in_mixed_pattern() {
    NamedCase::assert_type_error(
        r#"
alias Mixed = (left :: Int) * (Int * (right :: Int)) end
def value : Mixed = (left = 1, 2, right = 3) end
main
  let (
    left = left : String,
    middle : Int,
    right = right : Int
  ) = value in
  ! exit 0
end
"#,
    );
}

#[test]
fn rejects_mismatched_named_type_label() {
    NamedCase::assert_type_error(
        r#"
alias InvalidNamedType : (operation :: CType) = (other = OS) end
main ! exit 0 end
"#,
    );
}

#[test]
fn rejects_named_computation_classifiers_without_named_computations() {
    NamedCase::assert_type_error(
        r#"
alias InvalidNamedComputation : CType = (operation :: OS) end
main ! exit 0 end
"#,
    );
}

#[test]
fn rejects_missing_named_type_projection() {
    NamedCase::assert_type_error(
        r#"
alias NamedInt : (item :: VType) = (item = Int) end
alias InvalidProjection : VType = NamedInt/other end
main ! exit 0 end
"#,
    );
}

#[test]
fn rejects_missing_named_projection() {
    NamedCase::assert_type_error(
        r#"
alias Point = (x :: Int) * (y :: Int) end
def point : Point = (x = 0, y = 1) end
main ! exit (point/z) end
"#,
    );
}

#[test]
fn rejects_ambiguous_named_projection() {
    NamedCase::assert_type_error(
        r#"
alias DuplicateFields = (x :: Int) * (x :: Int) end
def duplicate : DuplicateFields = (x = 0, x = 1) end
main ! exit (duplicate/x) end
"#,
    );
}
