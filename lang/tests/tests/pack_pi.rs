use std::path::PathBuf;
use zydeco_driver::{BuildError, BuildSystem, PackId, check::err::CompileError};

struct PackPiCase;

impl PackPiCase {
    const RET_MONAD: &str = r#"
def ! mo-ret : Monad Ret =
  comatch
  | .return A value ->
    ret value
  | .bind A B computation continuation ->
    do value <- ! computation;
    ! continuation value
  end
end
"#;

    fn with_source(
        source: &str, test: impl FnOnce(&BuildSystem, PackId) -> Result<(), BuildError>,
    ) -> Result<(), BuildError> {
        let case_dir = tempfile::tempdir().unwrap();
        let source_path = case_dir.path().join("pack-pi.zy");
        std::fs::write(&source_path, source).unwrap();

        let std_proj = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/std/proj.toml");
        let mut build_sys = BuildSystem::new();
        build_sys.add_local_package(std_proj).unwrap();
        let pack = build_sys.add_orphan_file(source_path).unwrap();
        test(&build_sys, pack)
    }

    fn check(source: &str) -> Result<(), BuildError> {
        Self::with_source(source, |build_sys, pack| build_sys.test_pack(pack, true))
    }

    fn run(source: &str) -> Result<(), BuildError> {
        let source = format!("{}\n{source}", Self::RET_MONAD);
        Self::with_source(&source, |build_sys, pack| build_sys.test_pack(pack, false))
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
fn synthesizes_a_package_dependent_arrow() {
    PackPiCase::check(
        r#"
alias Core =
  exists (OS : CType) . Unit
end

alias Binary =
  pi ((OS, _) : Core) . OS
end

main ! exit 0 end
"#,
    )
    .unwrap();
}

#[test]
fn analyzes_a_package_dependent_arrow_as_ctype() {
    PackPiCase::check(
        r#"
alias Core =
  exists (OS : CType) . Unit
end

alias Binary : CType =
  pi ((OS, _) : Core) . OS
end

main ! exit 0 end
"#,
    )
    .unwrap();
}

#[test]
fn scopes_multiple_package_witnesses_over_the_codomain() {
    PackPiCase::check(
        r#"
alias Core =
  exists (A : VType) .
  exists (OS : CType) .
  Unit
end

alias Binary : CType =
  pi ((A, OS, _) : Core) . A -> OS
end

main ! exit 0 end
"#,
    )
    .unwrap();
}

#[test]
fn rejects_dependency_on_the_package_payload() {
    PackPiCase::assert_type_error(
        r#"
alias Core =
  exists (OS : CType) . Int
end

alias Bad : CType =
  pi ((OS, payload) : Core) . payload
end

main ! exit 0 end
"#,
    );
}

#[test]
fn checks_and_applies_a_package_dependent_abstraction() {
    PackPiCase::check(
        r#"
alias Box =
  exists (X : VType) . X
end

alias Unbox =
  pi ((X, _) : Box) . Ret X
end

def unbox : Thk Unbox = {
  fn ((X, value) : Box) -> ret value
} end

def boxed : Box = (Int, 41) end

main
  do value <- ! unbox boxed;
  ! exit value
end
"#,
    )
    .unwrap();
}

#[test]
fn synthesizes_a_package_dependent_abstraction() {
    PackPiCase::check(
        r#"
alias Box =
  exists (X : VType) . X
end

def unbox = {
  fn ((X, value) : Box) -> ret value
} end

def boxed : Box = (Int, 41) end

main
  do value <- ! unbox boxed;
  ! exit value
end
"#,
    )
    .unwrap();
}

#[test]
fn instantiates_multiple_package_witnesses() {
    PackPiCase::check(
        r#"
alias PairBox =
  exists (X : VType) .
  exists (Y : VType) .
  X * Y
end

alias UnboxPair =
  pi ((X, Y, _, _) : PairBox) . Ret (X * Y)
end

def unbox_pair : Thk UnboxPair = {
  fn ((X, Y, x, y) : PairBox) -> ret (x, y)
} end

def boxed : PairBox = (Int, Char, 41, 'z') end

main
  do (value, _) <- ! unbox_pair boxed;
  ! exit value
end
"#,
    )
    .unwrap();
}

#[test]
fn preserves_an_opened_witness_across_applications() {
    PackPiCase::check(
        r#"
alias Box =
  exists (X : VType) .
  X * Thk (X -> Ret Int)
end

alias Reveal =
  pi ((X, _, _) : Box) . Ret X
end

def reveal : Thk Reveal = {
  fn ((X, value, _) : Box) -> ret value
} end

def consume_twice : Thk (Box -> Ret Int) = {
  fn ((X, value, consume) : Box) ->
    do first <- ! reveal (X, value, consume);
    do second <- ! reveal (X, first, consume);
    ! consume second
} end

def boxed : Box = (
  Int,
  41,
  { fn (value : Int) -> ret value },
) end

main
  do status <- ! consume_twice boxed;
  ! exit status
end
"#,
    )
    .unwrap();
}

#[test]
fn infers_a_hole_in_the_package_dependent_codomain() {
    PackPiCase::check(
        r#"
alias Box =
  exists (X : VType) . X
end

alias Unbox : CType =
  pi ((X, _) : Box) . (_ : CType)
end

def unbox : Thk Unbox = {
  fn ((X, value) : Box) -> ret value
} end

def boxed : Box = (Int, 41) end

main
  do value <- ! unbox boxed;
  ! exit value
end
"#,
    )
    .unwrap();
}

#[test]
fn rejects_application_to_a_package_with_hidden_witnesses() {
    PackPiCase::assert_type_error(
        r#"
alias Box =
  exists (X : VType) . X
end

alias Unbox =
  pi ((X, _) : Box) . Ret X
end

def unbox : Thk Unbox = {
  fn ((X, value) : Box) -> ret value
} end

def hidden : Thk (Box -> Ret Int) = {
  fn (boxed : Box) ->
    do _ <- ! unbox boxed;
    ret 0
} end

main ! exit 0 end
"#,
    );
}

#[test]
fn translates_package_dependent_functions_in_monadic_blocks() {
    PackPiCase::check(
        r#"
alias Box =
  exists (X : VType) . X
end

def ! translated =
  monadic
    let unbox = {
      do _ <- ret ();
      fn ((X, value) : Box) -> ret value
    } in
    ! unbox (Unit, ())
  end
end

main ! exit 0 end
"#,
    )
    .unwrap();
}

#[test]
fn translates_multiple_package_witnesses_and_their_structures() {
    PackPiCase::check(
        r#"
alias Core =
  exists (A : VType) .
  exists (OS : CType) .
  A * Thk (A -> OS)
end

def ! translated =
  monadic
    let run = {
      do _ <- ret ();
      fn ((A, OS, value, execute) : Core) ->
        ! execute value
    } in
    ! run (
      Unit,
      Ret Unit,
      (),
      { fn (_ : Unit) -> ret () },
    )
  end
end

main ! exit 0 end
"#,
    )
    .unwrap();
}

#[test]
fn runs_package_dependent_destructors_after_a_monadic_bind() {
    PackPiCase::run(
        r#"
alias Box =
  exists (X : VType) . X
end

alias Service : CType =
  codata
  | .unbox : pi ((X, _) : Box) . Ret X
  end
end

def ! translated =
  monadic
    do _ <- ret ();
    (comatch
    | .unbox ->
      fn ((X, value) : Box) -> ret value
    end : Service)
  end
end

main
  do value <- ! translated Ret { ! mo-ret } .unbox (Int, triv, 41);
  do status <- ! sub value 41;
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
alias Core =
  exists (OS : CType) . Thk OS
end

alias Runner : CType =
  codata
  | .run : pi ((OS, _) : Core) . OS
  end
end

def ! ret-algebra (R : CType) : Algebra Ret R =
  comatch X computation continuation ->
    do value <- ! computation;
    ! continuation value
  end
end

def ! translated =
  monadic
    do _ <- ret ();
    (comatch
    | .run ->
      fn ((OS, computation) : Core) -> ! computation
    end : Runner)
  end
end

main
  do value <- ! translated Ret { ! mo-ret } .run (
    Ret Int,
    { ! ret-algebra (Ret Int) },
    { ret 41 },
  );
  do status <- ! sub value 41;
  ! exit status
end
"#,
    )
    .unwrap();
}

#[test]
fn translates_only_the_existential_prefix_opened_by_a_package_arrow() {
    PackPiCase::check(
        r#"
alias Inner =
  exists (Y : VType) . Y
end

alias NestedBox =
  exists (X : VType) . X * Inner
end

def ! translated =
  monadic
    let first = {
      do _ <- ret ();
      fn ((X, value, _) : NestedBox) -> ret value
    } in
    ! first (Unit, (), (Unit, ()))
  end
end

main ! exit 0 end
"#,
    )
    .unwrap();
}

#[test]
fn translates_a_manifest_component_before_a_pack_pi_witness() {
    PackPiCase::check(
        r#"
alias Mixed =
  exists (Y as Unit : VType) .
  exists (X : VType) .
  exists (Z as X : VType) .
    Z
end

def ! translated =
  monadic
    let reveal = {
      do _ <- ret ();
      fn ((Y, X, Z, value) : Mixed) -> ret value
    } in
    ! reveal (Unit, Unit, Unit, ())
  end
end

main ! exit 0 end
"#,
    )
    .unwrap();
}

#[test]
fn preserves_an_opened_witness_across_monadic_applications() {
    PackPiCase::check(
        r#"
alias Box =
  exists (X : VType) . X
end

def ! translated =
  monadic
    let reveal = {
      do _ <- ret ();
      fn ((X, value) : Box) -> ret value
    } in
    fn ((X, value) : Box) ->
      do first <- ! reveal (X, value);
      ! reveal (X, first)
  end
end

main ! exit 0 end
"#,
    )
    .unwrap();
}

#[test]
fn runs_package_dependent_destructors_from_monadic_blocks() {
    PackPiCase::run(
        r#"
alias Box =
  exists (X : VType) . X
end

alias Service : CType =
  codata
  | .unbox : pi ((X, _) : Box) . Ret X
  end
end

def ! translated =
  monadic
    (comatch
    | .unbox ->
      fn ((X, value) : Box) -> ret value
    end : Service)
  end
end

main
  do value <- ! translated Ret { ! mo-ret } .unbox (Int, triv, 41);
  do status <- ! sub value 41;
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
alias Core =
  exists (A : VType) .
  exists (OS : CType) .
  A * Thk (A -> OS)
end

def ! translated =
  monadic
    let run = {
      do _ <- ret ();
      fn ((A, OS, value, execute) : Core) ->
        ! execute value
    } in
    ! run (
      Unit,
      Ret Unit,
      (),
      { fn (_ : Unit) -> ret () },
    )
  end
end

main
  do _ <- ! translated Ret { ! mo-ret };
  ! exit 0
end
"#,
    )
    .unwrap();
}

#[test]
fn runs_with_an_unopened_existential_package_in_the_payload() {
    PackPiCase::run(
        r#"
alias Inner =
  exists (Y : VType) . Y
end

alias NestedBox =
  exists (X : VType) . X * Inner
end

def ! translated =
  monadic
    do _ <- ret ();
    fn ((X, value, _) : NestedBox) -> ret value
  end
end

main
  do value <- ! translated Ret { ! mo-ret } (Int, triv, 41, (Unit, triv, ()));
  do status <- ! sub value 41;
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
alias Box =
  exists (X : VType) . X
end

def ! translated =
  monadic
    let reveal = {
      do _ <- ret ();
      fn ((X, value) : Box) -> ret value
    } in
    fn ((X, value) : Box) ->
      do first <- ! reveal (X, value);
      ! reveal (X, first)
  end
end

main
  do value <- ! translated Ret { ! mo-ret } (Int, triv, 41);
  do status <- ! sub value 41;
  ! exit status
end
"#,
    )
    .unwrap();
}
