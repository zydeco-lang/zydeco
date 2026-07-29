use std::path::PathBuf;
use zydeco_driver::{BuildError, BuildSystem, check::err::CompileError};

struct PackPiCase;

impl PackPiCase {
    fn check(source: &str) -> Result<(), BuildError> {
        let case_dir = tempfile::tempdir().unwrap();
        let source_path = case_dir.path().join("pack-pi.zy");
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
