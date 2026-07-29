use std::path::PathBuf;
use zydeco_driver::{BuildError, BuildSystem, check::err::CompileError};

struct ExistentialCase;

impl ExistentialCase {
    fn check(source: &str) -> Result<(), BuildError> {
        let case_dir = tempfile::tempdir().unwrap();
        let source_path = case_dir.path().join("existential.zy");
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
fn accepts_payload_at_its_fresh_witness() {
    ExistentialCase::check(
        r#"
alias Box =
  exists (X : VType) . X * Thk (X -> Ret Int)
end

def boxed : Box = (
  Int,
  0,
  { fn (x : Int) -> ret x },
) end

main
  match boxed
  | (X, value, consume) ->
    do status <- ! consume value;
    ! exit status
  end
end
"#,
    )
    .unwrap();
}

#[test]
fn scopes_opened_witnesses_over_let_and_function_bodies() {
    ExistentialCase::check(
        r#"
alias Box =
  exists (X : VType) . X * Thk (X -> Ret Int)
end

def boxed : Box = (
  Int,
  0,
  { fn (x : Int) -> ret x },
) end

def consume_box : Thk (Box -> Ret Int) = {
  fn ((X, value, consume) : Box) -> ! consume value
} end

main
  let (Y, value, consume) = boxed in
  do from_let <- ! consume value;
  do from_function <- ! consume_box boxed;
  ! exit from_function
end
"#,
    )
    .unwrap();
}

#[test]
fn scopes_an_opened_witness_over_a_do_tail() {
    ExistentialCase::check(
        r#"
alias Box =
  exists (X : VType) . X * Thk (X -> Ret Int)
end

def boxed : Box = (
  Int,
  0,
  { fn (x : Int) -> ret x },
) end

def yield_box : Thk (Ret Box) = { ret boxed } end

main
  do (X, value, consume) <- ! yield_box;
  do status <- ! consume value;
  ! exit status
end
"#,
    )
    .unwrap();
}

#[test]
fn rejects_mixing_payloads_from_distinct_openings() {
    ExistentialCase::assert_type_error(
        r#"
alias Box =
  exists (X : VType) . X * Thk (X -> Ret Int)
end

def ints : Box = (
  Int,
  0,
  { fn (x : Int) -> ret x },
) end

def chars : Box = (
  Char,
  'z',
  { fn (_ : Char) -> ret 0 },
) end

main
  match ints
  | (XI, xi, _) ->
    match chars
    | (XC, _, from_char) ->
      do status <- ! from_char xi;
      ! exit status
    end
  end
end
"#,
    );
}

#[test]
fn rejects_an_opened_witness_in_the_result_type() {
    ExistentialCase::assert_type_error(
        r#"
alias Box = exists (X : VType) . X end

def boxed : Box = (Int, 0) end

def leak = {
  match boxed
  | (X, value) -> ret value
  end
} end

main ! exit 0 end
"#,
    );
}

#[test]
fn synthesizes_a_package_dependent_function_result() {
    ExistentialCase::check(
        r#"
alias Box = exists (X : VType) . X end

def unpack = {
  fn ((X, value) : Box) -> ret value
} end

main ! exit 0 end
"#,
    )
    .unwrap();
}

#[test]
fn allows_repacking_an_opened_witness() {
    ExistentialCase::check(
        r#"
alias Box = exists (X : VType) . X end

def repack : Thk (Box -> Ret Box) = {
  fn (boxed : Box) ->
    match boxed
    | (X, value) -> ret (X, value)
    end
} end

main ! exit 0 end
"#,
    )
    .unwrap();
}
