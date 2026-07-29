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
fn rejects_named_term_with_mismatched_label() {
    NamedCase::assert_type_error(
        r#"
def bad : (x = Int) = (y = 0) end
main ! exit 0 end
"#,
    );
}

#[test]
fn rejects_named_pattern_with_mismatched_label() {
    NamedCase::assert_type_error(
        r#"
def value : (x = Int) = (x = 0) end
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
alias Mixed = (left = Int) * (Int * (right = Int)) end
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
alias Nested = ((left = Int) * Int) * (right = Int) end
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
alias Mixed = (left = Int) * (Int * (right = Int)) end
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
fn rejects_named_computation_type_during_type_checking() {
    NamedCase::assert_type_error(
        r#"
alias InvalidNamedType = (operation = OS) end
main ! exit 0 end
"#,
    );
}

#[test]
fn rejects_missing_named_projection() {
    NamedCase::assert_type_error(
        r#"
alias Point = (x = Int) * (y = Int) end
def point : Point = (x = 0, y = 1) end
main ! exit (point/z) end
"#,
    );
}

#[test]
fn rejects_ambiguous_named_projection() {
    NamedCase::assert_type_error(
        r#"
alias DuplicateFields = (x = Int) * (x = Int) end
def duplicate : DuplicateFields = (x = 0, x = 1) end
main ! exit (duplicate/x) end
"#,
    );
}
