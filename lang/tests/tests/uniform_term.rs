use std::path::PathBuf;
use zydeco_driver::{BuildError, BuildSystem, PackId, check::err::CompileError};

struct UniformTermCase;

impl UniformTermCase {
    fn check(source: &str) -> Result<(), BuildError> {
        let case_dir = tempfile::tempdir().unwrap();
        let source_path = case_dir.path().join("uniform-term.zy");
        std::fs::write(&source_path, source).unwrap();

        let std_proj = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/std/proj.toml");
        let mut build_system = BuildSystem::new();
        build_system.add_local_package(std_proj).unwrap();
        let package: PackId = build_system.add_orphan_file(source_path).unwrap();
        build_system.test_pack(package, true)
    }

    fn assert_resolve_error(source: &str) {
        match Self::check(source) {
            | Err(BuildError::CompileError(
                CompileError::ResolveErrorReport { .. } | CompileError::ResolveError(_),
            )) => {}
            | Ok(()) => panic!("expected a resolution error, but the program was accepted"),
            | Err(error) => panic!("expected a resolution error, found: {error:?}"),
        }
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
fn rejects_that_without_an_enclosing_block() {
    UniformTermCase::assert_resolve_error(
        r#"
alias Bad = param A that A end
"#,
    );
}

#[test]
fn rejects_a_recursive_parameter_component() {
    UniformTermCase::assert_resolve_error(
        r#"
alias Bad =
  begin
    param (A : A) that
    A
  end
end
"#,
    );
}

#[test]
fn rejects_a_recursive_value_definition() {
    UniformTermCase::assert_type_error(
        r#"
def bad = {
  begin
    def value : Int = value that
    ret ()
  end
} end
"#,
    );
}
