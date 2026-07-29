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
