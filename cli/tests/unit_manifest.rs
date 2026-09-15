use std::path::{Path, PathBuf};
use zydeco_cli::{
    library::{LibraryDigest, LibraryError, LibraryFile, LibraryPlatform, LinkedLibraries},
    unit::{UnitEntryProfile, UnitManifest},
};
use zydeco_syntax::{ForeignAbi, ForeignSymbolName, IntegerType, UnitImport, UnitValueType};

struct Fixture {
    directory: tempfile::TempDir,
}

impl Fixture {
    fn new() -> Self {
        Self { directory: tempfile::tempdir().unwrap() }
    }

    fn publish(
        &self, label: &str, package: &str, exports: UnitValueType, dependencies: &[PathBuf],
        imports: Vec<UnitImport>,
    ) -> (PathBuf, UnitManifest) {
        let directory = self.directory.path().join(label);
        std::fs::create_dir(&directory).unwrap();
        std::fs::write(directory.join("unit.o"), label).unwrap();
        std::fs::write(directory.join("imports.zy"), exports.source().unwrap()).unwrap();
        let manifest = UnitManifest {
            schema: 1,
            abi: ForeignAbi::Zydeco,
            profile: UnitEntryProfile::WordInitializerV1,
            package: package.parse().unwrap(),
            platform: LibraryPlatform::Macos,
            compiler: LibraryDigest::file(&std::env::current_exe().unwrap()).unwrap(),
            model: zydeco_machine::native::ENTRY_SYMBOL.into(),
            runtime: "fixture-runtime".into(),
            symbol: ForeignSymbolName::parse(format!("zydeco_unit_{label}_init")).unwrap(),
            exports,
            artifact: LibraryFile {
                path: "unit.o".into(),
                sha3: LibraryDigest::file(&directory.join("unit.o")).unwrap(),
            },
            bindings: LibraryFile {
                path: "imports.zy".into(),
                sha3: LibraryDigest::file(&directory.join("imports.zy")).unwrap(),
            },
            imports,
            dependencies: dependencies
                .iter()
                .map(|path| LibraryFile {
                    path: path.clone(),
                    sha3: LibraryDigest::file(path).unwrap(),
                })
                .collect(),
        };
        let path = directory.join("manifest.unit.json");
        Self::write(&path, &manifest);
        (path, manifest)
    }

    fn write(path: &Path, manifest: &UnitManifest) {
        std::fs::write(path, serde_json::to_vec_pretty(manifest).unwrap()).unwrap();
    }

    fn validate(paths: &[PathBuf], imports: &[UnitImport]) -> Result<(), LibraryError> {
        LinkedLibraries::load(paths)?.validate_units(
            imports,
            LibraryPlatform::Macos,
            Some("fixture-runtime"),
            true,
        )
    }
}

#[test]
fn native_profiles_check_types_identity_and_content_before_linking() {
    let fixture = Fixture::new();
    let (path, manifest) = fixture.publish(
        "scalar",
        "example/scalar",
        UnitValueType::Integer(IntegerType::Int),
        &[],
        vec![],
    );
    let import = manifest.import();
    Fixture::validate(std::slice::from_ref(&path), std::slice::from_ref(&import)).unwrap();
    let mut wrong = import.clone();
    wrong.exports = UnitValueType::Integer(IntegerType::UInt);
    assert!(matches!(
        Fixture::validate(std::slice::from_ref(&path), &[wrong]),
        Err(LibraryError::UnitSignature(_))
    ));
    assert!(matches!(
        Fixture::validate(&[], std::slice::from_ref(&import)),
        Err(LibraryError::UnitSignature(_))
    ));
    let libraries = LinkedLibraries::load(std::slice::from_ref(&path)).unwrap();
    assert!(matches!(
        libraries.validate_units(
            std::slice::from_ref(&import),
            LibraryPlatform::Macos,
            Some("fixture-runtime"),
            false
        ),
        Err(LibraryError::UnitTarget)
    ));
    assert!(matches!(
        libraries.validate_units(
            std::slice::from_ref(&import),
            LibraryPlatform::Linux,
            Some("fixture-runtime"),
            true
        ),
        Err(LibraryError::UnitTarget)
    ));

    for changed in [
        UnitManifest { compiler: "different compiler".into(), ..manifest.clone() },
        UnitManifest { runtime: "different runtime".into(), ..manifest.clone() },
        UnitManifest { model: "different model".into(), ..manifest.clone() },
    ] {
        Fixture::write(&path, &changed);
        assert!(matches!(
            Fixture::validate(std::slice::from_ref(&path), std::slice::from_ref(&import)),
            Err(LibraryError::Runtime(_) | LibraryError::UnitCompiler(_))
        ));
    }
    Fixture::write(&path, &manifest);
    let original: serde_json::Value =
        serde_json::from_slice(&std::fs::read(&path).unwrap()).unwrap();
    for (field, value) in [
        ("schema", serde_json::json!(2)),
        ("profile", serde_json::json!("fresh-fatal-guarded-v1")),
        ("exports", serde_json::json!({"product": []})),
        ("exports", serde_json::json!({"named": {"name": "x) in ret 0", "value": "unit"}})),
        ("exports", serde_json::json!({"unknown": "?"})),
    ] {
        let mut invalid = original.clone();
        invalid[field] = value;
        std::fs::write(&path, serde_json::to_vec(&invalid).unwrap()).unwrap();
        assert!(
            LinkedLibraries::load(std::slice::from_ref(&path)).is_err(),
            "accepted invalid {field}"
        );
    }
    Fixture::write(&path, &manifest);
    for artifact in [&manifest.artifact, &manifest.bindings] {
        let file = path.parent().unwrap().join(&artifact.path);
        let bytes = std::fs::read(&file).unwrap();
        std::fs::write(&file, "corrupted").unwrap();
        assert!(matches!(
            LinkedLibraries::load(std::slice::from_ref(&path)),
            Err(LibraryError::Hash(_))
        ));
        std::fs::write(file, bytes).unwrap();
    }
}

#[test]
fn dependency_closures_share_one_artifact_and_reject_hidden_or_conflicting_providers() {
    let fixture = Fixture::new();
    let (leaf, leaf_manifest) =
        fixture.publish("leaf", "example/leaf", UnitValueType::Unit, &[], vec![]);
    let (left, _) = fixture.publish(
        "left",
        "example/left",
        UnitValueType::Unit,
        std::slice::from_ref(&leaf),
        vec![leaf_manifest.import()],
    );
    let (right, _) = fixture.publish(
        "right",
        "example/right",
        UnitValueType::Unit,
        std::slice::from_ref(&leaf),
        vec![leaf_manifest.import()],
    );
    Fixture::validate(&[left.clone(), right], &[leaf_manifest.import()]).unwrap();

    let (hidden, _) = fixture.publish(
        "hidden",
        "example/hidden",
        UnitValueType::Unit,
        &[],
        vec![leaf_manifest.import()],
    );
    assert!(matches!(Fixture::validate(&[hidden, leaf], &[]), Err(LibraryError::UnitSignature(_))));
    let (conflict, _) = fixture.publish(
        "conflict",
        "example/leaf",
        UnitValueType::Integer(IntegerType::Int),
        &[],
        vec![],
    );
    assert!(matches!(LinkedLibraries::load(&[left, conflict]), Err(LibraryError::Conflict(_))));
}
