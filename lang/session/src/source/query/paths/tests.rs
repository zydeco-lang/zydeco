use super::*;

struct Fixture {
    directory: tempfile::TempDir,
    session: CompilerSession,
    importer: PathBuf,
}

impl Fixture {
    fn new() -> Self {
        let directory = tempfile::tempdir().unwrap();
        let importer = directory.path().join("main.zy");
        std::fs::write(&importer, "()").unwrap();
        Self { directory, session: CompilerSession::default(), importer }
    }

    fn write(&self, path: &str) -> PathBuf {
        let path = self.directory.path().join(path);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(&path, "()").unwrap();
        path
    }

    fn overlay(&mut self, path: &str) {
        self.session.set_overlay(self.directory.path().join(path), "()".to_owned()).unwrap();
    }

    fn candidates(&self, directory: &str, prefix: &str) -> Vec<SourcePathCandidate> {
        self.session.complete_source_paths(&self.importer, Path::new(directory), prefix).unwrap()
    }

    fn names(&self, directory: &str, prefix: &str) -> Vec<String> {
        self.candidates(directory, prefix)
            .into_iter()
            .map(|candidate| candidate.path.file_name().unwrap().to_str().unwrap().to_owned())
            .collect()
    }
}

#[test]
fn source_paths_include_supported_files_and_directories_in_stable_order() {
    let fixture = Fixture::new();
    fixture.write("alpha.zy");
    fixture.write("alpha.zyi");
    fixture.write("runner.zydeco");
    fixture.write("README.md");
    fixture.write("unsupported.ZY");
    fixture.write("no-extension");
    fixture.write("z-library/leaf.zy");
    assert_eq!(fixture.names("", ""), ["z-library", "alpha.zy", "alpha.zyi", "runner.zydeco"]);
    assert_eq!(
        fixture.candidates("", "").into_iter().map(|candidate| candidate.kind).collect::<Vec<_>>(),
        [
            SourcePathCandidateKind::Directory,
            SourcePathCandidateKind::File(SourceKind::Implementation),
            SourcePathCandidateKind::File(SourceKind::Signature),
            SourcePathCandidateKind::File(SourceKind::Program),
        ]
    );
    assert_eq!(fixture.names("", "alpha"), ["alpha.zy", "alpha.zyi"]);
    assert!(fixture.names("", "missing").is_empty());
}

#[test]
fn source_paths_resolve_relative_parent_and_absolute_directories() {
    let mut fixture = Fixture::new();
    fixture.write("root.zy");
    fixture.write("nested/child.zy");
    fixture.importer = fixture.write("nested/main.zy");
    assert_eq!(fixture.names(".", "child"), ["child.zy"]);
    assert_eq!(fixture.names("..", "root"), ["root.zy"]);
    assert_eq!(fixture.names("../nested", "child"), ["child.zy"]);
    let absolute = fixture.directory.path().to_str().unwrap();
    assert_eq!(fixture.names(absolute, "root"), ["root.zy"]);
    assert!(fixture.names("missing", "").is_empty());
    assert!(fixture.names("child.zy", "").is_empty());
}

#[test]
fn source_paths_merge_active_overlays_and_discover_virtual_directories() {
    let mut fixture = Fixture::new();
    fixture.write("shared.zy");
    fixture.overlay("shared.zy");
    fixture.overlay("unsaved.zyi");
    fixture.overlay("virtual/deeper/leaf.zy");
    fixture.overlay("ignored.txt");
    assert_eq!(fixture.names("", ""), ["virtual", "shared.zy", "unsaved.zyi"]);
    assert_eq!(fixture.names("virtual", ""), ["deeper"]);
    assert_eq!(fixture.names("virtual/deeper", ""), ["leaf.zy"]);

    fixture.session.clear_overlay(fixture.directory.path().join("unsaved.zyi")).unwrap();
    fixture.session.clear_overlay(fixture.directory.path().join("virtual/deeper/leaf.zy")).unwrap();
    assert_eq!(fixture.names("", ""), ["shared.zy"]);
}

#[test]
fn source_paths_observe_disk_changes_without_cached_analysis() {
    let fixture = Fixture::new();
    assert!(fixture.names("", "").is_empty());
    let added = fixture.write("added.zy");
    assert_eq!(fixture.names("", ""), ["added.zy"]);
    std::fs::remove_file(added).unwrap();
    assert!(fixture.names("", "").is_empty());
}

#[test]
fn source_paths_exclude_an_overlay_only_importer() {
    let mut fixture = Fixture::new();
    fixture.importer = fixture.directory.path().join("virtual/main.zy");
    fixture.overlay("virtual/main.zy");
    fixture.overlay("virtual/sibling.zy");
    assert_eq!(fixture.names("", ""), ["sibling.zy"]);
}

#[cfg(unix)]
#[test]
fn source_paths_follow_symlinks_and_exclude_self_import_aliases() {
    use std::os::unix::fs::symlink;

    let fixture = Fixture::new();
    let library = fixture.write("library.zy");
    fixture.write("nested/leaf.zy");
    symlink(library, fixture.directory.path().join("alias.zy")).unwrap();
    symlink(&fixture.importer, fixture.directory.path().join("self.zy")).unwrap();
    symlink(fixture.directory.path().join("nested"), fixture.directory.path().join("folder"))
        .unwrap();
    symlink("missing.zy", fixture.directory.path().join("broken.zy")).unwrap();
    assert_eq!(fixture.names("", ""), ["folder", "nested", "alias.zy", "library.zy"]);
    assert_eq!(fixture.names("folder", ""), ["leaf.zy"]);
}

// macOS filesystems reject these names before completion can observe them.
#[cfg(target_os = "linux")]
#[test]
fn source_paths_skip_names_that_cannot_be_written_in_source() {
    use std::{ffi::OsString, os::unix::ffi::OsStringExt};

    let fixture = Fixture::new();
    let invalid = OsString::from_vec(b"bad\xff.zy".to_vec());
    std::fs::write(fixture.directory.path().join(invalid), "()").unwrap();
    fixture.write("valid.zy");
    assert_eq!(fixture.names("", ""), ["valid.zy"]);
}
