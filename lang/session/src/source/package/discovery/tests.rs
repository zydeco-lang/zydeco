use super::super::tests::Fixture;
use super::*;
use std::collections::BTreeSet;

impl Fixture {
    fn discover(&self, rules: &str) -> (BTreeSet<PathBuf>, BTreeSet<PathBuf>) {
        let source =
            SourceTemplate::parse(self.path("packages.zy"), format!("@[discover({rules})] ()"))
                .unwrap();
        let mut queries = BTreeSet::new();
        let paths = PackageDiscovery { source: &source }
            .walk(std::iter::empty(), |path| {
                queries.insert(path.strip_prefix(self.directory.path()).unwrap().to_path_buf());
            })
            .unwrap()
            .into_keys()
            .map(|path| path.strip_prefix(self.directory.path()).unwrap().to_path_buf())
            .collect();
        (paths, queries)
    }
}

#[test]
fn absent_rules_excludes_and_literal_files_never_enumerate_directories() {
    let fixture = Fixture::new();
    fixture.write("tests/one.zy", "1");
    for rules in ["", r#"exclude("**")"#, r#"include("missing/**/*.zy")"#] {
        assert_eq!(fixture.discover(rules), (BTreeSet::new(), BTreeSet::new()));
    }
    let (paths, queries) = fixture.discover(r#"include("tests/one.zy")"#);
    assert_eq!(paths, [PathBuf::from("tests/one.zy")].into());
    assert!(queries.is_empty());
    let source = SourceTemplate::parse(
        fixture.path("packages.zy"),
        r#"@[discover(exclude("**"))] ()"#.into(),
    )
    .unwrap();
    assert!(
        PackageDiscovery { source: &source }
            .paths(std::iter::from_fn(|| panic!("no includes need no overlay inventory")))
            .unwrap()
            .is_empty()
    );
}

#[test]
fn discovered_paths_retain_the_winning_include_span_for_disk_and_overlays() {
    let fixture = Fixture::new();
    fixture.write("tests/one.zy", "1");
    let text = r#"@[discover(include("tests/*.zy"), exclude("tests/skip.zy"), include("tests/one.zy"))] ()"#;
    let source = SourceTemplate::parse(fixture.path("packages.zy"), text.into()).unwrap();
    let overlays = ["tests/one.zy", "tests/new.zy", "tests/skip.zy"].map(|path| fixture.path(path));
    let paths = PackageDiscovery { source: &source }.paths(overlays.into_iter()).unwrap();
    assert_eq!(paths.len(), 2);
    for (path, pattern) in
        [("tests/one.zy", r#""tests/one.zy""#), ("tests/new.zy", r#""tests/*.zy""#)]
    {
        assert_eq!(&text[paths[&fixture.path(path)].range()], pattern);
    }
}

#[test]
fn nonrecursive_globs_read_only_their_prefix_and_required_depth() {
    let fixture = Fixture::new();
    fixture.write("tests/one.zy", "1");
    fixture.write("tests/nested/two.zy", "2");
    fixture.write("unrelated/three.zy", "3");
    fixture.write("tests/ignored.txt", "ignored");
    let (paths, queries) = fixture.discover(r#"include("tests/*.zy")"#);
    assert_eq!(paths, [PathBuf::from("tests/one.zy")].into());
    assert_eq!(queries, [PathBuf::from("tests")].into());
}

#[test]
fn subtree_pruning_does_not_exclude_a_file_with_the_same_name_as_its_root() {
    let fixture = Fixture::new();
    fixture.write("test.zy", "1");
    for include in ["test.zy", "*.zy"] {
        let (paths, _) =
            fixture.discover(&format!(r#"include("{include}"), exclude("test.zy/**")"#));
        assert_eq!(paths, [PathBuf::from("test.zy")].into());
    }
}

#[test]
fn directory_errors_point_to_the_authored_include_pattern() {
    let fixture = Fixture::new();
    fixture.write("file.zy", "1");
    let text = r#"@[discover(include("file.zy/child/*.zy"))] ()"#;
    let source = SourceTemplate::parse(fixture.path("packages.zy"), text.into()).unwrap();
    let error = PackageDiscovery { source: &source }.paths(std::iter::empty()).unwrap_err();
    let SourceLoadError::Package(error) = error else { panic!("discovery error") };
    let PackageError::Discovery { path, site, source } = *error else { panic!("directory error") };
    assert_eq!(source.kind(), io::ErrorKind::NotADirectory);
    assert_eq!(path, fixture.path("file.zy/child"));
    assert_eq!(&text[site.range().clone()], r#""file.zy/child/*.zy""#);
}

#[test]
fn subtree_excludes_prune_before_enumeration_and_later_includes_can_readd() {
    let fixture = Fixture::new();
    fixture.write("tests/one.zy", "1");
    fixture.write("tests/unit/two.zy", "2");
    fixture.write("tests/fixtures/broken.zy", "(");
    fixture.write("tests/fixtures/keep.zy", "3");
    fixture.write("tests/fixtures/deep/broken.zy", "(");
    let (paths, queries) = fixture.discover(r#"include("tests/**/*.zy"), exclude("tests/fixtures/**"), include("tests/fixtures/keep.zy")"#);
    assert_eq!(
        paths,
        ["tests/one.zy", "tests/unit/two.zy", "tests/fixtures/keep.zy"].map(PathBuf::from).into()
    );
    assert_eq!(queries, ["tests", "tests/unit"].map(PathBuf::from).into());
    let (paths, queries) = fixture.discover(r#"include("tests/**/*.zy"), exclude("tests/**"), include("tests/*.zy"), exclude("tests/one.zy")"#);
    assert!(paths.is_empty());
    assert_eq!(queries, [PathBuf::from("tests")].into());
}

#[cfg(unix)]
#[test]
fn symlink_files_directories_and_literal_prefixes_are_not_followed() {
    let fixture = Fixture::new();
    fixture.write("outside/one.zy", "1");
    std::fs::create_dir(fixture.path("tests")).unwrap();
    std::os::unix::fs::symlink(fixture.path("outside"), fixture.path("tests/link")).unwrap();
    std::os::unix::fs::symlink(fixture.path("outside/one.zy"), fixture.path("tests/one.zy"))
        .unwrap();
    let (paths, queries) =
        fixture.discover(r#"include("tests/**/*.zy", "tests/link/*.zy", "tests/link/one.zy")"#);
    assert!(paths.is_empty());
    assert_eq!(queries, [PathBuf::from("tests")].into());
}
