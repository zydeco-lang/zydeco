use super::*;

struct Fixture {
    _directory: tempfile::TempDir,
    session: CompilerSession,
    path: PathBuf,
}

impl Fixture {
    fn new(markdown: &str) -> Self {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().canonicalize().unwrap().join("main.zy");
        let mut session = CompilerSession::default();
        let text = markdown.lines().map(|line| format!("--| {line}\r\n")).collect::<String>();
        session.set_overlay(&path, format!("{text}@[doc] 42")).unwrap();
        Self { _directory: directory, session, path }
    }

    fn examples(&self) -> Vec<DocumentationExample> {
        let analysis = self.session.analyze(&self.path).unwrap();
        DocumentationExample::from_documentation(
            &analysis.documentation().entries()[0],
            analysis.source(&self.path).unwrap(),
        )
    }
}

#[test]
fn only_explicitly_opted_in_fences_are_checked() {
    let fixture = Fixture::new("```zydeco\nschematic ...\n```\n\n```zydeco check\n42\n```");
    let examples = fixture.examples();
    assert_eq!(examples.len(), 1);
    assert!(examples[0].request(&fixture.session).unwrap().check().status.is_passed());
}

#[test]
fn examples_keep_relative_imports_and_current_overlay_inputs() {
    let mut fixture =
        Fixture::new("```zydeco check\nlet value = @(import(\"library.zy\")) in value/value\n```");
    fixture
        .session
        .set_overlay(fixture.path.with_file_name("library.zy"), "(#value = 42)".to_owned())
        .unwrap();
    let example = &fixture.examples()[0];
    let before = fixture.session.source_text(&fixture.path).unwrap();
    assert!(example.request(&fixture.session).unwrap().check().status.is_passed());
    assert_eq!(fixture.session.source_text(&fixture.path).unwrap(), before);
    fixture
        .session
        .set_overlay(fixture.path.with_file_name("library.zy"), "(#other = 42)".to_owned())
        .unwrap();
    assert!(!example.request(&fixture.session).unwrap().check().status.is_passed());
    assert!(!example.request(&fixture.session).unwrap().path.exists());
}

#[test]
fn rejection_requires_the_declared_code_and_position() {
    let fixture = Fixture::new(
        "```zydeco reject=tyck.missing-named-field at=1:15\n(#value = 42)/missing\n```",
    );
    let example = &fixture.examples()[0];
    let result = example.request(&fixture.session).unwrap().check();
    assert!(result.status.is_passed(), "{result:?}");
    let wrong =
        Fixture::new("```zydeco reject=tyck.type-mismatch at=1:15\n(#value = 42)/missing\n```");
    assert!(!wrong.examples()[0].request(&wrong.session).unwrap().check().status.is_passed());
    let wrong_location = Fixture::new(
        "```zydeco reject=tyck.missing-named-field at=1:2\n(42, (#value = 1)/missing)\n```",
    );
    assert!(
        !wrong_location.examples()[0]
            .request(&wrong_location.session)
            .unwrap()
            .check()
            .status
            .is_passed()
    );
}

#[test]
fn unrelated_import_errors_do_not_satisfy_expected_rejection() {
    let fixture = Fixture::new(
        "```zydeco reject=tyck.type-expected at=1:1\nlet value = @(import(\"missing.zy\")) in ! 1\n```",
    );
    let result = fixture.examples()[0].request(&fixture.session).unwrap().check();
    assert!(!result.status.is_passed());
    assert!(result.diagnostics.iter().all(|diagnostic| diagnostic.code.is_none()));
}

#[test]
fn example_errors_map_back_to_unicode_crlf_comment_lines() {
    let fixture = Fixture::new("Some α prose.\n\n```zydeco check\nlet text = \"😀\" in\n! 1\n```");
    let example = &fixture.examples()[0];
    let result = example.request(&fixture.session).unwrap().check();
    assert!(!result.status.is_passed());
    let diagnostic = &result.diagnostics[0];
    let range = example.source_range(diagnostic.range.clone().unwrap()).unwrap();
    let original = fixture.session.source_text(&fixture.path).unwrap().unwrap();
    assert_eq!(&original[range.clone()], "1");
    assert_eq!(range.start, original.find("! 1").unwrap() + 2);
}

#[test]
fn invalid_options_and_runtime_execution_are_explicit_failures() {
    for option in
        ["check extra", "reject=tyck.fake at=1:1", "reject=tyck.type-expected at=0:1", "run"]
    {
        let fixture = Fixture::new(&format!("```zydeco {option}\n42\n```"));
        assert!(fixture.examples()[0].mode.is_err(), "{option}");
    }
}

#[test]
fn scratch_source_preserves_import_context_without_changing_other_strings() {
    let mut fixture = Fixture::new(
        "```zydeco check\nlet message = \"library.zy\" in\nlet library = @(import(\"library.zy\")) in library/value\n```",
    );
    fixture
        .session
        .set_overlay(fixture.path.with_file_name("library.zy"), "(#value = 42)".to_owned())
        .unwrap();
    let example = &fixture.examples()[0];
    let original = example.code.clone();
    let scratch = example.scratch_source().unwrap();
    assert!(scratch.contains("let message = \"library.zy\""));
    assert!(scratch.contains(fixture.path.parent().unwrap().to_str().unwrap()));
    let other = tempfile::tempdir().unwrap();
    let path = other.path().join("scratch.zydeco");
    fixture.session.set_overlay(&path, scratch).unwrap();
    assert!(fixture.session.analyze(&path).unwrap().outcome().root().is_some());
    assert_eq!(example.code, original);
    let invalid = Fixture::new("```zydeco check\nlet broken =\n```");
    assert!(invalid.examples()[0].scratch_source().is_err());
}
