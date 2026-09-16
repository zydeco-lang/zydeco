use super::*;
use zydeco_session::CompilerSession;

struct Fixture {
    _directory: tempfile::TempDir,
    root: PathBuf,
    session: CompilerSession,
}

impl Fixture {
    fn new(source: &str) -> Self {
        let directory = tempfile::tempdir().unwrap();
        let root = directory.path().canonicalize().unwrap().join("main.zy");
        let mut session = CompilerSession::default();
        session.set_overlay(&root, source.to_owned()).unwrap();
        Self { _directory: directory, root, session }
    }

    fn reference(&self) -> DocumentationView {
        let analysis = self.session.analyze(&self.root).unwrap();
        let program = self.session.checked_program(&analysis).unwrap();
        DocumentationView { analysis, program }
    }
}

#[test]
fn public_reference_shows_and_searches_documented_fields() {
    let fixture = Fixture::new(
        "let private = 0 in (\n--| Current counter.\n--|\n--| Read it to inspect progress.\n@[doc] (#value = 42))",
    );
    let reference = fixture.reference();
    let renderer = &reference;
    let show = renderer.show(&SemanticSelector::parse("value")).unwrap();
    assert!(show.starts_with("value : Int"), "{show}");
    assert!(show.contains("Read it to inspect progress."));
    assert!(renderer.search("counter progress").contains("value : Int"));
    assert!(renderer.search("private").is_empty());
    assert!(matches!(
        renderer.show(&SemanticSelector::parse("private")),
        Err(DocumentationError::UnknownSubject(_))
    ));
}

#[test]
fn semantic_links_reach_exposed_fields_without_prose() {
    let fixture = Fixture::new(
        "let Interface = (#value :: @(intrinsic(int))) in\n--| Read [value](zydeco:member:Interface/value).\n@[doc] ((#value = 42) : Interface)",
    );
    let reference = fixture.reference();
    let html = reference.html("Value", &[]).unwrap();
    assert!(html.contains("href=\"#api-f-76616c7565\">value</a>"), "{html}");
}

#[test]
fn offline_reference_includes_guides_and_exact_input_fingerprints() {
    let fixture = Fixture::new("--| A counter.\n@[doc] (#value = 42)");
    let reference = fixture.reference();
    let renderer = &reference;
    let guide =
        (PathBuf::from("getting-started.md"), "Read [value](zydeco:member:./value).".to_owned());
    let html = renderer.html("Counter", &[guide]).unwrap();
    assert!(html.contains("<title>Counter</title>"));
    assert!(html.contains("href=\"#api-f-76616c7565\">value</a>"));
    assert!(
        html.contains(&Html::digest(&fixture.session.source_text(&fixture.root).unwrap().unwrap()))
    );
    assert!(!html.contains("<script src="));
}

#[test]
fn guide_links_require_an_explicit_public_root_and_reject_missing_members() {
    let fixture = Fixture::new("(#value = 42)");
    let reference = fixture.reference();
    let renderer = &reference;
    for markdown in ["[private](zydeco:name:private)", "[missing](zydeco:member:./missing)"] {
        let guide = (PathBuf::from("guide.md"), markdown.to_owned());
        assert!(matches!(
            renderer.html("Counter", &[guide]),
            Err(DocumentationError::Verification(_) | DocumentationError::UnknownSubject(_))
        ));
    }
}

#[test]
fn rejected_links_prevent_a_build_without_rejecting_the_program() {
    let fixture = Fixture::new("--| [Missing](zydeco:name:missing)\n@[doc] (#value = 42)");
    let reference = fixture.reference();
    let renderer = &reference;
    assert!(reference.analysis.outcome().root().is_some());
    assert!(matches!(renderer.html("Counter", &[]), Err(DocumentationError::Verification(_))));
}

#[test]
fn documentation_html_is_escaped_and_does_not_activate_authored_scripts() {
    let html = Html::markdown(
        "<script>alert('bad')</script>\n\n[bad](javascript:alert)\n\n![remote](https://example.com/image.png)",
        Path::new("/project/main.zy"),
    );
    assert!(!html.contains("<script>"));
    assert!(html.contains("&lt;script&gt;"));
    assert!(!html.contains("href=\"javascript:"));
    assert!(!html.contains("<img"));
    assert!(html.contains("remote"));
}

#[test]
fn ordinary_relative_links_keep_their_authored_directory() {
    let html = Html::markdown(
        "[Guide](guide.md) and [field](#api-f-76616c7565)",
        Path::new("/project/library.zy"),
    );
    assert!(html.contains("href=\"file:///project/guide.md\""));
    assert!(html.contains("href=\"#api-f-76616c7565\""));
}

#[test]
fn public_companion_contract_excludes_private_implementation_docs() {
    let mut fixture = Fixture::new("--| Implementation algorithm.\n@[doc] (#value = 42)");
    fixture
        .session
        .set_overlay(
            fixture.root.with_extension("zyi"),
            "--| Public counter.\n@[doc] (#value :: @(intrinsic(int)))".to_owned(),
        )
        .unwrap();
    let reference = fixture.reference();
    let html = reference.html("Counter", &[]).unwrap();
    assert!(html.contains("Public counter."));
    assert!(!html.contains("Implementation algorithm."));
}

#[test]
fn generic_reference_exposes_its_result_without_evaluation() {
    let fixture =
        Fixture::new("param I : @(intrinsic(vtype)) in\n--| Generic field.\n@[doc] (#value :: I)");
    let reference = fixture.reference();
    assert!(
        reference.show(&SemanticSelector::parse("()/value")).unwrap().contains("Generic field.")
    );
}
