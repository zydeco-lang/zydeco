use super::*;

struct Fixture {
    _directory: tempfile::TempDir,
    path: PathBuf,
    position: Position,
    project: ProjectState,
    session: CompilerSession,
}

impl Fixture {
    fn new(marked: &str) -> Self {
        let offset = marked.find('¦').unwrap();
        let source = marked.replacen('¦', "", 1);
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().canonicalize().unwrap().join("main.zy");
        let map = FileMap::local(source.as_str(), None);
        let LineCol { line, column } = map.line_col_utf16(offset).unwrap();
        let (project, session) =
            ProjectState::load(&path, &HashMap::from([(path.clone(), source)])).unwrap();
        Self {
            _directory: directory,
            path,
            position: Position::new(line, column),
            project,
            session,
        }
    }

    fn hover(&self) -> String {
        let hover = self
            .project
            .hover(&self.session, &self.path, self.position, HoverOptions::default())
            .unwrap();
        let HoverContents::Markup(content) = hover.contents else {
            panic!("expected Markdown hover")
        };
        content.value
    }
}

#[test]
fn hover_combines_type_and_documentation_summary() {
    let fixture = Fixture::new(
        "--| The answer.\n--|\n--| Longer explanation.\n@[doc] let answer = 42 in ¦answer",
    );
    let hover = fixture.hover();
    assert!(hover.contains("answer : Int64"), "{hover}");
    assert!(hover.contains("The answer."), "{hover}");
    assert!(!hover.contains("Longer explanation."), "{hover}");
}

#[test]
fn projection_hover_includes_the_documented_field() {
    let fixture =
        Fixture::new("let module = (\n--| Current value.\n@[doc] (#value = 3)) in module/¦value");
    let hover = fixture.hover();
    assert!(hover.contains("Int64"), "{hover}");
    assert!(hover.contains("Current value."), "{hover}");
}

#[test]
fn shadowed_names_do_not_inherit_outer_documentation() {
    let fixture =
        Fixture::new("--| Outer answer.\n@[doc] let answer = 42 in let answer = 0 in ¦answer");
    let hover = fixture.hover();
    assert!(hover.contains("answer : Int64"), "{hover}");
    assert!(!hover.contains("Outer answer."), "{hover}");
}

#[test]
fn source_prose_is_readable_at_its_comment() {
    let fixture = Fixture::new("--| ¦The answer.\n@[doc] 42");
    assert_eq!(fixture.hover(), "The answer.");
    let prepared = fixture
        .project
        .prepare_documentation(&fixture.path, fixture.position, HoverOptions::default())
        .unwrap();
    assert!(prepared.view.signature.is_none());
    assert_eq!(prepared.view.declared_signature.as_deref(), Some("Int64"));
    let origin = prepared.view.origin.unwrap();
    let followed = fixture
        .project
        .prepare_documentation(&fixture.path, origin.range.start, HoverOptions::default())
        .unwrap();
    assert_eq!(followed.view.sections[0].markdown, "The answer.");
}

#[test]
fn unrelated_type_failure_preserves_available_documentation() {
    let fixture = Fixture::new("--| Known value.\n@[doc] let value = 42 in (¦value, ! 1)");
    assert!(fixture.project.analysis.outcome().root().is_none());
    assert!(fixture.hover().contains("Known value."));
}

#[test]
fn semantic_links_navigate_and_report_errors_at_the_authored_destination() {
    let fixture = Fixture::new(
        "let value = 42 in\n--| [value](zydeco:name:value) and [missing](zydeco:name:missing).\n@[doc] let alias = value in ¦alias",
    );
    let hover = fixture.hover();
    assert!(hover.contains("#L1,5"), "{hover}");
    let links = fixture.project.document_links(&fixture.path);
    assert_eq!(links.len(), 1);
    assert_eq!(links[0].target.as_ref().unwrap().fragment(), Some("L1,5"));
    let diagnostics = fixture.project.diagnostics(&fixture.path);
    let [diagnostic] = diagnostics.as_slice() else { panic!("{diagnostics:?}") };
    assert_eq!(diagnostic.code, Some(NumberOrString::String("doc.link.name".to_owned())));
    assert_eq!(diagnostic.range.start.line, 1);
}

#[test]
fn persistent_documentation_keeps_generic_contract_beside_concrete_type() {
    let fixture = Fixture::new(
        "let VType = @(intrinsic(vtype)) in\nlet I = @(intrinsic(i64)) in\nlet Module = param Integer : VType in\n--| Generic value.\n--|\n--| Read this value to inspect the module.\n@[doc] (#value :: Integer) in\nlet counter : Module I = (#value = 3) in\ncounter/¦value",
    );
    let prepared = fixture
        .project
        .prepare_documentation(&fixture.path, fixture.position, HoverOptions::default())
        .unwrap();
    assert_eq!(prepared.view.title, "value");
    assert_eq!(prepared.view.signature.as_deref(), Some("Int64"));
    assert_eq!(prepared.view.declared_signature.as_deref(), Some("Integer"));
    assert!(prepared.view.sections[0].markdown.contains("Read this value"));
    assert!(prepared.view.origin.is_some());
}

#[test]
fn panel_markdown_keeps_authored_html_inert() {
    let fixture = Fixture::new(
        "--| <script>bad()</script>\n--|\n--| [bad](command:bad) and ![remote](https://example.com/picture.png)\n@[doc] let answer = 42 in ¦answer",
    );
    let prepared = fixture
        .project
        .prepare_documentation(&fixture.path, fixture.position, HoverOptions::default())
        .unwrap();
    let html = &prepared.view.sections[0].html;
    assert!(html.contains("&lt;script&gt;"));
    assert!(!html.contains("<script>"));
    assert!(!html.contains("command:"));
    assert!(!html.contains("<img"));
}
