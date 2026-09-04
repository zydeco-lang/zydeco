use super::*;
use crate::{CompilerSession, ProgramAnalysis};

struct Fixture {
    _directory: tempfile::TempDir,
    session: CompilerSession,
    root: PathBuf,
    offset: usize,
}

impl Fixture {
    fn new(marked: &str) -> Self {
        Self::with_sources(marked, &[])
    }

    fn with_sources(marked: &str, sources: &[(&str, &str)]) -> Self {
        let directory = tempfile::tempdir().unwrap();
        let root = directory.path().canonicalize().unwrap().join("main.zy");
        let offset = marked.find('¦').expect("fixture needs a cursor");
        let mut session = CompilerSession::default();
        sources.iter().for_each(|(path, source)| {
            session.set_overlay(directory.path().join(path), (*source).to_owned()).unwrap();
        });
        session.set_overlay(&root, marked.replacen('¦', "", 1)).unwrap();
        Self { _directory: directory, session, root, offset }
    }

    fn analyze(&self) -> Arc<ProgramAnalysis> {
        self.session.analyze(&self.root).unwrap()
    }

    fn term(&self, analysis: &ProgramAnalysis) -> s::TermId {
        analysis
            .scoped()
            .terms
            .iter()
            .filter_map(|(term, _)| {
                let origin = analysis.scoped().origins.source(&term.into())?;
                let (file, range) =
                    analysis.spans().source_map()?.range(analysis.spans()[&origin])?;
                (file.path() == self.root && range.contains(&self.offset))
                    .then_some((term, range.len()))
            })
            .min_by_key(|(_, length)| *length)
            .unwrap()
            .0
    }

    fn markdown(&self) -> String {
        let analysis = self.analyze();
        assert!(analysis.outcome().root().is_some(), "{:?}", analysis.outcome());
        analysis.documentation().for_term(self.term(&analysis)).markdown()
    }
}

#[test]
fn binding_and_immediate_rhs_documentation_follow_aliases() {
    for source in [
        "--| The answer.\n@[doc] let answer = 42 in let alias = answer in ¦alias",
        "let answer =\n--| The answer.\n@[doc] 42 in let alias = answer in ¦alias",
        "--| The answer.\n@[doc] def answer = 42 in let alias = answer in ¦alias",
        "begin\n--| The answer.\n@[doc] let answer = 42 that\n¦answer\nend",
    ] {
        assert_eq!(Fixture::new(source).markdown(), "The answer.", "{source}");
    }
}

#[test]
fn nested_expression_docs_and_shadowed_bindings_do_not_leak() {
    for source in [
        "--| Outer answer.\n@[doc] let answer = 42 in let answer = 0 in ¦answer",
        "let pair = (\n--| First component.\n@[doc] 42, 0) in ¦pair",
        "--| A block.\n@[doc] begin let answer = 42 in ¦answer end",
        "--| Detached.\n\n@[doc] let answer = 42 in ¦answer",
        "--| Detached.\n-- A barrier.\n@[doc] let answer = 42 in ¦answer",
    ] {
        assert_eq!(Fixture::new(source).markdown(), "", "{source}");
    }
}

#[test]
fn imports_preserve_prose_origin_and_local_context() {
    let fixture = Fixture::with_sources(
        "let library =\n--| Local use.\n@[doc] @(import(\"library.zy\")) in ¦library",
        &[("library.zy", "--| Provider description.\n@[doc] 42")],
    );
    assert_eq!(fixture.markdown(), "Local use.\n\nProvider description.");
    let analysis = fixture.analyze();
    let content = analysis.documentation().for_term(fixture.term(&analysis));
    assert_eq!(content.entries().count(), 2);
    assert!(content.entries().last().unwrap().path.ends_with("library.zy"));
}

#[test]
fn documentation_follows_owned_fields_and_projection_patterns() {
    for use_site in ["counter/¦value", "let (/value = selected) = counter in ¦selected"] {
        let source = format!(
            "let I = @(intrinsic(i64)) in\nlet Interface =\n--| Counter value.\n@[doc] (#value :: I) in\nlet counter : Interface = (#value = 3) in\n{use_site}"
        );
        assert_eq!(Fixture::new(&source).markdown(), "Counter value.", "{source}");
    }
}

#[test]
fn identical_field_labels_keep_distinct_documentation() {
    let source = "let I = @(intrinsic(i64)) in\nlet First =\n--| First contract.\n@[doc] (#value :: I) in\nlet Second =\n--| Second contract.\n@[doc] (#value :: I) in\nlet first : First = (#value = 1) in\nlet second : Second = (#value = 2) in\nsecond/¦value";
    assert_eq!(Fixture::new(source).markdown(), "Second contract.");
}

#[test]
fn generic_field_docs_survive_type_substitution() {
    let source = "let VType = @(intrinsic(vtype)) in\nlet I = @(intrinsic(i64)) in\nlet Module = param Integer : VType in\n--| Generic value.\n@[doc] (#value :: Integer) in\nlet counter : Module I = (#value = 3) in\ncounter/¦value";
    assert_eq!(Fixture::new(source).markdown(), "Generic value.");
    let fixture = Fixture::new(source);
    let reference = fixture.session.documentation_reference(&fixture.root).unwrap();
    assert!(reference.get(&crate::source::DocumentationPath::default()).is_some());
    let package = Fixture::new(
        "let VType = @(intrinsic(vtype)) in\nlet I = @(intrinsic(i64)) in\nlet Module = param Integer : VType in\n--| Generic value.\n@[doc] (#value :: Integer) in\nlet counter : Module I = (#value = 3) in ¦counter",
    );
    let reference = package.session.documentation_reference(&package.root).unwrap();
    let member = reference
        .get(&crate::source::DocumentationPath::parse("value"))
        .expect("instantiated public field");
    assert_eq!(reference.content(member).markdown(), "Generic value.");
}

#[test]
fn companion_contract_documentation_follows_imported_fields() {
    let fixture = Fixture::with_sources(
        "let library = @(import(\"library.zy\")) in library/¦value",
        &[
            ("library.zy", "--| Implementation detail.\n@[doc] (#value = 3)"),
            ("library.zyi", "--| Public contract.\n@[doc] (#value :: @(intrinsic(i64)))"),
        ],
    );
    assert_eq!(fixture.markdown(), "Public contract.");
}

#[test]
fn invalid_projection_has_no_documentation_from_matching_labels() {
    let fixture =
        Fixture::new("let counter = (\n--| A value.\n@[doc] (#value = 3)) in counter/¦missing");
    let analysis = fixture.analyze();
    assert!(analysis.outcome().root().is_none());
    assert!(analysis.documentation().for_term(fixture.term(&analysis)).is_empty());
}

#[test]
fn revisions_replace_documentation_without_reusing_transient_ids() {
    let mut fixture = Fixture::new("--| Old.\n@[doc] let value = 3 in ¦value");
    let old = fixture.analyze();
    let old_term = fixture.term(&old);
    fixture
        .session
        .set_overlay(&fixture.root, "--| New.\n@[doc] let value = 3 in value".to_owned())
        .unwrap();
    let new = fixture.analyze();
    assert_eq!(old.documentation().for_term(old_term).markdown(), "Old.");
    assert_eq!(new.documentation().for_term(fixture.term(&new)).markdown(), "New.");
    assert!(new.documentation().for_term(old_term).is_empty());
}

#[test]
fn summary_selects_a_prose_paragraph_and_preserves_its_markdown() {
    let index = DocumentationIndex::recover(
        Path::new("main.zy"),
        concat!(
            "--| # Heading\n",
            "--|\n",
            "--| - A list\n",
            "--|\n",
            "--| ```zydeco\n",
            "--| 42\n",
            "--| ```\n",
            "--|\n",
            "--| A **useful** description.\n",
            "--| Continued here.\n",
            "--| \t\n",
            "--| More details.\n",
            "@[doc] 42",
        ),
    );
    assert_eq!(index.entries()[0].summary(), "A **useful** description.\nContinued here.");
}

#[test]
fn recovery_retains_prose_but_does_not_promote_it_by_containment() {
    for source in [
        "--| Read me.\n@[doc] let value = 42 in (value, )",
        "--| Read me.\n@[doc] let value = 42 in missing",
    ] {
        let fixture = Fixture::new(&format!("¦{source}"));
        let index = fixture.session.source_documentation(&fixture.root).unwrap();
        assert_eq!(index.at(&fixture.root, 4).markdown(), "Read me.");
        assert!(index.at(&fixture.root, source.len() - 1).is_empty());
    }
    let index = DocumentationIndex::recover(Path::new("main.zy"), "--| Detached.\n\n@[doc] 42");
    assert!(index.entries().is_empty());
}

#[test]
fn semantic_links_use_author_scope_and_map_unicode_crlf_source() {
    let source = "let answer = 42 in\r\n--| é [answer](zydeco:name:answer)\r\n@[doc] let alias = answer in let answer = 0 in ¦alias";
    let fixture = Fixture::new(source);
    let analysis = fixture.analyze();
    let entry = &analysis.documentation().entries()[0];
    let [link] = entry.links.as_slice() else { panic!("expected one semantic link") };
    let DocumentationLinkTarget::Definition(target) = link.target.as_ref().unwrap() else {
        panic!("expected a lexical target")
    };
    let origin = analysis.scoped().origins.source(&(*target).into()).unwrap();
    let (_, definition_range) =
        analysis.spans().source_map().unwrap().range(analysis.spans()[&origin]).unwrap();
    assert_eq!(definition_range.start, source.find("answer").unwrap());
    let original = analysis.source(&fixture.root).unwrap();
    assert_eq!(&original[link.source.clone()], "zydeco:name:answer");
    assert!(
        entry
            .markdown_with_links(false, &mut |_| Some("#resolved".to_owned()))
            .contains("[answer](#resolved)")
    );
}

#[test]
fn semantic_link_ranges_select_destinations_instead_of_repeated_labels_or_titles() {
    for markdown in [
        "[zydeco:name:answer](zydeco:name:answer \"zydeco:name:answer\")",
        "[`a](b)`](<zydeco:name:answer> \"title\")",
        "[**answer**](zydeco:name:answer)",
        "[](zydeco:name:answer)",
    ] {
        let links = DocumentationLinkSyntax::collect(markdown);
        let [link] = links.as_slice() else { panic!("{markdown}") };
        assert!(link.destination.is_ok(), "{markdown}: {:?}", link.destination);
        assert_eq!(&markdown[link.range.clone()], "zydeco:name:answer");
        assert!(
            markdown[..link.range.start].ends_with("(")
                || markdown[..link.range.start].ends_with("(<")
        );
    }
    let rejected = DocumentationLinkSyntax::collect("[answer][ref]\n\n[ref]: zydeco:name:answer");
    assert!(matches!(rejected[0].destination, Err(DocumentationLinkError::Syntax)));
}

#[test]
fn semantic_member_links_follow_the_explicit_owner() {
    let fixture = Fixture::new(
        "let I = @(intrinsic(i64)) in let Interface = (\n--| The field.\n@[doc] (#value :: I)) in\n--| See [value](zydeco:member:Interface/value).\n@[doc] let object : Interface = (#value = 3) in ¦object",
    );
    let analysis = fixture.analyze();
    let link = &analysis.documentation().entries()[1].links[0];
    let DocumentationLinkTarget::Member { declaration: Some(declaration), .. } =
        link.target.as_ref().unwrap()
    else {
        panic!("expected an owned field")
    };
    assert_eq!(analysis.documentation().for_term(*declaration).markdown(), "The field.");
}

#[test]
fn semantic_links_reject_missing_names_members_and_implicit_global_lookup() {
    let fixture = Fixture::new(
        "let I = @(intrinsic(i64)) in let Interface = (#value :: I) in\n--| [x](zydeco:name:missing) [v](zydeco:member:Interface/missing) [label](zydeco:name:value)\n@[doc] ¦42",
    );
    let analysis = fixture.analyze();
    let links = &analysis.documentation().entries()[0].links;
    assert!(matches!(links[0].target, Err(DocumentationLinkError::UnknownName(_))));
    assert!(matches!(links[1].target, Err(DocumentationLinkError::UnknownMember { .. })));
    assert!(matches!(links[2].target, Err(DocumentationLinkError::UnknownName(_))));
}

#[test]
fn imported_links_cannot_capture_the_consumer_scope() {
    let fixture = Fixture::with_sources(
        "let secret = 0 in let library = @(import(\"library.zy\")) in ¦library",
        &[("library.zy", "--| [secret](zydeco:name:secret)\n@[doc] 42")],
    );
    let analysis = fixture.analyze();
    assert!(matches!(
        analysis.documentation().entries()[0].links[0].target,
        Err(DocumentationLinkError::UnknownName(_))
    ));
}

#[test]
fn public_exposure_has_stable_paths_and_hides_implementation_bindings() {
    use crate::source::DocumentationExposureQuery;
    for source in [
        "let private = 0 in (#value = 42, #other = 1)",
        "let private = 0 in (\n #value = 42,\n #other = 1\n)",
    ] {
        let fixture = Fixture::new(&format!("¦{source}"));
        let analysis = fixture.analyze();
        let program = fixture.session.checked_program(&analysis).unwrap();
        let zydeco_statics::syntax::TermAnnId::Value(_, classifier) = program.root else {
            panic!()
        };
        let members =
            DocumentationExposureQuery::new(&program.statics).collect(classifier.into()).unwrap();
        assert_eq!(
            members.iter().map(|member| member.path.to_string()).collect::<Vec<_>>(),
            ["value", "other"]
        );
        assert_eq!(members[0].path.anchor(), "api-f-76616c7565");
    }
}

#[test]
fn public_exposure_rejects_ambiguous_routes() {
    use crate::source::{DocumentationExposureError, DocumentationExposureQuery};
    let fixture = Fixture::new("¦(#value = 42, #value = 1)");
    let analysis = fixture.analyze();
    let program = fixture.session.checked_program(&analysis).unwrap();
    let zydeco_statics::syntax::TermAnnId::Value(_, classifier) = program.root else { panic!() };
    assert!(matches!(
        DocumentationExposureQuery::new(&program.statics).collect(classifier.into()),
        Err(DocumentationExposureError::Ambiguous(_))
    ));
}
