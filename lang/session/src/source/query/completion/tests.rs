use super::*;
use std::path::PathBuf;
use zydeco_statics::fmt::{Formatter, Pretty};

struct Fixture {
    directory: tempfile::TempDir,
    session: CompilerSession,
    path: PathBuf,
    offset: usize,
}

impl Fixture {
    fn new(marked: &str) -> Self {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("main.zy");
        let mut fixture = Self { directory, session: CompilerSession::default(), path, offset: 0 };
        fixture.edit(marked);
        fixture
    }

    fn edit(&mut self, marked: &str) {
        self.offset = marked.find('¦').expect("a fixture needs a cursor");
        self.session.set_overlay(&self.path, marked.replacen('¦', "", 1)).unwrap();
    }

    fn with_dependency(mut self, name: &str, source: &str) -> Self {
        self.session.set_overlay(self.directory.path().join(name), source.to_owned()).unwrap();
        self
    }

    fn completion(&self) -> Arc<CompletionAnalysis> {
        self.session.complete(&self.path, self.offset).unwrap().unwrap_or_else(|| {
            panic!(
                "no term completion at {} in {:?}",
                self.offset,
                self.session.source_text(&self.path).unwrap()
            )
        })
    }

    fn names(&self) -> Vec<String> {
        self.completion().candidates.iter().map(|candidate| candidate.name.0.clone()).collect()
    }

    fn annotation(&self, name: &str) -> Option<String> {
        let completion = self.completion();
        let candidate = completion.candidates.iter().find(|candidate| candidate.name.0 == name)?;
        let semantics = completion.semantics.as_ref()?;
        let annotation = semantics.annotation(candidate.definition)?;
        let formatter = Formatter::new(&semantics.scoped, &semantics.statics);
        let mut rendered = String::new();
        annotation.pretty(&formatter).render_fmt(100, &mut rendered).ok()?;
        Some(rendered)
    }
}

#[test]
fn names_follow_lexical_proximity_and_shadowing() {
    let fixture = Fixture::new("let outer = 1 in let value = 2 in fn value => ¦");
    assert_eq!(fixture.names(), ["value", "outer"]);
    let completion = fixture.completion();
    let semantics = completion.semantics.as_ref().unwrap();
    let value = &completion.candidates[0];
    let origin = semantics.scoped.origins.source(&value.definition.into()).unwrap();
    let zydeco_surface::textual::syntax::EntityId::Def(_) = origin else {
        panic!("the winning definition must be an authored binder")
    };
    assert_eq!(value.distance, 0);
    assert_eq!(semantics.scoped.defs[&value.definition].0, "value");
}

#[test]
fn sequential_bindees_and_annotations_exclude_the_new_binder() {
    for source in [
        "let earlier = 1 in let later = ¦ in later",
        "let earlier = 1 in fn (later : ¦) => later",
        "do earlier <- ret 1; do later <- ¦; ret later",
        "let earlier = 1 in exists (later as ¦) . later",
    ] {
        assert_eq!(Fixture::new(source).names(), ["earlier"], "{source}");
    }
    assert_eq!(Fixture::new("let earlier = 1 in let later = 2 in ¦").names(), ["later", "earlier"]);
}

#[test]
fn dependent_parameters_see_only_preceding_parameters() {
    assert_eq!(Fixture::new("fn first (second : ¦) third => first").names(), ["first"]);
    assert_eq!(Fixture::new("pi First . pi (Second : ¦) . First").names(), ["First"]);
    assert_eq!(Fixture::new("sigma First . sigma (Second : ¦) . First").names(), ["First"]);
}

#[test]
fn recursive_binders_and_value_parameters_are_visible_in_their_bodies() {
    assert_eq!(Fixture::new("fix recurse => ¦").names(), ["recurse"]);
    assert_eq!(Fixture::new("val argument => ¦").names(), ["argument"]);
}

#[test]
fn block_wide_bindings_include_later_definitions_but_not_nested_blocks() {
    let fixture = Fixture::new(
        "begin let alpha = ¦ that let beta = 1 that (begin let hidden = 2 that hidden end) end",
    );
    assert_eq!(fixture.names(), ["alpha", "beta"]);
    assert_eq!(
        Fixture::new("begin let outer = 1 that begin let inner = 2 that ¦ end end").names(),
        ["inner", "outer"],
    );
}

#[test]
fn branches_do_not_leak_their_binders_to_siblings_or_the_enclosing_scope() {
    let fixture =
        Fixture::new("let outer = 1 in match missing | +A left => left | +B right => ¦ end");
    assert_eq!(fixture.names(), ["right", "outer"]);
    assert_eq!(
        Fixture::new("let outer = 1 in (match missing | +A left => left end, ¦)").names(),
        ["outer"],
    );
}

#[test]
fn unrelated_unbound_names_do_not_discard_the_cursor_scope() {
    for source in [
        "let value = 1 in (missing, _¦)",
        "let value = 1 in (_¦, missing)",
        "let broken = missing in let value = 1 in _¦",
    ] {
        let fixture = Fixture::new(source);
        assert!(fixture.names().iter().any(|name| name == "value"), "{source}");
        assert!(matches!(
            fixture.session.analyze(&fixture.path),
            Err(AnalysisError::Resolve { .. })
        ));
    }
}

#[test]
fn recovered_syntax_and_type_errors_keep_visible_names() {
    for source in ["let value = 1 in (?, ¦)", "let value = (1 : ()) in ¦", "fn untyped => ¦"] {
        let fixture = Fixture::new(source);
        assert_eq!(fixture.names().len(), 1, "{source}");
        assert!(fixture.completion().semantics.is_some(), "{source}");
    }
    assert_eq!(Fixture::new("fn untyped => ¦").names(), ["untyped"]);
}

#[test]
fn a_later_fatal_resolution_error_preserves_an_already_captured_scope() {
    let fixture = Fixture::new("let value = 1 in (¦, param invalid that invalid)");
    assert_eq!(fixture.names(), ["value"]);
    assert!(fixture.completion().semantics.is_none());
    assert!(matches!(
        fixture.session.analyze(&fixture.path),
        Err(AnalysisError::Source { .. }) | Err(AnalysisError::Resolve { .. })
    ));
}

#[test]
fn ordinary_definition_annotations_are_available_despite_the_completion_hole() {
    let fixture = Fixture::new("let value = 1 in ¦");
    assert_eq!(fixture.annotation("value").as_deref(), Some("Int64"));
    assert_eq!(
        Fixture::new("let Number = @[intrinsic(i64)] _ in ¦").annotation("Number").as_deref(),
        Some("VType"),
    );
}

#[test]
fn source_graph_copying_preserves_the_cursor_without_exporting_imported_locals() {
    let fixture = Fixture::new("let public = @[import(\"provider.zy\")] _ in ¦")
        .with_dependency("provider.zy", "let private = 1 in private")
        .with_dependency("provider.zyi", "@[intrinsic(i64)] _");
    assert_eq!(fixture.names(), ["public"]);
    assert_eq!(fixture.annotation("public").as_deref(), Some("Int64"));
}

#[test]
fn imported_sources_cannot_capture_the_importers_scope() {
    let fixture =
        Fixture::new("let secret = 1 in let imported = @[import(\"provider.zy\")] _ in ¦")
            .with_dependency("provider.zy", "secret");
    assert_eq!(fixture.names(), ["imported", "secret"]);
    assert!(fixture.annotation("imported").is_none());
    assert!(matches!(
        fixture.session.analyze(&fixture.path),
        Err(AnalysisError::Source { .. }) | Err(AnalysisError::Resolve { .. })
    ));
}

#[test]
fn discarded_import_payload_is_not_a_semantic_completion_site() {
    let fixture = Fixture::new("let local = 1 in @[import(\"provider.zy\")] ¦")
        .with_dependency("provider.zy", "1");
    assert!(fixture.session.complete(&fixture.path, fixture.offset).unwrap().is_none());
}

#[test]
fn prefix_filtering_uses_the_whole_token_edit_and_exact_matches_rank_first() {
    let fixture = Fixture::new("let value = 1 in let value_long = 2 in val¦ue_suffix");
    assert_eq!(fixture.names(), ["value_long", "value"]);
    let completion = fixture.completion();
    assert_eq!(&completion.source[completion.replacement.clone()], "value_suffix");
    let fixture = Fixture::new("let value = 1 in let value_long = 2 in value¦");
    assert_eq!(fixture.names(), ["value", "value_long"]);
    let fixture = Fixture::new("let value = 1 in let other = 2 in val¦");
    assert_eq!(fixture.names(), ["value"]);
    let completion = fixture.completion();
    assert_eq!(&completion.source[completion.replacement.clone()], "val");
}

#[test]
fn explicit_holes_are_replaced_and_do_not_become_a_name_prefix() {
    let fixture = Fixture::new("let value = 1 in _¦");
    assert_eq!(fixture.names(), ["value"]);
    let completion = fixture.completion();
    assert_eq!(&completion.source[completion.replacement.clone()], "_");
}

#[test]
fn binding_and_member_sites_do_not_offer_ordinary_names() {
    for source in [
        "let outer = 1 in fn ¦ => outer",
        "let outer = 1 in let ¦ = 2 in outer",
        "let outer = 1 in outer .fi¦eld",
        "let outer = 1 in outer #fi¦eld",
        "let outer = 1 in outer/fi¦eld",
        "let outer = 1 in +Con¦structor()",
    ] {
        let fixture = Fixture::new(source);
        assert!(
            fixture.session.complete(&fixture.path, fixture.offset).unwrap().is_none(),
            "{source}"
        );
    }
}

#[test]
fn invalid_and_opaque_cursors_are_rejected_without_affecting_valid_positions() {
    let fixture = Fixture::new("let value = 1 in \"🦀¦\"");
    assert!(matches!(
        fixture.session.complete(&fixture.path, fixture.offset),
        Err(CompletionError::Cursor(CompletionCursorError::OpaqueSource { .. }))
    ));
    assert!(matches!(
        fixture.session.complete(&fixture.path, fixture.offset - 1),
        Err(CompletionError::Cursor(CompletionCursorError::InvalidCharacterBoundary { .. }))
    ));
    assert!(matches!(
        fixture.session.complete(&fixture.path, usize::MAX),
        Err(CompletionError::Cursor(CompletionCursorError::OutOfBounds { .. }))
    ));
    for source in ["let value = 1 in /- ¦ -/", "let value = 1 in -- ¦", "let value = 1 in \"¦"] {
        let fixture = Fixture::new(source);
        assert!(matches!(
            fixture.session.complete(&fixture.path, fixture.offset),
            Err(CompletionError::Cursor(CompletionCursorError::OpaqueSource { .. }))
        ));
    }
    assert_eq!(Fixture::new("let value = 1 in /- 🦀 -/ ¦").names(), ["value"]);
}

#[test]
fn completion_does_not_replace_strict_analysis_or_accept_recovered_programs() {
    let fixture = Fixture::new("let value = 1 in val¦ue");
    let before = fixture.session.analyze(&fixture.path).unwrap();
    let source = fixture.session.source_text(&fixture.path).unwrap();
    assert_eq!(fixture.names(), ["value"]);
    let after = fixture.session.analyze(&fixture.path).unwrap();
    assert!(Arc::ptr_eq(&before, &after));
    assert_eq!(fixture.session.source_text(&fixture.path).unwrap(), source);

    let fixture = Fixture::new("let value = 1 in ¦");
    assert!(matches!(fixture.session.analyze(&fixture.path), Err(AnalysisError::Source { .. })));
    assert_eq!(fixture.names(), ["value"]);
    assert!(matches!(fixture.session.analyze(&fixture.path), Err(AnalysisError::Source { .. })));
}

#[test]
fn current_overlay_and_dependency_changes_invalidate_completion_results() {
    let mut fixture = Fixture::new("let old = 1 in ¦");
    let previous = fixture.completion();
    fixture.edit("let new = 2 in ¦");
    assert_eq!(fixture.names(), ["new"]);
    assert!(!Arc::ptr_eq(&previous, &fixture.completion()));

    let mut fixture = Fixture::new("let imported = @[import(\"provider.zy\")] _ in ¦")
        .with_dependency("provider.zy", "1");
    assert_eq!(fixture.annotation("imported").as_deref(), Some("Int64"));
    fixture
        .session
        .set_overlay(fixture.directory.path().join("provider.zy"), "\"text\"".into())
        .unwrap();
    assert_eq!(fixture.annotation("imported").as_deref(), Some("String"));
}
