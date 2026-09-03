use super::*;
use crate::{completion::Completer, hover::HoverLineWidth};
use tower_lsp::lsp_types::Position;
use zydeco_surface::textual::{StrictParser, syntax::Parser};
use zydeco_utils::span::LineCol;

struct Fixture {
    directory: tempfile::TempDir,
    session: CompilerSession,
    path: PathBuf,
    source: String,
    position: Position,
}

impl Fixture {
    fn new(marked: &str) -> Self {
        let offset = marked.find('¦').expect("a fixture needs a cursor");
        let source = marked.replacen('¦', "", 1);
        let LineCol { line, column } =
            FileMap::local(source.as_str(), None).line_col_utf16(offset).unwrap();
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("main.zy");
        let mut session = CompilerSession::default();
        session.set_overlay(&path, source.clone()).unwrap();
        Self { directory, session, path, source, position: Position::new(line, column) }
    }

    fn write(&self, path: &str) {
        let path = self.directory.path().join(path);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, "()").unwrap();
    }

    fn items(&self) -> Option<Vec<CompletionItem>> {
        let completer =
            Completer { snippets: true, label_details: true, line_width: HoverLineWidth::DEFAULT };
        match completer.complete(&self.session, &self.path, self.position)? {
            | CompletionResponse::List(list) => {
                assert!(list.is_incomplete, "typing must refresh the directory and replacement");
                Some(list.items)
            }
            | CompletionResponse::Array(items) => Some(items),
        }
    }

    fn labels(&self) -> Vec<String> {
        self.items().unwrap().into_iter().map(|item| item.label).collect()
    }

    fn apply(&self, item: &CompletionItem) -> String {
        let Some(CompletionTextEdit::Edit(edit)) = &item.text_edit else {
            panic!("path candidates need explicit edits")
        };
        let map = FileMap::local(self.source.as_str(), None);
        let offset = |position: Position| {
            map.offset_utf16(LineCol { line: position.line, column: position.character }).unwrap()
        };
        let mut source = self.source.clone();
        source.replace_range(offset(edit.range.start)..offset(edit.range.end), &edit.new_text);
        source
    }
}

#[test]
fn import_paths_complete_both_annotation_forms_and_unfinished_strings() {
    for marked in [
        r#"@[import("¦")] _"#,
        r#"@(import("¦"))"#,
        r#"@[import("¦"#,
        r#"-/ @[import( /- nested /- comment -/ -/ "¦"#,
    ] {
        let fixture = Fixture::new(marked);
        fixture.write("z-folder/child.zy");
        fixture.write("library.zy");
        fixture.write("library.zyi");
        fixture.write("program.zydeco");
        fixture.write("notes.md");
        let items = fixture.items().unwrap();
        assert_eq!(
            items.iter().map(|item| item.label.as_str()).collect::<Vec<_>>(),
            ["z-folder/", "library.zy", "library.zyi", "program.zydeco"],
            "{marked}"
        );
        assert_eq!(items[0].kind, Some(CompletionItemKind::FOLDER));
        assert_eq!(items[1].kind, Some(CompletionItemKind::FILE));
        assert_eq!(items[2].detail.as_deref(), Some("Zydeco signature"));
        assert!(items.iter().all(|item| item.insert_text_format.is_none()));
        assert!(items.windows(2).all(|pair| pair[0].sort_text < pair[1].sort_text));
    }
}

#[test]
fn import_paths_replace_the_complete_component_and_preserve_quotes_and_prefixes() {
    for (marked, expected) in [
        (r#"@[import("nested/li¦brary_suffix.zy")] _"#, r#"@[import("nested/library.zy")] _"#),
        (r#"@(import("./nested/li¦brary_suffix.zy"))"#, r#"@(import("./nested/library.zy"))"#),
        (r#"@[import("nested/li¦"#, r#"@[import("nested/library.zy"#),
    ] {
        let fixture = Fixture::new(marked);
        fixture.write("nested/library.zy");
        fixture.write("nested/other.zy");
        let items = fixture.items().unwrap();
        let [item] = items.as_slice() else { panic!("one prefix match") };
        assert_eq!(item.label, "library.zy");
        assert_eq!(fixture.apply(item), expected);
    }
}

#[test]
fn directory_completion_preserves_the_following_path_without_doubling_separators() {
    let fixture = Fixture::new(r#"@[import("nest¦ed_suffix/child.zy")] _"#);
    fixture.write("nested/child.zy");
    fixture.write("nested.zy");
    let items = fixture.items().unwrap();
    let [item] = items.as_slice() else { panic!("only directories can precede a separator") };
    assert_eq!(item.label, "nested/");
    assert_eq!(fixture.apply(item), r#"@[import("nested/child.zy")] _"#);

    let fixture = Fixture::new(r#"@[import("nest¦")] _"#);
    fixture.write("nested/child.zy");
    assert_eq!(fixture.apply(&fixture.items().unwrap()[0]), r#"@[import("nested/")] _"#);
}

#[test]
fn import_paths_use_current_overlays_and_return_empty_lists_for_missing_directories() {
    let mut fixture = Fixture::new(r#"@[import("virtual/¦")] _"#);
    fixture
        .session
        .set_overlay(fixture.directory.path().join("virtual/unsaved.zy"), "()".into())
        .unwrap();
    assert_eq!(fixture.labels(), ["unsaved.zy"]);
    assert!(Fixture::new(r#"@[import("missing/¦")] _"#).items().unwrap().is_empty());
}

#[test]
fn unrelated_literals_comments_and_invalid_metadata_arguments_reject_path_completion() {
    for marked in [
        r#""lib¦""#,
        r#"@[doc("lib¦")] _"#,
        r#"@[custom(import("lib¦"))] _"#,
        r#"@[ffi(c, library("lib¦"), symbol("entry"))] _"#,
        r#"@[import(1, "lib¦")] _"#,
        r#"@[import(1 "lib¦")] _"#,
        r#"@[import(_ "lib¦")] _"#,
        r#"@[import(: "lib¦")] _"#,
        r#"@[import(name"lib¦")] _"#,
        r#"@[import('l¦')] _"#,
        r#"@[import(1¦)] _"#,
        r#"-- @[import("lib¦"#,
        r#"/- @[import("lib¦ -/"#,
        r#"--| @[import("lib¦"#,
        r#"@[import("library.zy"¦)] _"#,
        "@[import(\"line\nlib¦\")] _",
        r#"@[import("lib\¦"#,
        r#"@[import("lib\¦"suffix.zy")] _"#,
    ] {
        let fixture = Fixture::new(marked);
        fixture.write("library.zy");
        assert!(fixture.items().is_none(), "{marked}");
    }
}

#[test]
fn import_path_edits_use_utf16_positions_and_preserve_unicode() {
    let fixture = Fixture::new("/- 🦀 -/\r\n@(import(\"目录/🦀¦suffix.zy\"))");
    fixture.write("目录/🦀 library.zy");
    let items = fixture.items().unwrap();
    let [item] = items.as_slice() else { panic!("one Unicode path") };
    let Some(CompletionTextEdit::Edit(edit)) = &item.text_edit else { unreachable!() };
    assert_eq!(edit.range.start, Position::new(1, 13));
    assert_eq!(edit.range.end, Position::new(1, 24));
    assert_eq!(fixture.apply(item), "/- 🦀 -/\r\n@(import(\"目录/🦀 library.zy\"))");
}

#[cfg(unix)]
#[test]
fn escaped_path_components_round_trip_through_import_parsing() {
    for (marked, name) in [
        (r#"@(import("quote\"¦suffix.zy"))"#, "quote\"name.zy"),
        (r#"@(import("back\\¦suffix.zy"))"#, "back\\name.zy"),
        (r#"@(import("line\n¦suffix.zy"))"#, "line\nname.zy"),
        (r#"@(import("tab\t¦suffix.zy"))"#, "tab\tname.zy"),
        (r#"@(import("¦"))"#, "dollar${name}.zy"),
    ] {
        let fixture = Fixture::new(marked);
        fixture.write(name);
        let items = fixture.items().unwrap();
        let [item] = items.as_slice() else { panic!("one escaped path: {marked}") };
        let completed = fixture.apply(item);
        let mut parser = Parser::new();
        let source = StrictParser::source(&completed, &mut parser).unwrap();
        let imports = source.imports(&parser.arena, &parser.spans).unwrap();
        assert_eq!(
            imports[0].directive.target,
            zydeco_surface::textual::ImportTarget::Path(PathBuf::from(name))
        );
    }

    let fixture = Fixture::new(r#"@(import("quote\"dir/back\\¦suffix.zy"))"#);
    fixture.write("quote\"dir/back\\name.zy");
    assert_eq!(
        fixture.apply(&fixture.items().unwrap()[0]),
        r#"@(import("quote\"dir/back\\name.zy"))"#
    );
}
