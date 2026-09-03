use super::*;
use std::path::PathBuf;

struct Fixture {
    _directory: tempfile::TempDir,
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
        Self { _directory: directory, session, path, source, position: Position::new(line, column) }
    }

    fn items(&self, label_details: bool) -> Option<Vec<CompletionItem>> {
        let completer =
            Completer { snippets: true, label_details, line_width: HoverLineWidth::DEFAULT };
        let CompletionResponse::Array(items) =
            completer.complete(&self.session, &self.path, self.position)?
        else {
            panic!("expected an array of completion items")
        };
        Some(items)
    }

    fn apply(&self, item: &CompletionItem) -> String {
        let Some(CompletionTextEdit::Edit(edit)) = &item.text_edit else {
            panic!("a name completion needs an explicit edit")
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
fn types_are_label_details_and_plain_detail_fallbacks_not_inserted_text() {
    let fixture = Fixture::new("let value = 1 in val¦ue_suffix");
    for label_details in [true, false] {
        let items = fixture.items(label_details).unwrap();
        let [item] = items.as_slice() else { panic!("one prefix-matching name") };
        assert_eq!(item.label, "value");
        assert_eq!(item.kind, Some(CompletionItemKind::VARIABLE));
        assert_eq!(item.detail.as_deref(), Some("Int64"));
        assert_eq!(item.filter_text.as_deref(), Some("value"));
        assert_eq!(item.insert_text_format, None);
        assert_eq!(
            item.label_details.as_ref().and_then(|details| details.detail.as_deref()),
            label_details.then_some(" : Int64"),
        );
        assert_eq!(fixture.apply(item), "let value = 1 in value");
    }
}

#[test]
fn type_definitions_share_classification_with_semantic_highlighting() {
    let fixture = Fixture::new("let Number = @[intrinsic(i64)] _ in Nu¦mber");
    let items = fixture.items(true).unwrap();
    assert_eq!(items.len(), 1);
    assert_eq!(items[0].kind, Some(CompletionItemKind::CLASS));
    assert_eq!(items[0].detail.as_deref(), Some("VType"));
}

#[test]
fn byte_edits_translate_to_utf16_even_with_astral_characters_and_crlf() {
    let fixture = Fixture::new("let value = 1 in\r\n/- 🦀 -/ val¦ue_suffix");
    let items = fixture.items(true).unwrap();
    let [item] = items.as_slice() else { panic!("one prefix-matching name") };
    let Some(CompletionTextEdit::Edit(edit)) = &item.text_edit else { unreachable!() };
    assert_eq!(edit.range.start, Position::new(1, 9));
    assert_eq!(edit.range.end, Position::new(1, 21));
    assert_eq!(fixture.apply(item), "let value = 1 in\r\n/- 🦀 -/ value");
}

#[test]
fn missing_semantics_leave_a_plain_name_completion() {
    let fixture = Fixture::new("let value = 1 in (¦, param invalid that invalid)");
    let items = fixture.items(true).unwrap();
    let [item] = items.as_slice() else { panic!("one visible name") };
    assert_eq!(item.label, "value");
    assert!(item.detail.is_none());
    assert!(item.label_details.is_none());
}

#[test]
fn sorting_preserves_compiler_order_and_shadowed_names_appear_once() {
    let fixture = Fixture::new("let first = 1 in let second = 2 in fn first => ¦");
    let items = fixture.items(true).unwrap();
    assert_eq!(
        items.iter().map(|item| item.label.as_str()).collect::<Vec<_>>(),
        ["first", "second"]
    );
    assert!(items.windows(2).all(|pair| pair[0].sort_text < pair[1].sort_text));
}

#[test]
fn expected_types_order_equal_names_and_omit_rigid_mismatches() {
    let fixture = Fixture::new(
        "let matching = 1 in let other = 'x' in val unknown => (_¦ : @[intrinsic(i64)] _)",
    );
    let items = fixture.items(true).unwrap();
    assert_eq!(
        items.iter().map(|item| item.label.as_str()).collect::<Vec<_>>(),
        ["matching", "unknown"]
    );
    assert_eq!(items[0].detail.as_deref(), Some("Int64"));
    assert!(items.windows(2).all(|pair| pair[0].sort_text < pair[1].sort_text));
}

#[test]
fn exact_prefix_quality_precedes_expected_type_evidence() {
    let fixture = Fixture::new("let item_equal = 1 in val item => (item¦ : @[intrinsic(i64)] _)");
    let items = fixture.items(true).unwrap();
    assert_eq!(
        items.iter().map(|item| item.label.as_str()).collect::<Vec<_>>(),
        ["item", "item_equal"]
    );
}

#[test]
fn metadata_owns_its_namespace_even_for_unsupported_arguments() {
    for source in [
        "let value = 1 in @[custom(val¦)] _",
        "let value = 1 in @[format(width(¦))] _",
        "let value = 1 in @[typeof(¦)] _",
        "let value = 1 in @[custom(value ¦)] _",
    ] {
        assert!(Fixture::new(source).items(true).is_none(), "{source}");
    }
    let items = Fixture::new("let value = 1 in @[int¦] _").items(true).unwrap();
    assert_eq!(items.iter().map(|item| item.label.as_str()).collect::<Vec<_>>(), ["intrinsic"]);
    let items = Fixture::new("let value = 1 in @[custom] val¦").items(true).unwrap();
    assert_eq!(items.iter().map(|item| item.label.as_str()).collect::<Vec<_>>(), ["value"]);
    assert_eq!(
        Fixture::new("let value = 1 in @[custom] val¦").apply(&items[0]),
        "let value = 1 in @[custom] value"
    );
}

#[test]
fn opaque_and_binding_positions_do_not_fall_back_to_all_definitions() {
    for source in [
        "let value = 1 in \"val¦ue\"",
        "let value = 1 in /- val¦ue -/ _",
        "let value = 1 in -- val¦",
        "let value = 1 in fn val¦ue => value",
        "let value = 1 in value .val¦ue",
        "let value = 1 in value/val¦ue",
    ] {
        assert!(Fixture::new(source).items(true).is_none(), "{source}");
    }
    assert_eq!(Fixture::new("let value = 1 in /- comment -/ val¦ue").items(true).unwrap().len(), 1);
}

#[test]
fn ordinary_names_survive_invalid_surrounding_syntax() {
    let fixture = Fixture::new("let value = 1 in (?, val¦ue)");
    let items = fixture.items(true).unwrap();
    assert_eq!(items.len(), 1);
    assert_eq!(items[0].label, "value");
}
