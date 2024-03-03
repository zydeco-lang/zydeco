use async_lsp::lsp_types::{Position, Range};
use lsp_textdocument::FullTextDocument;
use zydeco_lang::utils::span::{Cursor1, Span};

pub fn span_to_range(span: &Span, document: &FullTextDocument) -> Range {
    let (l, r) = span.get_cursor1();
    Range::new(cursor1_to_position(l, document), cursor1_to_position(r, document))
}

fn cursor1_to_position(cursor1: Cursor1, document: &FullTextDocument) -> Position {
    serde_json::from_value(serde_json::to_value(document.position_at(cursor1 as u32)).unwrap())
        .unwrap()
}
