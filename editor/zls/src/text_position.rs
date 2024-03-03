use async_lsp::lsp_types::{Position, Range};
use zydeco_lang::utils::span::{Cursor2, Span};

pub fn span_to_range(span: &Span) -> Range {
    let (l, r) = span.get_cursor2();
    Range::new(cursor2_to_position(l), cursor2_to_position(r))
}

// TODO: handle UTF-16 encoding
fn cursor2_to_position(cursor2: &Cursor2) -> Position {
    Position::new(cursor2.line as u32 - 1, cursor2.column as u32 - 1)
}
