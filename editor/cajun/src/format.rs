use tower_lsp::lsp_types::{Position, Range, TextEdit};
use zydeco_surface::textual::{
    Lexer, SourceUnitParser,
    fmt::PrettyFormatter,
    syntax::{LocationCtx, Parser},
};
use zydeco_utils::span::FileInfo;

/// One complete outcome of a whole-document formatting request.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum FormattingOutcome {
    Edit(TextEdit),
    Unchanged,
    Skipped(FormattingSkip),
}

/// A reason formatting could not produce an edit.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum FormattingSkip {
    InvalidSyntax,
    PositionOverflow,
}

impl FormattingSkip {
    pub(crate) fn message(self) -> &'static str {
        match self {
            | Self::InvalidSyntax => "the document does not parse",
            | Self::PositionOverflow => "the document is too large for an LSP text edit",
        }
    }
}

/// Cajun's protocol adapter for the surface pretty printer.
///
/// Formatting policy comes from `@[format(...)]` annotations in the source,
/// so the adapter carries no client configuration.
pub(crate) struct DocumentFormatter;

impl DocumentFormatter {
    pub(crate) fn format(&self, source: &str) -> FormattingOutcome {
        let mut parser = Parser::new();
        let unit = match SourceUnitParser::new().parse(
            source,
            &LocationCtx::Plain,
            &mut parser,
            Lexer::new(source),
        ) {
            | Ok(unit) => unit,
            | Err(_) => return FormattingOutcome::Skipped(FormattingSkip::InvalidSyntax),
        };
        let formatted = PrettyFormatter::new(&parser.arena).render_unit(unit);
        if formatted == source {
            return FormattingOutcome::Unchanged;
        }

        WholeDocumentEdit::replacing(source, formatted)
            .map(FormattingOutcome::Edit)
            .unwrap_or(FormattingOutcome::Skipped(FormattingSkip::PositionOverflow))
    }
}

struct WholeDocumentEdit;

impl WholeDocumentEdit {
    fn replacing(source: &str, new_text: String) -> Option<TextEdit> {
        let cursor = FileInfo::new(source, None).trans_span2_utf16(source, source.len())?;
        let line = u32::try_from(cursor.line).ok()?;
        let character = u32::try_from(cursor.column).ok()?;
        Some(TextEdit {
            range: Range::new(Position::new(0, 0), Position::new(line, character)),
            new_text,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{DocumentFormatter, FormattingOutcome, FormattingSkip, WholeDocumentEdit};
    use tower_lsp::lsp_types::Position;

    #[test]
    fn formats_with_canonical_punning_and_minimal_parentheses() {
        let source = "(field = field, ((x)))";
        let formatter = DocumentFormatter;
        let FormattingOutcome::Edit(edit) = formatter.format(source) else {
            panic!("expected a formatting edit")
        };

        assert_eq!(edit.range.start, Position::new(0, 0));
        assert_eq!(edit.range.end, Position::new(0, source.encode_utf16().count() as u32));
        assert_eq!(edit.new_text, "(= field, x)\n");
    }

    #[test]
    fn formatting_preserves_comments() {
        let source = concat!(
            "--| Keep this documentation.\n",
            "-- Keep this comment.\n",
            "/- Keep this block. -/\n",
            "(field = field, ((x)))",
        );
        let formatter = DocumentFormatter;
        let FormattingOutcome::Edit(edit) = formatter.format(source) else {
            panic!("expected a formatting edit")
        };

        assert_eq!(
            edit.new_text,
            concat!(
                "--| Keep this documentation.\n",
                "-- Keep this comment.\n",
                "/- Keep this block. -/\n",
                "(= field, x)\n",
            )
        );
    }

    #[test]
    fn formatting_follows_source_format_annotations() {
        let source = concat!(
            "@[format(layout(ignore))] ! (bool/if)\n",
            "  (Ret Int64)\n",
            "  greater\n",
            "  { ret left }\n",
            "  { ret right }\n",
        );
        let formatter = DocumentFormatter;
        let FormattingOutcome::Edit(edit) = formatter.format(source) else {
            panic!("expected a formatting edit")
        };

        assert_eq!(
            edit.new_text,
            "@[format(layout(ignore))] ! (bool/if) (Ret Int64) greater { ret left } { ret right }\n"
        );
    }

    #[test]
    fn whole_document_range_uses_utf16_positions() {
        let edit = WholeDocumentEdit::replacing("x\n\"😀\"", "x".to_string()).unwrap();

        assert_eq!(edit.range.end, Position::new(1, 4));
    }

    #[test]
    fn skips_invalid_documents() {
        let formatter = DocumentFormatter;

        assert_eq!(
            formatter.format("begin ?"),
            FormattingOutcome::Skipped(FormattingSkip::InvalidSyntax)
        );
    }
}
