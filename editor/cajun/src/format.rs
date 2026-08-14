use serde::Deserialize;
use serde_json::Value;
use tower_lsp::lsp_types::{FormattingOptions, Position, Range, TextEdit};
use zydeco_surface::textual::{
    Lexer, SourceUnitParser,
    fmt::{IndentWidth, LayoutIntentions, PrettyFormatter, PrettyOptions},
    syntax::{LocationCtx, Parser},
};
use zydeco_utils::span::FileInfo;

/// Server-side formatter policy from client initialization options.
///
/// The LSP `FormattingOptions` only carries indentation preferences, so the
/// line width and layout-intentions policy come from the `format` section of
/// Cajun's initialization options.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct FormatterSettings {
    line_width: usize,
    layout_intentions: LayoutIntentions,
}

impl Default for FormatterSettings {
    fn default() -> Self {
        Self {
            line_width: PrettyOptions::default().line_width,
            layout_intentions: LayoutIntentions::Preserve,
        }
    }
}

impl FormatterSettings {
    pub(crate) fn from_initialization_options(options: Option<&Value>) -> Self {
        let default = Self::default();
        options
            .cloned()
            .and_then(|options| serde_json::from_value::<FormatInitializationOptions>(options).ok())
            .map_or(default, |options| Self {
                line_width: options
                    .format
                    .line_width
                    .filter(|line_width| *line_width > 0)
                    .unwrap_or(default.line_width),
                layout_intentions: options.format.layout_intentions.unwrap_or_default().into(),
            })
    }
}

#[derive(Default, Deserialize)]
struct FormatInitializationOptions {
    #[serde(default)]
    format: FormatSectionOptions,
}

#[derive(Default, Deserialize)]
#[serde(rename_all = "camelCase")]
struct FormatSectionOptions {
    line_width: Option<usize>,
    layout_intentions: Option<LayoutIntentionsSetting>,
}

/// The client-side spelling of the layout-intentions policy.
#[derive(Copy, Clone, Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
enum LayoutIntentionsSetting {
    #[default]
    Preserve,
    BlankLinesOnly,
    Ignore,
}

impl From<LayoutIntentionsSetting> for LayoutIntentions {
    fn from(setting: LayoutIntentionsSetting) -> Self {
        match setting {
            | LayoutIntentionsSetting::Preserve => Self::Preserve,
            | LayoutIntentionsSetting::BlankLinesOnly => Self::BlankLinesOnly,
            | LayoutIntentionsSetting::Ignore => Self::Ignore,
        }
    }
}

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

/// Cajun's protocol adapter for the configurable surface pretty printer.
pub(crate) struct DocumentFormatter {
    options: PrettyOptions,
}

impl DocumentFormatter {
    pub(crate) fn from_lsp(options: &FormattingOptions, settings: FormatterSettings) -> Self {
        let default = PrettyOptions::default();
        let indent = usize::try_from(options.tab_size)
            .ok()
            .and_then(IndentWidth::new)
            .unwrap_or(default.indent);
        Self {
            options: default
                .with_indent(indent)
                .with_line_width(settings.line_width)
                .with_layout_intentions(settings.layout_intentions),
        }
    }

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
        let formatted =
            PrettyFormatter::with_options(&parser.arena, self.options).render_unit(unit);
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
    use super::{
        DocumentFormatter, FormatterSettings, FormattingOutcome, FormattingSkip, WholeDocumentEdit,
    };
    use tower_lsp::lsp_types::{FormattingOptions, Position};

    struct TestOptions;

    impl TestOptions {
        fn spaces() -> FormattingOptions {
            FormattingOptions { tab_size: 2, insert_spaces: true, ..FormattingOptions::default() }
        }
    }

    #[test]
    fn formats_with_canonical_punning_and_minimal_parentheses() {
        let source = "(field = field, ((x)))";
        let formatter =
            DocumentFormatter::from_lsp(&TestOptions::spaces(), FormatterSettings::default());
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
        let formatter =
            DocumentFormatter::from_lsp(&TestOptions::spaces(), FormatterSettings::default());
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
    fn whole_document_range_uses_utf16_positions() {
        let edit = WholeDocumentEdit::replacing("x\n\"😀\"", "x".to_string()).unwrap();

        assert_eq!(edit.range.end, Position::new(1, 4));
    }

    #[test]
    fn skips_invalid_documents() {
        let formatter =
            DocumentFormatter::from_lsp(&TestOptions::spaces(), FormatterSettings::default());

        assert_eq!(
            formatter.format("begin ?"),
            FormattingOutcome::Skipped(FormattingSkip::InvalidSyntax)
        );
    }

    #[test]
    fn settings_apply_line_width_and_layout_intentions() {
        let wrapped = concat!(
            "! (bool/if)\n",
            "  (Ret Int64)\n",
            "  greater\n",
            "  { ret left }\n",
            "  { ret right }\n",
        );
        let joined = "! (bool/if) (Ret Int64) greater { ret left } { ret right }\n";

        let settings = FormatterSettings::from_initialization_options(Some(&serde_json::json!({
            "format": {
                "lineWidth": 200,
                "layoutIntentions": "ignore"
            }
        })));
        let formatter = DocumentFormatter::from_lsp(&TestOptions::spaces(), settings);
        let FormattingOutcome::Edit(edit) = formatter.format(wrapped) else {
            panic!("expected a formatting edit")
        };
        assert_eq!(edit.new_text, joined);
    }

    #[test]
    fn settings_default_when_init_options_are_missing_or_invalid() {
        assert_eq!(
            FormatterSettings::from_initialization_options(None),
            FormatterSettings::default()
        );
        assert_eq!(
            FormatterSettings::from_initialization_options(Some(&serde_json::json!({
                "format": { "lineWidth": 0 }
            }))),
            FormatterSettings::default()
        );
        assert_eq!(
            FormatterSettings::from_initialization_options(Some(&serde_json::json!({
                "format": { "layoutIntentions": "not-a-mode" }
            }))),
            FormatterSettings::default()
        );
    }

    #[test]
    fn settings_map_each_layout_intention_spelling() {
        for (spelling, expected) in [
            ("preserve", zydeco_surface::textual::fmt::LayoutIntentions::Preserve),
            ("blank-lines-only", zydeco_surface::textual::fmt::LayoutIntentions::BlankLinesOnly),
            ("ignore", zydeco_surface::textual::fmt::LayoutIntentions::Ignore),
        ] {
            let settings =
                FormatterSettings::from_initialization_options(Some(&serde_json::json!({
                    "format": { "layoutIntentions": spelling }
                })));
            assert_eq!(settings.layout_intentions, expected, "spelling: {spelling}");
        }
    }
}
