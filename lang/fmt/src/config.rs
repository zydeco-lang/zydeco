//! Independent policy choices for textual pretty printing.

use zydeco_surface::metadata::FormatMeta;
pub use zydeco_surface::metadata::{IndentWidth, LayoutIntentions, Parentheses};

/// Independent policy choices used by the textual pretty printer.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct PrettyOptions {
    pub indent: IndentWidth,
    pub line_width: usize,
    pub layout_intentions: LayoutIntentions,
    pub parentheses: Parentheses,
}

impl Default for PrettyOptions {
    fn default() -> Self {
        Self {
            indent: IndentWidth::DEFAULT,
            line_width: 100,
            layout_intentions: LayoutIntentions::Preserve,
            parentheses: Parentheses::Minimal,
        }
    }
}

impl PrettyOptions {
    pub fn with_indent(mut self, indent: IndentWidth) -> Self {
        self.indent = indent;
        self
    }

    pub fn with_line_width(mut self, line_width: usize) -> Self {
        self.line_width = line_width;
        self
    }

    pub fn with_layout_intentions(mut self, layout_intentions: LayoutIntentions) -> Self {
        self.layout_intentions = layout_intentions;
        self
    }

    pub fn with_parentheses(mut self, parentheses: Parentheses) -> Self {
        self.parentheses = parentheses;
        self
    }

    /// Override the options that a format directive names, leaving the
    /// remaining options unchanged.
    pub(crate) fn with_format_meta(self, meta: &FormatMeta) -> Self {
        Self {
            line_width: meta.width.unwrap_or(self.line_width),
            indent: meta.indent.unwrap_or(self.indent),
            layout_intentions: meta.layout.unwrap_or(self.layout_intentions),
            parentheses: meta.parentheses.unwrap_or(self.parentheses),
        }
    }
}
