//! Textual formatting, layout-intention capture, and comment preservation for Zydeco.

mod config;
/// Author-selected layout attached to canonical textual IDs.
pub mod intention;
/// Formatting presentation captured outside `zydeco-surface`'s syntax arena.
pub mod presentation;
mod pretty;
/// Comments and other source material retained outside canonical syntax.
pub mod trivia;

pub use config::{IndentWidth, LayoutIntentions, Parentheses, PrettyOptions};
pub use intention::{BreakIntent, LayoutBoundary, LineExtent, SourceLine, SurfaceIntentions};
pub use presentation::FormatPresentation;
pub use pretty::PrettyFormatter;
pub use trivia::{
    BlockComment, LeadingComment, LineComment, LineSeparation, SurfaceComment, SurfaceTrivia,
    TrailingComment,
};
pub use zydeco_surface::textual::TextBlock;

use zydeco_surface::textual::{
    ParseFailure, StrictParser,
    arena::{SpanArena, TextArena},
    syntax::{Parser, SourceUnit},
};

/// A strictly parsed source unit paired with its formatting presentation.
pub struct FormattedSource {
    pub unit: SourceUnit,
    pub spans: SpanArena,
    pub arena: TextArena,
    pub presentation: FormatPresentation,
}

impl FormattedSource {
    /// Parse `source` strictly while capturing formatting prefixes, trivia, and layout intentions.
    pub fn parse(source: &str) -> Result<Self, ParseFailure> {
        let mut parser = Parser::for_formatting();
        let unit = StrictParser::source(source, &mut parser)?;
        let mut presentation = FormatPresentation::default();
        presentation.capture(source, &parser);
        let (spans, arena) = parser.finish();
        Ok(Self { unit, spans, arena, presentation })
    }

    /// Render the parsed source unit according to `options`.
    pub fn render(&self, source: &str, options: PrettyOptions) -> String {
        PrettyFormatter::with_options_source(
            &self.arena,
            &self.spans,
            &self.presentation,
            options,
            source,
        )
        .render_unit(self.unit)
    }
}

/// Strictly parse and format `source` with `options`.
pub fn format_source(source: &str, options: PrettyOptions) -> Result<String, ParseFailure> {
    let parsed = FormattedSource::parse(source)?;
    Ok(parsed.render(source, options))
}
