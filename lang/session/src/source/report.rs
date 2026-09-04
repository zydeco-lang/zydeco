//! Ariadne reports for type-checking diagnostics, shared by diagnostic frontends.
//!
//! The report shape — primary span, related labels with generated colors, and help
//! notes — is frontend-independent; only the output sink differs between callers.

use ariadne::{Label, Report, ReportKind};
use zydeco_statics::TyckDiagnostic;
use zydeco_utils::span::{PathDisplay, Span, internal_ariadne_span};

use super::ProgramAnalysis;

/// Construction of ariadne reports over a checked program analysis.
pub struct TyckReport;

impl TyckReport {
    /// The report of one diagnostic, with spans resolved against the analysis' source map.
    pub fn build(
        analysis: &ProgramAnalysis, diagnostic: &TyckDiagnostic,
    ) -> Report<'static, (PathDisplay, std::ops::Range<usize>)> {
        let resolve =
            |span: Span| analysis.spans().source_map().and_then(|map| map.ariadne_range(span));
        let primary_span = diagnostic
            .primary
            .as_ref()
            .and_then(|primary| resolve(primary.span))
            .unwrap_or_else(internal_ariadne_span);
        let mut colors = ariadne::ColorGenerator::new();
        let primary_color = colors.next();
        let mut report = Report::build(ReportKind::Error, primary_span)
            .with_code(diagnostic.code)
            .with_message(&diagnostic.message);
        if let Some((primary, span)) = diagnostic
            .primary
            .as_ref()
            .and_then(|primary| resolve(primary.span).map(|span| (primary, span)))
        {
            report = report.with_label(
                Label::new(span).with_message(&primary.message).with_color(primary_color),
            );
        }
        for related in &diagnostic.related {
            if let Some(span) = resolve(related.span) {
                report = report.with_label(
                    Label::new(span).with_message(&related.message).with_color(colors.next()),
                );
            }
        }
        for help in &diagnostic.help {
            report = report.with_help(help);
        }
        report.finish()
    }
}
