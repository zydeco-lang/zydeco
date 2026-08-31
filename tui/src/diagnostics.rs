use ariadne::{Label, Report, ReportKind};
use zydeco_session::{AnalysisError, ProgramAnalysis, SourceCaches};
use zydeco_statics::TyckDiagnostic;
use zydeco_utils::span::{PathDisplay, Span, internal_ariadne_span};

pub(crate) struct DiagnosticText;

impl DiagnosticText {
    pub(crate) fn analysis_error(error: &AnalysisError) -> String {
        match error {
            | AnalysisError::Resolve { error, graph, spans } => {
                let mut output = Vec::new();
                let _ = error.to_report(spans).write(SourceCaches::graph(graph), &mut output);
                Self::plain(output)
            }
            | _ => error.to_string(),
        }
    }

    pub(crate) fn rejected(analysis: &ProgramAnalysis) -> String {
        let Some(diagnostics) = analysis.outcome().diagnostics() else {
            return "type checking rejected this source".to_owned();
        };
        let mut output = Vec::new();
        let mut cache = SourceCaches::analysis(analysis);
        diagnostics.iter().for_each(|diagnostic| {
            let _ = Self::tyck_report(analysis, diagnostic).write(&mut cache, &mut output);
        });
        Self::plain(output)
    }

    fn tyck_report(
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

    fn plain(output: Vec<u8>) -> String {
        let rendered = String::from_utf8_lossy(&output);
        let mut state = EscapeState::Text;
        rendered
            .chars()
            .filter(|character| match state {
                | EscapeState::Text if *character == '\u{1b}' => {
                    state = EscapeState::Escape;
                    false
                }
                | EscapeState::Text => true,
                | EscapeState::Escape if *character == '[' => {
                    state = EscapeState::ControlSequence;
                    false
                }
                | EscapeState::Escape => {
                    state = EscapeState::Text;
                    false
                }
                | EscapeState::ControlSequence if ('@'..='~').contains(character) => {
                    state = EscapeState::Text;
                    false
                }
                | EscapeState::ControlSequence => false,
            })
            .collect::<String>()
            .trim()
            .to_owned()
    }
}

#[derive(Copy, Clone)]
enum EscapeState {
    Text,
    Escape,
    ControlSequence,
}
