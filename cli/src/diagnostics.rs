use crate::CompileError;
use ariadne::{Label, Report, ReportKind};
use zydeco_session::{AnalysisError, ProgramAnalysis, SourceCaches, SourceGraph, TyckReport};
use zydeco_statics::{TyckObservation, arena::StaticsArena, fmt as static_fmt, syntax as ss};
use zydeco_syntax::{Pretty, SpanView, Ugly};
use zydeco_utils::span::PathDisplay;

/// Render source-aware failures at the CLI boundary.
pub struct DiagnosticRenderer;

impl DiagnosticRenderer {
    pub fn error(error: &CompileError) {
        match error {
            | CompileError::Rejected(analysis) => {
                Self::warnings(analysis);
                if let Some(diagnostics) = analysis.outcome().diagnostics() {
                    let mut cache = SourceCaches::analysis(analysis);
                    diagnostics.iter().for_each(|diagnostic| {
                        let _ = TyckReport::build(analysis, diagnostic).eprint(&mut cache);
                    });
                }
            }
            | CompileError::Analysis(AnalysisError::Resolve { error, graph, spans }) => {
                Self::graph_warnings(graph);
                let _ = error.to_report(spans).eprint(SourceCaches::graph(graph));
            }
            | CompileError::SpsLower(failure) => {
                let mut cache = SourceCaches::span_arena(&failure.spans);
                for error in &failure.errors {
                    let _ = error
                        .to_report(&failure.spans, &failure.scoped, &failure.statics)
                        .eprint(&mut cache);
                }
            }
            | _ => eprintln!("{error}"),
        }
    }

    pub fn warnings(analysis: &ProgramAnalysis) {
        Self::graph_warnings(analysis.graph());
    }

    fn graph_warnings(graph: &SourceGraph) {
        let mut cache = SourceCaches::graph(graph);
        graph.warnings().into_iter().for_each(|site| {
            let path = PathDisplay::from(site.path().to_path_buf());
            let span = (path, site.warning.range().clone());
            let report = Report::build(ReportKind::Warning, span.clone())
                .with_message(site.warning.message())
                .with_label(Label::new(span).with_message("this text block contributes no text"))
                .with_note(site.warning.note())
                .finish();
            let _ = report.eprint(&mut cache);
        });
    }

    pub fn observations(analysis: &ProgramAnalysis, statics: &StaticsArena) {
        if analysis
            .observations()
            .iter()
            .any(|observation| matches!(observation, TyckObservation::HoleSolution { .. }))
        {
            println!("Hole Solutions:");
        }
        analysis.observations().iter().for_each(|observation| match observation {
            | TyckObservation::HoleSolution { site, solution } => {
                let formatter = zydeco_surface::scoped::fmt::Formatter::new(analysis.scoped());
                let site_text = match site {
                    | ss::InferenceSite::Term(term) => term.ugly(&formatter),
                    | ss::InferenceSite::Pattern(pattern) => pattern.ugly(&formatter),
                };
                let span_context = (analysis.spans(), analysis.scoped());
                let span = match site {
                    | ss::InferenceSite::Term(term) => term.span(&span_context),
                    | ss::InferenceSite::Pattern(pattern) => pattern.span(&span_context),
                };
                let span = analysis
                    .spans()
                    .source_map()
                    .map(|map| map.display(*span).to_string())
                    .unwrap_or_else(|| span.to_string());
                let solution = solution.map_or_else(
                    || "???".to_owned(),
                    |solution| {
                        solution.ugly(&static_fmt::Formatter::new(analysis.scoped(), statics))
                    },
                );
                println!("{site_text} @ {span} : {solution}");
            }
            | TyckObservation::Debug { metadata, result } => {
                print!("[debug printing] ");
                metadata.arguments().iter().for_each(|argument| print!("{argument}"));
                let formatter = static_fmt::Formatter::new(analysis.scoped(), statics);
                match result {
                    | ss::TermAnnId::Hole(fill) => println!(" (hole): {}", fill.concise()),
                    | ss::TermAnnId::Kind(kind) => {
                        println!(" (kind): {}", Self::pretty(&formatter, *kind))
                    }
                    | ss::TermAnnId::Type(ty, kind) => println!(
                        " (type):{}\nof kind:{}",
                        Self::nested(&formatter, *ty),
                        Self::nested(&formatter, *kind),
                    ),
                    | ss::TermAnnId::Value(value, ty) => println!(
                        " (value):{}\nof type:{}",
                        Self::nested(&formatter, *value),
                        Self::nested(&formatter, *ty),
                    ),
                    | ss::TermAnnId::Compu(computation, ty) => println!(
                        " (computation):{}\nof type:{}",
                        Self::nested(&formatter, *computation),
                        Self::nested(&formatter, *ty),
                    ),
                }
            }
        });
    }

    fn pretty<T>(formatter: &static_fmt::Formatter<'_>, item: T) -> String
    where
        T: for<'format> Pretty<'format, static_fmt::Formatter<'format>>,
    {
        let mut output = String::new();
        item.pretty(formatter).render_fmt(100, &mut output).unwrap();
        output
    }

    fn nested<T>(formatter: &static_fmt::Formatter<'_>, item: T) -> String
    where
        T: for<'format> Pretty<'format, static_fmt::Formatter<'format>>,
    {
        format!("\n\t{}", Self::pretty(formatter, item).replace('\n', "\n\t"))
    }
}
