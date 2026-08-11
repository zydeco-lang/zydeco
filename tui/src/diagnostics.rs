use ariadne::FnCache;
use std::{collections::HashMap, path::PathBuf};
use zydeco_session::{AnalysisError, ProgramAnalysis, SourceGraph};
use zydeco_utils::span::PathDisplay;

pub(crate) struct DiagnosticText;

impl DiagnosticText {
    pub(crate) fn analysis_error(error: &AnalysisError) -> String {
        match error {
            | AnalysisError::Resolve { error, graph } => {
                let mut output = Vec::new();
                let _ = error.to_report().write(Self::graph_cache(graph), &mut output);
                Self::plain(output)
            }
            | _ => error.to_string(),
        }
    }

    pub(crate) fn rejected(analysis: &ProgramAnalysis) -> String {
        let Some(reports) = analysis.outcome().reports() else {
            return "type checking rejected this source".to_owned();
        };
        let mut output = Vec::new();
        reports.iter().for_each(|report| {
            let _ = report.write(Self::analysis_cache(analysis), &mut output);
        });
        Self::plain(output)
    }

    fn analysis_cache(
        analysis: &ProgramAnalysis,
    ) -> FnCache<
        PathDisplay,
        impl FnMut(&PathDisplay) -> Result<String, Box<dyn std::fmt::Debug>>,
        String,
    > {
        Self::source_cache(
            analysis
                .sources()
                .map(|(path, source)| (path.to_path_buf(), source.to_owned()))
                .collect(),
        )
    }

    fn graph_cache(
        graph: &SourceGraph,
    ) -> FnCache<
        PathDisplay,
        impl FnMut(&PathDisplay) -> Result<String, Box<dyn std::fmt::Debug>>,
        String,
    > {
        Self::source_cache(
            graph
                .sources
                .iter()
                .map(|(_, source)| (source.path.clone(), source.source.clone()))
                .collect(),
        )
    }

    fn source_cache(
        sources: HashMap<PathBuf, String>,
    ) -> FnCache<
        PathDisplay,
        impl FnMut(&PathDisplay) -> Result<String, Box<dyn std::fmt::Debug>>,
        String,
    > {
        FnCache::new(move |path: &PathDisplay| {
            sources.get(path.as_path()).cloned().ok_or_else(|| {
                Box::new(format!("source file not found: {}", path.as_path().display()))
                    as Box<dyn std::fmt::Debug>
            })
        })
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
