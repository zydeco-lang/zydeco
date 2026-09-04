use zydeco_session::{AnalysisError, ProgramAnalysis, SourceCaches, TyckReport};

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
            let _ = TyckReport::build(analysis, diagnostic).write(&mut cache, &mut output);
        });
        Self::plain(output)
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
