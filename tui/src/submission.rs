use thiserror::Error;
use zydeco_surface::textual::{
    StrictParser,
    syntax::{Hole, MetaNode, MetaTerm, Parser, Term},
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum ExpressionMode {
    Evaluate,
    Run,
    Type,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum ControlCommand {
    Help,
    Quit,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum Submission {
    Control(ControlCommand),
    Expression(ExpressionMode),
}

#[derive(Debug)]
pub(crate) enum SubmissionState {
    Empty,
    Incomplete,
    Invalid,
    Complete(Result<Submission, CommandError>),
}

pub(crate) struct SubmissionParser;

impl SubmissionParser {
    pub(crate) fn parse(source: &str) -> SubmissionState {
        if source.trim().is_empty() {
            return SubmissionState::Empty;
        }

        let mut parser = Parser::new();
        match StrictParser::source(source, &mut parser) {
            | Ok(unit) => SubmissionState::Complete(Self::decode(unit.root, &parser)),
            | Err(error) if error.is_unrecognized_eof() => SubmissionState::Incomplete,
            | Err(_) => SubmissionState::Invalid,
        }
    }

    fn decode(
        root: zydeco_surface::textual::syntax::TermId, parser: &Parser,
    ) -> Result<Submission, CommandError> {
        let Term::Meta(MetaTerm(meta, payload)) = &parser.arena.terms[&root] else {
            return Ok(Submission::Expression(ExpressionMode::Evaluate));
        };
        let meta = &parser.arena.metas[meta];
        let Some(command) = ReplCommandName::from_meta(meta) else {
            return Ok(Submission::Expression(ExpressionMode::Evaluate));
        };
        if !meta.arguments().is_empty() {
            return Err(CommandError::UnexpectedArguments {
                command,
                found: meta.arguments().len(),
            });
        }

        let payload_is_hole = matches!(parser.arena.terms[payload], Term::Hole(Hole));
        match command {
            | ReplCommandName::Help if payload_is_hole => {
                Ok(Submission::Control(ControlCommand::Help))
            }
            | ReplCommandName::Quit if payload_is_hole => {
                Ok(Submission::Control(ControlCommand::Quit))
            }
            | ReplCommandName::Help | ReplCommandName::Quit => {
                Err(CommandError::ExpectedHole { command })
            }
            | ReplCommandName::Type if !payload_is_hole => {
                Ok(Submission::Expression(ExpressionMode::Type))
            }
            | ReplCommandName::Run if !payload_is_hole => {
                Ok(Submission::Expression(ExpressionMode::Run))
            }
            | ReplCommandName::Type | ReplCommandName::Run => {
                Err(CommandError::ExpectedExpression { command })
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum ReplCommandName {
    Help,
    Quit,
    Run,
    Type,
}

impl ReplCommandName {
    fn from_meta(meta: &MetaNode) -> Option<Self> {
        match meta.callee()? {
            | "help" => Some(Self::Help),
            | "quit" => Some(Self::Quit),
            | "run" => Some(Self::Run),
            | "type" => Some(Self::Type),
            | _ => None,
        }
    }
}

impl std::fmt::Display for ReplCommandName {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            | Self::Help => "help",
            | Self::Quit => "quit",
            | Self::Run => "run",
            | Self::Type => "type",
        })
    }
}

#[derive(Debug, Error)]
pub(crate) enum CommandError {
    #[error("REPL command `@[{}]` does not accept metadata arguments (found {found})", .command)]
    UnexpectedArguments { command: ReplCommandName, found: usize },
    #[error("REPL command `@[{}]` must annotate `_`", .command)]
    ExpectedHole { command: ReplCommandName },
    #[error("REPL command `@[{}]` must annotate an expression", .command)]
    ExpectedExpression { command: ReplCommandName },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn commands_are_root_metadata_annotations() {
        assert!(matches!(
            SubmissionParser::parse("@[type] ret 1"),
            SubmissionState::Complete(Ok(Submission::Expression(ExpressionMode::Type)))
        ));
        assert!(matches!(
            SubmissionParser::parse("@[help] _"),
            SubmissionState::Complete(Ok(Submission::Control(ControlCommand::Help)))
        ));
        assert!(matches!(
            SubmissionParser::parse("@[quit] _"),
            SubmissionState::Complete(Ok(Submission::Control(ControlCommand::Quit)))
        ));
        assert!(matches!(
            SubmissionParser::parse("@(help)"),
            SubmissionState::Complete(Ok(Submission::Control(ControlCommand::Help)))
        ));
    }

    #[test]
    fn numbered_imports_are_regular_expressions() {
        assert!(matches!(
            SubmissionParser::parse("@[import(1)] _"),
            SubmissionState::Complete(Ok(Submission::Expression(ExpressionMode::Evaluate)))
        ));
    }

    #[test]
    fn an_open_block_requests_another_line() {
        assert!(matches!(SubmissionParser::parse("begin\n"), SubmissionState::Incomplete));
    }
}
