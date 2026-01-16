use super::lexer::Tok;
use ariadne::{Label, Report, ReportKind};
use std::fmt::Display;
use zydeco_utils::span::{Cursor1, FileInfo};

/// Wrapper around LALRPOP parse errors with file context.
pub struct ParseError<'input> {
    pub error: lalrpop_util::ParseError<Cursor1, Tok<'input>, &'input str>,
    pub file_info: &'input FileInfo,
}

impl ParseError<'_> {
    /// Create an Ariadne report for this parse error.
    pub fn to_report(&self) -> Report<'static, (String, std::ops::Range<usize>)> {
        use lalrpop_util::ParseError::*;
        let ParseError { error, file_info: info } = self;
        let file_path = info.path().to_string_lossy().to_string();

        match error {
            | User { error } => {
                Report::build(ReportKind::Error, "<internal>".to_string(), 0)
                    .with_message("Parse error")
                    .with_note(error.to_string())
                    .finish()
            }
            | InvalidToken { location } => {
                let location_str = info.trans_span2(*location);
                Report::build(ReportKind::Error, file_path.clone(), *location)
                    .with_message("Invalid token")
                    .with_label(
                        Label::new((file_path.clone(), *location..*location))
                            .with_message(format!("invalid token at {}", location_str)),
                    )
                    .finish()
            }
            | UnrecognizedEof { location, expected } => {
                let location_str = info.trans_span2(*location);
                let expected_msg = fmt_expected(expected);
                let mut report = Report::build(ReportKind::Error, file_path.clone(), *location)
                    .with_message("Unrecognized EOF")
                    .with_label(
                        Label::new((file_path.clone(), *location..*location))
                            .with_message(format!("unexpected end of file at {}", location_str)),
                    );
                if !expected_msg.is_empty() {
                    report = report.with_note(expected_msg);
                }
                report.finish()
            }
            | UnrecognizedToken { token: (start, token, end), expected } => {
                let start_str = info.trans_span2(*start);
                let end_str = info.trans_span2(*end);
                let expected_msg = fmt_expected(expected);
                let mut report = Report::build(ReportKind::Error, file_path.clone(), *start)
                    .with_message(format!("Unrecognized token `{}`", token))
                    .with_label(
                        Label::new((file_path.clone(), *start..*end))
                            .with_message(format!("unrecognized token `{}` found at {} - {}", token, start_str, end_str)),
                    );
                if !expected_msg.is_empty() {
                    report = report.with_note(expected_msg);
                }
                report.finish()
            }
            | ExtraToken { token: (start, token, end) } => {
                let start_str = info.trans_span2(*start);
                let end_str = info.trans_span2(*end);
                Report::build(ReportKind::Error, file_path.clone(), *start)
                    .with_message(format!("Extra token `{}`", token))
                    .with_label(
                        Label::new((file_path.clone(), *start..*end))
                            .with_message(format!("extra token `{}` found at {} - {}", token, start_str, end_str)),
                    )
                    .finish()
            }
        }
    }
}

impl Display for ParseError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use lalrpop_util::ParseError::*;
        let ParseError { error, file_info: info } = self;
        match error {
            | User { error } => write!(f, "{error}"),
            | InvalidToken { location } => {
                write!(
                    f,
                    "Invalid token at {}:{}",
                    info.path().display(),
                    info.trans_span2(*location)
                )
            }
            | UnrecognizedEof { location, expected } => {
                write!(
                    f,
                    "Unrecognized EOF found at {}:{}{}",
                    info.path().display(),
                    info.trans_span2(*location),
                    fmt_expected(expected)
                )
            }
            | UnrecognizedToken { token: (start, token, end), expected } => {
                write!(
                    f,
                    "Unrecognized token `{token}` found at {}:{} - {}{}",
                    info.path().display(),
                    info.trans_span2(*start),
                    info.trans_span2(*end),
                    fmt_expected(expected)
                )
            }
            | ExtraToken { token: (start, token, end) } => {
                write!(
                    f,
                    "Extra token `{token}` found at {}:{} - {}",
                    info.path().display(),
                    info.trans_span2(*start),
                    info.trans_span2(*end),
                )
            }
        }
    }
}

fn fmt_expected(expected: &[String]) -> String {
    let mut res = format!("");
    if !expected.is_empty() {
        res += &format!("; ");
        for (i, e) in expected.iter().enumerate() {
            let sep = match i {
                | 0 => "Expected one of",
                | _ if i < expected.len() - 1 => ",",
                // Last expected message to be written
                | _ => " or",
            };
            res += &format!("{} {}", sep, e);
        }
    }
    res
}
