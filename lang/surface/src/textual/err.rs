use super::{DiagnosticToken, ParseFailure, ParseIssueKind, TokenKind};
use ariadne::{Label, Report, ReportKind};
use std::{fmt::Display, ops::Range};
use zydeco_utils::span::{FileMap, PathDisplay};

/// Wrapper around an owned strict parse failure with file context.
pub struct ParseError<'input> {
    pub error: ParseFailure,
    pub file_map: &'input FileMap,
}

impl ParseError<'_> {
    /// File-relative byte range identified by the primary parser issue.
    pub fn source_range(&self) -> Option<Range<usize>> {
        self.error.primary().range.clone()
    }

    /// Create an Ariadne report for the primary parser issue.
    pub fn to_report(&self) -> Report<'static, (PathDisplay, Range<usize>)> {
        let ParseError { error, file_map: info } = self;
        let issue = error.primary();
        let file_path = PathDisplay::from(info.path());
        let range = issue.range.clone().unwrap_or(0..0);
        let note = ParseErrorNote::new(issue.expected(), error.issue_count());

        match &issue.kind {
            | ParseIssueKind::Literal { .. }
            | ParseIssueKind::UnrecognizedToken { token: DiagnosticToken::Invalid(_), .. }
            | ParseIssueKind::ExtraToken { token: DiagnosticToken::Invalid(_) } => {
                let mut report =
                    Report::build(ReportKind::Error, (file_path.clone(), range.clone()))
                        .with_message("Parse error")
                        .with_label(
                            Label::new((file_path.clone(), range.clone()))
                                .with_message(issue.to_string()),
                        );
                if let Some(note) = note.render() {
                    report = report.with_note(note);
                }
                report.finish()
            }
            | ParseIssueKind::InvalidToken => {
                let location = range.start;
                let location_str = info.line_col(location);
                let mut report =
                    Report::build(ReportKind::Error, (file_path.clone(), range.clone()))
                        .with_message("Invalid token")
                        .with_label(
                            Label::new((file_path.clone(), range.clone()))
                                .with_message(format!("invalid token at {location_str}")),
                        );
                if let Some(note) = note.render() {
                    report = report.with_note(note);
                }
                report.finish()
            }
            | ParseIssueKind::UnrecognizedEof { .. } => {
                let location = range.start;
                let location_str = info.line_col(location);
                let mut report =
                    Report::build(ReportKind::Error, (file_path.clone(), range.clone()))
                        .with_message("Unrecognized EOF")
                        .with_label(
                            Label::new((file_path.clone(), range.clone()))
                                .with_message(format!("unexpected end of file at {location_str}")),
                        );
                if let Some(note) = note.render() {
                    report = report.with_note(note);
                }
                report.finish()
            }
            | ParseIssueKind::UnrecognizedToken { token, .. } => {
                let start_str = info.line_col(range.start);
                let end_str = info.line_col(range.end);
                let mut report =
                    Report::build(ReportKind::Error, (file_path.clone(), range.clone()))
                        .with_message(format!("Unrecognized token `{token}`"))
                        .with_label(Label::new((file_path.clone(), range.clone())).with_message(
                            format!(
                                "unrecognized token `{token}` found at {start_str} - {end_str}"
                            ),
                        ));
                if let Some(note) = note.render() {
                    report = report.with_note(note);
                }
                report.finish()
            }
            | ParseIssueKind::ExtraToken { token } => {
                let start_str = info.line_col(range.start);
                let end_str = info.line_col(range.end);
                let mut report =
                    Report::build(ReportKind::Error, (file_path.clone(), range.clone()))
                        .with_message(format!("Extra token `{token}`"))
                        .with_label(Label::new((file_path.clone(), range.clone())).with_message(
                            format!("extra token `{token}` found at {start_str} - {end_str}"),
                        ));
                if let Some(note) = note.render() {
                    report = report.with_note(note);
                }
                report.finish()
            }
        }
    }
}

impl Display for ParseError<'_> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ParseError { error, file_map: info } = self;
        let issue = error.primary();
        let range = issue.range.clone().unwrap_or(0..0);
        match &issue.kind {
            | ParseIssueKind::Literal { .. }
            | ParseIssueKind::UnrecognizedToken { token: DiagnosticToken::Invalid(_), .. }
            | ParseIssueKind::ExtraToken { token: DiagnosticToken::Invalid(_) } => write!(
                formatter,
                "{issue} at {}:{} - {}{}",
                info.path().display(),
                info.line_col(range.start),
                info.line_col(range.end),
                fmt_expected(issue.expected()),
            )?,
            | ParseIssueKind::InvalidToken => write!(
                formatter,
                "Invalid token at {}:{}",
                info.path().display(),
                info.line_col(range.start)
            )?,
            | ParseIssueKind::UnrecognizedEof { expected } => write!(
                formatter,
                "Unrecognized EOF found at {}:{}{}",
                info.path().display(),
                info.line_col(range.start),
                fmt_expected(expected)
            )?,
            | ParseIssueKind::UnrecognizedToken { token, expected } => write!(
                formatter,
                "Unrecognized token `{token}` found at {}:{} - {}{}",
                info.path().display(),
                info.line_col(range.start),
                info.line_col(range.end),
                fmt_expected(expected)
            )?,
            | ParseIssueKind::ExtraToken { token } => write!(
                formatter,
                "Extra token `{token}` found at {}:{} - {}",
                info.path().display(),
                info.line_col(range.start),
                info.line_col(range.end),
            )?,
        }
        if error.issue_count() > 1 {
            write!(formatter, "; {}", ParseIssueCount(error.issue_count() - 1))?;
        }
        Ok(())
    }
}

struct ParseErrorNote<'expected> {
    expected: &'expected [TokenKind],
    issue_count: usize,
}

impl<'expected> ParseErrorNote<'expected> {
    fn new(expected: &'expected [TokenKind], issue_count: usize) -> Self {
        Self { expected, issue_count }
    }

    fn render(&self) -> Option<String> {
        let expected = fmt_expected(self.expected);
        let expected = expected.strip_prefix("; ").unwrap_or_default();
        match (expected.is_empty(), self.issue_count.saturating_sub(1)) {
            | (true, 0) => None,
            | (false, 0) => Some(expected.to_owned()),
            | (true, additional) => Some(ParseIssueCount(additional).to_string()),
            | (false, additional) => Some(format!("{expected}; {}", ParseIssueCount(additional))),
        }
    }
}

struct ParseIssueCount(usize);

impl Display for ParseIssueCount {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(count) = self;
        let suffix = if *count == 1 { "issue" } else { "issues" };
        write!(formatter, "{count} additional parse {suffix}")
    }
}

fn fmt_expected(expected: &[TokenKind]) -> String {
    let mut res = String::new();
    if !expected.is_empty() {
        res += "; ";
        for (i, e) in expected.iter().enumerate() {
            let sep = match i {
                | 0 => "Expected one of",
                | _ if i < expected.len() - 1 => ",",
                | _ => " or",
            };
            res += &format!("{sep} {e}");
        }
    }
    res
}
