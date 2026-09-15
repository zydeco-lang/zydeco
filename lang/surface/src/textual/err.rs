use super::{DiagnosticToken, ParseFailure, ParseIssue, ParseIssueKind, TokenKind};
use ariadne::{Config, IndexType, Label, Report, ReportKind};
use std::{fmt::Display, ops::Range};
use zydeco_utils::span::{FileMap, PathDisplay};

/// A strict parse failure together with the exact source snapshot that failed.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub struct ParseError {
    #[source]
    pub error: ParseFailure,
    pub file_map: FileMap,
}

impl ParseError {
    /// File-relative byte range identified by the primary parser issue.
    pub fn source_range(&self) -> Option<Range<usize>> {
        self.error.primary().range.clone()
    }

    /// Every retained parser issue with its original source snapshot.
    pub fn diagnostics(&self) -> impl Iterator<Item = ParseDiagnostic<'_>> {
        self.error.issues().map(|issue| ParseDiagnostic { issue, file_map: &self.file_map })
    }
}

/// One parser issue and the source context used by every presentation surface.
pub struct ParseDiagnostic<'a> {
    pub issue: &'a ParseIssue,
    pub file_map: &'a FileMap,
}

impl ParseDiagnostic<'_> {
    /// Render this issue with its own location and token expectations.
    pub fn to_report(&self) -> Report<'static, (PathDisplay, Range<usize>)> {
        let Self { issue, file_map: info } = self;
        let file_path = PathDisplay::from(info.path());
        let range = issue.range.clone().unwrap_or(0..0);
        let note = ExpectedTokens::note(issue.expected());
        let report = Report::build(ReportKind::Error, (file_path.clone(), range.clone()))
            .with_config(Config::default().with_index_type(IndexType::Byte));

        match &issue.kind {
            | ParseIssueKind::Literal { .. }
            | ParseIssueKind::UnrecognizedToken { token: DiagnosticToken::Invalid(_), .. }
            | ParseIssueKind::ExtraToken { token: DiagnosticToken::Invalid(_) } => {
                let mut report = report.with_message("Parse error").with_label(
                    Label::new((file_path.clone(), range.clone())).with_message(issue.to_string()),
                );
                if let Some(note) = note {
                    report = report.with_note(note);
                }
                report.finish()
            }
            | ParseIssueKind::InvalidToken => {
                let location = range.start;
                let location_str = info.line_col(location);
                let mut report = report.with_message("Invalid token").with_label(
                    Label::new((file_path.clone(), range.clone()))
                        .with_message(format!("invalid token at {location_str}")),
                );
                if let Some(note) = note {
                    report = report.with_note(note);
                }
                report.finish()
            }
            | ParseIssueKind::UnrecognizedEof { .. } => {
                let location = range.start;
                let location_str = info.line_col(location);
                let mut report = report.with_message("Unrecognized EOF").with_label(
                    Label::new((file_path.clone(), range.clone()))
                        .with_message(format!("unexpected end of file at {location_str}")),
                );
                if let Some(note) = note {
                    report = report.with_note(note);
                }
                report.finish()
            }
            | ParseIssueKind::UnrecognizedToken { token, .. } => {
                let start_str = info.line_col(range.start);
                let end_str = info.line_col(range.end);
                let mut report = report
                    .with_message(format!("Unrecognized token `{token}`"))
                    .with_label(Label::new((file_path.clone(), range.clone())).with_message(
                        format!("unrecognized token `{token}` found at {start_str} - {end_str}"),
                    ));
                if let Some(note) = note {
                    report = report.with_note(note);
                }
                report.finish()
            }
            | ParseIssueKind::ExtraToken { token } => {
                let start_str = info.line_col(range.start);
                let end_str = info.line_col(range.end);
                let mut report = report.with_message(format!("Extra token `{token}`")).with_label(
                    Label::new((file_path.clone(), range.clone())).with_message(format!(
                        "extra token `{token}` found at {start_str} - {end_str}"
                    )),
                );
                if let Some(note) = note {
                    report = report.with_note(note);
                }
                report.finish()
            }
        }
    }
}

impl Display for ParseDiagnostic<'_> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { issue, file_map: info } = self;
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
                ExpectedTokens::suffix(issue.expected()),
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
                ExpectedTokens::suffix(expected)
            )?,
            | ParseIssueKind::UnrecognizedToken { token, expected } => write!(
                formatter,
                "Unrecognized token `{token}` found at {}:{} - {}{}",
                info.path().display(),
                info.line_col(range.start),
                info.line_col(range.end),
                ExpectedTokens::suffix(expected)
            )?,
            | ParseIssueKind::ExtraToken { token } => write!(
                formatter,
                "Extra token `{token}` found at {}:{} - {}",
                info.path().display(),
                info.line_col(range.start),
                info.line_col(range.end),
            )?,
        }
        Ok(())
    }
}

impl Display for ParseError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (index, diagnostic) in self.diagnostics().enumerate() {
            if index > 0 {
                writeln!(formatter)?;
            }
            diagnostic.fmt(formatter)?;
        }
        Ok(())
    }
}

struct ExpectedTokens;
impl ExpectedTokens {
    fn note(expected: &[TokenKind]) -> Option<String> {
        Self::suffix(expected).strip_prefix("; ").map(str::to_owned)
    }

    fn suffix(expected: &[TokenKind]) -> String {
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
}
