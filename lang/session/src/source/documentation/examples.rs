use super::{Documentation, DocumentationGuide};
use crate::{CompilerSession, SourceDiagnosticSite};
use pulldown_cmark::{CodeBlockKind, Event, Parser, Tag, TagEnd};
use serde::{Deserialize, Serialize};
use sha3::{Digest, Sha3_256};
use std::{
    ops::Range,
    path::{Path, PathBuf},
};
use thiserror::Error;
use zydeco_statics::check::TyckDiagnosticCode;
use zydeco_utils::span::{FileMap, LineCol};

mod process;
pub use process::*;

#[derive(Clone, Debug)]
pub enum DocumentationExampleMode {
    Check,
    Reject { code: TyckDiagnosticCode, offset: usize },
}

#[derive(Clone, Debug, Error)]
pub enum DocumentationExampleError {
    #[error("verified fences require `zydeco check` or `zydeco reject=tyck.code at=line:column`")]
    Options,
    #[error("unknown diagnostic code `{0}`")]
    DiagnosticCode(String),
    #[error(
        "expected diagnostic position must be a valid one-based UTF-16 line and column in the example"
    )]
    Position,
    #[error("verified examples must use an unindented, top-level code fence")]
    Indentation,
    #[error("example execution requires a capability policy and is not enabled")]
    ExecutionUnavailable,
    #[error("verified examples are limited to 64 KiB of complete source")]
    SourceLimit,
}

#[derive(Clone, Debug)]
pub struct DocumentationExample {
    pub path: PathBuf,
    pub range: Range<usize>,
    pub code: String,
    pub mode: Result<DocumentationExampleMode, DocumentationExampleError>,
    lines: Vec<ExampleLine>,
}

#[derive(Clone, Debug)]
struct ExampleLine {
    code: Range<usize>,
    source: Range<usize>,
}

#[derive(Debug, Error)]
pub enum DocumentationScratchError {
    #[error(transparent)]
    Parse(#[from] zydeco_surface::textual::ParseFailure),
    #[error(transparent)]
    Import(#[from] zydeco_surface::textual::ImportDirectiveError),
    #[error("scratch examples require explicit file imports")]
    NumberedImport,
    #[error("import has no path token")]
    MissingPathToken,
    #[error("could not resolve the example's source path: {0}")]
    SourcePath(#[from] std::io::Error),
    #[error("example has no source directory")]
    MissingDirectory,
    #[error("scratch imports require UTF-8 paths")]
    NonUtf8Path,
}

impl DocumentationExample {
    /// Produce complete scratch source with compiler-recognized relative imports
    /// made absolute, so a temporary editor file keeps the authored context.
    pub fn scratch_source(&self) -> Result<String, DocumentationScratchError> {
        use zydeco_surface::textual::{
            ImportTarget, LexicalTokenKind, LexicalTokens, StrictParser, syntax::Parser,
        };
        let mut parser = Parser::new();
        let unit = StrictParser::source(&self.code, &mut parser)?;
        let sites = unit.imports(&parser.arena, &parser.spans)?;
        let strings = LexicalTokens::new(&self.code)
            .filter(|token| token.kind == LexicalTokenKind::String)
            .collect::<Vec<_>>();
        let mut replacements = sites
            .iter()
            .map(|site| {
                let ImportTarget::Path(path) = &site.directive.target else {
                    return Err(DocumentationScratchError::NumberedImport);
                };
                let span = site.directive.span.range();
                let token = strings
                    .iter()
                    .find(|token| span.start <= token.range.start && token.range.end <= span.end)
                    .ok_or(DocumentationScratchError::MissingPathToken)?;
                let origin = std::path::absolute(&self.path)?;
                let target =
                    origin.parent().ok_or(DocumentationScratchError::MissingDirectory)?.join(path);
                let text = target.to_str().ok_or(DocumentationScratchError::NonUtf8Path)?;
                Ok((token.range.clone(), format!("{text:?}")))
            })
            .collect::<Result<Vec<_>, DocumentationScratchError>>()?;
        replacements.sort_by_key(|(range, _)| range.start);
        let mut code = self.code.clone();
        replacements
            .into_iter()
            .rev()
            .for_each(|(range, replacement)| code.replace_range(range, &replacement));
        StrictParser::source(&code, &mut Parser::new())?;
        Ok(code)
    }

    pub fn from_documentation(document: &Documentation, source: &str) -> Vec<Self> {
        Self::collect(&document.path, &document.markdown, |range| {
            document.source_range(source, range)
        })
    }

    pub fn from_guide(guide: &DocumentationGuide) -> Vec<Self> {
        Self::collect(&guide.path, &guide.markdown, Some)
    }

    fn collect(
        path: &Path, markdown: &str, map: impl Fn(Range<usize>) -> Option<Range<usize>>,
    ) -> Vec<Self> {
        let mut events = Parser::new(markdown).into_offset_iter().peekable();
        let mut examples = Vec::new();
        let mut depth = 0usize;
        while let Some((event, fence_range)) = events.next() {
            match event {
                | Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(info))) => {
                    let options = info.split_whitespace().collect::<Vec<_>>();
                    let opted_in = options.first() == Some(&"zydeco") && options.len() > 1;
                    let mut chunks = Vec::new();
                    for (event, range) in events.by_ref() {
                        match event {
                            | Event::Text(text) => chunks.push((text.into_string(), range)),
                            | Event::End(TagEnd::CodeBlock) => break,
                            | _ => {}
                        }
                    }
                    if !opted_in {
                        continue;
                    }
                    let code = chunks.iter().map(|(text, _)| text.as_str()).collect::<String>();
                    let contiguous = chunks.windows(2).all(|pair| pair[0].1.end == pair[1].1.start)
                        && chunks.iter().all(|(text, range)| {
                            markdown.get(range.clone()) == Some(text.as_str())
                        });
                    let mode = if code.len() > 65_536 {
                        Err(DocumentationExampleError::SourceLimit)
                    } else if depth != 0 || !contiguous {
                        Err(DocumentationExampleError::Indentation)
                    } else {
                        Self::mode(&options[1..], &code)
                    };
                    let start = chunks.first().map_or(fence_range.start, |(_, range)| range.start);
                    let lines = code
                        .split_inclusive('\n')
                        .scan(0usize, |offset, line| {
                            let range =
                                *offset..*offset + line.trim_end_matches(['\r', '\n']).len();
                            *offset += line.len();
                            Some(range)
                        })
                        .filter_map(|code| {
                            Some(ExampleLine {
                                source: map(start + code.start..start + code.end)?,
                                code,
                            })
                        })
                        .collect();
                    examples.push(Self {
                        path: path.to_owned(),
                        range: map(fence_range).unwrap_or_default(),
                        code,
                        mode,
                        lines,
                    });
                }
                | Event::Start(_) => depth += 1,
                | Event::End(_) => depth -= 1,
                | _ => {}
            }
        }
        examples
    }

    fn mode(
        options: &[&str], code: &str,
    ) -> Result<DocumentationExampleMode, DocumentationExampleError> {
        if options == ["check"] {
            return Ok(DocumentationExampleMode::Check);
        }
        if options.first().is_some_and(|option| option.starts_with("run")) {
            return Err(DocumentationExampleError::ExecutionUnavailable);
        }
        let [rejection, position] = options else { return Err(DocumentationExampleError::Options) };
        let name = rejection.strip_prefix("reject=").ok_or(DocumentationExampleError::Options)?;
        let expected = TyckDiagnosticCode::from_code(name)
            .ok_or_else(|| DocumentationExampleError::DiagnosticCode(name.to_owned()))?;
        let (line, column) = position
            .strip_prefix("at=")
            .and_then(|position| position.split_once(':'))
            .ok_or(DocumentationExampleError::Position)?;
        let positive = |value: &str| {
            value
                .parse::<u32>()
                .ok()
                .and_then(|value| value.checked_sub(1))
                .ok_or(DocumentationExampleError::Position)
        };
        let offset = FileMap::local(code, None)
            .offset_utf16(LineCol { line: positive(line)?, column: positive(column)? })
            .ok_or(DocumentationExampleError::Position)?;
        Ok(DocumentationExampleMode::Reject { code: expected, offset })
    }

    pub fn source_range(&self, code: Range<usize>) -> Option<Range<usize>> {
        let locate = |offset| {
            self.lines.iter().find_map(|line| {
                (line.code.start <= offset && offset <= line.code.end)
                    .then(|| line.source.start + offset - line.code.start)
            })
        };
        Some(locate(code.start)?..locate(code.end)?)
    }

    pub fn request(
        &self, session: &CompilerSession,
    ) -> Result<DocumentationExampleRequest, DocumentationExampleError> {
        let expectation = match self.mode.clone()? {
            | DocumentationExampleMode::Check => DocumentationExampleExpectation::Check,
            | DocumentationExampleMode::Reject { code, offset } => {
                DocumentationExampleExpectation::Reject { code: code.to_string(), offset }
            }
        };
        let path = std::path::absolute(&self.path)
            .unwrap_or_else(|_| self.path.clone())
            .with_extension("doc-example.zydeco");
        Ok(DocumentationExampleRequest {
            path,
            code: self.code.clone(),
            inputs: session.documentation_inputs(),
            expectation,
        })
    }
}

#[cfg(test)]
mod tests;

/// Process-boundary representation. The worker decodes the diagnostic code to
/// its compiler enum before checking; no renderer assigns rejection semantics.
#[derive(Serialize, Deserialize)]
pub struct DocumentationExampleRequest {
    pub path: PathBuf,
    pub code: String,
    pub inputs: Vec<DocumentationExampleInput>,
    pub expectation: DocumentationExampleExpectation,
}

#[derive(Serialize, Deserialize)]
pub struct DocumentationExampleInput {
    pub path: PathBuf,
    pub source: String,
}

#[derive(Serialize, Deserialize)]
pub enum DocumentationExampleExpectation {
    Check,
    Reject { code: String, offset: usize },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DocumentationExampleVerification {
    pub status: DocumentationExampleStatus,
    pub diagnostics: Vec<DocumentationExampleDiagnostic>,
    pub inputs: Vec<DocumentationExampleFingerprint>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DocumentationExampleFingerprint {
    pub path: PathBuf,
    pub sha3_256: String,
}

#[derive(Clone, Debug, Serialize, Deserialize, derive_more::IsVariant)]
pub enum DocumentationExampleStatus {
    Passed,
    Failed,
    TimedOut,
    WorkerFailure(String),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DocumentationExampleDiagnostic {
    pub code: Option<String>,
    pub message: String,
    pub path: Option<PathBuf>,
    pub range: Option<Range<usize>>,
}

impl DocumentationExampleVerification {
    pub fn worker_failure(message: impl Into<String>) -> Self {
        Self {
            status: DocumentationExampleStatus::WorkerFailure(message.into()),
            diagnostics: Vec::new(),
            inputs: Vec::new(),
        }
    }
}

impl DocumentationExampleRequest {
    /// Run in the bounded worker process, never in an editor's compiler database.
    pub fn check(self) -> DocumentationExampleVerification {
        let expectation = match self.expectation {
            | DocumentationExampleExpectation::Check => DocumentationExampleMode::Check,
            | DocumentationExampleExpectation::Reject { code, offset } => {
                let Some(code) = TyckDiagnosticCode::from_code(&code) else {
                    return DocumentationExampleVerification::worker_failure(
                        "unknown expected diagnostic code",
                    );
                };
                if !self.code.is_char_boundary(offset) {
                    return DocumentationExampleVerification::worker_failure(
                        "invalid expected diagnostic offset",
                    );
                }
                DocumentationExampleMode::Reject { code, offset }
            }
        };
        let mut session = CompilerSession::default();
        if self.code.len() > 65_536 {
            return DocumentationExampleVerification::worker_failure(
                "example source exceeds 64 KiB",
            );
        }
        for input in self.inputs {
            if let Err(error) = session.set_overlay(input.path, input.source) {
                return DocumentationExampleVerification::worker_failure(error.to_string());
            }
        }
        if let Err(error) = session.set_overlay(&self.path, self.code) {
            return DocumentationExampleVerification::worker_failure(error.to_string());
        }
        let analysis = match session.analyze(&self.path) {
            | Ok(analysis) => analysis,
            | Err(error) => {
                let site = error.diagnostic_site();
                return DocumentationExampleVerification {
                    status: DocumentationExampleStatus::Failed,
                    diagnostics: vec![DocumentationExampleDiagnostic {
                        code: None,
                        message: error.to_string(),
                        path: site.as_ref().map(|site| site.path().to_owned()),
                        range: site.map(|site| site.range().clone()),
                    }],
                    inputs: Vec::new(),
                };
            }
        };
        let passed = match expectation {
            | DocumentationExampleMode::Check => analysis.outcome().root().is_some(),
            | DocumentationExampleMode::Reject { code, offset } => {
                analysis.outcome().diagnostics().is_some_and(|diagnostics| {
                    !diagnostics.is_empty()
                        && diagnostics.iter().all(|diagnostic| {
                            diagnostic.code == code
                                && diagnostic
                                    .primary
                                    .as_ref()
                                    .and_then(|primary| {
                                        SourceDiagnosticSite::from_span(
                                            analysis.spans(),
                                            primary.span,
                                        )
                                    })
                                    .is_some_and(|site| {
                                        site.path() == analysis.root_path()
                                            && (site.range().contains(&offset)
                                                || *site.range() == (offset..offset))
                                    })
                        })
                })
            }
        };
        let diagnostics = analysis
            .outcome()
            .diagnostics()
            .into_iter()
            .flat_map(|diagnostics| diagnostics.iter())
            .map(|diagnostic| {
                let site = diagnostic.primary.as_ref().and_then(|primary| {
                    SourceDiagnosticSite::from_span(analysis.spans(), primary.span)
                });
                DocumentationExampleDiagnostic {
                    code: Some(diagnostic.code.to_string()),
                    message: diagnostic.message.clone(),
                    path: site.as_ref().map(|site| site.path().to_owned()),
                    range: site.map(|site| site.range().clone()),
                }
            })
            .collect::<Vec<_>>();
        let inputs = analysis
            .sources()
            .map(|(path, source)| DocumentationExampleFingerprint {
                path: path.to_owned(),
                sha3_256: format!("{:x}", Sha3_256::digest(source.as_bytes())),
            })
            .collect();
        DocumentationExampleVerification {
            status: if passed {
                DocumentationExampleStatus::Passed
            } else {
                DocumentationExampleStatus::Failed
            },
            diagnostics,
            inputs,
        }
    }
}
