use serde::{Deserialize, Serialize};
use std::path::Path;
use tower_lsp::lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position, Url};
use zydeco_session::CompilerSession;
use zydeco_session::source::DocumentationLinkTarget;
use zydeco_surface::{scoped::arena::ScopedArena, textual::syntax::SpanArena};
use zydeco_utils::span::{FileMap, LineCol};

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DocumentationView {
    pub revision: u64,
    pub title: String,
    pub signature: Option<String>,
    pub declared_signature: Option<String>,
    pub range: tower_lsp::lsp_types::Range,
    pub origin: Option<tower_lsp::lsp_types::Location>,
    pub sections: Vec<DocumentationSection>,
    pub examples: Vec<DocumentationExampleView>,
}

#[derive(Serialize)]
pub struct DocumentationSection {
    pub origin: tower_lsp::lsp_types::Location,
    pub markdown: String,
    pub html: String,
}

impl DocumentationSection {
    pub(crate) fn new(origin: tower_lsp::lsp_types::Location, markdown: String) -> Self {
        use pulldown_cmark::{Event, Options, Parser, Tag, TagEnd};
        let events =
            Parser::new_ext(&markdown, Options::ENABLE_TABLES | Options::ENABLE_STRIKETHROUGH)
                .filter_map(|event| match event {
                    | Event::Html(text) | Event::InlineHtml(text) => Some(Event::Text(text)),
                    | Event::Start(Tag::Image { .. }) | Event::End(TagEnd::Image) => None,
                    | Event::Start(Tag::Link { link_type, dest_url, title, id }) => {
                        let safe = dest_url.split_once(':').is_none_or(|(scheme, _)| {
                            matches!(
                                scheme.to_ascii_lowercase().as_str(),
                                "http" | "https" | "mailto" | "file"
                            )
                        });
                        let dest_url = if !safe {
                            "#".into()
                        } else if !dest_url.starts_with('#') && !dest_url.contains(':') {
                            origin
                                .uri
                                .join(&dest_url)
                                .map(|url| url.to_string().into())
                                .unwrap_or(dest_url)
                        } else {
                            dest_url
                        };
                        Some(Event::Start(Tag::Link { link_type, dest_url, title, id }))
                    }
                    | event => Some(event),
                });
        let mut html = String::new();
        pulldown_cmark::html::push_html(&mut html, events);
        Self { origin, markdown, html }
    }
}

#[derive(Serialize)]
pub struct DocumentationExampleView {
    pub id: usize,
    pub code: String,
    pub mode: String,
    pub scratch: Option<String>,
}

impl DocumentationExampleView {
    pub(crate) fn new(id: usize, example: &zydeco_session::source::DocumentationExample) -> Self {
        use zydeco_session::source::DocumentationExampleMode;
        Self {
            id,
            code: example.code.clone(),
            scratch: example.scratch_source().ok(),
            mode: match &example.mode {
                | Ok(DocumentationExampleMode::Check) => "Check".to_owned(),
                | Ok(DocumentationExampleMode::Reject { code, .. }) => {
                    format!("Expected rejection · {code}")
                }
                | Err(error) => error.to_string(),
            },
        }
    }
}

#[derive(Deserialize)]
pub struct DocumentationCheckParams {
    pub revision: u64,
    pub target: tower_lsp::lsp_types::TextDocumentPositionParams,
    pub example: usize,
}

pub(crate) struct PreparedDocumentation {
    pub view: DocumentationView,
    pub examples: Vec<zydeco_session::source::DocumentationExample>,
}

impl crate::Cajun {
    pub async fn documentation(
        &self, target: tower_lsp::lsp_types::TextDocumentPositionParams,
    ) -> tower_lsp::jsonrpc::Result<Option<DocumentationView>> {
        if !crate::ZydecoDocument::accepts(&target.text_document.uri) {
            return Ok(None);
        }
        let revision = self.session.lock().await.revision();
        let prepared = self.prepare_documentation(&target, revision).await;
        if self.session.lock().await.revision() != revision {
            return Ok(None);
        }
        Ok(prepared.map(|mut prepared| {
            prepared.view.revision = revision.0;
            prepared.view
        }))
    }

    pub async fn check_documentation_example(
        &self, params: DocumentationCheckParams,
    ) -> tower_lsp::jsonrpc::Result<Option<zydeco_session::source::DocumentationExampleVerification>>
    {
        use zydeco_session::source::{
            DocumentationExampleVerification, DocumentationExampleWorker,
        };
        if !crate::ZydecoDocument::accepts(&params.target.text_document.uri) {
            return Ok(None);
        }
        // Bound concurrent workers and re-resolve the example against its exact
        // source revision. Clients never submit executable source in this request.
        let _permit = self.documentation_checks.acquire().await.expect("semaphore remains open");
        let revision = crate::SourceRevision(params.revision);
        if self.session.lock().await.revision() != revision {
            return Ok(None);
        }
        let example = self
            .prepare_documentation(&params.target, revision)
            .await
            .and_then(|prepared| prepared.examples.into_iter().nth(params.example));
        let Some(example) = example else {
            return Ok(None);
        };
        let request = {
            let session = self.session.lock().await;
            if session.revision() != revision {
                return Ok(None);
            }
            match example.request(&session.compiler) {
                | Ok(request) => request,
                | Err(error) => {
                    return Ok(Some(DocumentationExampleVerification::worker_failure(
                        error.to_string(),
                    )));
                }
            }
        };
        let synthetic_path = request.path.clone();
        let mut verification = tokio::task::spawn_blocking(move || match std::env::current_exe() {
            | Ok(executable) => DocumentationExampleWorker::verify(
                &executable,
                &["--documentation-example-worker"],
                &request,
                std::time::Duration::from_secs(30),
            ),
            | Err(error) => DocumentationExampleVerification::worker_failure(error.to_string()),
        })
        .await
        .unwrap_or_else(|error| {
            DocumentationExampleVerification::worker_failure(error.to_string())
        });
        if self.session.lock().await.revision() != revision {
            return Ok(None);
        }
        for diagnostic in &mut verification.diagnostics {
            if diagnostic.path.as_ref() == Some(&synthetic_path) {
                diagnostic.path = Some(example.path.clone());
                diagnostic.range =
                    diagnostic.range.take().and_then(|range| example.source_range(range));
            }
        }
        Ok(Some(verification))
    }

    async fn prepare_documentation(
        &self, target: &tower_lsp::lsp_types::TextDocumentPositionParams,
        revision: crate::SourceRevision,
    ) -> Option<PreparedDocumentation> {
        match self.refresh(&target.text_document.uri).await {
            | crate::RefreshOutcome::Updated(path) => {
                let options = self.configuration.snapshot().await.hover;
                self.projects
                    .read()
                    .await
                    .get(&path)
                    .filter(|cached| cached.revision == revision)
                    .and_then(|cached| {
                        cached.project.prepare_documentation(&path, target.position, options)
                    })
            }
            | crate::RefreshOutcome::Superseded => None,
            | crate::RefreshOutcome::Failed(_) => {
                let path = Self::path(&target.text_document.uri).ok()?;
                let snapshot = self.session.lock().await.compiler.snapshot();
                let position = target.position;
                match tokio::task::spawn_blocking(move || {
                    crate::AnalysisTask::run(|| {
                        SourceDocumentationHover::prepare(&snapshot, &path, position)
                    })
                })
                .await
                .ok()?
                {
                    | crate::AnalysisTask::Completed(prepared) => prepared,
                    | crate::AnalysisTask::Cancelled => None,
                }
            }
        }
    }
}

pub(crate) struct SourceDocumentationHover;

pub(crate) struct DocumentationLinks<'arena> {
    pub scoped: &'arena ScopedArena,
    pub spans: &'arena SpanArena,
}

impl DocumentationLinks<'_> {
    pub(crate) fn url(&self, target: &DocumentationLinkTarget) -> Option<Url> {
        let origin = self.scoped.origins.source(&target.source_entity())?;
        let (file, range) = self.spans.source_map()?.range(self.spans[&origin])?;
        let position = file.line_col_utf16(range.start)?;
        let mut url = Url::from_file_path(file.path()).ok()?;
        url.set_fragment(Some(&format!("L{},{}", position.line + 1, position.column + 1)));
        Some(url)
    }
}

impl SourceDocumentationHover {
    fn prepare(
        session: &CompilerSession, path: &Path, position: Position,
    ) -> Option<PreparedDocumentation> {
        use tower_lsp::lsp_types::{Location, Range};
        use zydeco_session::{DocumentationSubject, source::DocumentationExample};
        let source = session.source_text(path).ok()??;
        let map = FileMap::local(source.as_str(), None);
        let offset =
            map.offset_utf16(LineCol { line: position.line, column: position.character })?;
        let index = session.source_documentation(path).ok()?;
        let content = index.at(path, offset);
        let entry = content.entries().last()?;
        let location = |entry: &zydeco_session::Documentation| -> Option<Location> {
            let start = map.line_col_utf16(entry.range.start)?;
            let end = map.line_col_utf16(entry.range.end)?;
            Some(Location {
                uri: Url::from_file_path(path).ok()?,
                range: Range::new(
                    Position::new(start.line, start.column),
                    Position::new(end.line, end.column),
                ),
            })
        };
        let origin = location(entry);
        let title = match &entry.subject {
            | DocumentationSubject::Binding(name) => name.0.clone(),
            | DocumentationSubject::Member(name) => name.0.clone(),
            | DocumentationSubject::Overview => "Overview".to_owned(),
            | DocumentationSubject::Expression => "Expression".to_owned(),
        };
        let sections = content
            .entries()
            .filter_map(|entry| {
                Some(DocumentationSection::new(location(entry)?, entry.markdown.to_string()))
            })
            .collect();
        let examples = content
            .entries()
            .flat_map(|entry| DocumentationExample::from_documentation(entry, &source))
            .collect::<Vec<_>>();
        let previews = examples
            .iter()
            .enumerate()
            .map(|(id, example)| DocumentationExampleView::new(id, example))
            .collect();
        Some(PreparedDocumentation {
            view: DocumentationView {
                revision: 0,
                title,
                signature: None,
                declared_signature: None,
                range: Range::new(position, position),
                origin,
                sections,
                examples: previews,
            },
            examples,
        })
    }

    pub(crate) fn at(session: &CompilerSession, path: &Path, position: Position) -> Option<Hover> {
        let source = session.source_text(path).ok()??;
        let offset = FileMap::local(source.as_str(), None)
            .offset_utf16(LineCol { line: position.line, column: position.character })?;
        let index = session.source_documentation(path).ok()?;
        let content = index.at(path, offset);
        (!content.is_empty()).then(|| Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: content.markdown(),
            }),
            range: None,
        })
    }
}
