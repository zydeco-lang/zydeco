mod analysis;
mod format;
mod hover;
mod progress;
mod semantic;
mod type_links;

use analysis::ProjectState;
use format::{DocumentFormatter, FormattingOutcome};
use hover::HoverLineWidth;
use progress::{AnalysisProgressReporter, AnalysisProgressSession};
use semantic::SemanticHighlighter;
use std::{
    collections::HashMap,
    panic::{self, AssertUnwindSafe},
    path::{Path, PathBuf},
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering},
    },
};
use tokio::sync::{Mutex, RwLock};
use tower_lsp::{
    Client, LanguageServer,
    jsonrpc::Result,
    lsp_types::{
        Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
        DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams,
        DocumentSymbolParams, DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse,
        Hover, HoverParams, HoverProviderCapability, InitializeParams, InitializeResult,
        InitializedParams, Location, MessageType, OneOf, Position, PositionEncodingKind, Range,
        ReferenceParams, SemanticTokens, SemanticTokensFullOptions, SemanticTokensOptions,
        SemanticTokensParams, SemanticTokensResult, ServerCapabilities, ServerInfo,
        TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
        TextDocumentSyncSaveOptions, TextEdit, Url,
    },
};
use zydeco_session::CompilerSession;

struct ZydecoDocument;

impl ZydecoDocument {
    fn accepts(uri: &Url) -> bool {
        uri.to_file_path().ok().is_some_and(|path| {
            matches!(
                path.extension().and_then(|extension| extension.to_str()),
                Some("zy") | Some("zydeco")
            )
        })
    }
}

enum AnalysisTask<T> {
    Completed(T),
    Cancelled,
}

impl<T> AnalysisTask<T> {
    fn run(operation: impl FnOnce() -> T) -> Self {
        // The operation owns a disposable Salsa snapshot. Cancellation drops
        // that snapshot, while `Cancelled::catch` resumes every unrelated panic.
        match salsa::Cancelled::catch(AssertUnwindSafe(operation)) {
            | Ok(output) => Self::Completed(output),
            | Err(salsa::Cancelled::Local | salsa::Cancelled::PendingWrite) => Self::Cancelled,
            | Err(cancelled) => panic::resume_unwind(Box::new(cancelled)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct DocumentRevision(u64);

#[derive(Clone)]
struct OpenDocument {
    source: String,
    revision: DocumentRevision,
}

struct SessionState {
    compiler: CompilerSession,
    open_documents: HashMap<PathBuf, OpenDocument>,
    next_document_revision: u64,
}

impl Default for SessionState {
    fn default() -> Self {
        Self {
            compiler: CompilerSession::default(),
            open_documents: HashMap::new(),
            next_document_revision: 1,
        }
    }
}

impl SessionState {
    fn set_document(&mut self, path: &Path, source: String) -> std::result::Result<(), String> {
        self.compiler.set_overlay(path, source.clone()).map_err(|error| error.to_string())?;
        let revision = DocumentRevision(self.next_document_revision);
        self.next_document_revision = self
            .next_document_revision
            .checked_add(1)
            .expect("document revision counter overflowed");
        self.open_documents.insert(path.to_path_buf(), OpenDocument { source, revision });
        Ok(())
    }

    fn close_document(&mut self, path: &Path) {
        self.open_documents.remove(path);
        let _ = self.compiler.clear_overlay(path);
    }

    fn revision(&self, path: &Path) -> Option<DocumentRevision> {
        self.open_documents.get(path).map(|document| document.revision)
    }

    fn source(&self, path: &Path) -> Option<String> {
        self.open_documents.get(path).map(|document| document.source.clone())
    }
}

struct CachedProject {
    revision: Option<DocumentRevision>,
    project: ProjectState,
}

enum RefreshOutcome {
    Updated(PathBuf),
    Failed(String),
    Superseded,
}

/// The Cajun Zydeco language server.
///
/// Cajun treats every open source file as a potential root term. Its imported
/// files are loaded through the same hygienic source graph as the compiler,
/// with in-memory editor contents overriding the corresponding files on disk.
pub struct Cajun {
    client: Client,
    session: Arc<Mutex<SessionState>>,
    projects: RwLock<HashMap<PathBuf, CachedProject>>,
    work_done_progress: AtomicBool,
    semantic_tokens_refresh: AtomicBool,
    hover_line_width: AtomicUsize,
    next_progress_sequence: AtomicU64,
}

impl Cajun {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            session: Arc::new(Mutex::new(SessionState::default())),
            projects: RwLock::new(HashMap::new()),
            work_done_progress: AtomicBool::new(false),
            semantic_tokens_refresh: AtomicBool::new(false),
            hover_line_width: AtomicUsize::new(HoverLineWidth::DEFAULT.columns()),
            next_progress_sequence: AtomicU64::new(1),
        }
    }

    async fn refresh(&self, uri: &Url) -> RefreshOutcome {
        self.refresh_with_progress(uri, AnalysisProgressReporter::default()).await
    }

    async fn refresh_with_progress(
        &self, uri: &Url, progress: AnalysisProgressReporter,
    ) -> RefreshOutcome {
        let path = match Self::path(uri) {
            | Ok(path) => path,
            | Err(error) => return RefreshOutcome::Failed(error),
        };
        let analysis_path = path.clone();
        let (snapshot, revision) = {
            let session = self.session.lock().await;
            (session.compiler.snapshot(), session.revision(&path))
        };
        let analysis = match tokio::task::spawn_blocking(move || {
            AnalysisTask::run(move || {
                ProjectState::load_from_session(&analysis_path, &snapshot, |update| {
                    progress.report(update)
                })
            })
        })
        .await
        {
            | Ok(analysis) => analysis,
            | Err(error) => {
                return self
                    .commit_analysis(path, revision, Err(format!("analysis task failed: {error}")))
                    .await;
            }
        };
        match analysis {
            | AnalysisTask::Completed(project) => {
                self.commit_analysis(path, revision, project).await
            }
            | AnalysisTask::Cancelled => RefreshOutcome::Superseded,
        }
    }

    async fn commit_analysis(
        &self, path: PathBuf, revision: Option<DocumentRevision>,
        result: std::result::Result<ProjectState, String>,
    ) -> RefreshOutcome {
        let session = self.session.lock().await;
        if session.revision(&path) != revision {
            return RefreshOutcome::Superseded;
        }
        let mut projects = self.projects.write().await;
        match result {
            | Ok(project) => {
                projects.insert(path.clone(), CachedProject { revision, project });
                RefreshOutcome::Updated(path)
            }
            | Err(error) => {
                projects.remove(&path);
                RefreshOutcome::Failed(error)
            }
        }
    }

    async fn analyze_and_publish(
        &self, uri: Url, version: Option<i32>, mut progress: Option<AnalysisProgressSession>,
    ) {
        let reporter =
            progress.as_mut().map(|progress| progress.take_reporter()).unwrap_or_default();
        let outcome = self.refresh_with_progress(&uri, reporter).await;
        let refresh_semantic_tokens =
            matches!(&outcome, RefreshOutcome::Updated(_) | RefreshOutcome::Failed(_));
        let diagnostics = match outcome {
            | RefreshOutcome::Updated(path) => self
                .projects
                .read()
                .await
                .get(&path)
                .map(|cached| cached.project.diagnostics(&path))
                .unwrap_or_default(),
            | RefreshOutcome::Superseded => Vec::new(),
            | RefreshOutcome::Failed(message) => vec![Diagnostic {
                range: Range::new(Position::new(0, 0), Position::new(0, 1)),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("cajun".to_string()),
                message,
                ..Diagnostic::default()
            }],
        };
        if let Some(progress) = progress {
            progress.finish().await;
        }
        self.client.publish_diagnostics(uri, diagnostics, version).await;
        if refresh_semantic_tokens {
            self.request_semantic_tokens_refresh().await;
        }
    }

    async fn request_semantic_tokens_refresh(&self) {
        if !self.semantic_tokens_refresh.load(Ordering::Relaxed) {
            return;
        }
        if let Err(error) = self.client.semantic_tokens_refresh().await {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("failed to request a semantic-token refresh: {error}"),
                )
                .await;
        }
    }

    fn progress_session(&self, uri: &Url) -> Option<AnalysisProgressSession> {
        if !self.work_done_progress.load(Ordering::Relaxed) {
            return None;
        }
        let root = Self::path(uri).ok()?;
        let sequence = self.next_progress_sequence.fetch_add(1, Ordering::Relaxed);
        Some(AnalysisProgressSession::new(self.client.clone(), root, sequence))
    }

    fn hover_line_width(&self) -> HoverLineWidth {
        HoverLineWidth::new(self.hover_line_width.load(Ordering::Relaxed)).unwrap_or_default()
    }

    async fn set_document(&self, uri: &Url, text: String) -> Option<PathBuf> {
        let path = Self::path(uri).ok()?;
        self.session.lock().await.set_document(&path, text).ok()?;
        Some(path)
    }

    async fn document_source(&self, path: &Path) -> Option<String> {
        let source = self.session.lock().await.source(path);
        source.or_else(|| std::fs::read_to_string(path).ok())
    }

    fn path(uri: &Url) -> std::result::Result<PathBuf, String> {
        uri.to_file_path()
            .map(|path| Self::normalize_path(&path))
            .map_err(|_| format!("Cajun supports file URIs, but received `{uri}`"))
    }

    fn normalize_path(path: &Path) -> PathBuf {
        path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Cajun {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let hover_line_width =
            HoverLineWidth::from_initialization_options(params.initialization_options.as_ref());
        let work_done_progress = params
            .capabilities
            .window
            .as_ref()
            .and_then(|window| window.work_done_progress)
            .unwrap_or(false);
        let semantic_tokens_refresh = params
            .capabilities
            .workspace
            .as_ref()
            .and_then(|workspace| workspace.semantic_tokens.as_ref())
            .and_then(|semantic_tokens| semantic_tokens.refresh_support)
            .unwrap_or(false);
        self.work_done_progress.store(work_done_progress, Ordering::Relaxed);
        self.semantic_tokens_refresh.store(semantic_tokens_refresh, Ordering::Relaxed);
        self.hover_line_width.store(hover_line_width.columns(), Ordering::Relaxed);
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "Cajun".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
            capabilities: ServerCapabilities {
                position_encoding: Some(PositionEncodingKind::UTF16),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        will_save: Some(false),
                        will_save_wait_until: Some(false),
                        save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                    },
                )),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensOptions {
                        legend: SemanticHighlighter::legend(),
                        range: None,
                        full: Some(SemanticTokensFullOptions::Bool(true)),
                        ..SemanticTokensOptions::default()
                    }
                    .into(),
                ),
                ..ServerCapabilities::default()
            },
            offset_encoding: None,
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "Cajun initialized").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let document = params.text_document;
        if !ZydecoDocument::accepts(&document.uri) {
            return;
        }
        self.set_document(&document.uri, document.text).await;
        let progress = self.progress_session(&document.uri);
        self.analyze_and_publish(document.uri, Some(document.version), progress).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let document = params.text_document;
        if !ZydecoDocument::accepts(&document.uri) {
            return;
        }
        let Some(change) = params.content_changes.into_iter().last() else {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("ignored empty change notification for {}", document.uri),
                )
                .await;
            return;
        };
        self.set_document(&document.uri, change.text).await;
        self.analyze_and_publish(document.uri, Some(document.version), None).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if !ZydecoDocument::accepts(&params.text_document.uri) {
            return;
        }
        if let Some(text) = params.text {
            self.set_document(&params.text_document.uri, text).await;
        }
        self.analyze_and_publish(params.text_document.uri, None, None).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        if !ZydecoDocument::accepts(&uri) {
            return;
        }
        if let Ok(path) = Self::path(&uri) {
            let mut session = self.session.lock().await;
            session.close_document(&path);
            self.projects.write().await.remove(&path);
        }
        self.client.publish_diagnostics(uri, Vec::new(), None).await;
    }

    async fn goto_definition(
        &self, params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let target = params.text_document_position_params;
        if !ZydecoDocument::accepts(&target.text_document.uri) {
            return Ok(None);
        }
        let path = match self.refresh(&target.text_document.uri).await {
            | RefreshOutcome::Updated(path) => path,
            | RefreshOutcome::Failed(_) | RefreshOutcome::Superseded => return Ok(None),
        };
        let projects = self.projects.read().await;
        let location = projects
            .get(&path)
            .and_then(|cached| cached.project.definition(&path, target.position));
        Ok(location.map(GotoDefinitionResponse::Scalar))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let include_declaration = params.context.include_declaration;
        let target = params.text_document_position;
        if !ZydecoDocument::accepts(&target.text_document.uri) {
            return Ok(None);
        }
        let path = match self.refresh(&target.text_document.uri).await {
            | RefreshOutcome::Updated(path) => path,
            | RefreshOutcome::Failed(_) | RefreshOutcome::Superseded => return Ok(None),
        };
        let projects = self.projects.read().await;
        Ok(projects.get(&path).and_then(|cached| {
            cached.project.references(&path, target.position, include_declaration)
        }))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let target = params.text_document_position_params;
        if !ZydecoDocument::accepts(&target.text_document.uri) {
            return Ok(None);
        }
        let path = match self.refresh(&target.text_document.uri).await {
            | RefreshOutcome::Updated(path) => path,
            | RefreshOutcome::Failed(_) | RefreshOutcome::Superseded => return Ok(None),
        };
        let line_width = self.hover_line_width();
        let projects = self.projects.read().await;
        Ok(projects
            .get(&path)
            .and_then(|cached| cached.project.hover(&path, target.position, line_width)))
    }

    async fn document_symbol(
        &self, params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        if !ZydecoDocument::accepts(&params.text_document.uri) {
            return Ok(None);
        }
        let path = match self.refresh(&params.text_document.uri).await {
            | RefreshOutcome::Updated(path) => path,
            | RefreshOutcome::Failed(_) | RefreshOutcome::Superseded => return Ok(None),
        };
        let projects = self.projects.read().await;
        let symbols = projects
            .get(&path)
            .map(|cached| cached.project.document_symbols(&path))
            .unwrap_or_default();
        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;
        if !ZydecoDocument::accepts(&uri) {
            return Ok(None);
        }
        let path = match Self::path(&uri) {
            | Ok(path) => path,
            | Err(_) => return Ok(None),
        };
        let Some(source) = self.document_source(&path).await else {
            return Ok(None);
        };
        let formatter = DocumentFormatter::from_lsp(&params.options);
        match formatter.format(&source) {
            | FormattingOutcome::Edit(edit) => Ok(Some(vec![edit])),
            | FormattingOutcome::Unchanged => Ok(Some(Vec::new())),
            | FormattingOutcome::Skipped(reason) => {
                self.client
                    .log_message(
                        MessageType::WARNING,
                        format!("skipped formatting {uri}: {}", reason.message()),
                    )
                    .await;
                Ok(None)
            }
        }
    }

    async fn semantic_tokens_full(
        &self, params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        if !ZydecoDocument::accepts(&uri) {
            return Ok(None);
        }
        let path = match Self::path(&uri) {
            | Ok(path) => path,
            | Err(_) => return Ok(None),
        };
        let (revision, source) = {
            let session = self.session.lock().await;
            (session.revision(&path), session.source(&path))
        };
        let refined = self
            .projects
            .read()
            .await
            .get(&path)
            .filter(|cached| cached.revision == revision)
            .and_then(|cached| cached.project.semantic_tokens(&path));
        let data = match refined {
            | Some(tokens) => tokens,
            | None => {
                let source = match source {
                    | Some(source) => Some(source),
                    | None => std::fs::read_to_string(&path).ok(),
                };
                source.map(|source| SemanticHighlighter::lexical(&source)).unwrap_or_default()
            }
        };
        Ok(Some(SemanticTokens { result_id: None, data }.into()))
    }
}

#[cfg(test)]
mod tests {
    use super::{AnalysisTask, ZydecoDocument};
    use std::panic;
    use tower_lsp::lsp_types::Url;

    #[test]
    fn pending_write_cancellation_is_an_analysis_outcome() {
        let outcome: AnalysisTask<()> =
            AnalysisTask::run(|| panic::resume_unwind(Box::new(salsa::Cancelled::PendingWrite)));

        assert!(matches!(outcome, AnalysisTask::Cancelled));
    }

    #[test]
    fn accepts_only_zydeco_source_extensions() {
        ["file:///workspace/main.zy", "file:///workspace/main.zydeco"]
            .into_iter()
            .for_each(|uri| assert!(ZydecoDocument::accepts(&Url::parse(uri).unwrap())));

        [
            "file:///workspace/.gitignore",
            "file:///workspace/main",
            "file:///workspace/main.zy.toml",
            "file:///workspace/main.ZY",
            "untitled:main.zy",
        ]
        .into_iter()
        .for_each(|uri| assert!(!ZydecoDocument::accepts(&Url::parse(uri).unwrap())));
    }
}
