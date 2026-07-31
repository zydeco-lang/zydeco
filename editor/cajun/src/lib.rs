mod analysis;
mod semantic;

use analysis::ProjectState;
use semantic::SemanticHighlighter;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};
use tokio::sync::RwLock;
use tower_lsp::{
    Client, LanguageServer,
    jsonrpc::Result,
    lsp_types::{
        Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
        DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentSymbolParams,
        DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse, InitializeParams,
        InitializeResult, InitializedParams, MessageType, OneOf, Position, PositionEncodingKind,
        Range, SemanticTokens, SemanticTokensFullOptions, SemanticTokensOptions,
        SemanticTokensParams, SemanticTokensResult, ServerCapabilities, ServerInfo,
        TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
        TextDocumentSyncSaveOptions, Url,
    },
};

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

/// The Cajun Zydeco language server.
///
/// Cajun treats every open source file as a potential root term. Its imported
/// files are loaded through the same hygienic source graph as the compiler,
/// with in-memory editor contents overriding the corresponding files on disk.
pub struct Cajun {
    client: Client,
    projects: RwLock<HashMap<PathBuf, ProjectState>>,
    open_documents: RwLock<HashMap<PathBuf, String>>,
}

impl Cajun {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            projects: RwLock::new(HashMap::new()),
            open_documents: RwLock::new(HashMap::new()),
        }
    }

    async fn refresh(&self, uri: &Url) -> std::result::Result<PathBuf, String> {
        let path = Self::path(uri)?;
        let overrides = self.open_documents.read().await.clone();
        match ProjectState::load(&path, &overrides) {
            | Ok(project) => {
                self.projects.write().await.insert(path.clone(), project);
                Ok(path)
            }
            | Err(error) => {
                self.projects.write().await.remove(&path);
                Err(error)
            }
        }
    }

    async fn analyze_and_publish(&self, uri: Url, version: Option<i32>) {
        let diagnostics = match self.refresh(&uri).await {
            | Ok(_) => Vec::new(),
            | Err(message) => vec![Diagnostic {
                range: Range::new(Position::new(0, 0), Position::new(0, 1)),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("cajun".to_string()),
                message,
                ..Diagnostic::default()
            }],
        };
        self.client.publish_diagnostics(uri, diagnostics, version).await;
    }

    async fn set_document(&self, uri: &Url, text: String) -> Option<PathBuf> {
        let path = Self::path(uri).ok()?;
        self.open_documents.write().await.insert(path.clone(), text);
        Some(path)
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
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
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
                document_symbol_provider: Some(OneOf::Left(true)),
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
        self.analyze_and_publish(document.uri, Some(document.version)).await;
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
        self.analyze_and_publish(document.uri, Some(document.version)).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if !ZydecoDocument::accepts(&params.text_document.uri) {
            return;
        }
        if let Some(text) = params.text {
            self.set_document(&params.text_document.uri, text).await;
        }
        self.analyze_and_publish(params.text_document.uri, None).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        if !ZydecoDocument::accepts(&uri) {
            return;
        }
        if let Ok(path) = Self::path(&uri) {
            self.open_documents.write().await.remove(&path);
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
            | Ok(path) => path,
            | Err(_) => return Ok(None),
        };
        let projects = self.projects.read().await;
        let location =
            projects.get(&path).and_then(|project| project.definition(&path, target.position));
        Ok(location.map(GotoDefinitionResponse::Scalar))
    }

    async fn document_symbol(
        &self, params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        if !ZydecoDocument::accepts(&params.text_document.uri) {
            return Ok(None);
        }
        let path = match self.refresh(&params.text_document.uri).await {
            | Ok(path) => path,
            | Err(_) => return Ok(None),
        };
        let projects = self.projects.read().await;
        let symbols =
            projects.get(&path).map(|project| project.document_symbols(&path)).unwrap_or_default();
        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
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
        let refined = self
            .projects
            .read()
            .await
            .get(&path)
            .and_then(|project| project.semantic_tokens(&path));
        let data = match refined {
            | Some(tokens) => tokens,
            | None => {
                let source = self
                    .open_documents
                    .read()
                    .await
                    .get(&path)
                    .cloned()
                    .or_else(|| std::fs::read_to_string(&path).ok());
                source.map(|source| SemanticHighlighter::lexical(&source)).unwrap_or_default()
            }
        };
        Ok(Some(SemanticTokens { result_id: None, data }.into()))
    }
}

#[cfg(test)]
mod tests {
    use super::ZydecoDocument;
    use tower_lsp::lsp_types::Url;

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
