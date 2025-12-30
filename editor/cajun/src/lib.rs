#![allow(unused)]

mod token;
use token::LEGEND_TYPE;

use dashmap::DashMap;
use std::{path::PathBuf, sync::Arc};
use tower_lsp::{
    Client, LanguageServer, LspService, Server,
    jsonrpc::Result,
    lsp_types::{notification::Notification, *},
};
use zydeco_driver::{Package, check::pack::PackageScoped};
use zydeco_surface::scoped::{
    arena::ArenaScoped,
    syntax::{DefId, Term, TermId},
};
use zydeco_syntax::SpanView;
use zydeco_utils::{arena::ArcGlobalAlloc, span::FileInfo};

/// Parsed file information
/// SAFETY: This is marked as Sync even though Span contains OnceCell.
/// This is safe because we only read from spans after they're initialized,
/// and we never mutate them across threads.
struct ParsedFile {
    scoped: PackageScoped,
    file_info: FileInfo,
}

unsafe impl Sync for ParsedFile {}

/// The state and main struct for the Cajun Zydeco Language Server.
pub struct Cajun {
    client: Client,
    files: Arc<DashMap<String, ParsedFile>>,
    alloc: ArcGlobalAlloc,
}

impl Cajun {
    pub fn new(client: Client) -> Self {
        let alloc = ArcGlobalAlloc::new();
        Self { client, files: Arc::new(DashMap::new()), alloc }
    }

    async fn parse_file(&self, uri: &Url, text: &str) -> std::result::Result<(), String> {
        let path = uri.to_file_path().map_err(|_| "URI is not a file path".to_string())?;
        let file_info = FileInfo::new(text, Some(Arc::new(path.clone())));

        // Use zydeco_driver to parse the file
        let pack = Package::parse_source(self.alloc.clone(), text.to_string(), Some(path.clone()))
            .map_err(|e| format!("Parse error: {}", e))?;

        // Resolve to get ScopedArena
        let scoped = pack
            .resolve(self.alloc.alloc())
            .map_err(|e| format!("Resolve error: {}", e))?
            .self_check("");

        let parsed = ParsedFile { scoped, file_info };

        self.files.insert(uri.to_string(), parsed);
        Ok(())
    }

    fn find_term_at_position(
        &self, file: &ParsedFile, line: u32, character: u32,
    ) -> Option<TermId> {
        // Convert LSP position to byte offset
        let source: &String = file.scoped.sources.values().next()?;
        let offset = position_to_offset(source, line, character)?;

        // Find the term that contains this offset
        let span_arena_pair = (&file.scoped.spans, &file.scoped.arena);
        for (term_id, _) in file.scoped.arena.terms.iter() {
            let span = term_id.span(&span_arena_pair);
            let (start, end) = span.get_cursor1();
            if offset >= start && offset < end {
                return Some(*term_id);
            }
        }
        None
    }

    fn find_def_location(&self, file: &ParsedFile, def_id: DefId) -> Option<Location> {
        let entity = file.scoped.arena.textual.back(&def_id.into())?;
        let span = &file.scoped.spans[&entity];
        let (start, end) = span.get_cursor1();

        let start_cursor = file.file_info.trans_span2(start);
        let end_cursor = file.file_info.trans_span2(end);

        let path = file.file_info.path();
        let uri = Url::from_file_path(&path).ok()?;

        Some(Location {
            uri,
            range: Range {
                start: Position {
                    line: start_cursor.line as u32,
                    character: start_cursor.column as u32,
                },
                end: Position { line: end_cursor.line as u32, character: end_cursor.column as u32 },
            },
        })
    }
}

fn position_to_offset(text: &str, line: u32, character: u32) -> Option<usize> {
    let mut current_line = 0;
    let mut line_start = 0;

    for (i, c) in text.char_indices() {
        if current_line == line {
            // We're on the target line, find the character position
            let line_text = &text[line_start..];
            let char_count =
                line_text.chars().take(character as usize).map(|c| c.len_utf8()).sum::<usize>();
            return Some(line_start + char_count);
        }
        if c == '\n' {
            current_line += 1;
            line_start = i + 1;
        }
    }

    // Handle end of file
    if current_line == line { Some(text.len()) } else { None }
}

#[tower_lsp::async_trait]
impl LanguageServer for Cajun {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "Cajun Zydeco Language Server".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
            offset_encoding: None,
            capabilities: ServerCapabilities {
                // inlay_hint_provider: Some(OneOf::Left(true)),
                // text_document_sync: Some(TextDocumentSyncCapability::Kind(
                //     TextDocumentSyncKind::FULL,
                // )),
                // completion_provider: Some(CompletionOptions {
                //     resolve_provider: Some(false),
                //     trigger_characters: Some(vec![".".to_string()]),
                //     work_done_progress_options: Default::default(),
                //     all_commit_characters: None,
                //     completion_item: None,
                // }),
                // execute_command_provider: None,
                // workspace: Some(WorkspaceServerCapabilities {
                //     workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                //         supported: Some(true),
                //         change_notifications: Some(OneOf::Left(true)),
                //     }),
                //     file_operations: Some({
                //         let zydeco_filters = vec![FileOperationFilter {
                //             scheme: Some("file".to_string()),
                //             pattern: FileOperationPattern {
                //                 glob: "**/*.{zy,zydeco}".to_string(),
                //                 ..Default::default()
                //             },
                //         }];
                //         let opts = FileOperationRegistrationOptions { filters: zydeco_filters };
                //         WorkspaceFileOperationsServerCapabilities {
                //             did_create: Some(opts.to_owned()),
                //             will_create: Some(opts.to_owned()),
                //             did_rename: Some(opts.to_owned()),
                //             will_rename: Some(opts.to_owned()),
                //             did_delete: Some(opts.to_owned()),
                //             will_delete: Some(opts.to_owned()),
                //         }
                //     }),
                // }),
                // semantic_tokens_provider: Some(
                //     SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                //         SemanticTokensRegistrationOptions {
                //             text_document_registration_options: {
                //                 TextDocumentRegistrationOptions {
                //                     document_selector: Some(vec![DocumentFilter {
                //                         language: Some("zydeco".to_string()),
                //                         scheme: Some("file".to_string()),
                //                         pattern: Some("*.{zy,zydeco}".to_string()),
                //                     }]),
                //                 }
                //             },
                //             semantic_tokens_options: SemanticTokensOptions {
                //                 work_done_progress_options: Default::default(),
                //                 legend: SemanticTokensLegend {
                //                     token_types: LEGEND_TYPE.into(),
                //                     token_modifiers: vec![],
                //                 },
                //                 range: Some(true),
                //                 full: Some(SemanticTokensFullOptions::Bool(true)),
                //             },
                //             static_registration_options: Default::default(),
                //         },
                //     ),
                // ),
                definition_provider: Some(OneOf::Left(true)),
                // references_provider: Some(OneOf::Left(true)),
                // rename_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
        })
    }
    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "initialized!").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let text = params.text_document.text;
        if let Err(e) = self.parse_file(&uri, &text).await {
            self.client
                .log_message(MessageType::ERROR, format!("Failed to parse file: {}", e))
                .await;
        } else {
            self.client.log_message(MessageType::INFO, "file opened!").await;
        }
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let text = std::mem::take(&mut params.content_changes[0].text);
        if let Err(e) = self.parse_file(&uri, &text).await {
            self.client
                .log_message(MessageType::ERROR, format!("Failed to parse file: {}", e))
                .await;
        }
    }

    async fn goto_definition(
        &self, params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri.clone();
        let position = params.text_document_position_params.position;

        let file = match self.files.get(&uri.to_string()) {
            | Some(f) => f,
            | None => return Ok(None),
        };

        // Find the term at the cursor position
        let term_id =
            match self.find_term_at_position(file.value(), position.line, position.character) {
                | Some(id) => id,
                | None => return Ok(None),
            };

        // Get the term and check if it's a variable reference
        let term = file.value().scoped.arena.term(&term_id);
        let def_id = match term {
            | Term::Var(def_id) => def_id,
            | _ => return Ok(None),
        };

        // Find the location of the definition
        let location = match self.find_def_location(file.value(), def_id) {
            | Some(loc) => loc,
            | None => return Ok(None),
        };

        Ok(Some(GotoDefinitionResponse::Scalar(location)))
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "file saved!").await;
    }
    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "file closed!").await;
    }
}
