#![allow(unused)]

mod token;
use token::LEGEND_TYPE;

use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    sync::Arc,
};
use tokio::sync::RwLock;
use tower_lsp::{
    Client, LanguageServer, LspService, Server,
    jsonrpc::Result,
    lsp_types::{notification::Notification, *},
};
use zydeco_driver::{BuildSystem, PackId, Package, check::PackageStew, check::pack::PackageScoped};
use zydeco_surface::scoped::{
    arena::ArenaScoped,
    syntax::{DefId, Term, TermId},
};
use zydeco_syntax::SpanView;
use zydeco_utils::{arena::ArcGlobalAlloc, span::FileInfo};

/// Parsed project information
/// SAFETY: This is marked as Sync even though Span contains OnceCell.
/// This is safe because we only read from spans after they're initialized,
/// and we never mutate them across threads.
struct ProjectState {
    scoped: PackageScoped,
    file_infos: HashMap<PathBuf, FileInfo>,
}

unsafe impl Sync for ProjectState {}

/// The state and main struct for the Cajun Zydeco Language Server.
pub struct Cajun {
    client: Client,
    projects: Arc<RwLock<HashMap<PathBuf, ProjectState>>>,
    open_documents: Arc<RwLock<HashMap<PathBuf, String>>>,
    alloc: ArcGlobalAlloc,
}

impl Cajun {
    pub fn new(client: Client) -> Self {
        let alloc = ArcGlobalAlloc::new();
        Self {
            client,
            projects: Arc::new(RwLock::new(HashMap::new())),
            open_documents: Arc::new(RwLock::new(HashMap::new())),
            alloc,
        }
    }

    async fn refresh_project_for_uri(&self, uri: &Url) -> std::result::Result<(), String> {
        let path = uri.to_file_path().map_err(|_| "URI is not a file path".to_string())?;
        let path = normalize_path(&path);
        let overrides = {
            let open_docs = self.open_documents.read().await;
            open_docs.clone()
        };

        if let Some(proj_toml) = find_project_toml(&path) {
            let proj_toml = normalize_path(&proj_toml);
            let project = self.build_project_state(&proj_toml, &path, &overrides)?;
            let mut projects = self.projects.write().await;
            projects.insert(proj_toml.clone(), project);
        } else {
            let project = self.build_orphan_state(&path, &overrides)?;
            let mut projects = self.projects.write().await;
            projects.insert(path.clone(), project);
        }
        Ok(())
    }

    fn build_project_state(
        &self, proj_toml: &Path, focus_path: &Path, overrides: &HashMap<PathBuf, String>,
    ) -> std::result::Result<ProjectState, String> {
        let proj_toml = normalize_path(proj_toml);
        let focus_path = normalize_path(focus_path);
        let mut build_sys = BuildSystem::new();
        let root_pack =
            build_sys.add_local_package(&proj_toml).map_err(|e| format!("Build error: {}", e))?;
        let root_name = build_sys.packages[&root_pack].name();

        let mut stew: Option<PackageStew> = None;
        let mut included = HashSet::new();
        for pack in collect_packages(&build_sys, root_pack) {
            let Package::Local(local) = &build_sys.packages[&pack] else {
                continue;
            };
            for src in &local.srcs {
                let path = normalize_path(&local.path.join(src));
                if !included.insert(path.clone()) {
                    continue;
                }
                let source = read_source(&path, overrides)?;
                let part =
                    Package::parse_source(self.alloc.clone(), source, Some(path.clone()))
                        .map_err(|e| format!("Parse error: {}", e))?;
                stew = Some(match stew {
                    | Some(s) => s + part,
                    | None => part,
                });
            }
        }

        if !included.contains(&focus_path) {
            let source = read_source(&focus_path, overrides)?;
            let part = Package::parse_source(
                self.alloc.clone(),
                source,
                Some(focus_path.clone()),
            )
            .map_err(|e| format!("Parse error: {}", e))?;
            stew = Some(match stew {
                | Some(s) => s + part,
                | None => part,
            });
        }

        let stew = stew.unwrap_or_else(|| PackageStew::new(self.alloc.clone()));
        let scoped = stew
            .resolve(self.alloc.alloc())
            .map_err(|e| format!("Resolve error: {}", e))?
            .self_check(root_name.as_str());
        let file_infos = build_file_infos(&scoped);

        Ok(ProjectState { scoped, file_infos })
    }

    fn build_orphan_state(
        &self, file_path: &Path, overrides: &HashMap<PathBuf, String>,
    ) -> std::result::Result<ProjectState, String> {
        let file_path = normalize_path(file_path);
        let source = read_source(&file_path, overrides)?;
        let stew = Package::parse_source(self.alloc.clone(), source, Some(file_path.clone()))
            .map_err(|e| format!("Parse error: {}", e))?;
        let scoped = stew
            .resolve(self.alloc.alloc())
            .map_err(|e| format!("Resolve error: {}", e))?
            .self_check("<orphan>");
        let file_infos = build_file_infos(&scoped);

        Ok(ProjectState { scoped, file_infos })
    }

    fn find_term_at_position(
        &self, project: &ProjectState, file_path: &Path, line: u32, character: u32,
    ) -> Option<TermId> {
        // Convert LSP position to byte offset
        let file_path = normalize_path(file_path);
        let source = project.scoped.sources.get(&file_path)?;
        let offset = position_to_offset(source, line, character)?;

        // Find the term that contains this offset
        let span_arena_pair = (&project.scoped.spans, &project.scoped.arena);
        for (term_id, _) in project.scoped.arena.terms.iter() {
            let span = term_id.span(&span_arena_pair);
            let Some(span_path) = span.get_path() else {
                continue;
            };
            if span_path != &file_path {
                continue;
            }
            let (start, end) = span.get_cursor1();
            if offset >= start && offset < end {
                return Some(*term_id);
            }
        }
        None
    }

    fn find_def_location(&self, project: &ProjectState, def_id: DefId) -> Option<Location> {
        let entity = project.scoped.arena.textual.back(&def_id.into())?;
        let span = &project.scoped.spans[&entity];
        let (start, end) = span.get_cursor1();
        let path = span.get_path()?;
        let path = normalize_path(path);

        let file_info = project.file_infos.get(&path)?;
        let start_cursor = file_info.trans_span2(start);
        let end_cursor = file_info.trans_span2(end);

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
        if let Ok(path) = uri.to_file_path() {
            let path = normalize_path(&path);
            let mut open_docs = self.open_documents.write().await;
            open_docs.insert(path, text);
        }
        if let Err(e) = self.refresh_project_for_uri(&uri).await {
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
        if let Ok(path) = uri.to_file_path() {
            let path = normalize_path(&path);
            let mut open_docs = self.open_documents.write().await;
            open_docs.insert(path, text);
        }
        if let Err(e) = self.refresh_project_for_uri(&uri).await {
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

        let path = match uri.to_file_path() {
            | Ok(path) => normalize_path(&path),
            | Err(_) => return Ok(None),
        };
        let project_key = find_project_toml(&path)
            .map(|proj| normalize_path(&proj))
            .unwrap_or_else(|| path.clone());
        let projects = self.projects.read().await;
        let project = match projects.get(&project_key) {
            | Some(project) => project,
            | None => return Ok(None),
        };

        // Find the term at the cursor position
        let term_id = match self.find_term_at_position(project, &path, position.line, position.character) {
            | Some(id) => id,
            | None => return Ok(None),
        };

        // Get the term and check if it's a variable reference
        let term = project.scoped.arena.term(&term_id);
        let def_id = match term {
            | Term::Var(def_id) => def_id,
            | _ => return Ok(None),
        };

        // Find the location of the definition
        let location = match self.find_def_location(project, def_id) {
            | Some(loc) => loc,
            | None => return Ok(None),
        };

        Ok(Some(GotoDefinitionResponse::Scalar(location)))
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "file saved!").await;
    }
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        if let Ok(path) = uri.to_file_path() {
            let path = normalize_path(&path);
            let mut open_docs = self.open_documents.write().await;
            open_docs.remove(&path);
        }
        if let Err(e) = self.refresh_project_for_uri(&uri).await {
            self.client
                .log_message(MessageType::ERROR, format!("Failed to parse file: {}", e))
                .await;
        }
        self.client.log_message(MessageType::INFO, "file closed!").await;
    }
}

fn collect_packages(build_sys: &BuildSystem, root: PackId) -> Vec<PackId> {
    let mut stack = vec![root];
    let mut seen = HashSet::new();
    let mut order = Vec::new();
    while let Some(pack) = stack.pop() {
        if !seen.insert(pack) {
            continue;
        }
        order.push(pack);
        for dep in build_sys.depends_on.query(&pack) {
            stack.push(dep);
        }
    }
    order
}

fn read_source(
    path: &Path, overrides: &HashMap<PathBuf, String>,
) -> std::result::Result<String, String> {
    if let Some(text) = overrides.get(path) {
        return Ok(text.clone());
    }
    std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read {}: {}", path.display(), e))
}

fn build_file_infos(scoped: &PackageScoped) -> HashMap<PathBuf, FileInfo> {
    let mut file_infos = HashMap::new();
    for (path, source) in &scoped.sources {
        let path = normalize_path(path);
        let info = FileInfo::new(source.as_str(), Some(Arc::new(path.clone())));
        file_infos.insert(path, info);
    }
    file_infos
}

fn find_project_toml(path: &Path) -> Option<PathBuf> {
    let mut dir = if path.is_dir() { path.to_path_buf() } else { path.parent()?.to_path_buf() };
    loop {
        let candidate = dir.join("proj.toml");
        if candidate.is_file() {
            return Some(candidate);
        }
        if !dir.pop() {
            break;
        }
    }
    None
}

fn normalize_path(path: &Path) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}
