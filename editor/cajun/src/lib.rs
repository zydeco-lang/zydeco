#![allow(unused)]

mod token;
use token::LEGEND_TYPE;

use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{notification::Notification, *},
    Client, LanguageServer, LspService, Server,
};

/// The state and main struct for the Cajun Zydeco Language Server.
pub struct Cajun {}

#[tower_lsp::async_trait]
impl LanguageServer for Cajun {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(
                ServerInfo {
                    name: "Cajun Zydeco Language Server".to_string(),
                    version: Some(env!("CARGO_PKG_VERSION").to_string()),
                }
                .into(),
            ),
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
                //                 ..FileOperationPattern::default()
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
                //                 work_done_progress_options: WorkDoneProgressOptions::default(),
                //                 legend: SemanticTokensLegend {
                //                     token_types: LEGEND_TYPE.into(),
                //                     token_modifiers: vec![],
                //                 },
                //                 range: Some(true),
                //                 full: Some(SemanticTokensFullOptions::Bool(true)),
                //             },
                //             static_registration_options: StaticRegistrationOptions::default(),
                //         },
                //     ),
                // ),
                // definition_provider: Some(OneOf::Left(true)),
                // references_provider: Some(OneOf::Left(true)),
                // rename_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}
