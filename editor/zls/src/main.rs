use std::ops::ControlFlow;

use async_lsp::client_monitor::ClientProcessMonitorLayer;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::lsp_types::notification::Notification;
use async_lsp::lsp_types::{
    notification, request, Hover, HoverContents, HoverProviderCapability, InitializeResult,
    MarkedString, MessageType, ServerCapabilities, ShowMessageParams, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextDocumentSyncOptions, TextDocumentSyncSaveOptions,
};
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::router::Router;
use async_lsp::server::LifecycleLayer;
use async_lsp::tracing::TracingLayer;
use async_lsp::ClientSocket;
use lsp_textdocument::TextDocuments;
use tower::ServiceBuilder;
use tracing::Level;

struct ServerState {
    client: ClientSocket,
    documents: TextDocuments,
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let (server, _) = async_lsp::MainLoop::new_server(|client| {
        let mut router =
            Router::new(ServerState { client: client.clone(), documents: TextDocuments::new() });
        router
            .request::<request::Initialize, _>(|_, _params| async move {
                eprintln!("init language server");
                Ok(InitializeResult {
                    capabilities: ServerCapabilities {
                        text_document_sync: Some(TextDocumentSyncCapability::Options(
                            TextDocumentSyncOptions {
                                open_close: Some(false),
                                change: Some(TextDocumentSyncKind::INCREMENTAL),
                                will_save: Some(false),
                                will_save_wait_until: Some(false),
                                save: Some(TextDocumentSyncSaveOptions::Supported(false)),
                            },
                        )),
                        hover_provider: Some(HoverProviderCapability::Simple(true)),
                        ..Default::default()
                    },
                    ..Default::default()
                })
            })
            .request::<request::HoverRequest, _>(|_, _| async move {
                Ok(Some(Hover {
                    contents: HoverContents::Scalar(MarkedString::String(
                        "hover from zls!!".to_owned(),
                    )),
                    range: None,
                }))
            })
            .notification::<notification::Initialized>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidChangeConfiguration>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidOpenTextDocument>(|state, params| {
                let value = serde_json::to_value(params).unwrap();
                state.documents.listen(notification::DidOpenTextDocument::METHOD, &value);
                ControlFlow::Continue(())
            })
            .notification::<notification::DidChangeTextDocument>(|state, params| {
                let value = serde_json::to_value(params).unwrap();
                state.documents.listen(notification::DidChangeTextDocument::METHOD, &value);
                ControlFlow::Continue(())
            })
            .notification::<notification::DidCloseTextDocument>(|state, params| {
                let value = serde_json::to_value(params).unwrap();
                state.documents.listen(notification::DidCloseTextDocument::METHOD, &value);
                ControlFlow::Continue(())
            });

        ServiceBuilder::new()
            .layer(TracingLayer::default())
            .layer(LifecycleLayer::default())
            .layer(CatchUnwindLayer::default())
            .layer(ConcurrencyLayer::default())
            .layer(ClientProcessMonitorLayer::new(client))
            .service(router)
    });

    tracing_subscriber::fmt()
        .with_max_level(Level::INFO)
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .init();

    // Prefer truly asynchronous piped stdin/stdout without blocking tasks.
    #[cfg(unix)]
    let (stdin, stdout) = (
        async_lsp::stdio::PipeStdin::lock_tokio().unwrap(),
        async_lsp::stdio::PipeStdout::lock_tokio().unwrap(),
    );
    // Fallback to spawn blocking read/write otherwise.
    #[cfg(not(unix))]
    let (stdin, stdout) = (
        tokio_util::compat::TokioAsyncReadCompatExt::compat(tokio::io::stdin()),
        tokio_util::compat::TokioAsyncWriteCompatExt::compat_write(tokio::io::stdout()),
    );

    server.run_buffered(stdin, stdout).await.unwrap()
}
