// use std::ops::ControlFlow;

// use async_lsp::client_monitor::ClientProcessMonitorLayer;
// use async_lsp::concurrency::ConcurrencyLayer;
// use async_lsp::lsp_types::{
//     notification, request, InitializeResult, OneOf, ServerCapabilities, TextDocumentSyncCapability,
//     TextDocumentSyncKind, TextDocumentSyncOptions, TextDocumentSyncSaveOptions,
// };
// use async_lsp::panic::CatchUnwindLayer;
// use async_lsp::router::Router;
// use async_lsp::server::LifecycleLayer;
// use async_lsp::tracing::TracingLayer;
// use async_lsp::ClientSocket;
// use documents::DocumentStore;
// use tower::ServiceBuilder;
// use tracing::Level;

// mod documents;
// mod document_symbols;
// mod text_position;

// pub struct ServerState {
//     _client: ClientSocket,
//     documents: DocumentStore,
// }

// #[tokio::main(flavor = "current_thread")]
// async fn main() {
//     let (server, _) = async_lsp::MainLoop::new_server(|client| {
//         let mut router =
//             Router::new(ServerState { _client: client.clone(), documents: Default::default() });
//         router
//             .request::<request::Initialize, _>(|_, _params| async move {
//                 Ok(InitializeResult {
//                     capabilities: ServerCapabilities {
//                         text_document_sync: Some(TextDocumentSyncCapability::Options(
//                             TextDocumentSyncOptions {
//                                 open_close: Some(true),
//                                 change: Some(TextDocumentSyncKind::INCREMENTAL),
//                                 will_save: Some(false),
//                                 will_save_wait_until: Some(false),
//                                 save: Some(TextDocumentSyncSaveOptions::Supported(false)),
//                             },
//                         )),
//                         document_symbol_provider: Some(OneOf::Left(true)),
//                         ..Default::default()
//                     },
//                     ..Default::default()
//                 })
//             })
//             .request::<request::DocumentSymbolRequest, _>(|state, params| {
//                 let document = state.documents.get_document(params.text_document.uri);

//                 async move {
//                     let document = document.await?;
//                     document_symbols::handle(&document)
//                 }
//             })
//             .notification::<notification::Initialized>(|_, _| ControlFlow::Continue(()))
//             .notification::<notification::DidChangeConfiguration>(|_, _| ControlFlow::Continue(()))
//             .notification::<notification::DidOpenTextDocument>(|state, params| {
//                 tokio::spawn(state.documents.open_document(params));
//                 ControlFlow::Continue(())
//             })
//             .notification::<notification::DidChangeTextDocument>(|state, params| {
//                 tokio::spawn(state.documents.change_document(params));
//                 ControlFlow::Continue(())
//             })
//             .notification::<notification::DidCloseTextDocument>(|state, params| {
//                 tokio::spawn(state.documents.close_document(params));
//                 ControlFlow::Continue(())
//             });

//         ServiceBuilder::new()
//             .layer(TracingLayer::default())
//             .layer(LifecycleLayer::default())
//             .layer(CatchUnwindLayer::default())
//             .layer(ConcurrencyLayer::default())
//             .layer(ClientProcessMonitorLayer::new(client))
//             .service(router)
//     });

//     tracing_subscriber::fmt()
//         .with_max_level(Level::INFO)
//         .with_ansi(false)
//         .with_writer(std::io::stderr)
//         .init();

//     // Prefer truly asynchronous piped stdin/stdout without blocking tasks.
//     #[cfg(unix)]
//     let (stdin, stdout) = (
//         async_lsp::stdio::PipeStdin::lock_tokio().unwrap(),
//         async_lsp::stdio::PipeStdout::lock_tokio().unwrap(),
//     );
//     // Fallback to spawn blocking read/write otherwise.
//     #[cfg(not(unix))]
//     let (stdin, stdout) = (
//         tokio_util::compat::TokioAsyncReadCompatExt::compat(tokio::io::stdin()),
//         tokio_util::compat::TokioAsyncWriteCompatExt::compat_write(tokio::io::stdout()),
//     );

//     server.run_buffered(stdin, stdout).await.unwrap()
// }

fn main() {}
