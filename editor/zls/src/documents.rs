// use std::collections::HashMap;
// use std::future::Future;
// use std::ops::Deref;
// use std::sync::Arc;

// use async_lsp::lsp_types::{
//     DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, Url,
// };
// use async_lsp::{ErrorCode, ResponseError};
// use lsp_textdocument::FullTextDocument;
// use tokio::sync::{OwnedRwLockReadGuard, RwLock};

// // TODO: virtual filesystem abstraction to handle on-disk and client-provided documents
// #[derive(Default)]
// pub struct DocumentStore {
//     inner: Arc<RwLock<HashMap<Url, FullTextDocument>>>,
// }

// impl DocumentStore {
//     /// Returns a [`Future`] which, if the document at the requested [`Url`] exists, resolves to an
//     /// owned read lock providing access to that document.
//     pub fn get_document(
//         &self, url: Url,
//     ) -> impl Future<Output = Result<impl Deref<Target = FullTextDocument>, ResponseError>>
//            + Send
//            + 'static {
//         // Notes on the signature and implementation:
//         // - The `Future`s returned by the LSP router require `Send + 'static`, so for convenience,
//         //   we want to provide a fully owned, thread safe `Future` given a reference to the
//         //   document store.
//         // - For correctness reasons, it is nice to put an `RwLock` around everything, since this
//         //   prevents modifications to the project from impacting ongoing analysis. A more complex,
//         //   but potentially faster, approach would be to allow for limited forms of this, or to
//         //   track and protect dependencies/dependents so that fined-grained locking can be used
//         //   instead.
//         // - The `impl Deref<...>` type name is used because the actual type is an implementation
//         //   detail (depends on our `RwLock` implementation) and is long and complex.

//         let inner = Arc::clone(&self.inner);
//         async move {
//             let guard = inner.read_owned().await;
//             let doc = OwnedRwLockReadGuard::try_map(guard, |doc_map| doc_map.get(&url)).ok();
//             doc.ok_or_else(|| {
//                 ResponseError::new(
//                     ErrorCode::INVALID_PARAMS,
//                     format!("could not find document {url}"),
//                 )
//             })
//         }
//     }

//     pub fn open_document(&self, params: DidOpenTextDocumentParams) -> impl Future<Output = ()> {
//         let inner = Arc::clone(&self.inner);
//         async move {
//             let doc = FullTextDocument::new(
//                 params.text_document.language_id,
//                 params.text_document.version,
//                 params.text_document.text,
//             );

//             let mut doc_map = inner.write().await;
//             doc_map.insert(params.text_document.uri, doc);
//         }
//     }

//     pub fn change_document(&self, params: DidChangeTextDocumentParams) -> impl Future<Output = ()> {
//         let inner = Arc::clone(&self.inner);
//         async move {
//             let mut doc_map = inner.write().await;
//             let Some(doc) = doc_map.get_mut(&params.text_document.uri) else {
//                 return;
//             };

//             // hack around mismatched `lsp_types` version
//             let changes: Vec<_> =
//                 serde_json::from_value(serde_json::to_value(params.content_changes).unwrap())
//                     .unwrap();
//             doc.update(&changes, params.text_document.version);
//         }
//     }

//     pub fn close_document(&self, params: DidCloseTextDocumentParams) -> impl Future<Output = ()> {
//         let inner = Arc::clone(&self.inner);
//         async move {
//             let mut doc_map = inner.write().await;
//             doc_map.remove(&params.text_document.uri);
//         }
//     }
// }
