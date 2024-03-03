use std::path::PathBuf;

use async_lsp::lsp_types::{DocumentSymbol, DocumentSymbolResponse, SymbolKind};
use async_lsp::{ErrorCode, ResponseError};
use lsp_textdocument::FullTextDocument;
use zydeco_lang::surface::parse::syntax::Declaration;
use zydeco_lang::syntax::{CtorV, DeclSymbol, DtorV, NameT};
use zydeco_lang::utils::span::{Sp, Span, SpanView};
use zydeco_lang::zydeco::ZydecoFile;

use crate::text_position::span_to_range;

pub fn handle(
    document: &FullTextDocument,
) -> Result<Option<DocumentSymbolResponse>, ResponseError> {
    let ast = ZydecoFile::parse_src(document.get_content(None), PathBuf::new()).map_err(|err| {
        ResponseError::new(ErrorCode::REQUEST_FAILED, format!("parsing error: {err}"))
    })?;

    let symbols = ast
        .inner
        .declarations
        .iter()
        .flat_map(|decl| declaration_to_symbol(decl, document))
        .collect();
    Ok(Some(DocumentSymbolResponse::Nested(symbols)))
}

fn declaration_to_symbol(
    decl: &Sp<DeclSymbol<Declaration>>, document: &FullTextDocument,
) -> Vec<DocumentSymbol> {
    match &decl.inner.inner {
        Declaration::Module(module_def) => {
            let children = module_def
                .declarations
                .iter()
                .flat_map(|decl| declaration_to_symbol(decl, document))
                .collect();

            match &module_def.name {
                Some(name_ref) => {
                    vec![name_to_symbol(
                        name_ref,
                        decl.span(),
                        document,
                        SymbolKind::MODULE,
                        children,
                    )]
                }
                None => children,
            }
        }
        Declaration::UseDef(_) => vec![],
        Declaration::Data(data_def) => {
            let children =
                data_def.ctors.iter().map(|ctor| ctorv_to_symbol(&ctor.ctorv, document)).collect();

            vec![name_to_symbol(&data_def.name, decl.span(), document, SymbolKind::ENUM, children)]
        }
        Declaration::Codata(codata_def) => {
            let children = codata_def
                .dtors
                .iter()
                .map(|dtor| dtorv_to_symbol(&dtor.dtorv, document))
                .collect();

            vec![name_to_symbol(
                &codata_def.name,
                decl.span(),
                document,
                SymbolKind::CLASS,
                children,
            )]
        }
        Declaration::Alias(alias_def) => {
            // TODO: lookup symbol to display correct kind
            vec![name_to_symbol(&alias_def.name, decl.span(), document, SymbolKind::ENUM, vec![])]
        }
        Declaration::Define(def) => {
            // TODO: handle local definitions
            let children = vec![];

            let kind = match def.0.params.len() {
                0 => SymbolKind::CONSTANT,
                _ => SymbolKind::FUNCTION,
            };

            vec![name_to_symbol(&def.0.name.0, decl.span(), document, kind, children)]
        }
        // TODO: handle main, local definitions
        Declaration::Main(_) => vec![],
    }
}

#[allow(deprecated)]
fn name_to_symbol(
    name: &impl NameT, item_span: &Span, document: &FullTextDocument, kind: SymbolKind,
    children: Vec<DocumentSymbol>,
) -> DocumentSymbol {
    let name_view = name.name();
    let name_str = name_view.ident.inner.clone();
    let name_range = span_to_range(name_view.ident.span(), document);

    let item_range = span_to_range(item_span, document);

    DocumentSymbol {
        name: name_str,
        detail: None,
        kind,
        tags: None,
        deprecated: None,
        range: item_range,
        selection_range: name_range,
        children: Some(children),
    }
}

#[allow(deprecated)]
fn ctorv_to_symbol(ctorv: &CtorV, document: &FullTextDocument) -> DocumentSymbol {
    let name_range = span_to_range(ctorv.span(), document);

    DocumentSymbol {
        name: ctorv.name().to_owned(),
        detail: None,
        kind: SymbolKind::CONSTRUCTOR,
        tags: None,
        deprecated: None,
        // TODO: get span of entire constructor
        range: name_range,
        selection_range: name_range,
        children: None,
    }
}

#[allow(deprecated)]
fn dtorv_to_symbol(dtorv: &DtorV, document: &FullTextDocument) -> DocumentSymbol {
    let name_range = span_to_range(dtorv.span(), document);

    DocumentSymbol {
        name: dtorv.name().to_owned(),
        detail: None,
        kind: SymbolKind::METHOD,
        tags: None,
        deprecated: None,
        // TODO: get span of entire constructor
        range: name_range,
        selection_range: name_range,
        children: None,
    }
}
