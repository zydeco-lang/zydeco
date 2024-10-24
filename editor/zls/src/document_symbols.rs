use async_lsp::lsp_types::{DocumentSymbol, DocumentSymbolResponse, Range, SymbolKind};
use async_lsp::{ErrorCode, ResponseError};
use lsp_textdocument::FullTextDocument;
use zydeco_driver::{check::pack::PackageStew, prelude::b, Package};
use zydeco_utils::arena::ArcGlobalAlloc;

use crate::text_position::span_to_range;

pub fn handle(
    document: &FullTextDocument,
) -> Result<Option<DocumentSymbolResponse>, ResponseError> {
    let alloc = ArcGlobalAlloc::new();

    let stew = Package::Repl(document.get_content(None).to_string()).parse_package(alloc).map_err(
        |err| ResponseError::new(ErrorCode::REQUEST_FAILED, format!("parsing error: {err}")),
    )?;

    let symbols = stew
        .top
        .0
        .iter()
        .flat_map(|decl| DocumentSymbolContext { document, stew: &stew }.declaration(decl))
        .collect();
    Ok(Some(DocumentSymbolResponse::Nested(symbols)))
}

pub struct DocumentSymbolContext<'a> {
    pub document: &'a FullTextDocument,
    pub stew: &'a PackageStew,
}

impl<'a> DocumentSymbolContext<'a> {
    fn declaration(&self, id: &b::DeclId) -> Vec<DocumentSymbol> {
        let b::Modifiers { public: _, external: _, inner } = &self.stew.arena.decls[id];
        match inner {
            | b::Declaration::AliasBody(d) => {
                let b::AliasBody { binder, bindee } = d;
                let binder = self.pattern(binder);
                let bindee = self.term(bindee);
                [].into_iter().chain(binder).chain(bindee).collect()
            }
            | b::Declaration::AliasHead(d) => {
                let b::AliasHead { binder, ty } = d;
                let binder = self.pattern(binder);
                let ty = ty.map(|ty| self.term(&ty)).unwrap_or_default();
                [].into_iter().chain(binder).chain(ty).collect()
            }
            | b::Declaration::Module(d) => {
                let b::Module { name: _, top } = d;
                let top = top.0.iter().flat_map(|decl| self.declaration(decl)).collect();
                top
            }
            | b::Declaration::Exec(d) => {
                let b::Exec(term) = d;
                self.term(term)
            }
        }
    }
    fn definition(&self, id: &b::DefId) -> DocumentSymbol {
        let def = &self.stew.arena.defs[id];
        let tid = self.stew.arena.textual.back(&id.clone().into()).unwrap();
        let span = &self.stew.spans[tid];
        let name_range = span_to_range(span, self.document);
        document_symbol_new(def.0.to_string(), SymbolKind::VARIABLE, name_range, name_range, None)
    }
    fn pattern(&self, id: &b::PatId) -> Vec<DocumentSymbol> {
        let pat = &self.stew.arena.pats[id];
        let tid = self.stew.arena.textual.back(&id.clone().into()).unwrap();
        let span = &self.stew.spans[tid];
        match pat {
            | b::Pattern::Ann(p) => {
                let b::Ann { tm, ty } = p;
                let tm = self.pattern(tm);
                let ty = self.term(ty);
                [].into_iter().chain(tm).chain(ty).collect()
            }
            | b::Pattern::Hole(p) => {
                let b::Hole = p;
                let range = span_to_range(span, self.document);
                vec![document_symbol_new("_".to_string(), SymbolKind::CONSTANT, range, range, None)]
            }
            | b::Pattern::Var(p) => {
                let def = p;
                vec![self.definition(def)]
            }
            | b::Pattern::Ctor(p) => {
                let b::Ctor(_ctorv, body) = p;
                let body = self.pattern(body);
                [].into_iter().chain(body).collect()
            }
            | b::Pattern::Triv(p) => {
                let b::Triv = p;
                vec![]
            }
            | b::Pattern::Cons(p) => {
                let b::Cons(a, b) = p;
                let a = self.pattern(a);
                let b = self.pattern(b);
                [].into_iter().chain(a).chain(b).collect()
            }
        }
    }
    fn term(&self, id: &b::TermId) -> Vec<DocumentSymbol> {
        let term = &self.stew.arena.terms[id];
        let tid = self.stew.arena.textual.back(&id.clone().into()).unwrap();
        let span = &self.stew.spans[tid];
        match term {
            | b::Term::Internal(t) => {
                let _ = t;
                vec![]
            }
            | b::Term::Sealed(t) => {
                let b::Sealed(body) = t;
                self.term(body)
            }
            | b::Term::Ann(t) => {
                let b::Ann { tm, ty } = t;
                let tm = self.term(tm);
                let ty = self.term(ty);
                [].into_iter().chain(tm).chain(ty).collect()
            }
            | b::Term::Hole(t) => {
                let b::Hole = t;
                let range = span_to_range(span, self.document);
                vec![document_symbol_new("_".to_string(), SymbolKind::CONSTANT, range, range, None)]
            }
            | b::Term::Var(t) => {
                let def = t;
                let name_range = span_to_range(span, self.document);
                vec![document_symbol_new(
                    def.2.to_string(),
                    SymbolKind::VARIABLE,
                    name_range,
                    name_range,
                    None,
                )]
            }
            | b::Term::Triv(t) => {
                let b::Triv = t;
                vec![]
            }
            | b::Term::Cons(t) => {
                let b::Cons(a, b) = t;
                let a = self.term(a);
                let b = self.term(b);
                [].into_iter().chain(a).chain(b).collect()
            }
            | b::Term::Abs(t) => {
                let b::Abs(binder, body) = t;
                let binder = self.pattern(binder);
                let body = self.term(body);
                [].into_iter().chain(binder).chain(body).collect()
            }
            | b::Term::App(t) => {
                let b::App(f, a) = t;
                let f = self.term(f);
                let a = self.term(a);
                [].into_iter().chain(f).chain(a).collect()
            }
            | b::Term::Fix(t) => {
                let b::Fix(binder, body) = t;
                let binder = self.pattern(binder);
                let body = self.term(body);
                [].into_iter().chain(binder).chain(body).collect()
            }
            | b::Term::Pi(t) => {
                let b::Pi(binder, body) = t;
                let binder = self.pattern(binder);
                let body = self.term(body);
                [].into_iter().chain(binder).chain(body).collect()
            }
            | b::Term::Sigma(t) => {
                let b::Sigma(binder, body) = t;
                let binder = self.pattern(binder);
                let body = self.term(body);
                [].into_iter().chain(binder).chain(body).collect()
            }
            | b::Term::Thunk(t) => {
                let b::Thunk(body) = t;
                self.term(body)
            }
            | b::Term::Force(t) => {
                let b::Force(body) = t;
                self.term(body)
            }
            | b::Term::Ret(t) => {
                let b::Ret(body) = t;
                self.term(body)
            }
            | b::Term::Do(t) => {
                let b::Bind { binder, bindee, tail } = t;
                let binder = self.pattern(binder);
                let bindee = self.term(bindee);
                let tail = self.term(tail);
                [].into_iter().chain(binder).chain(bindee).chain(tail).collect()
            }
            | b::Term::Let(t) => {
                let b::PureBind { binder, bindee, tail } = t;
                let binder = self.pattern(binder);
                let bindee = self.term(bindee);
                let tail = self.term(tail);
                [].into_iter().chain(binder).chain(bindee).chain(tail).collect()
            }
            | b::Term::MoBlock(t) => {
                let b::MoBlock(body) = t;
                self.term(body)
            }
            | b::Term::Data(t) => {
                let b::Data { arms } = t;
                arms.iter().flat_map(|arm| self.term(&arm.param)).collect()
            }
            | b::Term::CoData(t) => {
                let b::CoData { arms } = t;
                arms.iter().flat_map(|arm| self.term(&arm.out)).collect()
            }
            | b::Term::Ctor(t) => {
                let b::Ctor(_ctorv, body) = t;
                let body = self.term(body);
                [].into_iter().chain(body).collect()
            }
            | b::Term::Match(t) => {
                let b::Match { scrut, arms } = t;
                let scrut = self.term(scrut);
                let arms: Vec<_> = arms
                    .iter()
                    .flat_map(|arm| {
                        [].into_iter().chain(self.pattern(&arm.binder)).chain(self.term(&arm.tail))
                    })
                    .collect();
                [].into_iter().chain(scrut).chain(arms).collect()
            }
            | b::Term::CoMatch(t) => {
                let b::CoMatch { arms } = t;
                arms.iter().flat_map(|arm| [].into_iter().chain(self.term(&arm.tail))).collect()
            }
            | b::Term::Dtor(t) => {
                let b::Dtor(body, _dtorv) = t;
                let body = self.term(body);
                [].into_iter().chain(body).collect()
            }
            | b::Term::Lit(t) => {
                let _ = t;
                vec![]
            }
        }
    }
}

/// Constructs a [`DocumentSymbol`]. Since `DocumentSymbol::deprecated` is marked as
/// `#[deprecated]`, we must put `#[allow(deprecated)]` on any function directly constructing a
/// `DocumentSymbol`. We define this function to minimize where this happens.
#[allow(deprecated)]
fn document_symbol_new(
    name: String, kind: SymbolKind, range: Range, selection_range: Range,
    children: Option<Vec<DocumentSymbol>>,
) -> DocumentSymbol {
    DocumentSymbol {
        name,
        detail: None,
        kind,
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children,
    }
}
