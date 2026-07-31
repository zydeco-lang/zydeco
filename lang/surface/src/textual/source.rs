use super::syntax::*;
use std::path::PathBuf;
use thiserror::Error;

/// A validated source import attached to one term-level splice site.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportDirective {
    pub path: PathBuf,
    pub span: Span,
}

/// An import occurrence in a parsed source unit.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportSite {
    pub term: TermId,
    pub directive: ImportDirective,
}

/// A validated intrinsic CBPV term splice.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntrinsicDirective {
    pub role: IntrinsicRole,
    pub span: Span,
}

/// An intrinsic splice occurrence in a parsed source unit.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntrinsicSite {
    pub term: TermId,
    pub payload: TermId,
    pub directive: IntrinsicDirective,
}

/// A validated Builtin role annotation attached to a package-signature term.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuiltinDirective {
    pub role: BuiltinRole,
    pub span: Span,
}

/// A Builtin role occurrence in a parsed source unit.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuiltinSite {
    pub term: TermId,
    pub payload: TermId,
    pub directive: BuiltinDirective,
}

#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum ImportDirectiveError {
    #[error("import at {span} expects one path argument, but found {found}")]
    PathArity { term: TermId, span: Span, found: usize },
    #[error("import path at {span} must be a string literal")]
    PathNotString { term: TermId, span: Span },
    #[error("import path at {span} must not be empty")]
    EmptyPath { term: TermId, span: Span },
    #[error("import at {span} must annotate a hole expression")]
    PayloadNotHole { term: TermId, span: Span },
}

#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum BuiltinDirectiveError {
    #[error("invalid builtin annotation at {span}: {source}")]
    Invalid {
        term: TermId,
        span: Span,
        #[source]
        source: BuiltinMetaError,
    },
}

#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum IntrinsicDirectiveError {
    #[error("invalid intrinsic annotation at {span}: {source}")]
    Invalid {
        term: TermId,
        span: Span,
        #[source]
        source: IntrinsicMetaError,
    },
    #[error("intrinsic at {span} must annotate a hole expression")]
    PayloadNotHole { term: TermId, span: Span },
}

impl SourceUnit {
    /// Decode all import metadata parsed into this source unit.
    ///
    /// The textual arena must belong exclusively to this unit. A parser used
    /// for source loading is therefore finished after parsing one source file.
    pub fn imports(
        &self, arena: &TextArena, spans: &SpanArena,
    ) -> Result<Vec<ImportSite>, ImportDirectiveError> {
        let _root = &arena.terms[&self.root];
        let mut imports = arena
            .terms
            .iter()
            .filter_map(|(term, syntax)| match syntax {
                | Term::Meta(MetaT(meta, payload)) => {
                    ImportSite::decode(*term, meta, *payload, arena, spans)
                }
                | _ => None,
            })
            .collect::<Result<Vec<_>, _>>()?;
        imports.sort_by_key(|site| site.directive.span.get_cursor1());
        Ok(imports)
    }

    /// Decode and validate all Builtin role annotations in this source unit.
    pub fn builtins(
        &self, arena: &TextArena, spans: &SpanArena,
    ) -> Result<Vec<BuiltinSite>, BuiltinDirectiveError> {
        let _root = &arena.terms[&self.root];
        let mut builtins = arena
            .terms
            .iter()
            .filter_map(|(term, syntax)| match syntax {
                | Term::Meta(MetaT(meta, payload)) => {
                    BuiltinSite::decode(*term, meta, *payload, spans)
                }
                | _ => None,
            })
            .collect::<Result<Vec<_>, _>>()?;
        builtins.sort_by_key(|site| site.directive.span.get_cursor1());
        Ok(builtins)
    }

    /// Decode and validate intrinsic CBPV term splices in this source unit.
    pub fn intrinsics(
        &self, arena: &TextArena, spans: &SpanArena,
    ) -> Result<Vec<IntrinsicSite>, IntrinsicDirectiveError> {
        let _root = &arena.terms[&self.root];
        let mut intrinsics = arena
            .terms
            .iter()
            .filter_map(|(term, syntax)| match syntax {
                | Term::Meta(MetaT(meta, payload)) => {
                    IntrinsicSite::decode(*term, meta, *payload, arena, spans)
                }
                | _ => None,
            })
            .collect::<Result<Vec<_>, _>>()?;
        intrinsics.sort_by_key(|site| site.directive.span.get_cursor1());
        Ok(intrinsics)
    }
}

impl ImportSite {
    fn decode(
        term: TermId, meta: &Meta, payload: TermId, arena: &TextArena, spans: &SpanArena,
    ) -> Option<Result<Self, ImportDirectiveError>> {
        meta.is("import").then(|| {
            let span = spans[&EntityId::Term(term)].clone();
            let path = match meta.arguments() {
                | [path] => path.as_string().ok_or_else(|| {
                    ImportDirectiveError::PathNotString { term, span: span.clone() }
                })?,
                | arguments => {
                    return Err(ImportDirectiveError::PathArity {
                        term,
                        span,
                        found: arguments.len(),
                    });
                }
            };
            if path.is_empty() {
                return Err(ImportDirectiveError::EmptyPath { term, span });
            }
            if !matches!(arena.terms[&payload], Term::Hole(Hole)) {
                return Err(ImportDirectiveError::PayloadNotHole { term, span });
            }
            Ok(Self { term, directive: ImportDirective { path: PathBuf::from(path), span } })
        })
    }
}

impl BuiltinSite {
    fn decode(
        term: TermId, meta: &Meta, payload: TermId, spans: &SpanArena,
    ) -> Option<Result<Self, BuiltinDirectiveError>> {
        match BuiltinMeta::decode(meta) {
            | Ok(Some(meta)) => {
                let span = spans[&EntityId::Term(term)].clone();
                Some(Ok(Self {
                    term,
                    payload,
                    directive: BuiltinDirective { role: meta.role, span },
                }))
            }
            | Ok(None) => None,
            | Err(source) => {
                let span = spans[&EntityId::Term(term)].clone();
                Some(Err(BuiltinDirectiveError::Invalid { term, span, source }))
            }
        }
    }
}

impl IntrinsicSite {
    fn decode(
        term: TermId, meta: &Meta, payload: TermId, arena: &TextArena, spans: &SpanArena,
    ) -> Option<Result<Self, IntrinsicDirectiveError>> {
        match IntrinsicMeta::decode(meta) {
            | Ok(Some(meta)) => {
                let span = spans[&EntityId::Term(term)].clone();
                Some(if matches!(arena.terms[&payload], Term::Hole(Hole)) {
                    Ok(Self {
                        term,
                        payload,
                        directive: IntrinsicDirective { role: meta.role, span },
                    })
                } else {
                    Err(IntrinsicDirectiveError::PayloadNotHole { term, span })
                })
            }
            | Ok(None) => None,
            | Err(source) => {
                let span = spans[&EntityId::Term(term)].clone();
                Some(Err(IntrinsicDirectiveError::Invalid { term, span, source }))
            }
        }
    }
}
