use super::lexer::{LexicalToken, LexicalTokenKind, LexicalTokens};
use super::syntax::*;
use crate::metadata::{BuiltinMeta, BuiltinMetaError, DocMeta, IntrinsicMeta, IntrinsicMetaError};
use std::{ops::Range, path::PathBuf};
use thiserror::Error;

/// Markdown recovered from a contiguous `--|` block immediately above a
/// documentation annotation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DocumentationComment {
    pub markdown: String,
    pub range: Range<usize>,
}

/// A decoded `@[doc]` annotation and its optional preceding prose.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DocumentationDirective {
    pub meta: DocMeta,
    pub comment: Option<DocumentationComment>,
    pub span: Span,
}

/// One documented term in a parsed source unit.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DocumentationSite {
    pub term: TermId,
    pub payload: TermId,
    pub directive: DocumentationDirective,
}

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
    /// Collect every explicitly documented term in this source unit.
    ///
    /// Documentation comments remain parser trivia. The `@[doc]` annotation
    /// provides the durable attachment point, and only an uninterrupted block
    /// of `--|` lines immediately above that annotation becomes its prose.
    pub fn documentation(
        &self, source: &str, arena: &TextArena, spans: &SpanArena,
    ) -> Vec<DocumentationSite> {
        let _root = &arena.terms[&self.root];
        let comments = DocumentationComments::new(source);
        let mut sites = arena
            .terms
            .iter()
            .filter_map(|(term, syntax)| match syntax {
                | Term::Meta(MetaT(meta, payload)) => {
                    DocumentationSite::decode(*term, meta, *payload, spans, &comments)
                }
                | _ => None,
            })
            .collect::<Vec<_>>();
        sites.sort_by_key(|site| site.directive.span.get_cursor1());
        sites
    }

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

impl DocumentationSite {
    fn decode(
        term: TermId, meta: &Meta, payload: TermId, spans: &SpanArena,
        comments: &DocumentationComments<'_>,
    ) -> Option<Self> {
        let meta = meta
            .specialize::<DocMeta>()
            .expect("documentation metadata specialization is infallible")?;
        let span = spans[&EntityId::Term(term)].clone();
        let (start, _) = span.get_cursor1();
        let comment = comments.preceding(start);
        Some(Self { term, payload, directive: DocumentationDirective { meta, comment, span } })
    }
}

struct DocumentationComments<'source> {
    source: &'source str,
    comments: Vec<LexicalToken>,
}

impl<'source> DocumentationComments<'source> {
    fn new(source: &'source str) -> Self {
        let comments = LexicalTokens::new(source)
            .filter(|token| token.kind == LexicalTokenKind::DocumentationComment)
            .collect();
        Self { source, comments }
    }

    fn preceding(&self, annotation_start: usize) -> Option<DocumentationComment> {
        let preceding =
            self.comments.partition_point(|comment| comment.range.end <= annotation_start);
        let comments = self.comments[..preceding]
            .iter()
            .rev()
            .scan(annotation_start, |cursor, comment| {
                self.is_immediately_before(comment, *cursor).then(|| {
                    *cursor = comment.range.start;
                    comment
                })
            })
            .collect::<Vec<_>>();
        let first = comments.last()?;
        let last = comments.first()?;
        let markdown = comments
            .iter()
            .rev()
            .map(|comment| self.markdown_line(comment))
            .collect::<Vec<_>>()
            .join("\n");
        Some(DocumentationComment { markdown, range: first.range.start..last.range.end })
    }

    fn is_immediately_before(&self, comment: &LexicalToken, cursor: usize) -> bool {
        self.source
            .get(comment.range.end..cursor)
            .is_some_and(|gap| gap.chars().all(|ch| matches!(ch, ' ' | '\t' | '\u{000c}')))
    }

    fn markdown_line(&self, comment: &LexicalToken) -> &str {
        let line = &self.source[comment.range.clone()];
        let line = line.strip_suffix('\n').unwrap_or(line);
        let line = line.strip_suffix('\r').unwrap_or(line);
        let line = line.trim_start_matches([' ', '\t', '\u{000c}']);
        let line = line
            .strip_prefix("--|")
            .expect("documentation tokens always begin with the documentation marker");
        line.strip_prefix(' ').unwrap_or(line)
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
        match meta.specialize::<BuiltinMeta>() {
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
        match meta.specialize::<IntrinsicMeta>() {
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
