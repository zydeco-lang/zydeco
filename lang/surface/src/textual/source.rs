use super::syntax::*;
mod analyzers;
pub use analyzers::*;
mod scan;
use crate::metadata::{
    BuiltinMeta, BuiltinMetaError, DocMeta, IntrinsicMeta, IntrinsicMetaError, LiteralMeta,
    LiteralMetaError, MetadataKind,
};
pub use crate::metadata::{SourceReference, SourceReferenceError};
pub use scan::*;
use std::{
    collections::HashSet,
    num::NonZeroU64,
    ops::Range,
    path::{Path, PathBuf},
};
use thiserror::Error;

/// A decoded `@[doc]` annotation and its optional preceding text block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DocumentationDirective {
    pub meta: DocMeta,
    pub comment: Option<TextBlock>,
    pub span: Span,
}

/// One documented term in a parsed source unit.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DocumentationSite {
    pub term: TermId,
    pub payload: TermId,
    pub directive: DocumentationDirective,
}

/// A `--|` block that is not consumed by an adjacent annotation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnattachedTextWarning {
    pub range: Range<usize>,
}

/// A decoded `@[literal]` annotation and its attached text block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LiteralDirective {
    pub text: TextBlock,
    pub span: Span,
}

/// One literal term splice in a parsed source unit.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LiteralSite {
    pub term: TermId,
    pub payload: TermId,
    pub directive: LiteralDirective,
}

/// The provider named by an `@[import(...)]` term splice.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImportTarget {
    /// A source addressed by a catalog name or quoted path.
    Source(SourceReference),
    /// A numbered source retained by an interactive compiler session.
    Input(SourceNumber),
}

impl std::fmt::Display for ImportTarget {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Source(reference) => reference.fmt(formatter),
            | Self::Input(input) => input.fmt(formatter),
        }
    }
}

/// A nonzero interactive source identity written without quotes in metadata.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct SourceNumber(NonZeroU64);

impl SourceNumber {
    pub fn new(number: u64) -> Option<Self> {
        NonZeroU64::new(number).map(Self)
    }

    pub fn get(self) -> u64 {
        self.0.get()
    }

    /// Produce the opaque overlay key used to retain this input in a compiler session.
    pub fn overlay_path(self, directory: &Path) -> PathBuf {
        directory.join(format!(".zydeco-input-{}", self.get()))
    }
}

impl std::fmt::Display for SourceNumber {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}", self.get())
    }
}

/// A validated source import attached to one term-level splice site.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportDirective {
    pub target: ImportTarget,
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

/// A validated Builtin role annotation attached to a package-signature site.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuiltinDirective {
    pub role: BuiltinRole,
    pub span: Span,
}

/// A Builtin role occurrence in a parsed source unit.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuiltinSite {
    pub location: BuiltinLocation,
    pub directive: BuiltinDirective,
}

/// The surface form carrying a Builtin role annotation.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BuiltinLocation {
    Term { annotation: TermId, payload: TermId },
    ExistentialPattern { pattern: PatId },
}

#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum ImportDirectiveError {
    #[error("import at {span} expects one source argument, but found {found}")]
    TargetArity { term: TermId, span: Span, found: usize },
    #[error("import path at {span} must not be empty")]
    EmptyPath { term: TermId, span: Span },
    #[error("import input number at {span} must be positive")]
    NonPositiveInput { term: TermId, span: Span },
    #[error("import at {span} must annotate a hole expression")]
    PayloadNotHole { term: TermId, span: Span },
    #[error("invalid import source at {span}: {source}")]
    InvalidSource { term: TermId, span: Span, source: SourceReferenceError },
}

#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum BuiltinDirectiveError {
    #[error("invalid builtin annotation at {span}: {source}")]
    Invalid {
        location: BuiltinLocation,
        span: Span,
        #[source]
        source: Box<BuiltinMetaError>,
    },
    #[error("only `builtin(...)` metadata may annotate an existential pattern at {span}")]
    UnsupportedExistentialPattern { pattern: PatId, span: Span },
    #[error("builtin type role `{role}` at {span} must annotate an existential pattern")]
    TypeRoleOnTerm { term: TermId, span: Span, role: BuiltinTypeRole },
    #[error("builtin operation role `{role}` at {span} must annotate a term")]
    ValueRoleOnExistentialPattern { pattern: PatId, span: Span, role: BuiltinValueRole },
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

#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum LiteralDirectiveError {
    #[error("invalid literal annotation at {span}: {source}")]
    Invalid {
        term: TermId,
        span: Span,
        #[source]
        source: LiteralMetaError,
    },
    #[error("literal at {span} must annotate a hole expression")]
    PayloadNotHole { term: TermId, span: Span },
    #[error("literal at {span} requires an attached `--|` text block")]
    MissingText { term: TermId, span: Span },
}

impl ImportDirectiveError {
    pub fn span(&self) -> Span {
        match self {
            | Self::TargetArity { span, .. }
            | Self::EmptyPath { span, .. }
            | Self::NonPositiveInput { span, .. }
            | Self::InvalidSource { span, .. }
            | Self::PayloadNotHole { span, .. } => *span,
        }
    }
}

impl BuiltinDirectiveError {
    pub fn span(&self) -> Span {
        match self {
            | Self::Invalid { span, .. }
            | Self::UnsupportedExistentialPattern { span, .. }
            | Self::TypeRoleOnTerm { span, .. }
            | Self::ValueRoleOnExistentialPattern { span, .. } => *span,
        }
    }
}

impl IntrinsicDirectiveError {
    pub fn span(&self) -> Span {
        match self {
            | Self::Invalid { span, .. } | Self::PayloadNotHole { span, .. } => *span,
        }
    }
}

impl LiteralDirectiveError {
    pub fn span(&self) -> Span {
        match self {
            | Self::Invalid { span, .. }
            | Self::PayloadNotHole { span, .. }
            | Self::MissingText { span, .. } => *span,
        }
    }
}

impl SourceUnit {
    /// Collect reachable documentation through the shared source scan.
    pub fn documentation(&self, arena: &TextArena, spans: &SpanArena) -> Vec<DocumentationSite> {
        SourceScan::run(SourceView { unit: self, arena, spans }, DocumentationAnalyzer::default())
    }

    /// Find text blocks without a semantic attachment, including parser-retained terms.
    pub fn unattached_text(
        &self, arena: &TextArena, spans: &SpanArena,
    ) -> Vec<UnattachedTextWarning> {
        SourceScan::run(SourceView { unit: self, arena, spans }, UnattachedTextAnalyzer::default())
    }

    /// Validate every imports site, collecting independent failures.
    pub fn imports(
        &self, arena: &TextArena, spans: &SpanArena,
    ) -> Result<Vec<ImportSite>, crate::diagnostic::Diagnostics<ImportDirectiveError>> {
        SourceScan::run(SourceView { unit: self, arena, spans }, ImportAnalyzer::default())
            .into_result()
    }

    /// Validate every literals site, collecting independent failures.
    pub fn literals(
        &self, arena: &TextArena, spans: &SpanArena,
    ) -> Result<Vec<LiteralSite>, crate::diagnostic::Diagnostics<LiteralDirectiveError>> {
        SourceScan::run(SourceView { unit: self, arena, spans }, LiteralAnalyzer::default())
            .into_result()
    }

    /// Validate every builtins site, collecting independent failures.
    pub fn builtins(
        &self, arena: &TextArena, spans: &SpanArena,
    ) -> Result<Vec<BuiltinSite>, crate::diagnostic::Diagnostics<BuiltinDirectiveError>> {
        SourceScan::run(SourceView { unit: self, arena, spans }, BuiltinAnalyzer::default())
            .into_result()
    }

    /// Validate every intrinsics site, collecting independent failures.
    pub fn intrinsics(
        &self, arena: &TextArena, spans: &SpanArena,
    ) -> Result<Vec<IntrinsicSite>, crate::diagnostic::Diagnostics<IntrinsicDirectiveError>> {
        SourceScan::run(SourceView { unit: self, arena, spans }, IntrinsicAnalyzer::default())
            .into_result()
    }
}

impl DocumentationSite {
    fn decode(
        term: TermId, payload: TermId, semantic: &Meta, arena: &TextArena, spans: &SpanArena,
    ) -> Option<Self> {
        let meta = semantic
            .specialize::<DocMeta>()
            .expect("documentation metadata specialization is infallible")?;
        let span = spans[&EntityId::Term(term)];
        let comment = arena.trivia.attached_text(term.into()).cloned();
        Some(Self { term, payload, directive: DocumentationDirective { meta, comment, span } })
    }
}

impl LiteralSite {
    fn decode(
        term: TermId, meta: MetaId, payload: TermId, semantic: &Meta, arena: &TextArena,
        spans: &SpanArena,
    ) -> Option<Result<Self, LiteralDirectiveError>> {
        match semantic.specialize::<LiteralMeta>() {
            | Ok(Some(_)) => {
                let span = spans[&EntityId::Term(term)];
                Some(if matches!(arena.terms[&payload], Term::Hole(Hole)) {
                    match arena.trivia.attached_text(term.into()).cloned() {
                        | Some(text) => {
                            Ok(Self { term, payload, directive: LiteralDirective { text, span } })
                        }
                        | None => Err(LiteralDirectiveError::MissingText { term, span }),
                    }
                } else {
                    let span = spans[&EntityId::Term(payload)];
                    Err(LiteralDirectiveError::PayloadNotHole { term, span })
                })
            }
            | Ok(None) => None,
            | Err(source) => {
                let span = spans[&EntityId::Meta(meta)];
                Some(Err(LiteralDirectiveError::Invalid { term, span, source }))
            }
        }
    }
}

impl ImportSite {
    fn decode(
        term: TermId, meta: MetaId, payload: TermId, semantic: &Meta, arena: &TextArena,
        spans: &SpanArena,
    ) -> Option<Result<Self, ImportDirectiveError>> {
        let metadata = &arena.metas[&meta];
        metadata.is(MetadataKind::Import.name()).then(|| {
            let annotation_span = spans[&EntityId::Term(term)];
            let meta_span = spans[&EntityId::Meta(meta)];
            let [argument] = metadata.arguments() else {
                return Err(ImportDirectiveError::TargetArity {
                    term,
                    span: meta_span,
                    found: metadata.arguments().len(),
                });
            };
            let span = spans[&EntityId::Meta(*argument)];
            let target = match &semantic.arguments()[0] {
                | zydeco_syntax::Meta::Integer(number) => {
                    let input = u64::try_from(*number)
                        .ok()
                        .and_then(SourceNumber::new)
                        .ok_or(ImportDirectiveError::NonPositiveInput { term, span })?;
                    ImportTarget::Input(input)
                }
                | zydeco_syntax::Meta::String(path) if path.is_empty() => {
                    return Err(ImportDirectiveError::EmptyPath { term, span });
                }
                | meta => {
                    ImportTarget::Source(SourceReference::decode(meta).map_err(|source| {
                        ImportDirectiveError::InvalidSource { term, span, source }
                    })?)
                }
            };
            if !matches!(arena.terms[&payload], Term::Hole(Hole)) {
                let span = spans[&EntityId::Term(payload)];
                return Err(ImportDirectiveError::PayloadNotHole { term, span });
            }
            Ok(Self { term, directive: ImportDirective { target, span: annotation_span } })
        })
    }
}

impl BuiltinSite {
    fn decode_term(
        term: TermId, meta: MetaId, payload: TermId, semantic: &Meta, spans: &SpanArena,
    ) -> Option<Result<Self, BuiltinDirectiveError>> {
        let location = BuiltinLocation::Term { annotation: term, payload };
        match semantic.specialize::<BuiltinMeta>() {
            | Ok(Some(BuiltinMeta { role: BuiltinRole::Value(role) })) => {
                let span = spans[&EntityId::Term(term)];
                Some(Ok(Self {
                    location,
                    directive: BuiltinDirective { role: BuiltinRole::Value(role), span },
                }))
            }
            | Ok(Some(BuiltinMeta { role: BuiltinRole::Type(role) })) => {
                let span = spans[&EntityId::Term(term)];
                Some(Err(BuiltinDirectiveError::TypeRoleOnTerm { term, span, role }))
            }
            | Ok(None) => None,
            | Err(source) => {
                let span = spans[&EntityId::Meta(meta)];
                Some(Err(BuiltinDirectiveError::Invalid {
                    location,
                    span,
                    source: Box::new(source),
                }))
            }
        }
    }

    fn decode_existential_pattern(
        pattern: PatId, annotation: &Sp<MetaId>, semantic: &Meta, spans: &SpanArena,
    ) -> Result<Self, BuiltinDirectiveError> {
        let location = BuiltinLocation::ExistentialPattern { pattern };
        match semantic.specialize::<BuiltinMeta>() {
            | Ok(Some(BuiltinMeta { role: BuiltinRole::Type(role) })) => Ok(Self {
                location,
                directive: BuiltinDirective {
                    role: BuiltinRole::Type(role),
                    span: annotation.info,
                },
            }),
            | Ok(Some(BuiltinMeta { role: BuiltinRole::Value(role) })) => {
                Err(BuiltinDirectiveError::ValueRoleOnExistentialPattern {
                    pattern,
                    span: annotation.info,
                    role,
                })
            }
            | Ok(None) => Err(BuiltinDirectiveError::UnsupportedExistentialPattern {
                pattern,
                span: annotation.info,
            }),
            | Err(source) => Err(BuiltinDirectiveError::Invalid {
                location,
                span: spans[&EntityId::Meta(annotation.inner)],
                source: Box::new(source),
            }),
        }
    }
}

impl IntrinsicSite {
    fn decode(
        term: TermId, meta: MetaId, payload: TermId, semantic: &Meta, arena: &TextArena,
        spans: &SpanArena,
    ) -> Option<Result<Self, IntrinsicDirectiveError>> {
        match semantic.specialize::<IntrinsicMeta>() {
            | Ok(Some(meta)) => {
                let span = spans[&EntityId::Term(term)];
                Some(if matches!(arena.terms[&payload], Term::Hole(Hole)) {
                    Ok(Self {
                        term,
                        payload,
                        directive: IntrinsicDirective { role: meta.role, span },
                    })
                } else {
                    let span = spans[&EntityId::Term(payload)];
                    Err(IntrinsicDirectiveError::PayloadNotHole { term, span })
                })
            }
            | Ok(None) => None,
            | Err(source) => {
                let span = spans[&EntityId::Meta(meta)];
                Some(Err(IntrinsicDirectiveError::Invalid { term, span, source }))
            }
        }
    }
}

#[cfg(test)]
mod tests;
