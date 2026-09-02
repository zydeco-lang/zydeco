use super::syntax::*;
use crate::metadata::{
    BuiltinMeta, BuiltinMetaError, DocMeta, IntrinsicMeta, IntrinsicMetaError, LiteralMeta,
    LiteralMetaError, MetadataKind, MetadataValidationError,
};
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
    /// A disk or overlay source addressed by a quoted path.
    Path(PathBuf),
    /// A numbered source retained by an interactive compiler session.
    Input(SourceNumber),
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
    #[error("import source at {span} must be a path string or positive input number")]
    UnsupportedTarget { term: TermId, span: Span },
    #[error("import path at {span} must not be empty")]
    EmptyPath { term: TermId, span: Span },
    #[error("import input number at {span} must be positive")]
    NonPositiveInput { term: TermId, span: Span },
    #[error("import at {span} must annotate a hole expression")]
    PayloadNotHole { term: TermId, span: Span },
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
            | Self::UnsupportedTarget { span, .. }
            | Self::EmptyPath { span, .. }
            | Self::NonPositiveInput { span, .. }
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
    /// Collect every explicitly documented term in this source unit.
    ///
    /// Text blocks remain parser trivia. The `@[doc]` annotation
    /// provides the durable attachment point, and only an uninterrupted block
    /// of `--|` lines immediately above that annotation becomes its text.
    pub fn documentation(&self, arena: &TextArena, spans: &SpanArena) -> Vec<DocumentationSite> {
        let _root = &arena.terms[&self.root];
        let mut sites = arena
            .terms
            .iter()
            .filter_map(|(term, syntax)| match syntax {
                | Term::Meta(MetaTerm(meta, payload)) => {
                    DocumentationSite::decode(*term, *meta, *payload, arena, spans)
                }
                | _ => None,
            })
            .collect::<Vec<_>>();
        sites.sort_by_key(|site| site.directive.span.lo());
        sites
    }

    /// Whether this metadata annotation consumes an attached `--|` text block.
    fn consumes_attached_text(meta: MetaId, arena: &TextArena) -> bool {
        let meta = arena.semantic_meta(meta);
        meta.specialize::<DocMeta>().is_ok_and(|option| option.is_some())
            || meta.specialize::<LiteralMeta>().is_ok_and(|option| option.is_some())
    }

    /// Find text blocks that have no semantic attachment.
    pub fn unattached_text(&self, arena: &TextArena) -> Vec<UnattachedTextWarning> {
        let _root = &arena.terms[&self.root];
        let attached = arena
            .terms
            .iter()
            .filter_map(|(term, syntax)| match syntax {
                | Term::Meta(MetaTerm(meta, _)) if Self::consumes_attached_text(*meta, arena) => {
                    arena.trivia.attached_text((*term).into()).map(|text| text.range.clone())
                }
                | _ => None,
            })
            .collect::<HashSet<_>>();
        let mut warnings = arena
            .trivia
            .text_blocks()
            .filter(|text| !attached.contains(&text.range))
            .map(|text| UnattachedTextWarning { range: text.range.clone() })
            .collect::<Vec<_>>();
        warnings.sort_by_key(|warning| (warning.range.start, warning.range.end));
        warnings
    }

    /// Decode and validate all `@[literal]` term splices in this source unit.
    ///
    /// A literal splice requires a hole payload and an attached `--|` text
    /// block, which becomes the string value of the hole.
    pub fn literals(
        &self, arena: &TextArena, spans: &SpanArena,
    ) -> Result<Vec<LiteralSite>, LiteralDirectiveError> {
        let _root = &arena.terms[&self.root];
        let mut literals = arena
            .terms
            .iter()
            .filter_map(|(term, syntax)| match syntax {
                | Term::Meta(MetaTerm(meta, payload)) => {
                    LiteralSite::decode(*term, *meta, *payload, arena, spans)
                }
                | _ => None,
            })
            .collect::<Result<Vec<_>, _>>()?;
        literals.sort_by_key(|site| site.directive.span.lo());
        Ok(literals)
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
                | Term::Meta(MetaTerm(meta, payload)) => {
                    ImportSite::decode(*term, *meta, *payload, arena, spans)
                }
                | _ => None,
            })
            .collect::<Result<Vec<_>, _>>()?;
        imports.sort_by_key(|site| site.directive.span.lo());
        Ok(imports)
    }

    /// Decode and validate all Builtin role annotations in this source unit.
    pub fn builtins(
        &self, arena: &TextArena, spans: &SpanArena,
    ) -> Result<Vec<BuiltinSite>, BuiltinDirectiveError> {
        let _root = &arena.terms[&self.root];
        let term_sites = arena
            .terms
            .iter()
            .filter_map(|(term, syntax)| match syntax {
                | Term::Meta(MetaTerm(meta, payload)) => {
                    BuiltinSite::decode_term(*term, *meta, *payload, arena, spans)
                }
                | _ => None,
            })
            .collect::<Result<Vec<_>, _>>()?;
        let parameter_sites = arena
            .terms
            .iter()
            .flat_map(|(_, syntax)| match syntax {
                | Term::Exists(Exists { parameters, .. }) => parameters
                    .iter()
                    .flat_map(|parameter| {
                        parameter.annotations.iter().map(|annotation| {
                            BuiltinSite::decode_existential_pattern(
                                parameter.binder(),
                                annotation,
                                arena,
                                spans,
                            )
                        })
                    })
                    .collect::<Vec<_>>(),
                | _ => Vec::new(),
            })
            .collect::<Result<Vec<_>, _>>()?;
        let mut builtins = term_sites.into_iter().chain(parameter_sites).collect::<Vec<_>>();
        builtins.sort_by_key(|site| site.directive.span.lo());
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
                | Term::Meta(MetaTerm(meta, payload)) => {
                    IntrinsicSite::decode(*term, *meta, *payload, arena, spans)
                }
                | _ => None,
            })
            .collect::<Result<Vec<_>, _>>()?;
        intrinsics.sort_by_key(|site| site.directive.span.lo());
        Ok(intrinsics)
    }
}

impl DocumentationSite {
    fn decode(
        term: TermId, meta: MetaId, payload: TermId, arena: &TextArena, spans: &SpanArena,
    ) -> Option<Self> {
        let meta = arena.semantic_meta(meta);
        let meta = meta
            .specialize::<DocMeta>()
            .expect("documentation metadata specialization is infallible")?;
        let span = spans[&EntityId::Term(term)];
        let comment = arena.trivia.attached_text(term.into()).cloned();
        Some(Self { term, payload, directive: DocumentationDirective { meta, comment, span } })
    }
}

impl LiteralSite {
    fn decode(
        term: TermId, meta: MetaId, payload: TermId, arena: &TextArena, spans: &SpanArena,
    ) -> Option<Result<Self, LiteralDirectiveError>> {
        let semantic = arena.semantic_meta(meta);
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
        term: TermId, meta: MetaId, payload: TermId, arena: &TextArena, spans: &SpanArena,
    ) -> Option<Result<Self, ImportDirectiveError>> {
        let metadata = &arena.metas[&meta];
        metadata.is(MetadataKind::Import.name()).then(|| {
            let annotation_span = spans[&EntityId::Term(term)];
            let meta_span = spans[&EntityId::Meta(meta)];
            let semantic = arena.semantic_meta(meta);
            if let Err(error) =
                MetadataKind::Import.definition().validate_arguments(semantic.arguments())
            {
                return Err(match error {
                    | MetadataValidationError::Arity { found, .. } => {
                        ImportDirectiveError::TargetArity { term, span: meta_span, found }
                    }
                    | MetadataValidationError::ExpectedSource { .. } => {
                        let [argument] = metadata.arguments() else {
                            unreachable!("source validation reports arity separately")
                        };
                        let span = spans[&EntityId::Meta(*argument)];
                        match &arena.metas[argument] {
                            | MetaNode::String(path) if path.is_empty() => {
                                ImportDirectiveError::EmptyPath { term, span }
                            }
                            | MetaNode::Integer(_) => {
                                ImportDirectiveError::NonPositiveInput { term, span }
                            }
                            | MetaNode::Ident(_) | MetaNode::String(_) | MetaNode::Apply { .. } => {
                                ImportDirectiveError::UnsupportedTarget { term, span }
                            }
                        }
                    }
                    | _ => unreachable!("import has one source argument"),
                });
            }
            let [argument] = metadata.arguments() else {
                unreachable!("the import metadata contract validates one source")
            };
            let target = match &arena.metas[argument] {
                | MetaNode::String(path) => ImportTarget::Path(PathBuf::from(path)),
                | MetaNode::Integer(number) => ImportTarget::Input(
                    SourceNumber::new(
                        u64::try_from(*number)
                            .expect("the import metadata contract validates a positive input"),
                    )
                    .expect("the import metadata contract validates a nonzero input"),
                ),
                | MetaNode::Ident(_) | MetaNode::Apply { .. } => {
                    unreachable!("the import metadata contract validates a source")
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
        term: TermId, meta: MetaId, payload: TermId, arena: &TextArena, spans: &SpanArena,
    ) -> Option<Result<Self, BuiltinDirectiveError>> {
        let location = BuiltinLocation::Term { annotation: term, payload };
        let semantic = arena.semantic_meta(meta);
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
        pattern: PatId, annotation: &Sp<MetaId>, arena: &TextArena, spans: &SpanArena,
    ) -> Result<Self, BuiltinDirectiveError> {
        let location = BuiltinLocation::ExistentialPattern { pattern };
        let semantic = arena.semantic_meta(annotation.inner);
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
        term: TermId, meta: MetaId, payload: TermId, arena: &TextArena, spans: &SpanArena,
    ) -> Option<Result<Self, IntrinsicDirectiveError>> {
        let semantic = arena.semantic_meta(meta);
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
