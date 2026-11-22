use crate::metadata::{
    BuiltinMetaError, CbvMetaError, FfiMetaError, IntrinsicMetaError, MetadataValidationError,
    MonadicMetaError,
};
use crate::textual::syntax as t;
use ariadne::{Label, Report, ReportKind};
use std::ops::Range;
use thiserror::Error;
use zydeco_foundation::span::{BytePos, PathDisplay, Sp, Span, internal_ariadne_span};
use zydeco_syntax::{BuiltinTypeRole, BuiltinValueRole, VarName};

/// Errors reported during surface lowering and name resolution.
#[derive(Error, Debug, Clone)]
pub enum ResolveError {
    #[error("Invalid builtin annotation: {source}")]
    InvalidBuiltinMeta {
        term: Sp<t::TermId>,
        #[source]
        source: BuiltinMetaError,
    },
    #[error("Invalid builtin annotation on an existential pattern: {source}")]
    InvalidBuiltinPatternMeta {
        pattern: Sp<t::PatId>,
        #[source]
        source: BuiltinMetaError,
    },
    #[error("Builtin type role `{role}` must annotate an existential pattern")]
    BuiltinTypeRoleOnTerm { term: Sp<t::TermId>, role: BuiltinTypeRole },
    #[error("Builtin operation role `{role}` must annotate a term")]
    BuiltinValueRoleOnExistentialPattern { pattern: Sp<t::PatId>, role: BuiltinValueRole },
    #[error("Only `builtin(...)` metadata may annotate an existential pattern")]
    UnsupportedExistentialPatternMeta(Sp<t::PatId>),
    #[error("Invalid intrinsic annotation: {source}")]
    InvalidIntrinsicMeta {
        term: Sp<t::TermId>,
        #[source]
        source: IntrinsicMetaError,
    },
    #[error("Invalid prelude annotation: {source}")]
    InvalidPreludeMeta {
        term: Sp<t::TermId>,
        #[source]
        source: MetadataValidationError,
    },
    #[error("Invalid monadic annotation: {source}")]
    InvalidMonadicMeta {
        term: Sp<t::TermId>,
        #[source]
        source: MonadicMetaError,
    },
    #[error("Invalid cbv annotation: {source}")]
    InvalidCbvMeta {
        term: Sp<t::TermId>,
        #[source]
        source: CbvMetaError,
    },
    #[error("`@[cbn]` is reserved for call-by-name blocks, which are not implemented yet")]
    CbnNotImplemented(Sp<t::TermId>),
    #[error("Invalid typeof annotation: {source}")]
    InvalidTypeOfMeta {
        term: Sp<t::TermId>,
        #[source]
        source: MetadataValidationError,
    },
    #[error("Invalid partial annotation: {source}")]
    InvalidPartialMeta {
        term: Sp<t::TermId>,
        #[source]
        source: MetadataValidationError,
    },
    #[error("A partial annotation must annotate a binding, parameter, or function")]
    PartialPayloadNotBinding(Sp<t::TermId>),
    #[error("Intrinsic annotation must annotate a hole expression")]
    IntrinsicPayloadNotHole(Sp<t::TermId>),
    #[error("Invalid ffi annotation: {source}")]
    InvalidFfiMeta {
        term: Sp<t::TermId>,
        #[source]
        source: FfiMetaError,
    },
    #[error("An ffi annotation must provide the implementation of a hole expression")]
    FfiPayloadNotHole(Sp<t::TermId>),
    #[error("A quantified type parameter must be a pattern")]
    QuantifierParameterNotPattern(Sp<t::CoPatId>),
    #[error("A value-function parameter must be a pattern")]
    ValueParameterNotPattern(Sp<t::CoPatId>),
    #[error("A manifest `as` pattern is only valid as an existential parameter")]
    ManifestPatternOutsideExistential(Sp<t::PatId>),
    #[error("A `pack` parameter needs evidence: `(X : K) is W` or `(X as W : K)`")]
    PackParameterNeedsEvidence(Sp<t::PatId>),
    #[error("A manifest `pack` parameter carries its evidence in `as`")]
    PackParameterRedundantEvidence(Sp<t::PatId>),
    #[error("Unbound variable: {0}")]
    UnboundVar(Sp<VarName>),
    #[error("Duplicate definition: {0} and {1}")]
    DuplicateDefinition(Sp<VarName>, Sp<VarName>),
    #[error("`that` requires an enclosing `begin` block")]
    UnenclosedThat(Span),
    #[error("A recursive context component cannot contain a parameter")]
    RecursiveParameter(Span),
}

impl ResolveError {
    /// Primary source span of this lowering or resolution failure.
    pub fn primary_span(&self) -> Span {
        match self {
            | Self::InvalidBuiltinMeta { term, .. }
            | Self::BuiltinTypeRoleOnTerm { term, .. }
            | Self::InvalidIntrinsicMeta { term, .. }
            | Self::InvalidPreludeMeta { term, .. }
            | Self::InvalidMonadicMeta { term, .. }
            | Self::InvalidCbvMeta { term, .. }
            | Self::CbnNotImplemented(term)
            | Self::InvalidTypeOfMeta { term, .. }
            | Self::InvalidPartialMeta { term, .. }
            | Self::PartialPayloadNotBinding(term)
            | Self::InvalidFfiMeta { term, .. }
            | Self::IntrinsicPayloadNotHole(term)
            | Self::FfiPayloadNotHole(term) => term.info,
            | Self::InvalidBuiltinPatternMeta { pattern, .. }
            | Self::BuiltinValueRoleOnExistentialPattern { pattern, .. }
            | Self::UnsupportedExistentialPatternMeta(pattern)
            | Self::ManifestPatternOutsideExistential(pattern)
            | Self::PackParameterNeedsEvidence(pattern)
            | Self::PackParameterRedundantEvidence(pattern) => pattern.info,
            | Self::QuantifierParameterNotPattern(copattern)
            | Self::ValueParameterNotPattern(copattern) => copattern.info,
            | Self::UnboundVar(variable) => variable.info,
            | Self::DuplicateDefinition(_, duplicate) => duplicate.info,
            | Self::UnenclosedThat(span) | Self::RecursiveParameter(span) => *span,
        }
    }

    /// Shift every embedded source span by `base` when assembling a merged program map.
    pub fn rebase(self, base: BytePos) -> Self {
        let map_term = |mut term: Sp<t::TermId>| {
            term.info = term.info.rebase(base);
            term
        };
        let map_pat = |mut pattern: Sp<t::PatId>| {
            pattern.info = pattern.info.rebase(base);
            pattern
        };
        let map_copat = |mut copattern: Sp<t::CoPatId>| {
            copattern.info = copattern.info.rebase(base);
            copattern
        };
        let map_var = |mut var: Sp<VarName>| {
            var.info = var.info.rebase(base);
            var
        };
        match self {
            | Self::InvalidBuiltinMeta { term, source } => {
                Self::InvalidBuiltinMeta { term: map_term(term), source }
            }
            | Self::InvalidBuiltinPatternMeta { pattern, source } => {
                Self::InvalidBuiltinPatternMeta { pattern: map_pat(pattern), source }
            }
            | Self::BuiltinTypeRoleOnTerm { term, role } => {
                Self::BuiltinTypeRoleOnTerm { term: map_term(term), role }
            }
            | Self::BuiltinValueRoleOnExistentialPattern { pattern, role } => {
                Self::BuiltinValueRoleOnExistentialPattern { pattern: map_pat(pattern), role }
            }
            | Self::UnsupportedExistentialPatternMeta(pattern) => {
                Self::UnsupportedExistentialPatternMeta(map_pat(pattern))
            }
            | Self::InvalidIntrinsicMeta { term, source } => {
                Self::InvalidIntrinsicMeta { term: map_term(term), source }
            }
            | Self::InvalidPreludeMeta { term, source } => {
                Self::InvalidPreludeMeta { term: map_term(term), source }
            }
            | Self::InvalidMonadicMeta { term, source } => {
                Self::InvalidMonadicMeta { term: map_term(term), source }
            }
            | Self::InvalidCbvMeta { term, source } => {
                Self::InvalidCbvMeta { term: map_term(term), source }
            }
            | Self::CbnNotImplemented(term) => Self::CbnNotImplemented(map_term(term)),
            | Self::InvalidTypeOfMeta { term, source } => {
                Self::InvalidTypeOfMeta { term: map_term(term), source }
            }
            | Self::InvalidPartialMeta { term, source } => {
                Self::InvalidPartialMeta { term: map_term(term), source }
            }
            | Self::PartialPayloadNotBinding(term) => {
                Self::PartialPayloadNotBinding(map_term(term))
            }
            | Self::IntrinsicPayloadNotHole(term) => Self::IntrinsicPayloadNotHole(map_term(term)),
            | Self::InvalidFfiMeta { term, source } => {
                Self::InvalidFfiMeta { term: map_term(term), source }
            }
            | Self::FfiPayloadNotHole(term) => Self::FfiPayloadNotHole(map_term(term)),
            | Self::QuantifierParameterNotPattern(copattern) => {
                Self::QuantifierParameterNotPattern(map_copat(copattern))
            }
            | Self::ValueParameterNotPattern(copattern) => {
                Self::ValueParameterNotPattern(map_copat(copattern))
            }
            | Self::ManifestPatternOutsideExistential(pattern) => {
                Self::ManifestPatternOutsideExistential(map_pat(pattern))
            }
            | Self::PackParameterNeedsEvidence(pattern) => {
                Self::PackParameterNeedsEvidence(map_pat(pattern))
            }
            | Self::PackParameterRedundantEvidence(pattern) => {
                Self::PackParameterRedundantEvidence(map_pat(pattern))
            }
            | Self::UnboundVar(var) => Self::UnboundVar(map_var(var)),
            | Self::DuplicateDefinition(first, second) => {
                Self::DuplicateDefinition(map_var(first), map_var(second))
            }
            | Self::UnenclosedThat(span) => Self::UnenclosedThat(span.rebase(base)),
            | Self::RecursiveParameter(span) => Self::RecursiveParameter(span.rebase(base)),
        }
    }

    /// Create an Ariadne report for this resolve error.
    ///
    /// `spans` is the merged program's span arena; its attached source map
    /// resolves each span into a file and byte range.
    pub fn to_report(&self, spans: &t::SpanArena) -> Report<'static, (PathDisplay, Range<usize>)> {
        let resolve = |span: &Span| {
            spans
                .source_map()
                .and_then(|map| map.ariadne_range(*span))
                .unwrap_or_else(internal_ariadne_span)
        };
        match self {
            | Self::UnboundVar(var) => {
                let (file_path, range) = resolve(&var.info);
                Report::build(ReportKind::Error, (file_path.clone(), range.clone()))
                    .with_message("Unbound variable")
                    .with_label(
                        Label::new((file_path, range))
                            .with_message(format!("variable `{}` is not defined", var.inner)),
                    )
                    .finish()
            }
            | Self::DuplicateDefinition(var1, var2) => {
                let (file_path1, range1) = resolve(&var1.info);
                let (file_path2, range2) = resolve(&var2.info);
                let primary_span = (file_path1.clone(), range1.clone());
                let mut report = Report::build(ReportKind::Error, primary_span)
                    .with_message("Duplicate definition")
                    .with_label(
                        Label::new((file_path1.clone(), range1))
                            .with_message(format!("first definition of `{}`", var1.inner)),
                    );
                let message = if file_path1 == file_path2 {
                    format!("second definition of `{}`", var2.inner)
                } else {
                    format!("duplicate definition of `{}`", var2.inner)
                };
                report = report.with_label(Label::new((file_path2, range2)).with_message(message));
                report.finish()
            }
            | Self::UnenclosedThat(span) => {
                let (file_path, range) = resolve(span);
                Report::build(ReportKind::Error, (file_path.clone(), range.clone()))
                    .with_message("Mobile binding without a block")
                    .with_label(
                        Label::new((file_path, range))
                            .with_message("`that` contributes to the nearest `begin` block"),
                    )
                    .finish()
            }
            | Self::RecursiveParameter(span) => {
                let (file_path, range) = resolve(span);
                Report::build(ReportKind::Error, (file_path.clone(), range.clone()))
                    .with_message("Recursive parameter component")
                    .with_label(
                        Label::new((file_path, range))
                            .with_message("parameters must remain acyclic"),
                    )
                    .finish()
            }
            | other => {
                let site = resolve(&other.primary_span());
                Report::build(ReportKind::Error, site.clone())
                    .with_message(other.to_string())
                    .with_label(Label::new(site).with_message(other.to_string()))
                    .finish()
            }
        }
    }
}

pub type ResolveErrors = crate::diagnostic::Diagnostics<ResolveError>;

/// Strict name resolution rejects publication with every recorded error.
pub type Result<T> = std::result::Result<T, Box<ResolveErrors>>;
