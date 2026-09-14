use super::syntax as b;
use crate::metadata::{
    BuiltinMetaError, FfiMetaError, IntrinsicMetaError, MetadataValidationError, MonadicMetaError,
};
use crate::textual::syntax as t;
use thiserror::Error;
use zydeco_syntax::{BuiltinTypeRole, BuiltinValueRole};
use zydeco_utils::span::{Sp, Span};

#[derive(Error, Debug, Clone)]
pub enum DesugarError {
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
    #[error("Invalid monadic annotation: {source}")]
    InvalidMonadicMeta {
        term: Sp<t::TermId>,
        #[source]
        source: MonadicMetaError,
    },
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
    PackParameterNeedsEvidence(Sp<b::PatId>),
    #[error("A manifest `pack` parameter carries its evidence in `as`")]
    PackParameterRedundantEvidence(Sp<b::PatId>),
}

impl DesugarError {
    pub fn to_report(
        &self, spans: &t::SpanArena,
    ) -> ariadne::Report<'static, (zydeco_utils::span::PathDisplay, std::ops::Range<usize>)> {
        let span = self.span();
        let site = spans
            .source_map()
            .and_then(|map| map.ariadne_range(span))
            .unwrap_or_else(zydeco_utils::span::internal_ariadne_span);
        ariadne::Report::build(ariadne::ReportKind::Error, site.clone())
            .with_message(self.to_string())
            .with_label(ariadne::Label::new(site).with_message(self.to_string()))
            .finish()
    }

    /// Source span of the construct rejected during desugaring.
    pub fn span(&self) -> Span {
        match self {
            | Self::InvalidBuiltinMeta { term, .. }
            | Self::BuiltinTypeRoleOnTerm { term, .. }
            | Self::InvalidIntrinsicMeta { term, .. }
            | Self::InvalidMonadicMeta { term, .. }
            | Self::InvalidTypeOfMeta { term, .. }
            | Self::InvalidPartialMeta { term, .. }
            | Self::PartialPayloadNotBinding(term)
            | Self::InvalidFfiMeta { term, .. }
            | Self::IntrinsicPayloadNotHole(term)
            | Self::FfiPayloadNotHole(term) => term.info,
            | Self::InvalidBuiltinPatternMeta { pattern, .. }
            | Self::BuiltinValueRoleOnExistentialPattern { pattern, .. }
            | Self::UnsupportedExistentialPatternMeta(pattern)
            | Self::ManifestPatternOutsideExistential(pattern) => pattern.info,
            | Self::QuantifierParameterNotPattern(copattern)
            | Self::ValueParameterNotPattern(copattern) => copattern.info,
            | Self::PackParameterNeedsEvidence(pattern)
            | Self::PackParameterRedundantEvidence(pattern) => pattern.info,
        }
    }
}

pub type DesugarErrors = crate::diagnostic::Diagnostics<DesugarError>;
