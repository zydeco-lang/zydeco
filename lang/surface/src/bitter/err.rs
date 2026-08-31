use super::syntax as b;
use crate::metadata::{BuiltinMetaError, IntrinsicMetaError, MonadicMetaError};
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
    #[error("Intrinsic annotation must annotate a hole expression")]
    IntrinsicPayloadNotHole(Sp<t::TermId>),
    #[error("A quantified type parameter must be a pattern")]
    QuantifierParameterNotPattern(Sp<t::CoPatId>),
    #[error("A manifest `as` pattern is only valid as an existential parameter")]
    ManifestPatternOutsideExistential(Sp<t::PatId>),
    #[error("A `pack` parameter needs evidence: `(X : K) is W` or `(X as W : K)`")]
    PackParameterNeedsEvidence(Sp<b::PatId>),
    #[error("A manifest `pack` parameter carries its evidence in `as`")]
    PackParameterRedundantEvidence(Sp<b::PatId>),
    #[error("The binding has both `!` and `fix` modifiers")]
    CompWhileFix(Sp<b::PatId>),
}

impl DesugarError {
    /// Source span of the construct rejected during desugaring.
    pub fn span(&self) -> Span {
        match self {
            | Self::InvalidBuiltinMeta { term, .. }
            | Self::BuiltinTypeRoleOnTerm { term, .. }
            | Self::InvalidIntrinsicMeta { term, .. }
            | Self::InvalidMonadicMeta { term, .. }
            | Self::IntrinsicPayloadNotHole(term) => term.info,
            | Self::InvalidBuiltinPatternMeta { pattern, .. }
            | Self::BuiltinValueRoleOnExistentialPattern { pattern, .. }
            | Self::UnsupportedExistentialPatternMeta(pattern)
            | Self::ManifestPatternOutsideExistential(pattern) => pattern.info,
            | Self::QuantifierParameterNotPattern(copattern) => copattern.info,
            | Self::PackParameterNeedsEvidence(pattern)
            | Self::PackParameterRedundantEvidence(pattern)
            | Self::CompWhileFix(pattern) => pattern.info,
        }
    }
}

pub type Result<T> = std::result::Result<T, DesugarError>;
