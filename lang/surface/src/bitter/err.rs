use super::syntax as b;
use crate::textual::syntax as t;
use thiserror::Error;
use zydeco_syntax::{BuiltinMetaError, IntrinsicMetaError};
use zydeco_utils::span::Sp;

#[derive(Error, Debug, Clone)]
pub enum DesugarError {
    #[error("Invalid builtin annotation: {source}")]
    InvalidBuiltinMeta {
        term: Sp<t::TermId>,
        #[source]
        source: BuiltinMetaError,
    },
    #[error("Invalid intrinsic annotation: {source}")]
    InvalidIntrinsicMeta {
        term: Sp<t::TermId>,
        #[source]
        source: IntrinsicMetaError,
    },
    #[error("Intrinsic annotation must annotate a hole expression")]
    IntrinsicPayloadNotHole(Sp<t::TermId>),
    #[error("A quantified type parameter must be a pattern")]
    QuantifierParameterNotPattern(Sp<t::CoPatId>),
    #[error("The binding has both `!` and `fix` modifiers")]
    CompWhileFix(Sp<b::PatId>),
}

pub type Result<T> = std::result::Result<T, DesugarError>;
