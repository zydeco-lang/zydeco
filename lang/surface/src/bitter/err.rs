use super::syntax as b;
use crate::textual::syntax as t;
use thiserror::Error;
use zydeco_utils::span::Sp;

#[derive(Error, Debug, Clone)]
pub enum DesugarError {
    #[error("The declaration has both `!` and `fix` modifiers")]
    CompWhileFix(Sp<b::PatId>),
    #[error("Declaration without a body must be external")]
    EmptyDeclNotExternal(Sp<t::DeclId>),
    #[error("Extern declarations are not allowed to have `!` modifiers")]
    ExternCompNotAllowed(Sp<t::DeclId>),
}

pub type Result<T> = std::result::Result<T, DesugarError>;
