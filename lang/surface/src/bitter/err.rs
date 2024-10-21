use super::syntax as b;
use thiserror::Error;
use zydeco_utils::span::Sp;

#[derive(Error, Debug, Clone)]
pub enum DesugarError {
    #[error("The declaration has both `!` and `fix` modifiers")]
    CompWhileFix(Sp<b::PatId>),
}

pub type Result<T> = std::result::Result<T, DesugarError>;
