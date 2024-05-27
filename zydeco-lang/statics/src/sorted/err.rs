use crate::syntax::*;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum SortError {
}

pub type Result<T> = std::result::Result<T, SortError>;
