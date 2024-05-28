use crate::syntax::*;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum TyckError {}

pub type Result<T> = std::result::Result<T, TyckError>;
