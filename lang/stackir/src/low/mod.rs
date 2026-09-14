//! First-order stack-passing IR with explicit closure and continuation packages.

pub mod arena;
pub mod check;
pub mod convert;
pub mod contracts;
pub mod entry;
pub mod protocols;
pub mod fmt;
pub mod syntax;
pub mod traverse;
pub mod variables;

pub use arena::*;
pub use check::{SpsLowError, SpsLowProgram};
pub use convert::SpsLowConverter;
