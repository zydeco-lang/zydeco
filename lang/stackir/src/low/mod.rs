//! First-order stack-passing IR with explicit closure and continuation packages.

pub mod arena;
pub mod check;
pub mod convert;
pub mod fmt;
pub mod syntax;
pub mod variables;

pub use arena::*;
pub use check::{SpsLowError, SpsLowProgram};
pub use convert::SpsLowConverter;
