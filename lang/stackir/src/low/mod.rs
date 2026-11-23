//! First-order stack-passing IR with explicit closure and continuation packages.

pub mod arena;
pub mod check;
pub mod convert;
pub mod contracts;
pub mod entry;
pub mod protocols;
pub mod rep;
pub mod representation;
pub mod scalar;
pub mod unbox;
pub mod fmt;
pub mod syntax;
pub mod traverse;
pub mod variables;

pub use arena::*;
pub use check::{SpsLowProgram, SpsLowValidationError};
pub use convert::SpsLowConverter;
pub use rep::RepTable;
pub use representation::{RepresentationPolicy, RepresentationStrategy};
pub use unbox::LocalUnboxing;
