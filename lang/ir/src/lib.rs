//! Self-contained typed residual CBPV IR for Zydeco execution and lowering.

pub mod builtin;
pub mod fmt;
pub mod protocol;
pub mod syntax;
pub mod traverse;
pub mod validate;

pub use builtin::{BuiltinPackagePlan, BuiltinPackageValue};
pub use protocol::{CodataProtocol, CodataProtocolId, ProtocolGraph, StackProtocol, ValueProtocol};
pub use syntax::*;
