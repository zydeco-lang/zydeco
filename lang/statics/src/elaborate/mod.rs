//! Type-directed elaborations over checked Zydeco syntax.

/// Monadic block elaboration via algebra translation.
pub mod monadic;

/// Partial source protocol extraction into [`zydeco_ir::protocol::ProtocolGraph`].
pub(crate) mod protocols;

/// Shared static value evaluation and executable residualization into [`zydeco_ir::CoreProgram`].
pub mod static_values;
