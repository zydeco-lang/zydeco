//! Lexical stack-passing IR before closure conversion.

/// Arenas and builders for high Stack IR nodes.
pub mod arena;
/// Validation of lexical ownership and branch-join placement.
pub mod check;
/// Consumer demands used by normalization.
pub mod demand;
/// Pretty/ugly formatters for high Stack IR.
pub mod fmt;
/// Lowering from typed syntax into high Stack IR.
pub mod lower;
/// Local reductions and demand-driven pruning before closure conversion.
pub mod normalize;
/// High Stack IR syntax and identifiers.
pub mod syntax;
/// Free-variable analysis for high Stack IR.
pub mod variables;
