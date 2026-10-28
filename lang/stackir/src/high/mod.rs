//! Lexical stack-passing IR before closure conversion.

/// Arenas and builders for high Stack IR nodes.
pub mod arena;
/// Validation of lexical ownership, placement in the compiling scheme's fragment, and procedure scope.
pub mod check;
/// Deep copies of SPS subtrees for the duplicating scheme.
mod copy;
/// Consumer demands used by normalization.
pub mod demand;
/// Pretty/ugly formatters for high Stack IR.
pub mod fmt;
/// Localization of closures that are only called into second-class procedures.
pub mod localize;
/// Lowering from typed syntax into high Stack IR.
pub mod lower;
/// Local reductions and demand-driven pruning before closure conversion.
pub mod normalize;
/// High Stack IR syntax and identifiers.
pub mod syntax;
/// Shared structural traversal and independent analysis composition.
pub mod traverse;
/// Free-variable analysis for high Stack IR.
pub mod variables;
