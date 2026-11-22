#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod arena;
pub mod context;
pub mod dense;
pub mod diagnostic;
pub mod err;
pub mod fold;
pub mod graph;
pub mod pass;
pub mod scoped_map;
pub mod span;
pub mod stack;
pub mod symbol;
pub mod with;

pub mod prelude {
    /// Dense IDs, arenas, symbols, and scoped tables.
    pub use crate::dense::{DenseArena, DenseId, IdBitSet, Idx, SecondaryMap};
    pub use crate::diagnostic::Diagnostics;
    pub use crate::scoped_map::{ScopedMap, ScopedSet};
    /// Source code location.
    pub use crate::span::{FileId, Sp, Span};
    pub use crate::stack::maybe_grow;
    pub use crate::symbol::{Sym, SymbolInterner};
    /// Design patterns.
    pub use crate::with::With;
    /// Data structures.
    pub use crate::{
        arena::*,
        context::{CoContext, Context},
        graph::{DepGraph, SccGraph, SccGroup, SrcGraph, Tarjan},
        pass::CompilerPass,
    };
}
