#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod span;
pub mod arena;
pub mod cells;
pub mod context;
pub mod graph;
pub mod err;
pub mod pass;
pub mod phantom;
pub mod with;

pub mod prelude {
    /// Source code location.
    pub use crate::span::{Sp, Span};
    /// Design patterns.
    pub use crate::{
        with::With,
        phantom::Phantom,
    };
    /// Data structures.
    pub use crate::{
        arena::*,
        cells::{MultiCell, SingCell},
        context::{CoContext, Context},
        graph::{DepGraph, Kosaraju, SccGraph, SccGroup, SrcGraph},
        pass::CompilerPass,
    };
}
