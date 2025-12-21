#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod arena;
pub mod cells;
pub mod context;
pub mod graph;
pub mod err;
pub mod span;
pub mod phantom;

pub mod prelude {
    /// Source code location.
    pub use crate::span::{Sp, Span};
    /// Data structures.
    pub use crate::{
        arena::*,
        cells::{MultiCell, SingCell},
        context::{CoContext, Context},
        graph::{DepGraph, Kosaraju, SccGraph, SccGroup, SrcGraph},
        phantom::Phantom,
    };
}
