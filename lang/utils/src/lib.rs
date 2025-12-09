#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod arena;
pub mod context;
pub mod cells;
pub mod deps;
pub mod err;
pub mod scc;
pub mod span;
pub mod never;
pub mod monoid;
pub mod phantom;

pub mod prelude {
    /// Source code location.
    pub use crate::span::{Sp, Span};
    /// Data structures.
    pub use crate::{
        arena::*,
        cells::{MultiCell, SingCell},
        context::{CoContext, Context},
        monoid::Monoid,
        never::Never,
        phantom::Phantom,
    };
    /// SCC Graphs.
    pub use crate::{
        deps::{DepGraph, SrcGraph},
        scc::{Kosaraju, SccGraph},
    };
}
