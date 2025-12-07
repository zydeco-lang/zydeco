#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod arena;
pub mod context;
pub mod imc;
pub mod cells;
pub mod deps;
pub mod err;
pub mod scc;
pub mod span;
pub mod never;
pub mod monoid;

pub mod prelude {
    /// Source code location.
    pub use crate::span::{Sp, Span};
    /// Data structures.
    pub use crate::{
        arena::*,
        cells::{MultiCell, SingCell},
        imc::*,
        monoid::Monoid,
        never::Never,
    };
    /// SCC Graphs.
    pub use crate::{
        deps::{DepGraph, SrcGraph},
        scc::{Kosaraju, SccGraph},
    };
}
