#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod arena;
pub mod cells;
pub mod deps;
pub mod scc;
pub mod span;
pub mod never;
pub mod monoid;
pub mod wrappers;

pub mod prelude {
    pub use crate::{
        arena::*,
        cells::{MultiCell, SingCell},
        monoid::Monoid,
        never::Never,
        rc,
        span::{Sp, Span},
    };
}
