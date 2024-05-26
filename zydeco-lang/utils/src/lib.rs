#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod arena;
pub mod deps;
pub mod scc;
pub mod span;
pub mod never;
pub mod monoid;
pub mod wrappers;
pub mod interval_tree;
pub mod multi_cell;

pub mod prelude {
    pub use crate::{
        arena::*,
        interval_tree::IntervalTree,
        monoid::Monoid,
        multi_cell::MultiCell,
        never::Never,
        rc,
        span::{Sp, Span, SpanHolder, SpanView},
    };
}
