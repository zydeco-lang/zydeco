#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod arena;
pub mod span;
pub mod never;
pub mod monoid;
pub mod wrappers;
pub mod scc;

pub mod prelude {
    pub use crate::{
        arena::*,
        monoid::Monoid,
        never::Never,
        rc,
        scc::*,
        span::{Sp, Span, SpanHolder, SpanView},
    };
}
