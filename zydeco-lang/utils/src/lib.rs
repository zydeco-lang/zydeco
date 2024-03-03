#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod arena;
pub mod span;
pub mod never;
pub mod monoid;
pub mod wrappers;

pub mod prelude {
    pub use crate::{
        arena::*,
        span::{Sp, Span, SpanHolder, SpanView},
        never::Never,
        monoid::Monoid,
        rc,
    };
}
