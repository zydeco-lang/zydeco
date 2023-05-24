#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod span;
pub mod never;
pub mod wrappers;
pub mod monoid;

pub mod prelude {
    pub use crate::{
        monoid::Monoid,
        never::Never,
        rc,
        span::{Span, SpanHolder, SpanInfo, SpanView},
    };
}
