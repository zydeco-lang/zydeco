#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod surface_syntax {
    pub use zydeco_surface::scoped::syntax::*;
}

pub mod syntax;
pub mod arena;
pub mod prims;
pub mod alloc;
pub use alloc::*;
pub mod lub;
pub use lub::*;
pub mod norm;
pub mod construct;
pub use construct::*;
pub mod destruct;
pub mod syntactic;
pub use syntactic::*;
pub mod tyck;
pub use tyck::*;
pub mod lift;
pub mod monadic;
pub mod err;
pub use err::*;
pub mod fmt;
mod span;

pub(crate) use construct::syntax as cs;
pub(crate) use surface_syntax as su;
pub(crate) use syntax as ss;
