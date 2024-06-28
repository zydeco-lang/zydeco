#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod statics_syntax {
    pub use zydeco_statics::syntax::*;
}

pub mod syntax;
pub use syntax::{ProgKont, Runtime};
pub mod builtin;
mod impls;
pub mod link;
pub use link::*;
pub mod eval;
pub use eval::*;
pub mod fmt;

pub(crate) use statics_syntax as ss;
pub(crate) use syntax as ds;
