#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod statics_syntax {
    pub use zydeco_statics::syntax::*;
}

pub mod syntax;
pub mod link;
pub mod eval;

// pub(crate) use statics_syntax as ss;
// pub(crate) use syntax as ds;
