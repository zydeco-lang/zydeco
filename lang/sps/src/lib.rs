#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod statics_syntax {
    pub use zydeco_statics::tyck::syntax::*;
}

pub mod syntax;
pub mod lower;
pub mod fmt;
