#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod surface_syntax {
    pub use zydeco_surface::scoped::syntax::*;
}

pub mod syntax;
pub mod syntactic;
pub mod lub;
pub mod norm;
pub mod tyck;
pub mod err;

pub(crate) use surface_syntax as su;
pub(crate) use syntax as ss;
pub use syntactic::*;
pub use lub::*;
pub use norm::*;
pub use tyck::*;
pub use err::*;
