#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod surface_syntax {
    pub use zydeco_surface::scoped::syntax::*;
}

pub mod syntax;
pub mod prims;
pub mod alloc;
pub use alloc::*;
pub mod utils;
pub use utils::*;
pub mod lub;
pub use lub::*;
pub mod subst;
// pub use subst::*;
pub mod tyck;
pub use tyck::*;
pub mod err;
pub use err::*;
pub mod fmt;

pub(crate) use surface_syntax as su;
pub(crate) use syntax as ss;
