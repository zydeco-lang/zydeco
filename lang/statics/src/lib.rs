#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod surface_syntax {
    pub use zydeco_surface::scoped::syntax::*;
}

pub mod tyck {
    pub mod syntax;
    pub mod arena;

    pub mod check;
    pub use check::*;

    pub mod prims;
    pub mod env;
    pub mod alloc;
    pub use alloc::*;
    pub mod construct;
    pub use construct::*;
    pub mod moncons;
    pub use moncons::*;
    pub mod destruct;
    pub mod syntactic;
    pub use syntactic::*;
    pub mod lub;
    pub use lub::*;
    pub mod norm;
    pub mod monadic;

    pub mod err;
    pub use err::*;
    pub mod fmt;
    mod dump;
    mod span;

    use crate::*;
}
pub use tyck::{Alloc, Construct, Lub, MonConstruct, Tycker};

pub mod wf {
    pub mod syntax;
    pub mod arena;
    pub use arena::*;
    
    pub mod check;
    
//     pub mod err;
//     pub use err::*;
//     pub mod fmt;
//     mod dump;
    mod span;

//     use crate::*;
}
pub use wf::{WellFormedProgram};

pub(crate) use zydeco_surface::textual::syntax as t;
pub(crate) use surface_syntax as su;
pub(crate) use tyck::construct::syntax as cs;
pub(crate) use tyck::syntax as ss;
// pub(crate) use wf::syntax as sw;
