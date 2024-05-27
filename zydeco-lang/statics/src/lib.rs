#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod surface_syntax {
    pub use zydeco_surface::scoped::syntax::*;
}

pub mod syntax;

pub mod sorted {
    pub mod syntax;
    pub mod sorter;
    pub mod err;
}

pub mod typed {
    pub mod syntax;
}
