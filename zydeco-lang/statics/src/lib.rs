#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod syntax {
    pub use zydeco_surface::scoped::syntax::*;
}

pub mod sorted {
    pub mod syntax;
    pub mod sorter;
}

pub mod typed {
    pub mod syntax;
}
