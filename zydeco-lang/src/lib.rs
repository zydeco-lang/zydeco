#![allow(clippy::style)]
#![allow(clippy::useless_format)]
#![allow(clippy::clone_on_copy)]
#![allow(clippy::mutable_key_type)]

pub mod syntax;

pub mod surface {
    pub mod parse {
        pub mod syntax;
        pub mod err;
        pub mod lexer;

        #[allow(clippy::all)]
        pub mod parser {
            lalrpop_util::lalrpop_mod!(parser_impl, "/src/surface/parse/parser.rs");
            pub use parser_impl::*;
        }
        mod escape;
    }
    pub mod resolve {
        pub mod err;
        // pub mod elab;
    }
}
use surface::*;

pub mod statics {
    pub mod syntax;
    pub mod err;
    pub mod elab;
    pub mod tyck;
    mod fmt;
    pub use self::{
        elab::Elaboration,
        tyck::{Ctx, Seal, TypeCheck},
    };
}

/// monad transformers
pub mod lift {
    mod r#type;
    mod term;
    pub use r#type::MonadTransType;
    pub use term::MonadTransTerm;
}

pub mod library {
    pub mod syntax;
    mod builtins;
    mod link;
    mod impls;
    mod fmt;
}

pub mod dynamics {
    pub mod syntax;
    pub mod eval;
    mod fmt;

    pub use eval::Eval;
}

pub mod backend {
    pub mod cps;
}

pub mod utils {
    pub use zydeco_utils::*;
    pub mod fmt;
}
pub mod prelude {
    pub use super::utils::fmt::FmtArgs;
    pub use zydeco_utils::prelude::*;
}

pub mod zydeco;
