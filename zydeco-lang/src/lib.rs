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
            use lalrpop_util::lalrpop_mod;
            lalrpop_mod!(parser_impl, "/surface/parse/parser.rs");
            pub use parser_impl::*;
        }
        mod escape;
    }
    pub mod resolve {
        pub mod err;
        // pub mod elab;
        pub mod map;
    }
}
use surface::*;

pub mod statics {
    pub mod legacy {
        pub mod syntax;
        pub mod err;
        pub mod elab;
        pub mod tyck;
        mod fmt;
    }
    pub use legacy::{
        elab::Elaboration,
        err, syntax,
        tyck::{Ctx, Seal, TypeCheck},
    };
}

pub mod core {
    pub mod syntax;
}

pub mod library {
    pub mod syntax;
    pub mod builtins;
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

pub mod utils {
    pub mod fmt;
    pub mod span;
    pub mod never;
    pub mod wrappers;
    pub mod monoid;
}

pub mod prelude {
    pub use crate::utils::{
        fmt::FmtArgs,
        monoid::Monoid,
        never::Never,
        span::{Span, SpanHolder, SpanInfo, SpanView},
    };
}

pub mod zydeco;
