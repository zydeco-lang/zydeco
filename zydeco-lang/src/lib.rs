#![allow(clippy::style)]
#![allow(clippy::useless_format)]
#![allow(clippy::clone_on_copy)]
#![allow(clippy::mutable_key_type)]

pub mod syntax;

pub mod parse {
    pub mod syntax;
    pub mod err;
    pub mod lexer;

    #[allow(clippy::all)]
    pub mod parser {
        use lalrpop_util::lalrpop_mod;
        lalrpop_mod!(parser_impl, "/parse/parser.rs");
        pub use parser_impl::*;
    }

    mod escape;
    mod span;
}

pub mod resolve {
    pub mod err;
}

pub mod statics {
    pub mod syntax;
    pub mod err;
    mod elab;
    pub mod tyck;
    mod fmt;

    pub use tyck::TypeCheck;
}

pub mod library {
    pub mod syntax;
    mod link;
    pub mod builtins;
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
        span::{Span, SpanHolder, SpanInfo, SpanView},
    };
}

pub mod zydeco;
