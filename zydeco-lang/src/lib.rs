#![allow(clippy::style)]
#![allow(clippy::useless_format)]
#![allow(clippy::clone_on_copy)]
#![allow(clippy::mutable_key_type)]

pub mod syntax;

pub mod parse {
    #[allow(clippy::all)]
    pub mod parser {
        use lalrpop_util::lalrpop_mod;
        lalrpop_mod!(parser_impl, "/parse/parser.rs");
        pub use parser_impl::*;
    }

    pub mod token;
    pub mod syntax;
    pub mod err;
    mod escape;
    mod span;

    use logos::{Logos, SpannedIter};
    use token::Tok;

    pub struct Lexer<'source> {
        inner: SpannedIter<'source, Tok<'source>>,
    }

    impl<'source> Lexer<'source> {
        pub fn new(source: &'source str) -> Self {
            Self { inner: Tok::lexer(&source).spanned() }
        }
    }

    impl<'source> Iterator for Lexer<'source> {
        type Item = (usize, Tok<'source>, usize);

        fn next(&mut self) -> Option<Self::Item> {
            self.inner.next().map(|(tok, range)| (range.start, tok, range.end))
        }
    }
}

pub mod statics {
    pub mod syntax;
    mod elab;
    mod resolve;
    pub mod tyck;
    pub mod err;
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
