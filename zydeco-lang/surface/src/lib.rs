#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod driver {
    pub mod proj;
    pub mod err;
}

pub mod textual {
    pub mod syntax;
    pub mod lexer;
    #[allow(clippy::all)]
    pub mod parser {
        lalrpop_util::lalrpop_mod!(parser_impl, "/textual/parser.rs");
        pub use parser_impl::*;
    }
    pub mod escape;
    pub mod err;

    #[cfg(test)]
    mod tests;
}

pub mod desugar {
    pub mod syntax;
    // pub mod err;
}

pub mod scoped {
    pub mod syntax;
    // pub mod resolver;
    pub mod err;
}
