#![allow(clippy::style)]
#![allow(clippy::useless_format)]

/// the topmost compilation pipeline led by a package configuration
pub mod package {
    pub mod pack;
    pub mod conf;
    pub mod err;

    pub use pack::*;

    #[cfg(test)]
    mod tests;
}

/// defines the SpanArena and the id types for all the structures
pub mod arena;

/// lexing and parsing;
/// introduces the surface syntax (AST);
/// a formatter can be built on top of this
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

/// an elaboration atop the surface syntax;
/// introduces the desugared syntax;
/// Angostura
pub mod bitter {
    pub mod syntax;
    pub mod desugar;
    // pub mod err;
}

/// name resolution;
/// introduces the bound syntax (ABT)
pub mod scoped {
    pub mod syntax;
    // pub mod resolver;
    pub mod err;
}
