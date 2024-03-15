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

/// defines common syntax in the surface language
pub mod syntax;

/// lexing and parsing;
/// introduces the surface syntax (AST);
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

/// a formatter built on top of the textual syntax;
/// introduces the ugly syntax;
/// outputs a safe surface syntax
pub mod ugly {
    pub mod fmt;
}

/// a formatter built on top of the textual syntax;
/// introduces the pretty syntax;
/// outputs a pretty-printed surface syntax
pub mod pretty {
    pub mod syntax {
        pub struct NonBreak(pub String);
        pub struct MorallyNonBreak(pub Vec<NonBreak>);
        pub struct SoftBreak(pub Vec<MorallyNonBreak>);
        pub struct HardBreak(pub Vec<SoftBreak>);
        pub struct ParagraphBreak(pub Vec<HardBreak>);
    }
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
/// introduces the bound syntax (ABT);
pub mod scoped {
    pub mod syntax;
    pub mod detective;
    pub mod resolver;
    pub mod err;
}
