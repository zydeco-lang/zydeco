#![allow(clippy::style)]
#![allow(clippy::useless_format)]

/// the topmost compilation pipeline led by a package configuration
pub mod package {
    pub mod pack;
    pub use pack::*;
    pub mod conf;
    pub mod err;
    pub use err::*;

    #[cfg(test)]
    mod tests;
}

/// defines the GenArena and the id types for all the structures
pub mod arena {
    pub use zydeco_utils::arena::*;
}

/// defines common syntax in the surface language
pub mod syntax;

/// lexing and parsing;
/// introduces the surface syntax (AST);
pub mod textual {
    pub mod syntax;
    pub mod lexer;
    pub use lexer::*;
    #[allow(clippy::all)]
    pub mod parser {
        lalrpop_util::lalrpop_mod!(parser_impl, "/textual/parser.rs");
        pub use parser_impl::*;
    }
    pub use parser::*;
    pub mod escape;
    pub mod err;
    pub use err::*;

    /// a formatter built on top of the textual syntax;
    /// introduces the ugly syntax;
    /// outputs a safe surface syntax
    mod ugly;

    // /// a formatter built on top of the textual syntax;
    // /// introduces the pretty syntax;
    // /// outputs a pretty-printed surface syntax
    // mod pretty {
    // }

    pub mod fmt {
        pub use super::ugly::*;
        // pub use super::pretty::*;
    }

    #[cfg(test)]
    mod tests;
}

/// an elaboration atop the surface syntax;
/// introduces the desugared syntax;
/// Angostura
pub mod bitter {
    pub mod syntax;
    pub mod alloc;
    pub use alloc::*;
    pub mod desugar;
    pub use desugar::*;
    // pub mod err;
    // pub use err::*;
    pub mod fmt;
}

/// name resolution;
/// introduces the bound syntax (ABT);
pub mod scoped {
    pub mod syntax;
    pub mod binders;
    pub use binders::*;
    pub mod resolver;
    pub use resolver::*;
    pub mod err;
    pub use err::*;
    pub mod fmt;
}
