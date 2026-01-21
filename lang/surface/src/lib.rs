/// Defines the GenArena and the id types for all the structures
pub mod arena {
    pub use zydeco_utils::arena::*;
}

/// Defines common syntax in the surface language
pub mod syntax;

#[doc = include_str!("textual/README.md")]
pub mod textual {
    /// Surface AST, IDs, and parser allocation helpers.
    pub mod syntax;
    /// Textual arenas and span storage.
    pub mod arena;
    /// Logos-based lexer and token definitions.
    pub mod lexer;
    pub use lexer::*;
    #[allow(clippy::all)]
    /// LALRPOP-generated parser wrappers.
    pub mod parser {
        lalrpop_util::lalrpop_mod!(parser_impl, "/textual/parser.rs");
        pub use parser_impl::*;
    }
    pub use parser::*;
    /// Literal escape expansion helpers.
    pub mod escape;
    /// Parse error formatting.
    pub mod err;
    pub use err::*;

    /// A formatter built on top of the textual syntax.
    /// Introduces the ugly syntax.
    /// Outputs a safe surface syntax.
    mod ugly;

    // /// a formatter built on top of the textual syntax;
    // /// introduces the pretty syntax;
    // /// outputs a pretty-printed surface syntax
    // mod pretty {
    // }

    /// A wrapper for the formatters.
    pub mod fmt {
        pub use super::ugly::*;
        // pub use super::pretty::*;
    }
    /// Span lookup and cursor/region utilities.
    mod span;

    #[cfg(test)]
    /// Parser smoke tests.
    mod tests;
}

#[doc = include_str!("bitter/README.md")]
pub mod bitter {
    /// Core bitter AST and identifiers.
    pub mod syntax;
    /// Bitter arena storage and textual back-mapping.
    pub mod arena;
    /// Allocation helpers that preserve textual-to-bitter mappings.
    pub mod alloc;
    pub use alloc::*;
    /// Deep cloning helpers for bitter nodes with span preservation.
    pub mod clone;
    pub use clone::*;
    /// Desugaring pass from textual syntax into bitter syntax.
    pub mod desugar;
    pub use desugar::*;
    /// Desugaring error definitions.
    pub mod err;
    pub use err::*;
    /// Debug formatter for bitter syntax (ugly surface syntax).
    pub mod fmt;
    /// Span lookup for bitter IDs.
    mod span;
}

#[doc = include_str!("scoped/README.md")]
pub mod scoped {
    /// Scoped AST aliases and primitive definition tracking.
    pub mod syntax;
    /// Scoped arena storage plus dependency/context metadata.
    pub mod arena;
    /// Binder collection and primitive allocation helpers.
    pub mod binders;
    pub use binders::*;
    /// Name resolution driver and context collection.
    pub mod resolver;
    pub use resolver::*;
    /// Name resolution error definitions.
    pub mod err;
    pub use err::*;
    /// Debug formatter for scoped syntax (ugly surface syntax).
    pub mod fmt;
    /// Span lookup for scoped IDs.
    mod span;
}
