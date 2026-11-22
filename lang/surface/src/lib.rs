/// Defines the GenArena and the id types for all the structures
pub mod arena {
    pub use zydeco_foundation::arena::*;
}

/// Typed interpretations of structurally parsed metadata annotations.
pub mod metadata;

/// Defines common syntax in the surface language
pub mod syntax;

pub mod diagnostic;

pub mod textual {
    #![doc = include_str!("textual/README.md")]

    /// Contiguous `--|` text blocks retained outside canonical syntax.
    pub mod text_block;
    pub use text_block::*;
    /// Surface AST, IDs, and parser allocation helpers.
    pub mod syntax;
    /// Textual arenas and span storage.
    pub mod arena;
    /// Logos-based lexer and token definitions.
    pub mod lexer;
    pub use lexer::*;
    /// Strict and recovering textual parser entry points.
    pub mod parser;
    pub use parser::*;
    /// Literal escape expansion helpers.
    pub mod escape;
    /// Parse error formatting.
    pub mod err;
    pub use err::*;
    /// Complete term source units and typed source-level directives.
    pub mod source;
    pub use source::*;
    /// Declarative package entries extracted without checking or evaluating source terms.
    mod package;
    pub use package::*;
    mod discovery;
    pub use discovery::*;

    /// Span lookup and cursor/region utilities.
    mod span;

    #[cfg(test)]
    /// Textual syntax tests and shared repository corpus.
    mod tests;
}

pub mod scoped {
    #![doc = include_str!("scoped/README.md")]

    /// Scoped AST aliases and primitive definition tracking.
    pub mod syntax;
    /// Scoped arena storage and dependency metadata.
    pub mod arena;
    /// Shared structural traversal and visitor composition.
    pub mod traverse;
    /// Checker-local free-variable analysis over resolved syntax.
    pub mod context;
    /// Binder collection helpers.
    mod binders;
    /// Collection, dependency analysis, and elaboration of nested term blocks.
    mod blocks;
    /// Lookup and borrowed lexical scope views.
    pub mod scope;
    pub use scope::*;
    /// Passive consumers of reference and scope events.
    pub mod observers;
    pub use observers::*;
    mod dependencies;
    mod alloc;
    /// Visible definitions captured by completion-oriented resolution.
    mod completion;
    pub use completion::*;
    /// Name resolution driver.
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
