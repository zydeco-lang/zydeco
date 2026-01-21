/// Re-exports of scoped surface syntax used as tyck input.
pub mod surface_syntax {
    pub use zydeco_surface::scoped::syntax::*;
}

#[doc = include_str!("tyck/README.md")]
pub mod tyck {
    /// Typed syntax and annotation IDs used by the type checker.
    pub mod syntax;
    /// Typed arena storage and annotation tables.
    pub mod arena;

    /// Type-checking driver and per-node rules.
    pub mod check;
    pub use check::*;

    /// Primitive registration for internal kinds and types.
    pub mod prims;
    /// Typing environments and substitution helpers.
    pub mod env;
    /// Typed arena allocation helpers.
    pub mod alloc;
    pub use alloc::*;
    /// HOAS-style constructors for internal transformations.
    pub mod construct;
    pub use construct::*;
    /// Monadic constructors used by monadic translation.
    pub mod moncons;
    pub use moncons::*;
    /// Destructors and inspection helpers for typed nodes.
    pub mod destruct;
    /// Syntactic checks for annotations, seals, and usage.
    pub mod syntactic;
    pub use syntactic::*;
    /// Least-upper-bound operations for kinds and types.
    pub mod lub;
    pub use lub::*;
    /// Normalization and substitution utilities.
    pub mod norm;
    /// Monadic block elaboration via algebra translation.
    pub mod monadic;

    /// Type-checker error definitions and reporting.
    pub mod err;
    pub use err::*;
    /// Formatters for scoped and statics syntax.
    pub mod fmt;
    /// Debug dump helpers.
    mod dump;
    /// Span lookup for typed entities.
    mod span;

    use crate::*;
}
pub use tyck::{Alloc, Construct, Lub, MonConstruct, Tycker};

pub(crate) use surface_syntax as su;
pub(crate) use tyck::construct::syntax as cs;
pub(crate) use tyck::syntax as ss;
