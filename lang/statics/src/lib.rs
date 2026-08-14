//! Static semantics, typed syntax, and type-directed elaboration for Zydeco.
//!
//! The crate is organized around the durable typed representation rather than
//! around the type-checking driver. [`check`] elaborates scoped surface syntax
//! into [`arena::StaticsArena`], [`normalize`] provides substitution and
//! definitional normalization, and [`elaborate`] contains transformations whose
//! behavior depends on typing information.

/// Re-exports of scoped surface syntax consumed by static elaboration.
pub mod surface_syntax {
    pub use zydeco_surface::scoped::syntax::*;
}

/// Typed syntax and annotation identifiers.
pub mod syntax;
pub use syntax::*;
/// Typing, substitution, and monadic environments.
pub mod environment;
pub use environment::*;
/// Typed arena storage and annotation tables.
pub mod arena;
pub use arena::*;

/// Backend-independent interpretation of a checked Builtin package signature.
pub mod builtin;
pub use builtin::*;

/// Typed arena allocation helpers.
pub mod alloc;
pub use alloc::*;
/// HOAS-style constructors for typed internal transformations.
pub mod construct;
pub use construct::*;
/// Destructors and inspection helpers for typed nodes.
pub mod destruct;

#[doc = include_str!("check/README.md")]
pub mod check;
pub use check::*;

/// Normalization, substitution, hole solving, and scope support.
pub mod normalize;
/// Type-directed source elaborations.
pub mod elaborate;
pub use elaborate::monadic::construct::MonConstruct;
/// Post-check validation passes over typed syntax.
pub mod validate;

/// Salsa-backed query entry points for type checking.
pub mod query;

/// Formatters for scoped and statics syntax.
pub mod fmt;
/// Span lookup for typed entities.
mod source_span;

/// Compatibility facade for the former checker-centric module layout.
///
/// New code should use the responsibility-specific modules at the crate root.
pub mod tyck {
    pub use crate::alloc;
    pub use crate::arena;
    pub use crate::builtin;
    pub use crate::check;
    pub use crate::check::error as err;
    pub use crate::check::lub;
    pub use crate::check::syntactic;
    pub use crate::construct;
    pub use crate::destruct;
    pub use crate::elaborate::monadic;
    pub use crate::elaborate::monadic::construct as moncons;
    pub use crate::environment as env;
    pub use crate::fmt;
    pub use crate::normalize as norm;
    pub mod syntax {
        pub use crate::arena::*;
        pub use crate::environment::*;
        pub use crate::syntax::*;
    }

    pub use crate::alloc::*;
    pub use crate::builtin::*;
    pub use crate::check::*;
    pub use crate::construct::*;
    pub use crate::elaborate::monadic::construct::*;
}

pub(crate) use construct::syntax as cs;
pub(crate) use surface_syntax as su;

pub(crate) mod static_syntax {
    pub use crate::environment::*;
    pub use crate::syntax::*;
}
pub(crate) use static_syntax as ss;
