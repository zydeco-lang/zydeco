//! Syntax shared by high and low Stack IR, parameterized by each phase's node IDs.
//!
//! The phase modules own their identifiers and control-flow variants. Patterns,
//! products, primitive calls, bindings, and data eliminations use these common forms.

pub use super::{arena::*, builtin::*};
pub use zydeco_syntax::{fmt, *};
pub use zydeco_utils::{
    arena::*,
    context::{CoContext, Context},
};

use crate::static_syntax as ss;
use derive_more::From;

pub type DefId = ss::DefId;

#[derive(From, Clone, Debug, Eq, Hash, PartialEq)]
pub struct CtorIdx {
    pub idx: usize,
    pub name: CtorName,
}

#[derive(From, Clone, Debug, Eq, Hash, PartialEq)]
pub struct DtorIdx {
    pub idx: usize,
    pub name: DtorName,
}

/* ---------------------------------- Value --------------------------------- */

/// Physical layout of a product value.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ProductLayout {
    pub arity: usize,
}

/// A logical value cons together with its canonical physical product layout.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct VCons<T> {
    pub items: Vec<T>,
    pub layout: ProductLayout,
}

impl<T> VCons<T> {
    pub fn new(items: Vec<T>, layout: ProductLayout) -> Self {
        assert!(layout.arity > 0);
        assert!(items.len() <= layout.arity);
        Self { items, layout }
    }
}

#[derive(From, Clone, Debug)]
pub enum ValuePattern<Pat> {
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<CtorIdx, Pat>),
    Alias(Alias<Pat>),
    Triv(Triv),
    VCons(VCons<Pat>),
}

#[derive(Clone, Debug)]
pub struct Complex<V> {
    /// Operator name; can be found in builtins map
    pub operator: String,
    pub operands: Vec<V>,
}

/* ---------------------------------- Stack --------------------------------- */

/// The one and only stack variable.
#[derive(From, Clone, Debug)]
pub struct Bullet;

/* ------------------------------- Computation ------------------------------ */

#[derive(Clone, Debug)]
pub struct SHole<S>(pub S);

/// Elimination of one irrefutable product-like value pattern.
#[derive(Clone, Debug)]
pub struct SProductMatch<V, Pat, C> {
    pub scrut: V,
    pub binder: Pat,
    pub body: C,
}

/// Branching elimination of a value coproduct.
#[derive(Clone, Debug)]
pub struct SCoprodMatch<V, Pat, C> {
    pub scrut: V,
    pub arms: Vec<Matcher<Pat, C>>,
}

#[derive(Clone, Debug)]
pub struct SCoMatch<Sc, Br, Tail> {
    pub scrut: Sc,
    pub arms: Vec<CoMatcher<Br, Tail>>,
}

#[derive(Clone, Debug)]
pub struct ExternCall<S> {
    pub function: ExternalFunction,
    pub stack: S,
}

/// A call through either the Zydeco host ABI or a source-declared foreign ABI.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ExternalFunction {
    Host(String),
    Foreign(ForeignImport),
}
