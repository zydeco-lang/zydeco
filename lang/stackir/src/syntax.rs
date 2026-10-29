//! Syntax shared by SPS and SPSLow, parameterized by each phase's node IDs.
//!
//! The phase modules own their identifiers and control-flow variants. Patterns,
//! products, primitive calls, bindings, and data eliminations use these common forms.

pub use super::{arena::*, builtin::*};
pub use zydeco_syntax::{fmt, *};
pub use zydeco_utils::{
    arena::*,
    context::{CoContext, Context},
};

use crate::{
    scheme::{StackConsumer, StackShape},
    static_syntax as ss,
    traverse::Children,
    variables::FreeVariables,
};
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

/// Binary scalar arithmetic, evaluating the second operand before the first
/// to preserve construction order when consuming an argument stack.
#[derive(Clone, Debug)]
pub struct Primitive<V> {
    pub operation: PrimitiveOp,
    pub operands: [V; 2],
}

/// Select one computation under a shared ambient stack. Evaluate the second
/// operand before the first, matching argument-stack construction.
#[derive(Clone, Debug)]
pub struct CompareBranch<V, C> {
    pub operation: ComparisonOp,
    pub operands: [V; 2],
    pub when_true: C,
    pub when_false: C,
}

/// Pure wrapping byte displacement of an unmanaged address. Evaluate the
/// displacement before the base, as when consuming their argument stack.
#[derive(Clone, Debug)]
pub struct AddrOffset<V> {
    pub base: V,
    pub displacement: V,
}

/// Ordered memory effects. A load binds an ordinary value only after validation;
/// a store completes before entering its successor. Neither is a pure value.
#[derive(Clone, Debug)]
pub enum MemoryStep<V, Pat, C> {
    Load { scalar: memory::MemoryScalar, address: V, result: Pat, next: C },
    Store { scalar: memory::MemoryScalar, address: V, value: V, next: C },
}

/* ---------------------------------- Stack --------------------------------- */

/// The one and only stack variable, printed as `•`.
#[derive(From, Clone, Debug)]
pub struct StackVar;

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
    Host(BuiltinValueRole),
    Foreign(ForeignImport),
    Unit(UnitImport),
}

/* ---------------------------------- Phases -------------------------------- */

/// One level of the stack IR: lexical SPS or first-order SPSLow.
///
/// A phase owns its node identifiers and the forms it adds to the shared values, stacks,
/// and computations. Structural traversal, variable summaries, printing, and stack-use
/// classification are written once over the shared forms and reach a phase's own forms
/// through the hooks that its extension types implement.
pub trait Phase: Copy + Eq + std::hash::Hash + std::fmt::Debug + 'static {
    type VPatId: ArenaId + std::fmt::Debug;
    type ValueId: ArenaId + std::fmt::Debug;
    type StackId: ArenaId + std::fmt::Debug;
    type CompuId: ArenaId + std::fmt::Debug;
    /// The allocation scope whose schema stores this phase's nodes.
    type Scope: ArenaSchema<Self::VPatId, Item = ValuePattern<Self::VPatId>>
        + ArenaSchema<Self::ValueId, Item = Value<Self>>
        + ArenaSchema<Self::StackId, Item = Stack<Self>>
        + ArenaSchema<Self::CompuId, Item = Computation<Self>>
        + Allocates<Self::VPatId>
        + Allocates<Self::ValueId>
        + Allocates<Self::StackId>
        + Allocates<Self::CompuId>
        + std::fmt::Debug;
    /// The arena that holds this phase's nodes.
    type Nodes: Nodes<Phase = Self>;
    /// The value forms this phase adds to the shared ones.
    type ValueExt: Clone + std::fmt::Debug + Children<Self> + FreeVariables<Self>;
    /// The stack forms this phase adds to the shared ones.
    type StackExt: Clone + std::fmt::Debug + Children<Self> + FreeVariables<Self> + StackShape<Self>;
    /// The computation forms this phase adds to the shared ones.
    type CompuExt: Clone
        + std::fmt::Debug
        + Children<Self>
        + FreeVariables<Self>
        + StackConsumer<Self>;
}

/// A value of one phase: the shared forms and the phase's own.
#[derive(Clone, Debug, From)]
pub enum Value<P: Phase> {
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<CtorIdx, P::ValueId>),
    Triv(Triv),
    VCons(VCons<P::ValueId>),
    Literal(Literal),
    Primitive(Primitive<P::ValueId>),
    AddrOffset(AddrOffset<P::ValueId>),
    #[from(ignore)]
    Ext(P::ValueExt),
}

/// A stack of one phase: the shared frames and the phase's own.
#[derive(Clone, Debug, From)]
pub enum Stack<P: Phase> {
    Var(StackVar),
    #[from(ignore)]
    Arg(Cons<P::ValueId, P::StackId>),
    #[from(ignore)]
    Tag(Cons<DtorIdx, P::StackId>),
    #[from(ignore)]
    Ext(P::StackExt),
}

/// A computation of one phase: the shared forms and the phase's own.
#[derive(Clone, Debug, From)]
pub enum Computation<P: Phase> {
    Hole(SHole<P::StackId>),
    ProductMatch(SProductMatch<P::ValueId, P::VPatId, P::CompuId>),
    CoprodMatch(SCoprodMatch<P::ValueId, P::VPatId, P::CompuId>),
    Compare(CompareBranch<P::ValueId, P::CompuId>),
    #[from(ignore)]
    LetValue(Let<P::VPatId, P::ValueId, P::CompuId>),
    #[from(ignore)]
    LetStack(Let<StackVar, P::StackId, P::CompuId>),
    #[from(ignore)]
    LetArg(Let<Cons<P::VPatId, StackVar>, P::StackId, P::CompuId>),
    CoCase(SCoMatch<P::StackId, Cons<DtorIdx, StackVar>, P::CompuId>),
    ExternCall(ExternCall<P::StackId>),
    Memory(MemoryStep<P::ValueId, P::VPatId, P::CompuId>),
    #[from(ignore)]
    Ext(P::CompuExt),
}

impl<P: Phase> Computation<P> {
    pub fn is_branch(&self) -> bool {
        matches!(self, Self::CoprodMatch(_) | Self::Compare(_))
    }
}

/// Dispatcher over the terms of one phase.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum TermId<P: Phase> {
    Value(P::ValueId),
    Compu(P::CompuId),
    Stack(P::StackId),
}

/// The conversions of one phase into the shared enums: the shared forms whose generic `From`
/// impls would overlap, the phase's own forms, and the dispatchers over its identifiers.
/// Invoke it where the phase's `VPatId`, `ValueId`, `StackId`, `CompuId`, `ValueExt`,
/// `StackExt`, and `CompuExt` are in scope.
macro_rules! phase_conversions {
    ($phase:ty) => {
        impl From<$crate::syntax::Cons<ValueId, StackId>> for $crate::syntax::Stack<$phase> {
            fn from(frame: $crate::syntax::Cons<ValueId, StackId>) -> Self {
                Self::Arg(frame)
            }
        }
        impl From<$crate::syntax::Cons<$crate::syntax::DtorIdx, StackId>>
            for $crate::syntax::Stack<$phase>
        {
            fn from(frame: $crate::syntax::Cons<$crate::syntax::DtorIdx, StackId>) -> Self {
                Self::Tag(frame)
            }
        }
        impl<T: Into<StackExt>> From<T> for $crate::syntax::Stack<$phase> {
            fn from(form: T) -> Self {
                Self::Ext(form.into())
            }
        }
        impl<T: Into<ValueExt>> From<T> for $crate::syntax::Value<$phase> {
            fn from(form: T) -> Self {
                Self::Ext(form.into())
            }
        }
        impl From<$crate::syntax::Let<VPatId, ValueId, CompuId>>
            for $crate::syntax::Computation<$phase>
        {
            fn from(binding: $crate::syntax::Let<VPatId, ValueId, CompuId>) -> Self {
                Self::LetValue(binding)
            }
        }
        impl From<$crate::syntax::Let<$crate::syntax::StackVar, StackId, CompuId>>
            for $crate::syntax::Computation<$phase>
        {
            fn from(
                binding: $crate::syntax::Let<$crate::syntax::StackVar, StackId, CompuId>,
            ) -> Self {
                Self::LetStack(binding)
            }
        }
        impl
            From<
                $crate::syntax::Let<
                    $crate::syntax::Cons<VPatId, $crate::syntax::StackVar>,
                    StackId,
                    CompuId,
                >,
            > for $crate::syntax::Computation<$phase>
        {
            fn from(
                binding: $crate::syntax::Let<
                    $crate::syntax::Cons<VPatId, $crate::syntax::StackVar>,
                    StackId,
                    CompuId,
                >,
            ) -> Self {
                Self::LetArg(binding)
            }
        }
        impl<T: Into<CompuExt>> From<T> for $crate::syntax::Computation<$phase> {
            fn from(form: T) -> Self {
                Self::Ext(form.into())
            }
        }
        impl From<ValueId> for $crate::syntax::TermId<$phase> {
            fn from(id: ValueId) -> Self {
                Self::Value(id)
            }
        }
        impl From<CompuId> for $crate::syntax::TermId<$phase> {
            fn from(id: CompuId) -> Self {
                Self::Compu(id)
            }
        }
        impl From<StackId> for $crate::syntax::TermId<$phase> {
            fn from(id: StackId) -> Self {
                Self::Stack(id)
            }
        }
        impl From<VPatId> for $crate::traverse::EntityId<$phase> {
            fn from(id: VPatId) -> Self {
                Self::Pattern(id)
            }
        }
        impl From<ValueId> for $crate::traverse::EntityId<$phase> {
            fn from(id: ValueId) -> Self {
                Self::Value(id)
            }
        }
        impl From<StackId> for $crate::traverse::EntityId<$phase> {
            fn from(id: StackId) -> Self {
                Self::Stack(id)
            }
        }
        impl From<CompuId> for $crate::traverse::EntityId<$phase> {
            fn from(id: CompuId) -> Self {
                Self::Computation(id)
            }
        }
    };
}
pub(crate) use phase_conversions;
