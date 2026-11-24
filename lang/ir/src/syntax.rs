//! Dense residual CBPV syntax and metadata owned independently of front-end arenas.

use crate::protocol::{ProtocolGraph, StackProtocol, ValueProtocol};
use derive_more::From;
use std::{ops::Deref, sync::Arc};
pub use zydeco_foundation::dense::{DenseArena, DenseId, SecondaryMap};
pub use zydeco_foundation::span::Span;
pub use zydeco_syntax::{
    Abs, Alias, App, Bind, CheckedImport, CoMatcher, ConsN, Ctor, CtorName, DtorName, Force,
    ForeignImport, Hole, Let, Literal, Match, Matcher, Return, Thunk, Triv, UnitImport, VarName,
};

zydeco_foundation::define_idx! {
    /// Dense variable binder identifier local to one [`CoreProgram`].
    pub struct VarId;
    /// Dense value pattern identifier local to one [`CoreProgram`].
    pub struct VPatId;
    /// Dense value expression identifier local to one [`CoreProgram`].
    pub struct ValueId;
    /// Dense computation expression identifier local to one [`CoreProgram`].
    pub struct CompuId;
    /// Dense data-type descriptor identifier local to one [`CoreProgram`].
    pub struct DataId;
}

/// Dispatcher over value and computation nodes in a [`CoreProgram`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum TermId {
    Value(ValueId),
    Compu(CompuId),
}

impl From<ValueId> for TermId {
    fn from(id: ValueId) -> Self {
        Self::Value(id)
    }
}

impl From<CompuId> for TermId {
    fn from(id: CompuId) -> Self {
        Self::Compu(id)
    }
}

/// A resolved data constructor tag within a closed declaration order.
#[derive(From, Clone, Debug, Eq, Hash, PartialEq)]
pub struct CtorIdx {
    pub idx: usize,
    pub name: CtorName,
}

/// A resolved codata destructor tag within a canonical observation order.
#[derive(From, Clone, Debug, Eq, Hash, PartialEq)]
pub struct DtorIdx {
    pub idx: usize,
    pub name: DtorName,
}

/// Physical layout of a product value.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ProductLayout {
    pub arity: usize,
}

/// One resolved product projection step in a field path.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ProductProjection {
    pub layout: ProductLayout,
    pub position: usize,
}

/// A logical value cons together with its canonical physical product layout.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct VCons<T> {
    pub items: Vec<T>,
    pub layout: ProductLayout,
}

impl<T> VCons<T> {
    /// Construct a validated product node with positive arity.
    pub fn new(items: Vec<T>, layout: ProductLayout) -> Self {
        assert!(layout.arity > 0);
        assert!(items.len() <= layout.arity);
        Self { items, layout }
    }
}

/// Residual value patterns permitted after static elimination.
#[derive(Clone, Debug)]
pub enum ValuePattern {
    Hole(Hole),
    Var(VarId),
    Ctor { data: DataId, tag: CtorIdx, payload: VPatId },
    Lit(Literal),
    Alias(Alias<VPatId>),
    Triv(Triv),
    VCons(VCons<VPatId>),
}

/// Residual values permitted after static elimination.
#[derive(Clone, Debug)]
pub enum Value {
    Hole(Hole),
    Var(VarId),
    Let(Let<VPatId, ValueId, ValueId>),
    Thunk(Thunk<CompuId>),
    Ctor {
        tag: CtorIdx,
        payload: ValueId,
    },
    Triv(Triv),
    VCons(VCons<ValueId>),
    /// Non-empty chain of resolved product projections.
    Proj {
        head: ValueId,
        steps: Vec<ProductProjection>,
    },
    Lit(Literal),
    IntrinsicOp(zydeco_syntax::IntrinsicOp),
    Foreign(CheckedImport),
}

/// Residual computations permitted after static elimination.
#[derive(Clone, Debug)]
pub enum Computation {
    Hole(Hole),
    VAbs(Abs<VPatId, CompuId>),
    VApp(App<CompuId, ValueId>),
    Fix { param: VarId, body: CompuId },
    Force(Force<ValueId>),
    Ret(Return<ValueId>),
    Do(Bind<VPatId, CompuId, CompuId>),
    Let(Let<VPatId, ValueId, CompuId>),
    Match(Match<ValueId, VPatId, CompuId>),
    CoMatch { arms: Vec<CoMatcher<DtorIdx, CompuId>> },
    Dtor { body: CompuId, tag: DtorIdx },
}

/// Entry root kind of a residual [`CoreProgram`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CoreRoot {
    Value(ValueId),
    Compu(CompuId),
}

impl From<CoreRoot> for TermId {
    fn from(root: CoreRoot) -> Self {
        match root {
            | CoreRoot::Value(id) => TermId::Value(id),
            | CoreRoot::Compu(id) => TermId::Compu(id),
        }
    }
}

/// Dense arenas, protocols, and source provenance owned by a [`CoreProgram`].
#[derive(Clone, Debug, Default)]
pub struct CoreArena {
    pub vars: DenseArena<VarId, VarName>,
    pub datas: DenseArena<DataId, Vec<CtorName>>,
    pub vpats: DenseArena<VPatId, ValuePattern>,
    pub values: DenseArena<ValueId, Value>,
    pub compus: DenseArena<CompuId, Computation>,
    pub protocols: Arc<ProtocolGraph>,
    pub pattern_protocols: SecondaryMap<VPatId, ValueProtocol>,
    pub value_protocols: SecondaryMap<ValueId, ValueProtocol>,
    pub compu_protocols: SecondaryMap<CompuId, StackProtocol>,
    pub vpat_spans: SecondaryMap<VPatId, Span>,
    pub value_spans: SecondaryMap<ValueId, Span>,
    pub compu_spans: SecondaryMap<CompuId, Span>,
    /// Authored runtime holes from dead top-level bindings that must still reject execution.
    pub retained_holes: Vec<TermId>,
}

/// Complete, immutable residual CBPV program decoupled from surface and statics arenas.
#[derive(Clone, Debug)]
pub struct CoreProgram {
    arena: CoreArena,
    root: CoreRoot,
}

impl CoreProgram {
    pub fn new(arena: CoreArena, root: CoreRoot) -> Self {
        Self { arena, root }
    }

    pub fn arena(&self) -> &CoreArena {
        &self.arena
    }

    pub fn root(&self) -> CoreRoot {
        self.root
    }

    pub fn into_parts(self) -> (CoreArena, CoreRoot) {
        (self.arena, self.root)
    }
}

impl Deref for CoreProgram {
    type Target = CoreArena;

    fn deref(&self) -> &Self::Target {
        &self.arena
    }
}
