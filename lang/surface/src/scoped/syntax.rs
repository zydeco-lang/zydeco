pub use super::arena::*;
use crate::textual::syntax as t;
pub use crate::{arena::*, syntax::*};
use derive_more::From;
pub use t::{NameDef, SpanArena};
pub use zydeco_foundation::span::{Sp, Span};
pub use zydeco_syntax::*;

/* ------------------------------- Identifier ------------------------------- */

zydeco_foundation::new_key_type! {
    pub struct DefId;
    pub struct PatId;
    pub struct TermId;
    /// Identifier for a node in the condensation DAG of context bindings.
    pub struct ContextNodeId;
}

#[derive(From, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
/// Identifier for any resolved surface entity, used for back-mapping spans to textual IDs.
pub enum EntityId {
    Def(DefId),
    Pat(PatId),
    Term(TermId),
}

/* --------------------------------- Pattern -------------------------------- */

#[derive(From, Clone, Debug)]
pub enum Pattern {
    Ann(Ann<PatId, TermId>),
    Hole(Hole),
    Var(DefId),
    Named(Named<FieldName, PatId>),
    Ctor(Ctor<CtorName, PatId>),
    Lit(Literal),
    Project(ProjectionPattern<FieldName, PatId>),
    View(ViewPattern),
    Alias(Alias<PatId>),
    Triv(Triv),
    Cons(Vec<PatId>),
}

/// A view head precomposed with an ordinary pattern.
#[derive(Clone, Debug)]
pub struct ViewPattern {
    pub function: TermId,
    pub pattern: PatId,
}

#[derive(From, Clone, Debug)]
pub enum CoPatternItem {
    Pat(PatId),
    Dtor(DtorName),
}

/// A nonempty sequence of observations on the left of a comatch clause.
#[derive(Clone, Debug)]
pub struct CoPatternSpine {
    pub head: CoPatternItem,
    pub tail: Vec<CoPatternItem>,
}

impl CoPatternSpine {
    pub fn from_items(items: Vec<CoPatternItem>) -> Option<Self> {
        let mut items = items.into_iter();
        let head = items.next()?;
        Some(Self { head, tail: items.collect() })
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &CoPatternItem> {
        std::iter::once(&self.head).chain(self.tail.iter())
    }

    pub fn into_items(self) -> impl DoubleEndedIterator<Item = CoPatternItem> {
        std::iter::once(self.head).chain(self.tail)
    }
}

/* ---------------------------------- Term ---------------------------------- */

/// Metadata decoded once during surface lowering/resolution for type checking and observations.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ResolvedMeta {
    Builtin(BuiltinRole),
    Ffi(ForeignTarget),
    Debug(Meta),
    Opaque(Meta),
}

/// A term wrapped with typed or opaque metadata.
#[derive(Clone, Debug)]
pub struct MetaTerm {
    pub meta: ResolvedMeta,
    pub body: TermId,
}

/// An operand whose synthesized classifier replaces it during elaboration.
#[derive(Clone, Debug)]
pub struct TypeOf(pub TermId);

/// A `pi` telescope: its binders in source order, never empty, and the body they scope over.
#[derive(Clone, Debug)]
pub struct Pi(pub Vec<PatId>, pub TermId);

/// One binder of a lowered `val pi` telescope.
#[derive(Clone, Debug)]
pub struct ValPi(pub PatId, pub TermId);

/// One binder of a lowered `sigma` telescope.
#[derive(Clone, Debug)]
pub struct Sigma(pub PatId, pub TermId);

/// `exists (X as A : K) . B`
#[derive(Clone, Debug)]
pub struct ManifestExists {
    pub binder: PatId,
    pub definition: TermId,
    pub body: TermId,
}

/// Whether a `pack` layer discloses its witness in the synthesized type
/// or seals it behind an abstract binder.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PackMode {
    /// The layer's witness is recorded as a manifest binder.
    Disclosed,
    /// The layer's witness stays abstract; only the package value carries it.
    Sealed,
}

/// One binder of a lowered `pack` telescope.
///
/// Unlike `ManifestExists`, the body is a package payload rather than a type,
/// so the layer synthesizes a value at the corresponding existential type.
#[derive(Clone, Debug)]
pub struct Pack {
    pub mode: PackMode,
    pub binder: PatId,
    pub definition: TermId,
    pub body: TermId,
}

/// The ordinary type definitions used to elaborate a monadic block.
#[derive(Clone, Debug)]
pub struct MonadicBasis {
    pub monad: TermId,
    pub algebra: TermId,
}

/// Monadic block body kept as a single node until later translation.
#[derive(Clone, Debug)]
pub struct MoBlock {
    pub body: TermId,
    pub basis: MonadicBasis,
}

/// Call-by-value block body kept as a single node until type checking.
///
/// Unlike [`MoBlock`], the target of this translation is the CBPV core itself,
/// so there is no basis to carry: `Thk` and `Ret` are the only structure the
/// elaboration needs.
#[derive(Clone, Debug)]
pub struct CbvBlock {
    pub body: TermId,
}

/// A term whose nested `that` forms contribute to one block context.
#[derive(Clone, Debug)]
pub struct Block(pub TermId);

/// The residual left at the source position of a mobile binding.
///
/// This indirection preserves tree ownership after the binding itself moves
/// into the block context.
#[derive(Clone, Debug)]
pub struct Residual(pub TermId);

#[derive(Clone, Debug)]
pub struct RecursiveDefinition {
    pub binder: PatId,
    pub bindee: TermId,
}

/// A recursive type component followed by the residual term of its block.
#[derive(Clone, Debug)]
pub struct RecGroup {
    pub definitions: Vec<RecursiveDefinition>,
    pub tail: TermId,
}

/// data | C_1 ty | ... end
#[derive(Clone, Debug)]
pub struct Data {
    pub arms: Vec<DataArm>,
}
#[derive(Clone, Debug)]
pub struct DataArm {
    pub name: CtorName,
    pub param: TermId,
}

/// `codata | .d_1 cp : ty | ... end`
#[derive(Clone, Debug)]
pub struct CoData {
    pub arms: Vec<CoDataArm>,
}
#[derive(Clone, Debug)]
pub struct CoDataArm {
    pub name: DtorName,
    pub out: TermId,
}

/// One generalized comatch clause.
#[derive(Clone, Debug)]
pub struct CoPatternClause {
    pub spine: CoPatternSpine,
    pub tail: TermId,
}

/// Source comatch clauses retained until type-directed elaboration.
#[derive(Clone, Debug)]
pub struct CoMatchClauses {
    pub clauses: Vec<CoPatternClause>,
}

/// Lowered and name-resolved surface term syntax stored in the dominant dense arena.
///
/// Uncommon ternary and boxed metadata payloads stay behind `Box` so their size
/// does not inflate every atomic and binary term slot.
#[derive(From, Clone, Debug)]
pub enum Term {
    Meta(Box<MetaTerm>),
    TypeOf(TypeOf),
    SourceBoundary(SourceBoundary<TermId>),
    SignatureBoundary(SignatureBoundary<TermId>),
    Internal(Internal),
    Sealed(Sealed<TermId>),
    Ann(Ann<TermId, TermId>),
    Hole(Hole),
    #[from(ignore)]
    Var(DefId),
    Named(Named<FieldName, TermId>),
    Label(Label<FieldName, TermId>),
    Triv(Triv),
    Cons(Vec<TermId>),
    /// An abstraction: its consecutive parameters in source order, never empty.
    Abs(Abs<Vec<PatId>, TermId>),
    #[from(ignore)]
    ValAbs(Abs<PatId, TermId>),
    /// An application: its head and its consecutive arguments, never empty.
    App(App<TermId, Vec<TermId>>),
    Fix(Fix<PatId, TermId>),
    Pi(Pi),
    ValPi(ValPi),
    Sigma(Sigma),
    ManifestExists(Box<ManifestExists>),
    Pack(Box<Pack>),
    Thunk(Thunk<TermId>),
    Force(Force<TermId>),
    Ret(Return<TermId>),
    Do(Box<Bind<PatId, TermId, TermId>>),
    Let(Box<Let<PatId, TermId, TermId>>),
    Residual(Residual),
    Block(Block),
    RecGroup(RecGroup),
    MoBlock(Box<MoBlock>),
    CbvBlock(CbvBlock),
    Data(Data),
    CoData(CoData),
    Ctor(Ctor<CtorName, TermId>),
    Match(Match<TermId, PatId, TermId>),
    CoMatchClauses(CoMatchClauses),
    CoMatch(CoMatch<DtorName, TermId>),
    Dtor(Dtor<TermId, DtorName>),
    Proj(Proj<TermId, FieldName>),
    Lit(Literal),
}

impl From<MetaTerm> for Term {
    fn from(value: MetaTerm) -> Self {
        Self::Meta(Box::new(value))
    }
}

impl From<ManifestExists> for Term {
    fn from(value: ManifestExists) -> Self {
        Self::ManifestExists(Box::new(value))
    }
}

impl From<Pack> for Term {
    fn from(value: Pack) -> Self {
        Self::Pack(Box::new(value))
    }
}

impl From<Bind<PatId, TermId, TermId>> for Term {
    fn from(value: Bind<PatId, TermId, TermId>) -> Self {
        Self::Do(Box::new(value))
    }
}

impl From<Let<PatId, TermId, TermId>> for Term {
    fn from(value: Let<PatId, TermId, TermId>) -> Self {
        Self::Let(Box::new(value))
    }
}

impl From<MoBlock> for Term {
    fn from(value: MoBlock) -> Self {
        Self::MoBlock(Box::new(value))
    }
}

#[cfg(test)]
mod term_layout_tests {
    use super::*;

    #[test]
    fn rare_payloads_keep_term_slots_compact() {
        assert!(std::mem::size_of::<Term>() <= 48);
    }
}

/* --------------------------- Contextual program --------------------------- */

/// The term site that contributed a binding to a contextual term.
pub type BindingId = TermId;

/// A definition contributing a type or value to the surrounding context.
#[derive(Clone, Debug)]
pub struct Definition {
    pub binder: PatId,
    pub bindee: TermId,
}

/// A parameter contributed by `param ... that ...` or `param val ... that ...`.
#[derive(Clone, Debug)]
pub struct Parameter {
    pub flavor: ParameterFlavor,
    pub binder: PatId,
}

/// A single binding stored in a contextual term.
#[derive(Clone, Debug)]
pub struct Binding {
    pub id: BindingId,
    pub inner: BindingForm,
    pub(crate) source_order: usize,
}

/// The forms admitted by contextual terms.
#[derive(Clone, Debug)]
pub enum BindingForm {
    Parameter(Parameter),
    Definition(Definition),
}

impl Binding {
    pub fn from_term(source: TermId, inner: BindingForm, source_order: usize) -> Self {
        Self { id: source, inner, source_order }
    }

    pub fn source_order(&self) -> usize {
        self.source_order
    }
}

/// One strongly connected component in a contextual term.
///
/// `Acyclic` is a singleton component without a self edge. `Recursive`
/// therefore contains either one self-recursive binding or several mutually
/// recursive bindings.
#[derive(Clone, Debug)]
pub enum ContextNode {
    Acyclic(Binding),
    Recursive(Vec<Binding>),
}

impl ContextNode {
    pub fn bindings(&self) -> &[Binding] {
        match self {
            | ContextNode::Acyclic(binding) => std::slice::from_ref(binding),
            | ContextNode::Recursive(bindings) => bindings,
        }
    }

    pub fn source_order(&self) -> usize {
        self.bindings()
            .iter()
            .map(Binding::source_order)
            .min()
            .expect("a context node must contain at least one binding")
    }
}

/// A term together with the context that it inhabits.
#[derive(Clone, Debug)]
pub struct ContextualTerm<C, B> {
    pub context: C,
    pub body: B,
}

/// The source residual and dependency-ordered elaboration of a `begin` block.
#[derive(Clone, Debug)]
pub struct BlockBody {
    pub residual: TermId,
    pub elaborated: TermId,
}

/* --------------------------------- Context -------------------------------- */

/// Context is what variables we *can use* at a given term site.
pub type Context = zydeco_foundation::context::Context<DefId>;

/* -------------------------------- CoContext ------------------------------- */

/// CoContext is what variables we *have used* at a given term site.
pub type CoContext = zydeco_foundation::context::CoContext<DefId>;
