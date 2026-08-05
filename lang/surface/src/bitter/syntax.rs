pub use super::arena::*;
pub use crate::{arena::*, syntax::*};
pub use zydeco_syntax::*;
pub use zydeco_utils::span::{LocationCtx, Sp, Span};

use crate::textual::syntax as t;
use derive_more::From;
use zydeco_utils::cells::MultiCell;

/* ------------------------------- Identifier ------------------------------- */

zydeco_utils::new_key_type! {
    pub struct DefId;
    pub struct PatId;
    pub struct TermId;
}

#[derive(From, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
/// Identifier for any bitter entity, used for back-mapping spans to textual IDs.
pub enum EntityId {
    Def(DefId),
    Pat(PatId),
    Term(TermId),
}

/* --------------------------------- Binder --------------------------------- */

pub use t::NameDef;

/* --------------------------------- Pattern -------------------------------- */

#[derive(From, Clone, Debug)]
pub enum Pattern {
    Ann(Ann<PatId, TermId>),
    Hole(Hole),
    Var(DefId),
    Named(Named<FieldName, PatId>),
    Ctor(Ctor<CtorName, PatId>),
    Triv(Triv),
    Cons(ConsN<PatId, PatId>),
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

/// One binder of a desugared `pi` telescope.
#[derive(Clone, Debug)]
pub struct Pi(pub PatId, pub TermId);

/// One binder of a desugared `sigma` telescope.
#[derive(Clone, Debug)]
pub struct Sigma(pub PatId, pub TermId);

/// `exists (X as A : K) . B`
#[derive(Clone, Debug)]
pub struct ManifestExists {
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

/// `monadic ... end`
#[derive(Clone, Debug)]
/// Monadic block body kept as a single node until later translation.
pub struct MoBlock {
    pub body: TermId,
    pub basis: MonadicBasis,
}

/// A term whose nested `that` forms contribute to one block context.
#[derive(Clone, Debug)]
pub struct Block(pub TermId);

/// A parameter contributed to the nearest enclosing block.
#[derive(Clone, Debug)]
pub struct MobileParam {
    pub binder: PatId,
    pub tail: TermId,
}

/// A transparent or sealed definition contributed to the nearest enclosing
/// block. Sealing is represented directly on `bindee`.
#[derive(Clone, Debug)]
pub struct MobileBind {
    pub binder: PatId,
    pub bindee: TermId,
    pub tail: TermId,
}

/// The residual left at the source position of a mobile binding.
///
/// This indirection preserves tree ownership after the binding itself moves
/// into the block context.
#[derive(Clone, Debug)]
pub struct Residual(pub TermId);

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

#[derive(From, Clone, Debug)]
pub enum Term<Ref> {
    Meta(MetaT<TermId>),
    SourceBoundary(SourceBoundary<TermId>),
    Internal(Internal),
    Sealed(Sealed<TermId>),
    Ann(Ann<TermId, TermId>),
    Hole(Hole),
    #[from(ignore)]
    Var(Ref),
    Named(Named<FieldName, TermId>),
    Label(Label<FieldName, TermId>),
    Triv(Triv),
    Cons(ConsN<TermId, TermId>),
    Abs(Abs<PatId, TermId>),
    App(App<TermId, TermId>),
    Fix(Fix<PatId, TermId>),
    Pi(Pi),
    // Arrow(Arrow),
    // Forall(Forall),
    Sigma(Sigma),
    ManifestExists(ManifestExists),
    // Prod(Prod),
    // Exists(Exists),
    Thunk(Thunk<TermId>),
    Force(Force<TermId>),
    Ret(Return<TermId>),
    Do(Bind<PatId, TermId, TermId>),
    Let(Let<PatId, TermId, TermId>),
    MobileParam(MobileParam),
    MobileBind(MobileBind),
    Residual(Residual),
    Block(Block),
    RecGroup(RecGroup),
    MoBlock(MoBlock),
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

#[derive(Clone, Debug)]
pub struct RecursiveDefinition {
    pub binder: PatId,
    pub bindee: TermId,
}

/* -------------------------------- Primitive ------------------------------- */

/// Internal terms introduced while desugaring one source unit.
///
/// The terms remain explicit nodes through name resolution. This inventory is
/// retained for compatibility with monadic transformations that still carry
/// legacy primitive-definition metadata.
#[derive(Clone, Default, derive_more::AddAssign)]
pub struct PrimTerms {
    /// VType kind
    pub vtype: MultiCell<TermId>,
    /// CType kind
    pub ctype: MultiCell<TermId>,
    /// Thk type
    pub thk: MultiCell<TermId>,
    /// Ret type
    pub ret: MultiCell<TermId>,
    /// Unit type
    pub unit: MultiCell<TermId>,
    /// Int type
    pub int: MultiCell<TermId>,
    /// Char type
    pub char: MultiCell<TermId>,
    /// String type
    pub string: MultiCell<TermId>,
    /// Top type
    pub top: MultiCell<TermId>,
    /// OS type
    pub os: MultiCell<TermId>,
    /// Monad type
    pub monad: MultiCell<TermId>,
    /// Algebra type
    pub algebra: MultiCell<TermId>,
}
