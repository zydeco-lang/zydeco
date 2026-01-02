pub use super::arena::*;
pub use crate::{arena::*, syntax::*};
pub use zydeco_syntax::*;
pub use zydeco_utils::span::{LocationCtx, Sp, Span};

use crate::textual::syntax as t;
use derive_more::From;
use std::ops::AddAssign;
use zydeco_utils::cells::MultiCell;

/* ------------------------------- Identifier ------------------------------- */

zydeco_utils::new_key_type! {
    pub struct DefId;
    pub struct PatId;
    pub struct TermId;
    pub struct DeclId;
}

#[derive(From, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
/// Identifier for any bitter entity, used for back-mapping spans to textual IDs.
pub enum EntityId {
    Def(DefId),
    Pat(PatId),
    Term(TermId),
    Decl(DeclId),
}

/* --------------------------------- Binder --------------------------------- */

pub use t::{NameDef, NameRef};

/* ----------------------------------- Use ---------------------------------- */

pub use t::{UseAlias, UseAll, UseEnum, UsePath, Uses};

/* --------------------------------- Pattern -------------------------------- */

#[derive(From, Clone, Debug)]
pub enum Pattern {
    Ann(Ann<PatId, TermId>),
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<PatId>),
    Triv(Triv),
    Cons(Cons<PatId, PatId>),
}

#[derive(From, Clone, Debug)]
pub enum CoPatternItem {
    Pat(PatId),
    Dtor(DtorName),
}

/* ---------------------------------- Term ---------------------------------- */

/// `pi (x: A) -> B`
#[derive(Clone, Debug)]
pub struct Pi(pub PatId, pub TermId);

/// `sigma (x: A) . A'`
#[derive(Clone, Debug)]
pub struct Sigma(pub PatId, pub TermId);

// /// `use let x = a in ...`
// #[derive(Clone, Debug)]
// pub struct UseBind {
//     pub uses: UsePath,
//     pub tail: TermId,
// }

/// `monadic ... end`
#[derive(Clone, Debug)]
/// Monadic block body kept as a single node until later translation.
pub struct MoBlock(pub TermId);

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

#[derive(From, Clone, Debug)]
pub enum Term<Ref> {
    Meta(MetaT<TermId>),
    Internal(Internal),
    Sealed(Sealed<TermId>),
    Ann(Ann<TermId, TermId>),
    Hole(Hole),
    #[from(ignore)]
    Var(Ref),
    Triv(Triv),
    Cons(Cons<TermId, TermId>),
    Abs(Abs<PatId, TermId>),
    App(App<TermId, TermId>),
    Fix(Fix<PatId, TermId>),
    Pi(Pi),
    // Arrow(Arrow),
    // Forall(Forall),
    Sigma(Sigma),
    // Prod(Prod),
    // Exists(Exists),
    Thunk(Thunk<TermId>),
    Force(Force<TermId>),
    Ret(Return<TermId>),
    Do(Bind<PatId, TermId, TermId>),
    Let(Let<PatId, TermId, TermId>),
    // UseLet(UseBind),
    MoBlock(MoBlock),
    Data(Data),
    CoData(CoData),
    Ctor(Ctor<TermId>),
    Match(Match<TermId, PatId, TermId>),
    CoMatch(CoMatch<TermId>),
    Dtor(Dtor<TermId>),
    Lit(Literal),
}

/* -------------------------------- TopLevel -------------------------------- */

#[derive(Clone, Debug)]
pub struct AliasBody {
    pub binder: PatId,
    pub bindee: TermId,
}

#[derive(Clone, Debug)]
pub struct AliasHead {
    pub binder: PatId,
    pub ty: Option<TermId>,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub name: Option<NameRef<VarName>>,
    pub top: TopLevel,
}

// #[derive(Clone, Debug)]
// pub struct UseBlock {
//     pub uses: UsePath,
//     pub top: TopLevel,
// }

#[derive(Clone, Debug)]
pub struct Exec(pub TermId);

#[derive(Clone, From, Debug)]
pub enum Declaration {
    Meta(MetaT<DeclId>),
    AliasBody(AliasBody),
    AliasHead(AliasHead),
    Module(Module),
    // UseBlock(UseBlock),
    Exec(Exec),
}

#[derive(From, Clone, Debug)]
pub enum ReplInput {
    Declaration(Modifiers<Declaration>),
    Term(TermId),
}

#[derive(Clone, Debug)]
pub struct TopLevel(pub Vec<DeclId>);
impl AddAssign for TopLevel {
    fn add_assign(&mut self, rhs: TopLevel) {
        self.0.extend(rhs.0);
    }
}

/* -------------------------------- Primitive ------------------------------- */

/// Primitive terms injected by desugaring.
///
/// Collects those terms that are `Internal` within the bitter syntax so name
/// resolution can treat them as primitives and avoid accidental capture.
///
/// To add a new primitive term, add a field here and follow instructions at
/// [`crate::scoped::syntax::PrimDefs`].
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
