//! Desugaring of the zydeco surface syntax.

pub use crate::syntax::*;
use crate::{arena::*, textual::syntax as t};
use derive_more::{AddAssign, From};
use std::ops::AddAssign;
pub use zydeco_syntax::*;
use zydeco_utils::cells::MultiCell;
pub use zydeco_utils::span::{LocationCtx, Sp, Span};

/* ------------------------------- Identifier ------------------------------- */

new_key_type! {
    pub struct DefId;
    pub struct PatId;
    pub struct TermId;
    pub struct DeclId;
}
impl DefPtr for DefId {}
impl PatPtr for PatId {}
impl TermPtr for TermId {}
impl DeclPtr for DeclId {}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
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
    Unit(Unit),
    Cons(Cons<PatId, PatId>),
}

#[derive(From, Clone, Debug)]
pub enum CoPatternItem {
    Pat(PatId),
    Dtor(DtorName),
}

/* ---------------------------------- Term ---------------------------------- */

/// `rec (x: A) -> b`
#[derive(Clone, Debug)]
pub struct Rec(pub PatId, pub TermId);

/// `pi (x: A) -> B`
#[derive(Clone, Debug)]
pub struct Pi(pub PatId, pub TermId);

/// `sigma (x: A) . A'`
#[derive(Clone, Debug)]
pub struct Sigma(pub PatId, pub TermId);

/// `ret a` has type `Ret A`
#[derive(Clone, Debug)]
pub struct Ret(pub TermId);
/// `do x <- b; ...`
#[derive(Clone, Debug)]
pub struct Bind {
    pub binder: PatId,
    pub bindee: TermId,
    pub tail: TermId,
}
/// `let x = a in ...`
#[derive(Clone, Debug)]
pub struct PureBind {
    pub binder: PatId,
    pub bindee: TermId,
    pub tail: TermId,
}

// /// `use let x = a in ...`
// #[derive(Clone, Debug)]
// pub struct UseBind {
//     pub uses: UsePath,
//     pub tail: TermId,
// }

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

/// `match a | C_1 p -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct Match {
    pub scrut: TermId,
    pub arms: Vec<Matcher>,
}
#[derive(Clone, Debug)]
pub struct Matcher {
    pub binder: PatId,
    pub tail: TermId,
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

/// `comatch | .d_1 -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct CoMatch {
    pub arms: Vec<CoMatcher>,
}
#[derive(Clone, Debug)]
pub struct CoMatcher {
    pub dtor: DtorName,
    pub tail: TermId,
}

#[derive(From, Clone, Debug)]
pub enum Term<Ref> {
    Internal(Internal),
    Sealed(Sealed<TermId>),
    Ann(Ann<TermId, TermId>),
    Hole(Hole),
    #[from(ignore)]
    Var(Ref),
    Unit(Unit),
    Cons(Cons<TermId, TermId>),
    Abs(Abs<PatId, TermId>),
    App(App<TermId, TermId>),
    Rec(Rec),
    Pi(Pi),
    // Arrow(Arrow),
    // Forall(Forall),
    Sigma(Sigma),
    // Prod(Prod),
    // Exists(Exists),
    Thunk(Thunk<TermId>),
    Force(Force<TermId>),
    Ret(Ret),
    Do(Bind),
    Let(PureBind),
    // UseLet(UseBind),
    Data(Data),
    CoData(CoData),
    Ctor(Ctor<TermId>),
    Match(Match),
    CoMatch(CoMatch),
    Dtor(Dtor<TermId>),
    Lit(Literal),
}

/* -------------------------------- TopLevel -------------------------------- */

#[derive(Clone, Debug)]
pub struct Alias {
    pub binder: PatId,
    pub bindee: TermId,
}

#[derive(Clone, Debug)]
pub struct Extern {
    pub binder: PatId,
    pub params: Vec<PatId>,
    pub ty: Option<TermId>,
}

// #[derive(Clone, Debug)]
// pub struct Layer {
//     pub name: Option<NameRef<VarName>>,
//     pub uses: Vec<Modifiers<UsePath>>,
//     pub top: TopLevel,
// }

// #[derive(From, Clone, Debug)]
// pub struct UseDef(pub UsePath);

// #[derive(Clone, Debug)]
// pub struct UseBlock {
//     pub uses: UsePath,
//     pub top: TopLevel,
// }

#[derive(Clone, Debug)]
pub struct Main(pub TermId);

#[derive(Clone, From, Debug)]
pub enum Declaration {
    Alias(Alias),
    Extern(Extern),
    // Layer(Layer),
    // UseDef(UseDef),
    // UseBlock(UseBlock),
    Main(Main),
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

/* ---------------------------------- Arena --------------------------------- */

#[derive(Default, Debug, AddAssign)]
pub struct Arena {
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatId, Pattern>,
    pub terms: ArenaAssoc<TermId, Term<NameRef<VarName>>>,
    pub decls: ArenaAssoc<DeclId, Modifiers<Declaration>>,
}

// Fixme: keep a bidirectional map instead of duplicating the spans
// Fixme: unify allocation for all entities
pub type SpanArena = ArenaGen<Span, DefId, PatId, TrivId, TermId, DeclId>;

/* -------------------------------- Primitive ------------------------------- */

#[derive(Default, AddAssign)]
pub struct PrimTerm {
    /// VType kind
    pub vtype: MultiCell<TermId>,
    /// CType kind
    pub ctype: MultiCell<TermId>,
    /// Thunk type
    pub thunk: MultiCell<TermId>,
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
    /// OS type
    pub os: MultiCell<TermId>,
    /// Monad type
    pub monad: MultiCell<TermId>,
    /// Algebra type
    pub algebra: MultiCell<TermId>,
}
