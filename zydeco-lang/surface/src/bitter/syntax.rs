//! Desugaring of the zydeco surface syntax.

use crate::{arena::*, textual::syntax as t};
use derive_more::From;
use std::ops::AddAssign;
pub use zydeco_syntax::*;
pub use zydeco_utils::span::{LocationCtx, Sp, Span};

/* ------------------------------- Identifier ------------------------------- */

new_key_type! {
    pub struct DefId;
    pub struct PatId;
    pub struct CoPatId;
    pub struct TermId;
}
impl DefPtr for DefId {}
impl PatPtr for PatId {}
impl CoPatPtr for CoPatId {}
impl TermPtr for TermId {}

/* --------------------------------- Binder --------------------------------- */

pub use t::{NameDef, NameRef};

/* --------------------------------- Pattern -------------------------------- */

#[derive(From, Clone, Debug)]
pub enum Pattern {
    Ann(Ann<PatId, TermId>),
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<PatId>),
    Paren(Paren<PatId>),
}

#[derive(From, Clone, Debug)]
pub enum CoPattern {
    Pat(PatId),
    Dtor(DtorName),
    App(App<CoPatId>),
}

/* ---------------------------------- Term ---------------------------------- */

/// sealed term which is abstract, only eq to itself during tyck
#[derive(Clone, Debug)]
pub struct Sealed(pub TermId);

/// any binding structure
#[derive(Clone, Debug)]
pub struct Abs<Tail>(pub CoPatId, pub Tail);
/// `rec (x: A) -> b`
#[derive(Clone, Debug)]
pub struct Rec(pub PatId, pub TermId);

/// `pi (x: A) -> B`
#[derive(Clone, Debug)]
pub struct Pi(pub CoPatId, pub TermId);
// /// `a -> b`
// #[derive(Clone, Debug)]
// pub struct Arrow(pub TermId, pub TermId);
// /// `forall (x: A) . B`
// #[derive(Clone, Debug)]
// pub struct Forall(pub CoPatId, pub TermId);
/// `sigma (x: A) . A'`
#[derive(Clone, Debug)]
pub struct Sigma(pub CoPatId, pub TermId);
// /// `A * ...`
// #[derive(Clone, Debug)]
// pub struct Prod(pub Vec<TermId>);
// /// `exists (x: A) . A'`
// #[derive(Clone, Debug)]
// pub struct Exists(pub CoPatId, pub TermId);

/// `{ b }` has type `Thunk B`
#[derive(Clone, Debug)]
pub struct Thunk(pub TermId);
/// `! a` has type `B` where `A = Thunk B`
#[derive(Clone, Debug)]
pub struct Force(pub TermId);

/// `ret a` has type `Ret A`
#[derive(Clone, Debug)]
pub struct Return(pub TermId);
/// `do x <- b; ...`
#[derive(Clone, Debug)]
pub struct Bind<Tail> {
    pub binder: PatId,
    pub bindee: TermId,
    pub tail: Tail,
}
/// `let x = a in ...`
#[derive(Clone, Debug)]
pub struct PureBind<Tail> {
    pub binder: PatId,
    pub bindee: TermId,
    pub tail: Tail,
}

/// `use let x = a in ...`
#[derive(Clone, Debug)]
pub struct UseBind {
    pub uses: t::UsePath,
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

/// `match a | C_1 p -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct Match<Tail> {
    pub scrut: TermId,
    pub arms: Vec<Matcher<Tail>>,
}
#[derive(Clone, Debug)]
pub struct Matcher<Tail> {
    pub binder: PatId,
    pub tail: Tail,
}

/// `codata | .d_1 cp : ty | ... end`
#[derive(Clone, Debug)]
pub struct CoData {
    pub arms: Vec<CoDataArm>,
}
#[derive(Clone, Debug)]
pub struct CoDataArm {
    pub name: DtorName,
    pub params: Option<CoPatId>,
    pub out: TermId,
}

/// `comatch | .d_1 -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct CoMatch<Tail> {
    pub arms: Vec<CoMatcher<Tail>>,
}
#[derive(Clone, Debug)]
pub struct CoMatcher<Tail> {
    pub params: CoPatId,
    pub tail: Tail,
}

#[derive(From, Clone, Debug)]
pub enum Term<Ref> {
    Sealed(Sealed),
    Ann(Ann<TermId, TermId>),
    Hole(Hole),
    #[from(ignore)]
    Var(Ref),
    Paren(Paren<TermId>),
    Abs(Abs<TermId>),
    App(App<TermId>),
    Rec(Rec),
    Pi(Pi),
    // Arrow(Arrow),
    // Forall(Forall),
    Sigma(Sigma),
    // Prod(Prod),
    // Exists(Exists),
    Thunk(Thunk),
    Force(Force),
    Ret(Return),
    Do(Bind<TermId>),
    Let(PureBind<TermId>),
    UseLet(UseBind),
    Data(Data),
    CoData(CoData),
    Ctor(Ctor<TermId>),
    Match(Match<TermId>),
    CoMatch(CoMatch<TermId>),
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
    pub params: Option<CoPatId>,
    pub ty: Option<TermId>,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub name: VarName,
    pub top: TopLevel,
}

#[derive(From, Clone, Debug)]
pub struct UseDef(pub t::UsePath);

#[derive(Clone, Debug)]
pub struct UseBlock {
    pub uses: t::UsePath,
    pub top: TopLevel,
}

#[derive(Clone, Debug)]
pub struct Main(pub TermId);

#[derive(Clone, From, Debug)]
pub enum Declaration {
    Alias(Alias),
    Extern(Extern),
    Module(Module),
    UseDef(UseDef),
    UseBlock(UseBlock),
    Main(Main),
}

#[derive(Clone, Debug)]
pub struct Modifiers<T> {
    pub public: bool,
    pub inner: T,
}
impl<T> Modifiers<T> {
    pub fn try_map_ref<F, U, E>(&self, f: F) -> Result<Modifiers<U>, E>
    where
        F: FnOnce(&T) -> Result<U, E>,
    {
        let Modifiers { public, inner } = self;
        Ok(Modifiers { public: *public, inner: f(inner)? })
    }
}

#[derive(From, Clone, Debug)]
pub enum ReplInput {
    Declaration(Modifiers<Declaration>),
    Term(TermId),
}

#[derive(Clone, Debug)]
pub struct TopLevel(pub Vec<Modifiers<Declaration>>);
impl AddAssign<TopLevel> for TopLevel {
    fn add_assign(&mut self, rhs: TopLevel) {
        self.0.extend(rhs.0);
    }
}

/* --------------------------------- Context -------------------------------- */

pub type SpanArenaBitter = SpanArena<DefId, PatId, CoPatId, TermId>;

#[derive(Default, Debug)]
pub struct Ctx {
    // arenas
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatId, Pattern>,
    pub copats: ArenaAssoc<CoPatId, CoPattern>,
    pub terms: ArenaAssoc<TermId, Term<t::NameRef<VarName>>>,
}

impl AddAssign<Ctx> for Ctx {
    fn add_assign(&mut self, rhs: Ctx) {
        self.defs += rhs.defs;
        self.pats += rhs.pats;
        self.copats += rhs.copats;
        self.terms += rhs.terms;
    }
}
