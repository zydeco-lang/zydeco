//! Desugaring of the zydeco surface syntax.

use std::ops::AddAssign;

use crate::textual::{
    syntax as t,
    syntax::{CoPatternId, DefId, PatternId, TermId},
};
use derive_more::From;
use zydeco_syntax::*;
pub use zydeco_utils::{
    arena::*,
    span::{LocationCtx, Sp, Span},
};

/* --------------------------------- Pattern -------------------------------- */

#[derive(From, Clone, Debug)]
pub enum Pattern {
    Ann(Annotation<PatternId, TermId>),
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<PatternId>),
    Paren(Paren<PatternId>),
}

#[derive(From, Clone, Debug)]
pub enum CoPattern {
    Pat(PatternId),
    Dtor(DtorName),
    App(App<CoPatternId>),
}

/* ---------------------------------- Term ---------------------------------- */

/// any binding structure
#[derive(Clone, Debug)]
pub struct Abs<Tail>(pub CoPatternId, pub Tail);
/// `rec (x: A) -> b`
#[derive(Clone, Debug)]
pub struct Rec(pub PatternId, pub TermId);

/// `pi (x: A) -> B`
#[derive(Clone, Debug)]
pub struct Pi(pub CoPatternId, pub TermId);
/// `a -> b`
#[derive(Clone, Debug)]
pub struct Arrow(pub TermId, pub TermId);
/// `forall (x: A) . B`
#[derive(Clone, Debug)]
pub struct Forall(pub CoPatternId, pub TermId);
/// `exists (x: A) . B`
#[derive(Clone, Debug)]
pub struct Exists(pub CoPatternId, pub TermId);

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
    pub binder: PatternId,
    pub bindee: TermId,
    pub tail: Tail,
}
/// `let x = a in ...`
#[derive(Clone, Debug)]
pub struct PureBind<Tail> {
    pub binder: PatternId,
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
    pub binder: PatternId,
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
    pub params: Option<CoPatternId>,
    pub out: TermId,
}

/// `comatch | .d_1 -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct CoMatch<Tail> {
    pub arms: Vec<CoMatcher<Tail>>,
}
#[derive(Clone, Debug)]
pub struct CoMatcher<Tail> {
    pub params: CoPatternId,
    pub tail: Tail,
}

#[derive(From, Clone, Debug)]
pub enum Term<Ref> {
    Ann(Annotation<TermId, TermId>),
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
pub struct Define {
    pub binder: PatternId,
    pub bindee: TermId,
}

#[derive(Clone, Debug)]
pub struct Extern {
    pub binder: PatternId,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub name: t::NameDef<ModName>,
    pub top: Option<TopLevel>,
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
    Define(Define),
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

/* --------------------------------- Context -------------------------------- */

#[derive(Debug)]
pub struct Ctx {
    // span arena
    pub spans: t::SpanArena,
    // arenas
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatternId, Pattern>,
    pub copats: ArenaAssoc<CoPatternId, CoPattern>,
    pub terms: ArenaAssoc<TermId, Term<t::NameRef<VarName>>>,
}

impl Ctx {
    // pub fn def(&mut self, def: Sp<VarName>) -> DefId {
    //     let id = self.spans.defs.alloc(def.info);
    //     self.defs.insert(id, def.inner);
    //     id
    // }
    // pub fn pat(&mut self, pat: Sp<Pattern>) -> PatternId {
    //     let id = self.spans.pats.alloc(pat.info);
    //     self.pats.insert(id, pat.inner);
    //     id
    // }
    // pub fn copat(&mut self, copat: Sp<CoPattern>) -> CoPatternId {
    //     let id = self.spans.copats.alloc(copat.info);
    //     self.copats.insert(id, copat.inner);
    //     id
    // }
    // pub fn term(&mut self, term: Sp<Term<t::NameRef<VarName>>>) -> TermId {
    //     let id = self.spans.terms.alloc(term.info);
    //     self.terms.insert(id, term.inner);
    //     id
    // }
}

impl AddAssign<Ctx> for Ctx {
    fn add_assign(&mut self, rhs: Ctx) {
        self.defs += rhs.defs;
        self.pats += rhs.pats;
        self.copats += rhs.copats;
        self.terms += rhs.terms;
        self.spans += rhs.spans;
    }
}
