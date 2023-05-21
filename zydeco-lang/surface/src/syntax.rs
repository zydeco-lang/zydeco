//! The surface syntax of zydeco is defined in this module.

use crate::arena::*;
pub use codespan::Span;
use zydeco_derive::IntoEnum;

/* --------------------------------- Binder --------------------------------- */

pub struct NameDef(pub String);
pub struct NameRef(pub Vec<String>, pub String);

/* ----------------------------- Bi-Directional ----------------------------- */

/// `(...: t)` for analyze mode motivator
pub struct Annotation<Term, Type> {
    pub term: Term,
    pub ty: Type,
}
/// `_` for synthesize mode motivator
pub struct Hole;

/* --------------------------------- Pattern -------------------------------- */

#[derive(IntoEnum)]
pub enum Pattern {
    Var(NameDef),
    Ann(Annotation<PatternId, TermId>),
    Hole(Hole),
}

/* ---------------------------------- Term ---------------------------------- */

/// general binding structure
pub struct GenBind {
    pub rec: bool,
    pub fun: bool,
    pub binder: PatternId,
    pub params: Vec<PatternId>,
    pub ty: Option<TermId>,
    pub bindee: Option<TermId>,
}

/// literals in term
#[derive(IntoEnum)]
pub enum Literal {
    Int(i64),
    String(String),
    Char(char),
}

/// any binding structure
pub struct Abstraction<Tail>(pub Vec<PatternId>, pub Tail);
/// any application
pub struct Application(pub TermId, pub Vec<TermId>);

/// `{ b }` has type `Thunk B`
pub struct Thunk(pub TermId);
/// `! a` has type `B` where `A = Thunk B`
pub struct Force(pub TermId);

/// `ret a` has type `Ret A`
pub struct Return(pub TermId);
/// `do x <- b; ...`
pub struct Bind<Tail> {
    pub binder: PatternId,
    pub bindee: TermId,
    pub tail: Tail,
}
/// `let x = a in ...`
pub struct PureBind<Tail> {
    pub bind: GenBind,
    pub tail: Tail,
}

/// `rec (x: A) -> b`
pub struct Recursion(pub PatternId, pub TermId);

/// `C(a_1, ...)`
pub struct Constructor(pub NameRef, pub Vec<TermId>);
/// `match a | C_1(x_11, ...) -> b_1 | ...`
pub struct Match<Tail> {
    pub scrut: TermId,
    pub arms: Vec<Matcher<Tail>>,
}
pub struct Matcher<Tail> {
    pub name: NameRef,
    pub binder: PatternId,
    pub tail: Tail,
}

/// `comatch | .d_1(x_11, ...) -> b_1 | ...`
pub struct CoMatch<Tail> {
    pub arms: Vec<CoMatcher<Tail>>,
}
pub struct CoMatcher<Tail> {
    pub name: NameRef,
    pub binder: PatternId,
    pub tail: Tail,
}
/// `b .d(a_1, ...)`
pub struct Destructor(pub TermId, pub NameRef, pub Vec<TermId>);

#[derive(IntoEnum)]
pub enum Term {
    Ann(Annotation<TermId, TermId>),
    Hole(Hole),
    Var(NameRef),
    Lit(Literal),
    Abs(Abstraction<TermId>),
    App(Application),
    Thunk(Thunk),
    Force(Force),
    Ret(Return),
    Do(Bind<TermId>),
    Let(PureBind<TermId>),
    Rec(Recursion),
    Ctor(Constructor),
    Match(Match<TermId>),
    CoMatch(CoMatch<TermId>),
    Dtor(Destructor),
}

/* -------------------------------- TopLevel -------------------------------- */
pub struct TypeDef {
    pub name: NameDef,
    pub params: Vec<PatternId>,
    /// `data` or `codata`
    pub kind: TermId,
    pub arms: Vec<TypeArm>,
}
pub struct TypeArm(pub NameDef, pub Vec<TermId>, pub Option<TermId>);

pub struct Define(pub GenBind);

pub struct Module {
    pub name: NameDef,
    pub top: TopLevel,
}

pub struct UseAll;

pub struct UseCluster {
    pub path: NameRef,
    pub cluster: Vec<UseDef>,
}

pub enum UseDef {
    Name(NameRef),
    UseAll(UseAll),
    Cluster(UseCluster),
}

pub struct Main(pub TermId);

#[derive(IntoEnum)]
pub enum Declaration {
    Module(Module),
    UseDef(UseDef),
    Type(TypeDef),
    Define(Define),
    Main(Main),
}

pub struct Modifiers<T> {
    pub public: bool,
    pub external: bool,
    pub inner: T,
}

pub struct Sp<T> {
    pub span: Span,
    pub inner: T,
}

pub struct TopLevel(pub Vec<Modifiers<Declaration>>);
