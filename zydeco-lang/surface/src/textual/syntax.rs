//! The surface syntax of zydeco is defined in this module.

pub use super::arena::*;
use zydeco_derive::IntoEnum;
pub use zydeco_utils::span::Span;

/* --------------------------------- Binder --------------------------------- */

pub struct ModName(pub String);
pub struct VarName(pub String);
pub struct CtorName(pub String);
pub struct DtorName(pub String);
#[derive(IntoEnum)]
pub enum TypeArmName {
    Ctor(CtorName),
    Dtor(DtorName),
}

pub struct NameDef<T>(pub T);
pub struct NameRef<T>(pub Vec<ModName>, pub T);

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
    Var(DefId),
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

/// any binding structure
pub struct Abstraction<Tail>(pub Vec<PatternId>, pub Tail);
/// any application
pub struct Application(pub TermId, pub Vec<TermId>);
/// `rec (x: A) -> b`
pub struct Recursion(pub PatternId, pub TermId);

// `pi (x: A) -> B`
pub struct Pi(pub Vec<PatternId>, pub TermId);
// `a -> b`
pub struct Arrow(pub TermId, pub TermId);
// `forall (x: A) . B`
pub struct Forall(pub Vec<PatternId>, pub TermId);
// `exists (x: A) . B`
pub struct Exists(pub Vec<PatternId>, pub TermId);

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
    pub binding: GenBind,
    pub tail: Tail,
}

/// `C(a_1, ...)`
pub struct Constructor(pub NameRef<CtorName>, pub Vec<TermId>);
/// `match a | C_1(x_11, ...) -> b_1 | ...`
pub struct Match<Tail> {
    pub scrut: TermId,
    pub arms: Vec<Matcher<Tail>>,
}
pub struct Matcher<Tail> {
    pub name: NameRef<CtorName>,
    pub binders: Vec<PatternId>,
    pub tail: Tail,
}

/// `comatch | .d_1(x_11, ...) -> b_1 | ...`
pub struct CoMatch<Tail> {
    pub arms: Vec<CoMatcher<Tail>>,
}
pub struct CoMatcher<Tail> {
    pub name: NameRef<DtorName>,
    pub binders: Vec<PatternId>,
    pub tail: Tail,
}
/// `b .d(a_1, ...)`
pub struct Destructor(pub TermId, pub NameRef<DtorName>, pub Vec<TermId>);

/// literals in term
#[derive(IntoEnum)]
pub enum Literal {
    Int(i64),
    String(String),
    Char(char),
}

#[derive(IntoEnum)]
pub enum Term {
    Ann(Annotation<TermId, TermId>),
    Hole(Hole),
    #[skip]
    Var(NameRef<VarName>),
    Abs(Abstraction<TermId>),
    App(Application),
    Rec(Recursion),
    Pi(Pi),
    Arrow(Arrow),
    Forall(Forall),
    Exists(Exists),
    Thunk(Thunk),
    Force(Force),
    Ret(Return),
    Do(Bind<TermId>),
    Let(PureBind<TermId>),
    Ctor(Constructor),
    Match(Match<TermId>),
    CoMatch(CoMatch<TermId>),
    Dtor(Destructor),
    Lit(Literal),
}

/* -------------------------------- TopLevel -------------------------------- */

pub enum TypeDefHead {
    Data,
    CoData,
}

pub struct TypeDef {
    /// `data` or `codata`
    pub head: TypeDefHead,
    pub name: DefId,
    pub params: Vec<PatternId>,
    pub arms: Option<Vec<TypeArm>>,
}
pub struct TypeArm {
    pub name: NameDef<TypeArmName>,
    pub args: Vec<TermId>,
    pub out: Option<TermId>,
}

pub struct Define(pub GenBind);

pub struct Module {
    pub name: NameDef<ModName>,
    pub top: Option<TopLevel>,
}

pub struct UseAll;
pub struct UseAlias(pub VarName, pub VarName);
pub struct UseCluster(pub Vec<UseDef>);
#[derive(IntoEnum)]
pub enum UseEnum {
    Name(VarName),
    Alias(UseAlias),
    All(UseAll),
    Cluster(UseCluster),
}
pub struct UseDef(pub NameRef<UseEnum>);

pub struct Main(pub TermId);

#[derive(IntoEnum)]
pub enum Declaration {
    Type(TypeDef),
    Define(Define),
    Module(Module),
    UseDef(UseDef),
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

#[derive(IntoEnum)]
pub enum ReplInput {
    Declaration(Modifiers<Declaration>),
    Term(TermId),
}

pub struct TopLevel(pub Vec<Modifiers<Declaration>>);
