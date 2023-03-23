pub use crate::{syntax::Span, syntax::*};
use zydeco_derive::EnumGenerator;

/* ---------------------------------- Kind ---------------------------------- */

pub use crate::syntax::Kind;

/* ---------------------------------- Type ---------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeApp(pub BoxType, pub BoxType);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Arrow(pub BoxType, pub BoxType);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Forall(pub Vec<(TypeV, Kind)>, pub BoxType);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Exists(pub Vec<(TypeV, Kind)>, pub BoxType);

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Basic(TCtor),
    App(TypeApp),
    Arrow(Arrow),
    Forall(Forall),
    Exists(Exists),
}
pub type BoxType = Box<Span<Type>>;
impl TypeT for Type {}

/* ---------------------------------- Term ---------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExistsVal(pub Type, pub BoxValue);

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum TermValue {
    TermAnn(TermAnn<BoxValue, Span<Type>>),
    Var(TermV),
    Thunk(Thunk<BoxComp>),
    Ctor(Ctor<CtorV, Span<TermValue>>),
    Literal(Literal),
    ExistsVal(ExistsVal),
}
pub type BoxValue = Box<Span<TermValue>>;
impl ValueT for TermValue {}

pub type TermPattern = (TermV, Option<Span<Type>>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Abstraction {
    pub params: Vec<TermPattern>,
    pub body: BoxComp,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Application {
    pub body: BoxComp,
    pub arg: BoxValue,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GenLet {
    pub rec: bool,
    pub fun: bool,
    pub name: TermPattern,
    pub params: Vec<TermPattern>,
    pub def: Box<Span<Term>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Let {
    pub gen: GenLet,
    pub body: BoxComp,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypFun {
    pub params: Vec<(TypeV, Kind)>,
    pub body: BoxComp,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypApp {
    pub body: BoxComp,
    pub arg: Box<Span<Type>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MatchExists {
    pub scrut: BoxValue,
    pub ty: TypeV,
    pub tv: TermV,
    pub tc: BoxComp,
}

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum TermComputation {
    TermAnn(TermAnn<BoxComp, Span<Type>>),
    Ret(Ret<BoxValue>),
    Force(Force<BoxValue>),
    Let(Let),
    Do(Do<TermPattern, BoxComp, BoxComp>),
    Rec(Rec<TermPattern, BoxComp>),
    Match(Match<CtorV, TermV, BoxValue, Span<TermComputation>>),
    Abs(Abstraction),
    App(Application),
    CoMatch(CoMatch<DtorV, TermV, Span<TermComputation>>),
    Dtor(Dtor<BoxComp, DtorV, Span<TermValue>>),
    TypFun(TypFun),
    TypApp(TypApp),
    MatchExists(MatchExists),
}
pub type BoxComp = Box<Span<TermComputation>>;
impl ComputationT for TermComputation {}

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Value(TermValue),
    Computation(TermComputation),
}

/* --------------------------------- Module --------------------------------- */

pub type Define = GenLet;

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum Declaration {
    Data(Data<TypeV, CtorV, Span<Type>>),
    Codata(Codata<TypeV, DtorV, Span<Type>>),
    Define(Define),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Module {
    pub name: Option<String>,
    pub declarations: Vec<DeclSymbol<Declaration>>,
    pub entry: Span<TermComputation>,
}
