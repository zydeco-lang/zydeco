pub use crate::{syntax::Span, syntax::*};
use zydeco_derive::EnumGenerator;

/* ---------------------------------- Kind ---------------------------------- */

pub use crate::syntax::Kind;

/* ---------------------------------- Type ---------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeApp(pub TT, pub TT);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeAbs(pub TT, pub TT);

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Basic(TCtor),
    App(TypeApp),
    Abs(TypeAbs),
}
type TT = Box<Span<Type>>;
impl TypeT for Type {}

/* ---------------------------------- Term ---------------------------------- */

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum TermValue {
    TermAnn(TermAnn<BoxValue, Span<Type>>),
    Var(TermV),
    Thunk(Thunk<BoxComp>),
    Ctor(Ctor<CtorV, Span<TermValue>>),
    Literal(Literal),
}
type BoxValue = Box<Span<TermValue>>;
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
    pub def: BoxValue,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Let {
    pub gen: GenLet,
    pub body: BoxComp,
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
}
type BoxComp = Box<Span<TermComputation>>;
impl ComputationT for TermComputation {}

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Value(TermValue),
    Computation(TermComputation),
}

/* --------------------------------- Module --------------------------------- */

type Define = GenLet;

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
