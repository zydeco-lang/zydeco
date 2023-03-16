// use lalrpop_util::lalrpop_mod;
// lalrpop_mod!(pub parser, "/parse/next/parser.rs");
pub use crate::{syntax::Ann, syntax::*};
use zydeco_derive::EnumGenerator;

/* ---------------------------------- Kind ---------------------------------- */

pub use crate::syntax::Kind;

/* ---------------------------------- Type ---------------------------------- */


#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeApp(pub Box<Type>, pub Box<Type>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeArrow(pub Box<Type>, pub Box<Type>);

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Basic(TCtor),
    App(TypeApp),
    Arrow(TypeArrow),
}
impl TypeT for Type {}

/* ---------------------------------- Term ---------------------------------- */

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum TermValue {
    TermAnn(TermAnn<Box<Ann<TermValue>>, Ann<Type>>),
    Var(TermV),
    Thunk(Thunk<Box<Ann<TermComputation>>>),
    Ctor(Ctor<CtorV, Ann<TermValue>>),
    Literal(Literal),
}
impl ValueT for TermValue {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    pub params: Vec<(TermV, Option<Ann<Type>>)>,
    pub body: Box<Ann<TermComputation>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Application {
    pub expr_in: Box<Ann<TermComputation>>,
    pub args: Vec<Ann<TermValue>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Let {
    pub binder: TermV,
    pub ty_ann: Option<Ann<Type>>,
    pub def: Box<Ann<TermValue>>,
    pub body: Box<Ann<TermComputation>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Do {
    pub binder: TermV,
    pub ty_ann: Option<Ann<Type>>,
    pub task: Box<Ann<TermComputation>>,
    pub body: Box<Ann<TermComputation>>,
}

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum TermComputation {
    TermAnn(TermAnn<Box<Ann<TermComputation>>, Ann<Type>>),
    Ret(Ret<Box<Ann<TermValue>>>),
    Force(Force<Box<Ann<TermValue>>>),
    Let(Let),
    Do(Do),
    Rec(Rec<TermV, Box<Ann<TermComputation>>>),
    Match(Match<CtorV, TermV, Box<Ann<TermValue>>, Ann<TermComputation>>),
    Function(Function),
    Application(Application),
    CoMatch(CoMatch<DtorV, TermV, Ann<TermComputation>>),
    Dtor(Dtor<Box<Ann<TermComputation>>, DtorV, Ann<TermValue>>),
}
impl ComputationT for TermComputation {}

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Val(TermValue),
    Comp(TermComputation),
}

/* --------------------------------- Module --------------------------------- */

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum Declaration {
    Data(Data<TypeV, CtorV, Ann<Type>>),
    Codata(Codata<TypeV, DtorV, Ann<Type>>),
    Define(Define<TermV, Ann<Type>, Box<Ann<TermValue>>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Module {
    pub name: Option<String>,
    pub declarations: Vec<Declaration>,
    pub entry: Ann<TermComputation>,
}
