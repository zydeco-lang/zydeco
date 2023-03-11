pub use crate::{syntax::Ann, syntax::*};
use enum_dispatch::enum_dispatch;
use std::{collections::HashMap, rc::Rc};

/* ---------------------------------- Kind ---------------------------------- */

pub use crate::syntax::Kind;

/* ---------------------------------- Type ---------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypePair (
    pub Box<Type>,
    pub Box<Type> 
);


#[enum_dispatch(TypeT)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Basic(TCtor),
    App(TypePair),
    Arr(TypePair),
}
type T = Box<Ann<Type>>;
impl TypeT for Type {}

/* ---------------------------------- Term ---------------------------------- */

#[enum_dispatch(ValueT)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TermValue {
    TermAnn(TermAnn<TV, T>),
    Var(TermV),
    Thunk(Thunk<TC>),
    Ctor(Ctor<CtorV, TV>),
    Literal(Literal),
}
type TV = Box<Ann<TermValue>>;
impl ValueT for TermValue {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    pub params : Vec<(TermV, Option<T>)>,
    pub body : TC
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Application {
    pub expr_in : TC,
    pub args : Vec<TV>
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Let {
    pub binder : TermV,
    pub ty_ann : Option<T>,
    pub def : TC,
    pub body : TC
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Do {
    pub binder : TermV,
    pub ty_ann : Option<T>,
    pub task : TV,
    pub body : TC
}

#[enum_dispatch(ComputationT)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TermComputation {
    TermAnn(TermAnn<TC, T>),
    Ret(Ret<TV>),
    Force(Force<TV>),
    Let(Let),
    Do(Do),
    Rec(Rec<TermV, TC>),
    Match(Match<CtorV, TermV, TV, TC>),
    Function(Function),
    Application(Application),
    CoMatch(CoMatch<DtorV, TermV, TC>),
    Dtor(Dtor<TC, DtorV, TV>),
}
type TC = Box<Ann<TermComputation>>;
impl ComputationT for TermComputation {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Val(TermValue),
    Comp(TermComputation),
}

/* --------------------------------- Module --------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Declaration {
    Data(Data<TypeV, CtorV, T>),
    Codata(Codata<TypeV, DtorV, T>),
    Define(Define<TermV, T, TV>)
}


#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Module {
    pub name: Option<String>,
    pub declarations : Vec<Declaration>,
    pub entry: Ann<TermComputation>,
}
