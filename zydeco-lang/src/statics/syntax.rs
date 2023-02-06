use crate::{syntax::*, utils::ann::Ann};
use enum_dispatch::enum_dispatch;
use std::rc::Rc;

/* ---------------------------------- Type ---------------------------------- */

#[enum_dispatch(TypeT)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    ThunkT(Ann<ThunkT<T>>),
    RetT(Ann<RetT<T>>),
    TCtor(Ann<TCtor<T>>),
}
type T = Rc<Type>;
impl TypeT for Type {}

/* ---------------------------------- Term ---------------------------------- */

#[enum_dispatch(ValueT)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TermValue {
    Annotation(Ann<Annotation<TV, T>>),
    Var(Ann<TermV>),
    Thunk(Ann<Thunk<TC>>),
    Ctor(Ann<Ctor<Ann<CtorV>, TV>>),
}
type TV = Rc<TermValue>;
impl ValueT for TermValue {}

#[enum_dispatch(ComputationT)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TermComputation {
    Annotation(Ann<Annotation<TC, T>>),
    Return(Ann<Ret<TV>>),
    Let(Ann<Let<Ann<TermV>, TV, TC>>),
    Do(Ann<Do<Ann<TermV>, TC>>),
    Match(Ann<Match<Ann<TermV>, TV, TC>>),
    CoMatch(Ann<CoMatch<Ann<TermV>, TC>>),
    Dtor(Ann<Dtor<Ann<DtorV>, TC, TV>>),
}
type TC = Rc<TermComputation>;
impl ComputationT for TermComputation {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Val(TermValue),
    Comp(TermComputation),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Module {
    pub name: Option<String>,
    pub decls: Vec<Declare>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Declare {
    Data(Ann<Data<Type>>),
    Codata(Ann<Codata<Type>>),
    Define(Ann<Define<Type, TermValue>>),
    Entry(Ann<TermComputation>),
}
