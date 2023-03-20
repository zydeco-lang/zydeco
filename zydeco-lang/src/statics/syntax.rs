use std::{collections::HashMap, rc::Rc};
use zydeco_derive::EnumGenerator;

pub use crate::{syntax::Span, syntax::*};

/* ---------------------------------- Kind ---------------------------------- */

pub use crate::syntax::Kind;

/* ---------------------------------- Type ---------------------------------- */

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum Type {
    TypeAnn(TypeAnn<T, Span<Kind>>),
    TypeApp(TypeApp<TCtor, T>),
}
pub(crate) type T = Rc<Span<Type>>;
impl TypeT for Type {}

/* ---------------------------------- Term ---------------------------------- */

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum TermValue {
    TermAnn(TermAnn<TV, T>),
    Var(TermV),
    Thunk(Thunk<TC>),
    Ctor(Ctor<CtorV, TV>),
    Literal(Literal),
}
type TV = Rc<Span<TermValue>>;
impl ValueT for TermValue {}

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum TermComputation {
    TermAnn(TermAnn<TC, T>),
    Ret(Ret<TV>),
    Force(Force<TV>),
    Let(Let<TermV, TV, TC>),
    Do(Do<TermV, TC, TC>),
    Rec(Rec<TermV, TC>),
    Match(Match<CtorV, TermV, TV, TC>),
    CoMatch(CoMatch<DtorV, TermV, TC>),
    Dtor(Dtor<TC, DtorV, TV>),
}
type TC = Rc<Span<TermComputation>>;
impl ComputationT for TermComputation {}

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Val(TermValue),
    Comp(TermComputation),
}

/* --------------------------------- Module --------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Module {
    pub name: Option<String>,
    pub type_ctx: HashMap<TypeV, TypeArity<Kind>>,
    pub term_ctx: HashMap<TermV, T>,
    pub data: Vec<Span<Data<TypeV, CtorV, T>>>,
    pub codata: Vec<Span<Codata<TypeV, DtorV, T>>>,
    pub define: Vec<Span<Define<TermV, T, TV>>>,
    pub entry: Span<TermComputation>,
}
