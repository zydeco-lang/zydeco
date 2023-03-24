use crate::utils::span::{span, Span};
use std::rc::Rc;
use zydeco_derive::EnumGenerator;

pub use crate::syntax::*;

/* ---------------------------------- Kind ---------------------------------- */

pub use crate::syntax::Kind;

/* ---------------------------------- Type ---------------------------------- */

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum Type {
    TypeAnn(TypeAnn<RcType, Kind>),
    TypeApp(TypeApp<TCtor, RcType>),
}
pub type RcType = Rc<Span<Type>>;
impl TypeT for Type {}

impl Type {
    pub fn internal(name: &'static str, args: Vec<RcType>) -> Self {
        TypeApp::internal(name, args).into()
    }
}
impl TypeApp<TCtor, RcType> {
    pub fn internal(name: &'static str, args: Vec<RcType>) -> Self {
        TypeApp { tctor: TCtor::Var(TypeV::new(name.into(), span(0, 0))), args }
    }
}

/* ---------------------------------- Term ---------------------------------- */

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum TermValue {
    TermAnn(TermAnn<RcValue, RcType>),
    Var(TermV),
    Thunk(Thunk<RcComp>),
    Ctor(Ctor<CtorV, RcValue>),
    Literal(Literal),
}
pub type RcValue = Rc<Span<TermValue>>;
impl ValueT for TermValue {}

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
pub enum TermComputation {
    TermAnn(TermAnn<RcComp, RcType>),
    Ret(Ret<RcValue>),
    Force(Force<RcValue>),
    Let(Let<TermV, RcValue, RcComp>),
    Do(Do<TermV, RcComp, RcComp>),
    Rec(Rec<TermV, RcComp>),
    Match(Match<CtorV, TermV, RcValue, RcComp>),
    CoMatch(CoMatch<DtorV, TermV, RcComp>),
    Dtor(Dtor<RcComp, DtorV, RcValue>),
}
pub type RcComp = Rc<Span<TermComputation>>;
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
    pub data: Vec<Data<TypeV, CtorV, RcType>>,
    pub codata: Vec<Codata<TypeV, DtorV, RcType>>,
    pub define: Vec<Define<TermV, RcValue>>,
    pub entry: Span<TermComputation>,
}
