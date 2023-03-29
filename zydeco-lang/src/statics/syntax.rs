use crate::syntax::env::Env;
use crate::utils::span::{span, Span};
use std::rc::Rc;
use zydeco_derive::EnumGenerator;

pub use crate::syntax::*;

/* ---------------------------------- Kind ---------------------------------- */

pub use crate::syntax::Kind;

/* ---------------------------------- Type ---------------------------------- */

#[derive(Clone, Debug)]
pub struct Type {
    pub app: TypeApp<TypeV, RcType>,
    pub kd: Option<Kind>,
    pub env: Env<TypeV, Type>,
}
pub type RcType = Rc<Span<Type>>;
impl TypeT for Type {}

impl Type {
    pub fn internal(name: &'static str, args: Vec<RcType>) -> Self {
        TypeApp::internal(name, args).into()
    }
    pub fn make_thunk(arg: RcType) -> Self {
        TypeApp::internal("Thunk_U", vec![arg]).into()
    }
    pub fn make_ret(arg: RcType) -> Self {
        TypeApp::internal("Ret_F", vec![arg]).into()
    }
}
impl TypeApp<TypeV, RcType> {
    pub fn internal(name: &'static str, args: Vec<RcType>) -> Self {
        TypeApp { tvar: TypeV::new(name.into(), span(0, 0)), args }
    }
    pub fn elim_thunk(&self) -> Option<Type> {
        if self.tvar.name() == "Thunk_U" {
            Some(self.args.first().unwrap().inner_ref().clone())
        } else {
            None
        }
    }
    pub fn elim_ret(&self) -> Option<Type> {
        if self.tvar.name() == "Ret_F" {
            Some(self.args.first().unwrap().inner_ref().clone())
        } else {
            None
        }
    }
}
impl From<TypeApp<TypeV, RcType>> for Type {
    fn from(app: TypeApp<TypeV, RcType>) -> Self {
        Self { app, kd: None, env: Env::new() }
    }
}

/* ---------------------------------- Term ---------------------------------- */

#[derive(EnumGenerator, Clone, Debug)]
pub enum TermValue {
    TermAnn(TermAnn<RcValue, RcType>),
    Var(TermV),
    Thunk(Thunk<RcComp>),
    Ctor(Ctor<CtorV, RcValue>),
    Literal(Literal),
}
pub type RcValue = Rc<Span<TermValue>>;
impl ValueT for TermValue {}

#[derive(EnumGenerator, Clone, Debug)]
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

#[derive(EnumGenerator, Clone, Debug)]
pub enum Term {
    Value(TermValue),
    Computation(TermComputation),
}

/* --------------------------------- Module --------------------------------- */

#[derive(Clone, Debug)]
pub struct Module {
    pub name: Option<String>,
    pub data: Vec<DeclSymbol<Data<TypeV, CtorV, RcType>>>,
    pub codata: Vec<DeclSymbol<Codata<TypeV, DtorV, RcType>>>,
    pub define: Vec<DeclSymbol<Define<TermV, RcValue>>>,
    pub define_ext: Vec<DeclSymbol<Define<(TermV, RcType), ()>>>,
    pub entry: Span<TermComputation>,
}
