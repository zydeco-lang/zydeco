use crate::dynamics::syntax as ds;
pub use crate::syntax::{env::Env, *};
use indexmap::IndexMap;
use std::io::{BufRead, Write};
use std::rc::Rc;
use zydeco_derive::EnumGenerator;

/* ---------------------------------- Term ---------------------------------- */

#[derive(EnumGenerator, Clone)]
pub enum TermValue {
    Var(TermV),
    Thunk(Thunk<RcComp>),
    Ctor(Ctor<CtorV, RcValue>),
    Literal(Literal),
    SemValue(ds::TermValue)
}
type RcValue = Rc<TermValue>;
impl ValueT for TermValue {}

pub type PrimComp = fn(
    Vec<ds::TermValue>,
    &mut (dyn BufRead),
    &mut (dyn Write),
    &[String],
) -> Result<TermComputation, i32>;

#[derive(Clone)]
pub struct Prim {
    pub arity: u64,
    pub body: PrimComp,
}

#[derive(EnumGenerator, Clone)]
pub enum TermComputation {
    Ret(Ret<RcValue>),
    Force(Force<RcValue>),
    Let(Let<TermV, RcValue, RcComp>),
    Do(Do<TermV, RcComp, RcComp>),
    Rec(Rec<TermV, RcComp>),
    Match(Match<CtorV, TermV, RcValue, RcComp>),
    CoMatch(CoMatch<DtorV, TermV, RcComp>),
    Dtor(Dtor<RcComp, DtorV, RcValue>),
    Prim(Prim),
}
type RcComp = Rc<TermComputation>;
impl ComputationT for TermComputation {}

#[derive(EnumGenerator, Clone)]
pub enum Term {
    Val(TermValue),
    Comp(TermComputation),
}

/* --------------------------------- Module --------------------------------- */

#[derive(Clone)]
pub struct Module {
    pub name: Option<String>,
    pub define: IndexMap<TermV, TermValue>,
    pub entry: TermComputation,
}
