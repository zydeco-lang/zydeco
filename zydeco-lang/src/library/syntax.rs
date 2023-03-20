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
    Thunk(Thunk<TC>),
    Ctor(Ctor<CtorV, TV>),
    Literal(Literal),
}
type TV = Rc<TermValue>;
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
    Ret(Ret<TV>),
    Force(Force<TV>),
    Let(Let<TermV, TV, TC>),
    Do(Do<TermV, TC, TC>),
    Rec(Rec<TermV, TC>),
    Match(Match<CtorV, TermV, TV, TC>),
    CoMatch(CoMatch<DtorV, TermV, TC>),
    Dtor(Dtor<TC, DtorV, TV>),
    Prim(Prim),
}
type TC = Rc<TermComputation>;
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
