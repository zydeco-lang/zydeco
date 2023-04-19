use crate::dynamics::syntax as ds;
pub use crate::syntax::{env::Env, *};
use im::Vector;
use std::{
    io::{BufRead, Write},
    rc::Rc,
};
use zydeco_derive::{FmtArgs, IntoEnum};

/* ---------------------------------- Term ---------------------------------- */

#[derive(IntoEnum, FmtArgs, Clone)]
pub enum SynVal {
    Var(TermV),
    Thunk(Thunk<RcComp>),
    Ctor(Ctor<CtorV, RcValue>),
    Literal(Literal),
    SemValue(ds::SemVal),
}
type RcValue = Rc<SynVal>;
impl ValueT for SynVal {}

pub type PrimComp =
    fn(Vec<ds::SemVal>, &mut (dyn BufRead), &mut (dyn Write), &[String]) -> Result<SynComp, i32>;

#[derive(Clone)]
pub struct Prim {
    pub arity: u64,
    pub body: PrimComp,
}

#[derive(IntoEnum, FmtArgs, Clone)]
pub enum SynComp {
    Ret(Ret<RcValue>),
    Force(Force<RcValue>),
    Let(Let<TermV, RcValue, RcComp>),
    Do(Do<TermV, RcComp, RcComp>),
    Rec(Rec<TermV, RcComp>),
    Match(Match<CtorV, TermV, RcValue, RcComp>),
    Comatch(Comatch<DtorV, TermV, RcComp>),
    Dtor(Dtor<RcComp, DtorV, RcValue>),
    Prim(Prim),
}
type RcComp = Rc<SynComp>;
impl ComputationT for SynComp {}

#[derive(IntoEnum, FmtArgs, Clone)]
pub enum Term {
    Val(SynVal),
    Comp(SynComp),
}

/* --------------------------------- Module --------------------------------- */

#[derive(Clone)]
pub struct Module {
    pub name: Option<String>,
    pub define: Vector<(TermV, SynVal)>,
}

#[derive(Clone)]
pub struct Program {
    pub module: Module,
    pub entry: SynComp,
}
