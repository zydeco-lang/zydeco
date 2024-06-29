use crate::dynamics::syntax as ds;
pub use crate::syntax::*;
use derive_more::From;
use im::Vector;
use std::{
    io::{BufRead, Write},
    rc::Rc,
};
use zydeco_derive::FmtArgs;

/* ---------------------------------- Term ---------------------------------- */

#[derive(From, FmtArgs, Clone)]
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

#[derive(From, FmtArgs, Clone)]
pub enum SynComp {
    Abs(Abs<TermV, RcComp>),
    App(App<RcComp, RcValue>),
    Ret(Ret<RcValue>),
    Force(Force<RcValue>),
    Let(Let<TermV, RcValue, RcComp>),
    Do(Do<TermV, RcComp, RcComp>),
    Rec(Rec<TermV, RcComp>),
    Match(Match<CtorV, TermV, RcValue, RcComp>),
    Comatch(Comatch<DtorV, RcComp>),
    Dtor(Dtor<RcComp, DtorV>),
    Prim(Prim),
}
type RcComp = Rc<SynComp>;
impl ComputationT for SynComp {}

#[derive(From, FmtArgs, Clone)]
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
