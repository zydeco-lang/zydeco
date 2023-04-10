use crate::dynamics::syntax as ds;
pub use crate::syntax::{env::Env, *};
use im::Vector;
use std::{
    io::{BufRead, Write},
    rc::Rc,
};
use zydeco_derive::{EnumGenerator, FmtArgs};

/* ---------------------------------- Term ---------------------------------- */

#[derive(EnumGenerator, FmtArgs, Clone)]
pub enum ZVal {
    Var(TermV),
    Thunk(Thunk<RcComp>),
    Ctor(Ctor<CtorV, RcValue>),
    Literal(Literal),
    SemValue(ds::SemVal),
}
type RcValue = Rc<ZVal>;
impl ValueT for ZVal {}

pub type PrimComp =
    fn(Vec<ds::SemVal>, &mut (dyn BufRead), &mut (dyn Write), &[String]) -> Result<ZComp, i32>;

#[derive(Clone)]
pub struct Prim {
    pub arity: u64,
    pub body: PrimComp,
}

#[derive(EnumGenerator, FmtArgs, Clone)]
pub enum ZComp {
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
type RcComp = Rc<ZComp>;
impl ComputationT for ZComp {}

#[derive(EnumGenerator, FmtArgs, Clone)]
pub enum Term {
    Val(ZVal),
    Comp(ZComp),
}

/* --------------------------------- Module --------------------------------- */

#[derive(Clone)]
pub struct Module {
    pub name: Option<String>,
    pub define: Vector<(TermV, ZVal)>,
}

#[derive(Clone)]
pub struct Program {
    pub module: Module,
    pub entry: ZComp,
}
