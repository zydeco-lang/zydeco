use crate::statics_syntax::{self as ss, Env};
pub use zydeco_syntax::*;

use derive_more::From;
use std::{
    io::{BufRead, Write},
    rc::Rc,
};

/* ------------------------------- Identifier ------------------------------- */

pub type DefId = ss::DefId;
pub type RcVPat = Rc<ValuePattern>;
pub type RcValue = Rc<Value>;
pub type RcCompu = Rc<Computation>;
pub type RcSemVal = Rc<SemValue>;

/* ---------------------------------- Value --------------------------------- */

#[derive(From, Clone, Debug)]
pub enum ValuePattern {
    // Ann(Ann<RcVPat, TypeId>),
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<RcVPat>),
    Triv(Triv),
    VCons(Cons<RcVPat, RcVPat>),
}

#[derive(From, Clone, Debug)]
pub enum Value {
    Hole(Hole),
    Var(DefId),
    Thunk(Thunk<RcCompu>),
    Ctor(Ctor<RcValue>),
    Triv(Triv),
    VCons(Cons<RcValue, RcValue>),
    Lit(Literal),
}

/* ------------------------------- Computation ------------------------------ */

pub type PrimComp =
    fn(Vec<RcSemVal>, &mut (dyn BufRead), &mut (dyn Write), &[String]) -> Result<RcCompu, i32>;

#[derive(Clone)]
pub struct Prim {
    pub arity: u64,
    pub body: PrimComp,
}

#[derive(From, Clone, Debug)]
pub enum Computation {
    Hole(Hole),
    VAbs(Abs<RcVPat, RcCompu>),
    VApp(App<RcCompu, RcValue>),
    Rec(Rec<RcVPat, RcCompu>),
    Force(Force<RcValue>),
    Ret(Ret<RcValue>),
    Do(Bind<RcVPat, RcCompu, RcCompu>),
    Let(PureBind<RcVPat, RcValue, RcCompu>),
    Match(Match<RcValue, RcVPat, RcCompu>),
    CoMatch(CoMatch<RcCompu>),
    Dtor(Dtor<RcCompu>),
}

/* -------------------------------- Semantic -------------------------------- */

#[derive(Clone)]
pub struct EnvThunk {
    pub body: RcCompu,
    pub env: Env<SemValue>,
}

#[derive(From, Clone)]
pub enum SemValue {
    Thunk(EnvThunk),
    Ctor(Ctor<RcSemVal>),
    Literal(Literal),
}

#[derive(Clone)]
pub enum SemCompu {
    Kont(RcCompu, Env<RcSemVal>, DefId),
    App(RcSemVal),
    Dtor(DtorName),
}
