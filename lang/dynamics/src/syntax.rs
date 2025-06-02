pub use zydeco_syntax::*;

use crate::statics_syntax::{self as ss, Env};
use derive_more::From;
use std::{
    io::{BufRead, Write},
    rc::Rc,
};
use zydeco_utils::{arena::*, scc::SccGraph};

/* ------------------------------- Identifier ------------------------------- */

pub type DefId = ss::DefId;
pub type RcVPat = Rc<ValuePattern>;
pub type RcValue = Rc<Value>;
pub type RcCompu = Rc<Computation>;
pub type DeclId = ss::DeclId;

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
    SemValue(SemValue),
}

/* ------------------------------- Computation ------------------------------ */

pub type PrimComp =
    fn(Vec<SemValue>, &mut (dyn BufRead), &mut (dyn Write), &[String]) -> Result<Computation, i32>;

#[derive(Clone, Debug)]
pub struct Prim {
    pub arity: u64,
    pub body: PrimComp,
}

#[derive(From, Clone, Debug)]
pub enum Computation {
    Hole(Hole),
    VAbs(Abs<RcVPat, RcCompu>),
    VApp(App<RcCompu, RcValue>),
    Fix(Fix<RcVPat, RcCompu>),
    Force(Force<RcValue>),
    Ret(Return<RcValue>),
    Do(Bind<RcVPat, RcCompu, RcCompu>),
    Let(Let<RcVPat, RcValue, RcCompu>),
    Match(Match<RcValue, RcVPat, RcCompu>),
    CoMatch(CoMatch<RcCompu>),
    Dtor(Dtor<RcCompu>),
    Prim(Prim),
}

/* ------------------------------- Declaration ------------------------------ */

#[derive(Clone, Debug)]
pub struct VAliasBody {
    pub binder: RcVPat,
    pub bindee: RcValue,
}

#[derive(Clone, Debug)]
pub struct Exec(pub RcCompu);

#[derive(Clone, From, Debug)]
pub enum Declaration {
    VAliasBody(VAliasBody),
    Exec(Exec),
}

/* ---------------------------------- Arena --------------------------------- */

pub struct DynamicsArena {
    // arenas
    pub defs: ArenaSparse<DefId, VarName>,
    pub decls: ArenaSparse<DeclId, Declaration>,
    pub top: SccGraph<DeclId>,
}

/* -------------------------------- Semantics ------------------------------- */

#[derive(Clone, Debug)]
pub struct EnvThunk {
    pub body: RcCompu,
    pub env: Env<SemValue>,
}

#[derive(From, Clone, Debug)]
pub enum SemValue {
    Thunk(EnvThunk),
    Ctor(Ctor<Box<SemValue>>),
    Triv(Triv),
    VCons(Cons<Box<SemValue>, Box<SemValue>>),
    Literal(Literal),
}

#[derive(Clone, Debug)]
pub enum SemCompu {
    Kont(RcCompu, Env<SemValue>, RcVPat),
    App(SemValue),
    Dtor(DtorName),
}

pub struct Runtime<'rt> {
    pub input: &'rt mut (dyn BufRead),
    pub output: &'rt mut (dyn Write),
    pub args: &'rt [String],
    pub stack: im::Vector<SemCompu>,
    pub env: Env<SemValue>,
    pub arena: DynamicsArena,
}

#[derive(Clone)]
pub enum ProgKont {
    Dry,
    Ret(SemValue),
    ExitCode(i32),
}
