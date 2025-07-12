pub use zydeco_syntax::*;

use crate::statics_syntax::{self as ss, Env};
use derive_more::From;
use std::rc::Rc;
use zydeco_utils::{arena::*};

/* ------------------------------- Identifier ------------------------------- */

pub type DefId = ss::DefId;
pub type RcVPat = Rc<ValuePattern>;
pub type RcValue = Rc<Value>;
pub type RcSPat = Rc<StackPattern>;
pub type RcStack = Rc<Stack>;
pub type RcCompu = Rc<Computation>;
pub type DeclId = ss::DeclId;

/* ---------------------------------- Value --------------------------------- */

#[derive(From, Clone, Debug)]
pub enum ValuePattern {
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<RcVPat>),
    Triv(Triv),
    VCons(Cons<RcVPat, RcVPat>),
}

#[derive(Clone, Debug)]
pub struct Proc {
    pub stack: RcSPat,
    pub body: RcCompu,
}

#[derive(From, Clone, Debug)]
pub enum Value {
    Hole(Hole),
    Var(DefId),
    Proc(Proc),
    Ctor(Ctor<RcValue>),
    Triv(Triv),
    VCons(Cons<RcValue, RcValue>),
    Lit(Literal),
    SemValue(SemValue),
}

/* ---------------------------------- Stack --------------------------------- */

#[derive(From, Clone, Debug)]
pub enum StackPattern {
    Var(DefId),
}

#[derive(From, Clone, Debug)]
pub struct Kont {
    pub binder: RcVPat,
    pub body: RcCompu,
}

#[derive(From, Clone, Debug)]
pub enum Stack {
    Var(DefId),
    Kont(Kont),
    Arg(RcValue, RcStack),
    Tag(DtorName, RcStack),
}

/* ------------------------------- Computation ------------------------------ */

#[derive(From, Clone, Debug)]
pub struct LetAbs {
    pub binder: RcVPat,
    pub stack: RcSPat,
    pub bindee: RcStack,
    pub body: RcCompu,
}

#[derive(From, Clone, Debug)]
pub struct Call {
    pub thunk: RcValue,
    pub stack: RcStack,
}

#[derive(From, Clone, Debug)]
pub struct ReturnKont {
    pub stack: RcStack,
    pub value: RcValue,
}

#[derive(From, Clone, Debug)]
pub enum Computation {
    Hole(Hole),
    VAbs(LetAbs),
    Fix(Fix<(RcVPat, RcSPat), RcCompu>),
    Call(Call),
    Ret(ReturnKont),
    Let(Let<RcVPat, RcValue, RcCompu>),
    Match(Match<RcValue, RcVPat, RcCompu>),
    CoMatch(CoMatch<RcCompu>),
    Dtor(Dtor<RcCompu>),
}

/* ------------------------------- Declaration ------------------------------ */

#[derive(Clone, Debug)]
pub struct VAliasBody {
    pub binder: RcVPat,
    pub bindee: RcValue,
}

#[derive(Clone, Debug)]
pub struct Exec {
    pub stack: RcSPat,
    pub body: RcCompu,
}

#[derive(Clone, From, Debug)]
pub enum Declaration {
    VAliasBody(VAliasBody),
    Exec(Exec),
}

/* ---------------------------------- Arena --------------------------------- */

pub struct DynamicsArena {
    // arenas
    pub defs: ArenaSparse<DefId, VarName>,
    pub top: Vec<Exec>,
}

/* -------------------------------- Semantics ------------------------------- */

#[derive(Clone, Debug)]
pub struct SemProc {
    pub env: Env<SemValue>,
    pub stack: RcSPat,
    pub body: RcCompu,
}

#[derive(From, Clone, Debug)]
pub enum SemValue {
    Proc(SemProc),
    Ctor(Ctor<Box<SemValue>>),
    Triv(Triv),
    VCons(Cons<Box<SemValue>, Box<SemValue>>),
    Literal(Literal),
}

#[derive(From, Clone, Debug)]
pub struct SemKont {
    pub env: Env<SemValue>,
    pub stack: RcSPat,
    pub body: RcCompu,
}
