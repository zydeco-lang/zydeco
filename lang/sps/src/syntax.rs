use crate::statics_syntax::{self as ss, Env};
use derive_more::From;
use std::rc::Rc;
use zydeco_utils::arena::*;

pub use zydeco_syntax::*;

/* ------------------------------- Identifier ------------------------------- */

pub type DefId = ss::DefId;
pub type RcVPat = Rc<ValuePattern>;
pub type RcValue = Rc<Value>;
pub type RcStack = Rc<Stack>;
pub type RcCompu = Rc<Computation>;
pub type DeclId = ss::DeclId;

new_key_type! {
    pub struct BlockId;
}

/* ---------------------------------- Value --------------------------------- */

#[derive(From, Clone, Debug)]
pub enum ValuePattern {
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
    Proc(Thunk<RcCompu>),
    Ctor(Ctor<RcValue>),
    Triv(Triv),
    VCons(Cons<RcValue, RcValue>),
    Lit(Literal),
}

/* ---------------------------------- Stack --------------------------------- */

#[derive(From, Clone, Debug)]
pub struct Current;

#[derive(From, Clone, Debug)]
pub struct Kont {
    pub binder: RcVPat,
    pub body: RcCompu,
}

#[derive(From, Clone, Debug)]
pub struct StackItem<T> {
    pub item: T,
    pub next: RcStack,
}

#[derive(From, Clone, Debug)]
pub enum Stack {
    Kont(Kont),
    Var(Current),
    Arg(StackItem<RcValue>),
    Tag(StackItem<DtorName>),
}

/* ------------------------------- Computation ------------------------------ */

#[derive(From, Clone, Debug)]
pub struct LetAbs {
    pub binder: RcVPat,
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
    Fix(Fix<RcVPat, RcCompu>),
    Call(Call),
    Ret(ReturnKont),
    Let(Let<RcVPat, RcValue, RcCompu>),
    Match(Match<RcValue, RcVPat, RcCompu>),
    CoMatch(CoMatch<RcCompu>),
    Dtor(Dtor<RcCompu>),
}

pub struct Block {
    pub body: Computation,
}

/* ------------------------------- Declaration ------------------------------ */

#[derive(Clone, Debug)]
pub struct VAliasBody {
    pub binder: RcVPat,
    pub bindee: RcValue,
}

#[derive(Clone, Debug)]
pub struct Exec {
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
    pub body: RcCompu,
}
