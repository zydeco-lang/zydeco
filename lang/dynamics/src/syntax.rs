pub use zydeco_syntax::*;

use crate::statics_syntax::{self as ss, Env};
use derive_more::From;
use std::{
    io::{BufRead, Write},
    rc::Rc,
};
use zydeco_utils::prelude::{ArenaSchema, ArenaSparse};

/* ------------------------------- Identifier ------------------------------- */

/// Definition identifier reused from the statics arena.
pub type DefId = ss::DefId;
/// Shared value pattern pointer for dynamic syntax.
pub type RcVPat = Rc<ValuePattern>;
/// Shared value pointer for dynamic syntax.
pub type RcValue = Rc<Value>;
/// Shared computation pointer for dynamic syntax.
pub type RcCompu = Rc<Computation>;

/* ---------------------------------- Value --------------------------------- */

/// Patterns used for value binders in runtime computations.
#[derive(From, Clone, Debug)]
pub enum ValuePattern {
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<CtorName, RcVPat>),
    Triv(Triv),
    VCons(ConsN<RcVPat, RcVPat>),
}

/// Runtime values: variables, thunks, constructors, and literals.
#[derive(From, Clone, Debug)]
pub enum Value {
    Hole(Hole),
    Var(DefId),
    Let(Let<RcVPat, RcValue, RcValue>),
    VAbs(Abs<RcVPat, RcValue>),
    VApp(App<RcValue, RcValue>),
    Thunk(Thunk<RcCompu>),
    Ctor(Ctor<CtorName, RcValue>),
    Triv(Triv),
    VCons(ConsN<RcValue, RcValue>),
    Proj(Proj<RcValue, usize>),
    Lit(Literal),
    SemValue(SemValue),
}

/* ------------------------------- Computation ------------------------------ */

/// Function signature for builtin primitives.
pub type PrimComp =
    fn(Vec<SemValue>, &mut dyn BufRead, &mut dyn Write, &[String]) -> Result<Computation, i32>;

/// A primitive function together with its arity.
#[derive(Clone, Debug)]
pub struct Prim {
    pub arity: u64,
    pub body: PrimComp,
}

/// Computations in the dynamic language.
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
    CoMatch(CoMatch<DtorName, RcCompu>),
    Dtor(Dtor<RcCompu, DtorName>),
    Prim(Prim),
}

/* ---------------------------------- Arena --------------------------------- */

/// Owning storage scope for linked runtime syntax.
#[derive(Debug)]
pub enum DynamicsScope {}

impl ArenaSchema<DefId> for DynamicsScope {
    type Item = VarName;
}

/// Storage for one linked runtime computation.
pub struct DynamicsArena {
    pub defs: ArenaSparse<DynamicsScope, DefId>,
    pub root: RcCompu,
}

/* -------------------------------- Semantics ------------------------------- */

/// A thunk value paired with the environment in which it was created.
#[derive(Clone, Debug)]
pub struct EnvThunk {
    pub body: RcCompu,
    pub env: Env<SemValue>,
}

/// A pure value function paired with its lexical environment.
#[derive(Clone, Debug)]
pub struct EnvValueClosure {
    pub binder: RcVPat,
    pub body: RcValue,
    pub env: Env<SemValue>,
}

/// Semantic values used by the evaluator.
#[derive(From, Clone, Debug)]
pub enum SemValue {
    Closure(EnvValueClosure),
    Thunk(EnvThunk),
    Ctor(Ctor<CtorName, Box<SemValue>>),
    Triv(Triv),
    VCons(ConsN<SemValue, Box<SemValue>>),
    Literal(Literal),
}

/// Runtime stack frames for computations.
#[derive(Clone, Debug)]
pub enum SemCompu {
    Kont(RcCompu, Env<SemValue>, RcVPat),
    App(SemValue),
    Dtor(DtorName),
}

/// Mutable runtime state threaded through evaluation.
pub struct Runtime<'rt> {
    pub input: &'rt mut dyn BufRead,
    pub output: &'rt mut dyn Write,
    pub args: &'rt [String],
    pub stack: im::Vector<SemCompu>,
    pub env: Env<SemValue>,
    pub arena: DynamicsArena,
}

/// Program-level continuation produced by evaluation.
#[derive(Clone, Debug)]
pub enum ProgKont {
    Dry,
    Ret(SemValue),
    ExitCode(i32),
}
