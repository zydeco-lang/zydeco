pub use zydeco_syntax::*;

use crate::statics_syntax::{self as ss, Env};
use derive_more::From;
use std::{
    io::{BufRead, Write},
    rc::Rc,
};
use zydeco_utils::prelude::{ArenaSparse, SccGraph};

/* ------------------------------- Identifier ------------------------------- */

/// Definition identifier reused from the statics arena.
pub type DefId = ss::DefId;
/// Shared value pattern pointer for dynamic syntax.
pub type RcVPat = Rc<ValuePattern>;
/// Shared value pointer for dynamic syntax.
pub type RcValue = Rc<Value>;
/// Shared computation pointer for dynamic syntax.
pub type RcCompu = Rc<Computation>;
/// Declaration identifier reused from the statics arena.
pub type DeclId = ss::DeclId;

/* ---------------------------------- Value --------------------------------- */

/// Patterns used for value binders in runtime declarations and computations.
#[derive(From, Clone, Debug)]
pub enum ValuePattern {
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<CtorName, RcVPat>),
    Triv(Triv),
    VCons(Cons<RcVPat, RcVPat>),
}

/// Runtime values: variables, thunks, constructors, and literals.
#[derive(From, Clone, Debug)]
pub enum Value {
    Hole(Hole),
    Var(DefId),
    Thunk(Thunk<RcCompu>),
    Ctor(Ctor<CtorName, RcValue>),
    Triv(Triv),
    VCons(Cons<RcValue, RcValue>),
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

/* ------------------------------- Declaration ------------------------------ */

/// Runtime value definition: binder and expression to evaluate.
#[derive(Clone, Debug)]
pub struct VAliasBody {
    pub binder: RcVPat,
    pub bindee: RcValue,
}

/// Runtime entry point for execution.
#[derive(Clone, Debug)]
pub struct Exec(pub RcCompu);

/// Top-level dynamic declaration.
#[derive(Clone, From, Debug)]
pub enum Declaration {
    VAliasBody(VAliasBody),
    Exec(Exec),
}

/* ---------------------------------- Arena --------------------------------- */

/// Storage for dynamic declarations and dependency tracking.
pub struct DynamicsArena {
    // arenas
    pub defs: ArenaSparse<DefId, VarName>,
    pub decls: ArenaSparse<DeclId, Declaration>,
    pub top: SccGraph<DeclId>,
}

/* -------------------------------- Semantics ------------------------------- */

/// A thunk value paired with the environment in which it was created.
#[derive(Clone, Debug)]
pub struct EnvThunk {
    pub body: RcCompu,
    pub env: Env<SemValue>,
}

/// Semantic values used by the evaluator.
#[derive(From, Clone, Debug)]
pub enum SemValue {
    Thunk(EnvThunk),
    Ctor(Ctor<CtorName, Box<SemValue>>),
    Triv(Triv),
    VCons(Cons<Box<SemValue>, Box<SemValue>>),
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
#[derive(Clone)]
pub enum ProgKont {
    Dry,
    Ret(SemValue),
    ExitCode(i32),
}
