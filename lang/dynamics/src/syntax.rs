pub use zydeco_syntax::*;

use crate::host::{HostRuntime, HostValue};
use crate::statics_syntax as ss;
use derive_more::From;
use std::{
    io::{BufRead, Write},
    rc::Rc,
};
use zydeco_statics::environment::Env;
use zydeco_utils::prelude::{ArenaSchema, ArenaSparse, FrozenArena};

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
    /// A refutable numeric-equality pattern on a primitive integer type.
    Lit(Literal),
    Alias(Alias<RcVPat>),
    Triv(Triv),
    VCons(Vec<RcVPat>),
    View(ViewPattern),
}

/// A total value transformation performed before matching `pattern`.
#[derive(Clone, Debug)]
pub struct ViewPattern {
    pub function: RcValue,
    pub pattern: RcVPat,
}

/// Runtime values: variables, thunks, constructors, and literals.
#[derive(From, Clone, Debug)]
pub enum Value {
    Hole(Hole),
    Var(DefId),
    Let(Let<RcVPat, RcValue, RcValue>),
    ValAbs(Abs<RcVPat, RcValue>),
    ValApp(App<RcValue, RcValue>),
    Thunk(Thunk<RcCompu>),
    Ctor(Ctor<CtorName, RcValue>),
    Triv(Triv),
    VCons(Vec<RcValue>),
    Proj(Proj<RcValue, usize>),
    Lit(Literal),
    SemValue(SemValue),
}

/* ------------------------------- Computation ------------------------------ */

/// A typed host operation together with its runtime arity.
#[derive(Clone, Debug)]
pub struct Prim {
    pub arity: u64,
    pub role: BuiltinValueRole,
}

/// A checked foreign import together with its source-level argument count.
#[derive(Clone, Debug)]
pub struct ForeignPrim {
    pub import: ForeignImport,
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
    Foreign(ForeignPrim),
}

/* ---------------------------------- Arena --------------------------------- */

/// Owning storage scope for linked runtime syntax.
#[derive(Debug)]
pub enum DynamicsScope {}

impl ArenaSchema<DefId> for DynamicsScope {
    type Item = VarName;
}

/// A linked runtime program with one computation at its top level.
pub struct DynamicsProgram {
    defs: FrozenArena<ArenaSparse<DynamicsScope, DefId>>,
    root: RcCompu,
}

impl DynamicsProgram {
    pub fn new(defs: ArenaSparse<DynamicsScope, DefId>, root: RcCompu) -> Self {
        Self { defs: FrozenArena::new(defs), root }
    }

    pub fn defs(&self) -> &ArenaSparse<DynamicsScope, DefId> {
        &self.defs
    }

    pub fn root(&self) -> &RcCompu {
        &self.root
    }
}

/* -------------------------------- Semantics ------------------------------- */

/// A thunk value paired with the environment in which it was created.
#[derive(Clone, Debug)]
pub struct EnvThunk {
    pub body: RcCompu,
    pub env: Env<SemValue>,
}

/// A total value function paired with its lexical environment.
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
    VCons(Vec<SemValue>),
    Literal(Literal),
    Host(HostValue),
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
    pub(crate) host: HostRuntime,
    pub(crate) foreign: crate::foreign::ForeignRuntime,
    pub stack: rpds::VectorSync<SemCompu>,
    pub env: Env<SemValue>,
    pub program: DynamicsProgram,
}

/// Program-level continuation produced by evaluation.
#[derive(Clone, Debug)]
pub enum ProgKont {
    Dry,
    Ret(SemValue),
    ExitCode(i32),
    Error(RuntimeError),
}

/// Recoverable failures raised by runtime-managed boundaries.
#[derive(Clone, Debug, thiserror::Error)]
pub enum RuntimeError {
    #[error(transparent)]
    Foreign(#[from] crate::foreign::ForeignRuntimeError),
}
