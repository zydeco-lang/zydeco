pub use super::arena::*;
pub use zydeco_syntax::*;
pub use zydeco_utils::arena::*;

use derive_more::From;
use super::*;

pub type DefId = ss::DefId;
pub type VPatId = ss::VPatId;
pub type DeclId = ss::DeclId;

new_key_type! {
    pub struct ValueId;
    pub struct CompuId;
    pub struct StackId;
}

#[derive(From, Clone, Debug)]
pub enum TermId {
    Value(ValueId),
    Compu(CompuId),
    Stack(StackId),
}

/* ---------------------------------- Value --------------------------------- */

pub type ValuePattern = ss::ValuePattern;

/// A closure that captures minimal environment.
#[derive(Clone, Debug)]
pub struct Clo {
    pub capture: Vec<DefId>,
    pub stack: Bullet,
    pub body: CompuId,
}

#[derive(From, Clone, Debug)]
pub enum Value {
    Hole(Hole),
    Var(DefId),
    Clo(Clo),
    Ctor(Ctor<ValueId>),
    Triv(Triv),
    VCons(Cons<ValueId, ValueId>),
    Lit(Literal),
}

/* ---------------------------------- Stack --------------------------------- */

/// The one and only stack variable.
#[derive(From, Clone, Debug)]
pub struct Bullet;

/// A continuation that waits for a value and resumes the computation.
#[derive(From, Clone, Debug)]
pub struct Kont {
    pub binder: VPatId,
    pub body: CompuId,
}

#[derive(From, Clone, Debug)]
pub enum Stack {
    Kont(Kont),
    Var(Bullet),
    Arg(Cons<ValueId, StackId>),
    Tag(Cons<DtorName, StackId>),
}

/* ------------------------------- Computation ------------------------------ */

#[derive(From, Clone, Debug)]
pub struct SForce {
    pub thunk: ValueId,
    pub stack: StackId,
}

#[derive(From, Clone, Debug)]
pub struct SReturn {
    pub stack: StackId,
    pub value: ValueId,
}

#[derive(From, Clone, Debug)]
pub enum Computation {
    Hole(Hole),
    VAbs(Abs<VPatId, CompuId>),
    VApp(App<CompuId, ValueId>),
    Fix(Fix<VPatId, CompuId>),
    Force(SForce),
    Ret(SReturn),
    Case(Match<ValueId, VPatId, CompuId>),
    LetStack(Let<Bullet, StackId, CompuId>),
    LetArg(Let<Cons<VPatId, Bullet>, StackId, CompuId>),
    CoCase(CoMatch<CompuId, Cons<DtorName, Bullet>>),
}
