pub use super::arena::*;
pub use zydeco_syntax::{fmt, *};
pub use zydeco_utils::arena::*;
pub use zydeco_utils::context::Context;

use super::*;
use derive_more::From;

pub type DefId = ss::DefId;
pub type DeclId = ss::DeclId;

new_key_type! {
    pub struct VPatId;
    pub struct ValueId;
    pub struct CompuId;
    pub struct StackId;
}

#[derive(From, Clone, Debug, Eq, Hash, PartialEq)]
pub enum TermId {
    Value(ValueId),
    Compu(CompuId),
    Stack(StackId),
}

/* ---------------------------------- Value --------------------------------- */

#[derive(From, Clone, Debug)]
pub enum ValuePattern {
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<VPatId>),
    Triv(Triv),
    VCons(Cons<VPatId, VPatId>),
}

/// A closure that captures minimal environment.
#[derive(Clone, Debug)]
pub struct Clo {
    pub capture: Context<DefId>,
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
pub struct SFix {
    pub capture: Context<DefId>,
    pub param: DefId,
    pub body: CompuId,
}

#[derive(From, Clone, Debug)]
pub enum Computation {
    Hole(Hole),
    Force(SForce),
    Ret(SReturn),
    Fix(SFix),
    Case(Match<ValueId, VPatId, CompuId>),
    LetValue(Let<VPatId, ValueId, CompuId>),
    LetStack(Let<Bullet, StackId, CompuId>),
    LetArg(Let<Cons<VPatId, Bullet>, StackId, CompuId>),
    CoCase(CoMatch<CompuId, Cons<DtorName, Bullet>>),
}

/* ------------------------------- Declaration ------------------------------ */

#[derive(From, Clone, Debug)]
pub enum Global {
    Extern(Extern),
    Defined(ValueId),
}

#[derive(Clone, Debug)]
pub struct Extern;
