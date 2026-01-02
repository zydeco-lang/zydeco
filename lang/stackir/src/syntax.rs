pub use super::{arena::*, builtin::*};
pub use zydeco_syntax::{fmt, *};
pub use zydeco_utils::{arena::*, context::Context};

use super::*;
use derive_more::From;

pub type DefId = ss::DefId;
pub type DeclId = ss::DeclId;

zydeco_utils::new_key_type! {
    pub struct VPatId;
    pub struct ValueId;
    pub struct CompuId;
    pub struct StackId;
}

/// Dispatcher for stack terms (value, computation, or stack).
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
pub struct Closure {
    pub capture: Context<DefId>,
    pub stack: Bullet,
    pub body: CompuId,
}

#[derive(From, Clone, Debug)]
pub struct Operator {
    pub name: SymName,
    pub arity: usize,
}

#[derive(Clone, Debug)]
pub struct Complex {
    pub operator: Operator,
    pub operands: Vec<ValueId>,
}

#[derive(From, Clone, Debug)]
pub enum Value {
    Hole(Hole),
    Var(DefId),
    Closure(Closure),
    Ctor(Ctor<ValueId>),
    Triv(Triv),
    VCons(Cons<ValueId, ValueId>),
    Literal(Literal),
    Complex(Complex),
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

/// Stack cells used by continuations.
#[derive(From, Clone, Debug)]
pub enum Stack {
    Kont(Kont),
    Var(Bullet),
    Arg(Cons<ValueId, StackId>),
    Tag(Cons<DtorName, StackId>),
}

/* ------------------------------- Computation ------------------------------ */

#[derive(Clone, Debug)]
pub struct SForce {
    pub thunk: ValueId,
    pub stack: StackId,
}

#[derive(Clone, Debug)]
pub struct SReturn {
    pub stack: StackId,
    pub value: ValueId,
}

#[derive(Clone, Debug)]
pub struct SFix {
    pub capture: Context<DefId>,
    pub param: DefId,
    pub body: CompuId,
}

#[derive(Clone, Debug)]
pub struct ExternCall {
    pub name: SymName,
    pub arity: usize,
    pub stack: Bullet,
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
    ExternCall(ExternCall),
}

/* ------------------------------- Declaration ------------------------------ */

#[derive(From, Clone, Debug)]
pub enum Global {
    Extern(Extern),
    Defined(ValueId),
}

#[derive(Clone, Debug)]
pub struct Extern;
