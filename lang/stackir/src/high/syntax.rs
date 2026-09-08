//! Lexical high Stack IR with implicit closure and continuation captures.

pub use super::arena::*;
pub use crate::syntax::*;

use crate::syntax as common;
use derive_more::From;

zydeco_utils::new_key_type! {
    pub struct VPatId;
    pub struct ValueId;
    pub struct CompuId;
    pub struct StackId;
}

/// Dispatcher for high Stack IR terms.
#[derive(From, Clone, Debug, Eq, Hash, PartialEq)]
pub enum TermId {
    Value(ValueId),
    Compu(CompuId),
    Stack(StackId),
}

pub type ValuePattern = common::ValuePattern<VPatId>;
pub type Complex = common::Complex<ValueId>;
pub type SProductMatch = common::SProductMatch<ValueId, VPatId, CompuId>;
pub type SCoprodMatch = common::SCoprodMatch<ValueId, VPatId, CompuId>;
pub type SCoMatch = common::SCoMatch<StackId, Cons<DtorIdx, Bullet>, CompuId>;
pub type ExternCall = common::ExternCall<StackId>;

/* ---------------------------------- Values -------------------------------- */

/// A closure whose minimal capture list is computed during closure conversion.
#[derive(Clone, Debug)]
pub struct Closure {
    pub stack: Bullet,
    pub body: CompuId,
}

#[derive(From, Clone, Debug)]
pub enum Value {
    Hole(Hole),
    Var(DefId),
    Closure(Closure),
    Ctor(Ctor<CtorIdx, ValueId>),
    Triv(Triv),
    VCons(VCons<ValueId>),
    Literal(Literal),
    Complex(Complex),
}

/* ---------------------------------- Stacks -------------------------------- */

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
    Tag(Cons<DtorIdx, StackId>),
}

/* ------------------------------- Computations ----------------------------- */

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
    pub param: DefId,
    pub stack: StackId,
    pub body: CompuId,
}

#[derive(From, Clone, Debug)]
pub enum LetJoin {
    Value(Let<VPatId, ValueId, CompuId>),
    Stack(Let<Bullet, StackId, CompuId>),
}

#[derive(From, Clone, Debug)]
pub enum Computation<Join> {
    Hole(SHole<StackId>),
    Force(SForce),
    Ret(SReturn),
    Fix(SFix),
    ProductMatch(SProductMatch),
    CoprodMatch(SCoprodMatch),
    #[from(ignore)]
    Join(Join),
    LetArg(Let<Cons<VPatId, Bullet>, StackId, CompuId>),
    CoCase(SCoMatch),
    ExternCall(ExternCall),
}

impl<T> From<T> for Computation<LetJoin>
where
    T: Into<LetJoin>,
{
    fn from(j: T) -> Self {
        Computation::Join(j.into())
    }
}
