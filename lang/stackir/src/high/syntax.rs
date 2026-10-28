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
pub type Primitive = common::Primitive<ValueId>;
pub type CompareBranch = common::CompareBranch<ValueId, CompuId>;
pub type AddrOffset = common::AddrOffset<ValueId>;
pub type MemoryStep = common::MemoryStep<ValueId, VPatId, CompuId>;
pub type SProductMatch = common::SProductMatch<ValueId, VPatId, CompuId>;
pub type SCoprodMatch = common::SCoprodMatch<ValueId, VPatId, CompuId>;
pub type SCoMatch = common::SCoMatch<StackId, Cons<DtorIdx, StackVar>, CompuId>;
pub type ExternCall = common::ExternCall<StackId>;

/* ---------------------------------- Values -------------------------------- */

/// A closure whose minimal capture list is computed during closure conversion.
#[derive(Clone, Debug)]
pub struct Closure {
    pub stack: StackVar,
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
    Primitive(Primitive),
    AddrOffset(AddrOffset),
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
    Var(StackVar),
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
    Stack(Let<StackVar, StackId, CompuId>),
}

/// A second-class procedure: `body` runs on the stack that each call supplies, bound to its own `•`.
/// The procedure is in scope in `tail` only, where it can be called and nothing else.
#[derive(Clone, Debug)]
pub struct LetProc {
    pub procedure: DefId,
    pub stack: StackVar,
    pub body: CompuId,
    pub tail: CompuId,
}

/// A call of a procedure in scope, with a stack built over the ambient one.
#[derive(Clone, Debug)]
pub struct CallProc {
    pub procedure: DefId,
    pub stack: StackId,
}

#[derive(From, Clone, Debug)]
pub enum Computation<Join> {
    Hole(SHole<StackId>),
    Force(SForce),
    Ret(SReturn),
    Fix(SFix),
    ProductMatch(SProductMatch),
    CoprodMatch(SCoprodMatch),
    Compare(CompareBranch),
    #[from(ignore)]
    Join(Join),
    LetProc(LetProc),
    CallProc(CallProc),
    LetArg(Let<Cons<VPatId, StackVar>, StackId, CompuId>),
    CoCase(SCoMatch),
    ExternCall(ExternCall),
    Memory(MemoryStep),
}

impl<T> From<T> for Computation<LetJoin>
where
    T: Into<LetJoin>,
{
    fn from(j: T) -> Self {
        Computation::Join(j.into())
    }
}

impl<Join> Computation<Join> {
    pub fn is_branch(&self) -> bool {
        matches!(self, Self::CoprodMatch(_) | Self::Compare(_))
    }
}
