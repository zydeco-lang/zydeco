//! Lexical SPS: the shared forms with closures and continuations that capture implicitly.

pub use super::arena::{AdminArena, *};
pub use crate::syntax::*;

use crate::syntax as common;
use derive_more::From;

zydeco_utils::new_key_type! {
    pub struct VPatId;
    pub struct ValueId;
    pub struct CompuId;
    pub struct StackId;
}

/// The lexical SPS phase.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Sps;

impl Phase for Sps {
    type VPatId = VPatId;
    type ValueId = ValueId;
    type StackId = StackId;
    type CompuId = CompuId;
    type Scope = SpsScope;
    type Nodes = SpsInnerArena;
    type ValueExt = ValueExt;
    type StackExt = StackExt;
    type CompuExt = CompuExt;
}

pub type TermId = common::TermId<Sps>;
pub type ValuePattern = common::ValuePattern<VPatId>;
pub type Value = common::Value<Sps>;
pub type Stack = common::Stack<Sps>;
pub type Computation = common::Computation<Sps>;
pub type Primitive = common::Primitive<ValueId>;
pub type CompareBranch = common::CompareBranch<ValueId, CompuId>;
pub type AddrOffset = common::AddrOffset<ValueId>;
pub type MemoryStep = common::MemoryStep<ValueId, VPatId, CompuId>;
pub type SProductMatch = common::SProductMatch<ValueId, VPatId, CompuId>;
pub type SCoprodMatch = common::SCoprodMatch<ValueId, VPatId, CompuId>;
pub type SCoMatch = common::SCoMatch<StackId, Cons<DtorIdx, StackVar>, CompuId>;
pub type ExternCall = common::ExternCall<StackId>;
pub type LetValue = Let<VPatId, ValueId, CompuId>;
pub type LetStack = Let<StackVar, StackId, CompuId>;
pub type LetArg = Let<Cons<VPatId, StackVar>, StackId, CompuId>;

/* ---------------------------------- Values -------------------------------- */

/// A closure whose minimal capture list is computed during closure conversion.
#[derive(Clone, Debug)]
pub struct Closure {
    pub stack: StackVar,
    pub body: CompuId,
}

/// The value forms of lexical SPS.
#[derive(Clone, Debug, From)]
pub enum ValueExt {
    Closure(Closure),
}

/* ---------------------------------- Stacks -------------------------------- */

/// A continuation that waits for a value and resumes the computation.
#[derive(Clone, Debug)]
pub struct Kont {
    pub binder: VPatId,
    pub body: CompuId,
}

/// The stack forms of lexical SPS.
#[derive(Clone, Debug, From)]
pub enum StackExt {
    Kont(Kont),
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

/// The computation forms of lexical SPS.
#[derive(Clone, Debug, From)]
pub enum CompuExt {
    Force(SForce),
    Ret(SReturn),
    Fix(SFix),
    LetProc(LetProc),
    CallProc(CallProc),
}

common::phase_conversions!(Sps);
