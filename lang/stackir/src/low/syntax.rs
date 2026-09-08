//! Syntax of the paper's first-order `SPS_l` target, extended with Zydeco primitives.

pub use super::arena::*;
pub use crate::syntax::*;

use crate::syntax as common;
use derive_more::From;

zydeco_utils::new_key_type! {
    pub struct VPatId;
    pub struct ValueId;
    pub struct StackId;
    pub struct CompuId;
}

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
pub type LetValue = Let<VPatId, ValueId, CompuId>;
pub type LetStack = Let<Bullet, StackId, CompuId>;
pub type LetArg = Let<Cons<VPatId, Bullet>, StackId, CompuId>;

/* ---------------------------------- Values -------------------------------- */

/// A first-order code value. `label` binds the block's own address in `body`.
#[derive(Clone, Debug)]
pub struct Block {
    pub label: DefId,
    pub body: CompuId,
}

/// Runtime-erased value-existential package for a source closure.
#[derive(Clone, Debug)]
pub struct ClosurePackage {
    pub environment: ValueId,
    pub code: ValueId,
}

#[derive(From, Clone, Debug)]
pub enum Value {
    Hole(Hole),
    Var(DefId),
    Block(Block),
    ClosurePackage(ClosurePackage),
    Ctor(Ctor<CtorIdx, ValueId>),
    Triv(Triv),
    VCons(VCons<ValueId>),
    Literal(Literal),
    Complex(Complex),
}

/* ---------------------------------- Stacks -------------------------------- */

/// Runtime-erased stack-existential package for a source continuation.
#[derive(Clone, Debug)]
pub struct ContinuationPackage {
    pub code: ValueId,
    pub residual: StackId,
}

#[derive(From, Clone, Debug)]
pub enum Stack {
    Var(Bullet),
    Arg(Cons<ValueId, StackId>),
    Tag(Cons<DtorIdx, StackId>),
    ContinuationPackage(ContinuationPackage),
}

/* ------------------------------- Computations ----------------------------- */

#[derive(Clone, Debug)]
pub struct Jump {
    pub target: ValueId,
    pub stack: StackId,
}

/// Eliminate a closure's value-existential package.
#[derive(Clone, Debug)]
pub struct OpenClosure {
    pub package: ValueId,
    pub environment: VPatId,
    pub code: VPatId,
    pub body: CompuId,
}

/// Eliminate a continuation's stack-existential package.
///
/// `body` runs with the package's residual stack rebound as the ambient stack.
#[derive(Clone, Debug)]
pub struct OpenContinuation {
    pub package: StackId,
    pub code: VPatId,
    pub body: CompuId,
}

#[derive(From, Clone, Debug)]
pub enum Computation {
    Hole(SHole<StackId>),
    Jump(Jump),
    ProductMatch(SProductMatch),
    CoprodMatch(SCoprodMatch),
    LetValue(LetValue),
    LetStack(LetStack),
    LetArg(LetArg),
    CoCase(SCoMatch),
    OpenClosure(OpenClosure),
    OpenContinuation(OpenContinuation),
    ExternCall(ExternCall),
}
