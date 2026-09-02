//! Syntax of the paper's first-order `SPS_l` target, extended with Zydeco primitives.

pub use super::arena::*;
pub use crate::{
    builtin::*,
    syntax::{Bullet, CtorIdx, DefId, DtorIdx, ExternalFunction, ProductLayout, VCons},
};
pub use zydeco_syntax::*;
pub use zydeco_utils::arena::*;

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

/* -------------------------------- Patterns ------------------------------- */

#[derive(From, Clone, Debug)]
pub enum ValuePattern {
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<CtorIdx, VPatId>),
    Alias(Alias<VPatId>),
    Triv(Triv),
    VCons(VCons<VPatId>),
}

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

#[derive(Clone, Debug)]
pub struct Complex {
    pub operator: String,
    pub operands: Vec<ValueId>,
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
pub struct SHole(pub StackId);

#[derive(Clone, Debug)]
pub struct Jump {
    pub target: ValueId,
    pub stack: StackId,
}

#[derive(Clone, Debug)]
pub struct SProductMatch {
    pub scrut: ValueId,
    pub binder: VPatId,
    pub body: CompuId,
}

#[derive(Clone, Debug)]
pub struct SCoprodMatch {
    pub scrut: ValueId,
    pub arms: Vec<Matcher<VPatId, CompuId>>,
}

#[derive(Clone, Debug)]
pub struct SCoMatch {
    pub scrut: StackId,
    pub arms: Vec<CoMatcher<Cons<DtorIdx, Bullet>, CompuId>>,
}

#[derive(Clone, Debug)]
pub struct LetValue {
    pub binder: VPatId,
    pub bindee: ValueId,
    pub body: CompuId,
}

#[derive(Clone, Debug)]
pub struct LetStack {
    pub bindee: StackId,
    pub body: CompuId,
}

#[derive(Clone, Debug)]
pub struct LetArg {
    pub binder: VPatId,
    pub bindee: StackId,
    pub body: CompuId,
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

#[derive(Clone, Debug)]
pub struct ExternCall {
    pub function: ExternalFunction,
    pub stack: StackId,
}

#[derive(From, Clone, Debug)]
pub enum Computation {
    Hole(SHole),
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
