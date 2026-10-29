//! Syntax of the paper's first-order `SPS_l` target, extended with Zydeco primitives.

pub use super::arena::*;
pub use super::entry::*;
pub use crate::syntax::*;

use crate::syntax as common;
use derive_more::From;

zydeco_utils::new_key_type! {
    pub struct VPatId;
    pub struct ValueId;
    pub struct StackId;
    pub struct CompuId;
}

/// The first-order SPSLow phase.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct SpsLow;

impl Phase for SpsLow {
    type VPatId = VPatId;
    type ValueId = ValueId;
    type StackId = StackId;
    type CompuId = CompuId;
    type Scope = SpsLowScope;
    type Nodes = SpsLowInnerArena;
    type ValueExt = ValueExt;
    type StackExt = StackExt;
    type CompuExt = CompuExt;
}

pub type TermId = common::TermId<SpsLow>;
pub type ValuePattern = common::ValuePattern<VPatId>;
pub type Value = common::Value<SpsLow>;
pub type Stack = common::Stack<SpsLow>;
pub type Computation = common::Computation<SpsLow>;
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

/// A first-order code value. `label` binds the block's own address in `body`.
#[derive(Clone, Debug)]
pub struct Block {
    pub label: DefId,
    pub entry: EntryParameters,
    pub body: CompuId,
}

/// Runtime-erased value-existential package for a source closure.
#[derive(Clone, Debug)]
pub struct ClosurePackage {
    pub environment: ValueId,
    pub code: ValueId,
}

/// The value forms of first-order SPSLow.
#[derive(Clone, Debug, From)]
pub enum ValueExt {
    Block(Block),
    ClosurePackage(ClosurePackage),
}

/* ---------------------------------- Stacks -------------------------------- */

/// Runtime-erased stack-existential package for a source continuation.
#[derive(Clone, Debug)]
pub struct ContinuationPackage {
    pub code: ValueId,
    pub residual: StackId,
}

/// Checked provenance retained by closure conversion for native frame lowering.
/// The executable SPSLow tree still contains the portable capture package.
#[derive(Clone, Debug)]
pub struct ContinuationEntry {
    pub result: VPatId,
    pub body: CompuId,
    pub captures: Vec<CaptureBinding>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CaptureBinding {
    pub source: DefId,
    pub binding: DefId,
}

/// The stack forms of first-order SPSLow.
#[derive(Clone, Debug, From)]
pub enum StackExt {
    ContinuationPackage(ContinuationPackage),
}

/* ------------------------------- Computations ----------------------------- */

#[derive(Clone, Debug)]
pub struct Jump {
    pub target: ValueId,
    pub argument: EntryArgument,
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

/// How a label reaches the variables its body reads.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LabelKind {
    /// The body reads the frame of the block that binds the label, so every jump to it
    /// lies in that block and passes only a stack.
    Local,
    /// The body reads no variable, so jumps from continuation blocks nested in the label's scope
    /// reach it too; each jump pushes the variables the procedure needs over its stack.
    Closed,
}

/// The label of a second-class procedure, in scope in `tail`. `body` runs on the stack a jump
/// supplies, bound to its own `•`.
#[derive(Clone, Debug)]
pub struct LetLabel {
    pub label: DefId,
    pub kind: LabelKind,
    pub body: CompuId,
    pub tail: CompuId,
}

/// A direct jump to a label in scope, with no administrative word.
#[derive(Clone, Debug)]
pub struct JumpLabel {
    pub label: DefId,
    pub stack: StackId,
}

/// The computation forms of first-order SPSLow.
#[derive(Clone, Debug, From)]
pub enum CompuExt {
    Jump(Jump),
    LetLabel(LetLabel),
    JumpLabel(JumpLabel),
    OpenClosure(OpenClosure),
    OpenContinuation(OpenContinuation),
}

common::phase_conversions!(SpsLow);
