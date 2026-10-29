//! Stack-passing compilation schemes and the placement rule of each scheme's fragment.
//!
//! A scheme decides where compilation names the current stack. Its output lies in the scheme's
//! fragment, which SPS and SPSLow validation both check through [`SpsScheme::check`].

use crate::{syntax::*, traverse::Edge};
use std::fmt;

/// Where stack-passing compilation names the current stack.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub enum SpsScheme {
    /// Name no stack; every arm of a branch receives its own copy of the consumer.
    Duplicating,
    /// Name the consumer stack once at each branch, so that the arms share it.
    #[default]
    BranchJoin,
    /// Name every frame as it is pushed, so that every consumer receives the ambient stack.
    Full,
}

impl SpsScheme {
    pub const ALL: &'static [Self] = &[Self::Duplicating, Self::BranchJoin, Self::Full];

    pub fn name(self) -> &'static str {
        match self {
            | Self::Duplicating => "dup",
            | Self::BranchJoin => "br",
            | Self::Full => "full",
        }
    }

    pub fn description(self) -> &'static str {
        match self {
            | Self::Duplicating => "copy the consumer stack into every arm of a branch",
            | Self::BranchJoin => "name the consumer stack once at each branch",
            | Self::Full => "name every frame as it is pushed",
        }
    }

    /// Whether a branch names its consumer stack once, so that its arms share it.
    /// Otherwise every arm consumes its own copy of the stack the branch receives.
    pub(crate) fn shares_branch_stack(self) -> bool {
        match self {
            | Self::Duplicating | Self::Full => false,
            | Self::BranchJoin => true,
        }
    }

    /// Whether pushing a frame names it with a stack let-binding, so that the computation
    /// consuming the pushed stack receives the ambient stack.
    pub(crate) fn names_frames(self) -> bool {
        match self {
            | Self::Duplicating | Self::BranchJoin => false,
            | Self::Full => true,
        }
    }

    /// The placement violation, if any, of one computation's stack use in this scheme's fragment.
    pub(crate) fn check(self, usage: StackUse) -> Option<PlacementError> {
        match (self, usage) {
            | (Self::Duplicating, StackUse::Let { .. }) => Some(PlacementError::UnexpectedStackLet),
            | (Self::Duplicating, _) => None,
            | (Self::BranchJoin, StackUse::Let { tail_branch: false, .. }) => {
                Some(PlacementError::NonBranchStackLet)
            }
            | (Self::BranchJoin, StackUse::Branch { guarded: false }) => {
                Some(PlacementError::UnguardedBranch)
            }
            | (Self::BranchJoin, _) => None,
            | (Self::Full, StackUse::Let { bindee: FrameShape::Ambient, .. }) => {
                Some(PlacementError::IdentityStackLet)
            }
            | (Self::Full, StackUse::Let { bindee: FrameShape::Compound, .. }) => {
                Some(PlacementError::CompoundStackLet)
            }
            | (Self::Full, StackUse::Consumer(FrameShape::Frame | FrameShape::Compound)) => {
                Some(PlacementError::NonAmbientConsumer)
            }
            | (Self::Full, _) => None,
        }
    }
}

impl fmt::Display for SpsScheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// A stack use that the fragment of the compiling scheme excludes.
#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
pub enum PlacementError {
    #[error("the branch is not immediately guarded by a stack let-binding")]
    UnguardedBranch,
    #[error("the stack let-binding does not immediately guard a branch")]
    NonBranchStackLet,
    #[error("the duplicating scheme names no stack, so it admits no stack let-binding")]
    UnexpectedStackLet,
    #[error("the full scheme names frames, so a stack let-binding cannot name the ambient stack")]
    IdentityStackLet,
    #[error(
        "the full scheme names one frame at a time, so a stack let-binding binds a single frame"
    )]
    CompoundStackLet,
    #[error("the full scheme names every frame, so a consumer receives the ambient stack")]
    NonAmbientConsumer,
}

/// What the placement rules observe about one computation.
#[derive(Clone, Copy, Debug)]
pub enum StackUse {
    /// A stack let-binding, recording the shape it binds and whether its tail is a branch.
    Let { bindee: FrameShape, tail_branch: bool },
    /// A value case split or scalar comparison, recording whether a stack let-binding owns it.
    Branch { guarded: bool },
    /// A computation that consumes a stack it receives, such as a call, a return, or a stack match.
    Consumer(FrameShape),
    /// Any other computation.
    Other,
}

/// How much of a stack the placement rules observe.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FrameShape {
    /// The ambient stack `•` itself.
    Ambient,
    /// One frame: a continuation, or an argument or tag pushed onto `•`.
    Frame,
    /// Several frames.
    Compound,
}

/// How a phase's own computation forms consume their stack.
pub trait StackConsumer<P: Phase> {
    /// The stack this computation consumes, when it is a consumer.
    fn consumed_stack(&self) -> Option<P::StackId>;
}

/// The frame shapes of a phase's own stack forms.
pub trait StackShape<P: Phase> {
    fn shape(&self, nodes: &P::Nodes) -> FrameShape;
}

impl<P: Phase> Stack<P> {
    /// How much of the stack `id` the placement rules observe.
    pub(crate) fn shape(nodes: &P::Nodes, id: P::StackId) -> FrameShape {
        let ambient = |id: &P::StackId| matches!(nodes.stacks()[id], Stack::Var(_));
        match &nodes.stacks()[&id] {
            | Stack::Var(_) => FrameShape::Ambient,
            | Stack::Arg(Cons(_, rest)) | Stack::Tag(Cons(_, rest)) if ambient(rest) => {
                FrameShape::Frame
            }
            | Stack::Arg(_) | Stack::Tag(_) => FrameShape::Compound,
            | Stack::Ext(form) => form.shape(nodes),
        }
    }
}

impl<P: Phase> Computation<P> {
    /// What the placement rules observe about this computation, reached through `edge`.
    pub(crate) fn stack_use(&self, edge: Edge<P>, nodes: &P::Nodes) -> StackUse {
        match self {
            | Self::LetStack(Let { bindee, tail, .. }) => StackUse::Let {
                bindee: Stack::<P>::shape(nodes, *bindee),
                tail_branch: nodes.compus()[tail].is_branch(),
            },
            | computation if computation.is_branch() => {
                StackUse::Branch { guarded: matches!(edge, Edge::StackLetTail(_)) }
            }
            | Self::Hole(SHole(stack))
            | Self::ExternCall(ExternCall { stack, .. })
            | Self::LetArg(Let { bindee: stack, .. })
            | Self::CoCase(SCoMatch { scrut: stack, .. }) => {
                StackUse::Consumer(Stack::<P>::shape(nodes, *stack))
            }
            | Self::Ext(form) => match form.consumed_stack() {
                | Some(stack) => StackUse::Consumer(Stack::<P>::shape(nodes, stack)),
                | None => StackUse::Other,
            },
            | _ => StackUse::Other,
        }
    }
}
