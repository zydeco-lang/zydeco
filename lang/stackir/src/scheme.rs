//! Stack-passing compilation schemes and the placement rule of each scheme's fragment.
//!
//! A scheme decides where compilation names the current stack. Its output lies in the scheme's
//! fragment, which SPS and SPSLow validation both check through [`SpsScheme::check`].

use std::fmt;

/// Where stack-passing compilation names the current stack.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub enum SpsScheme {
    /// Name no stack; every arm of a branch receives its own copy of the consumer.
    Duplicating,
    /// Name the consumer stack once at each branch, so that the arms share it.
    #[default]
    BranchJoin,
}

impl SpsScheme {
    pub const ALL: &'static [Self] = &[Self::Duplicating, Self::BranchJoin];

    pub fn name(self) -> &'static str {
        match self {
            | Self::Duplicating => "dup",
            | Self::BranchJoin => "br",
        }
    }

    pub fn description(self) -> &'static str {
        match self {
            | Self::Duplicating => "copy the consumer stack into every arm of a branch",
            | Self::BranchJoin => "name the consumer stack once at each branch",
        }
    }

    /// Whether a branch names its consumer stack once, so that its arms share it.
    /// Otherwise every arm consumes its own copy of the stack the branch receives.
    pub(crate) fn shares_branch_stack(self) -> bool {
        match self {
            | Self::Duplicating => false,
            | Self::BranchJoin => true,
        }
    }

    /// The placement violation, if any, of one computation's stack use in this scheme's fragment.
    pub(crate) fn check(self, usage: StackUse) -> Option<PlacementError> {
        match (self, usage) {
            | (Self::Duplicating, StackUse::Let { .. }) => Some(PlacementError::UnexpectedStackLet),
            | (Self::Duplicating, _) => None,
            | (Self::BranchJoin, StackUse::Let { tail_branch: false }) => {
                Some(PlacementError::NonBranchStackLet)
            }
            | (Self::BranchJoin, StackUse::Branch { guarded: false }) => {
                Some(PlacementError::UnguardedBranch)
            }
            | (Self::BranchJoin, _) => None,
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
}

/// What the placement rules observe about one computation.
#[derive(Clone, Copy, Debug)]
pub(crate) enum StackUse {
    /// A stack let-binding, recording whether its tail is a branch.
    Let { tail_branch: bool },
    /// A value case split or scalar comparison, recording whether a stack let-binding owns it.
    Branch { guarded: bool },
    /// Any other computation.
    Other,
}
