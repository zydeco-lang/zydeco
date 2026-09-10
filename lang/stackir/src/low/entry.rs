//! Administrative value words shared by code entries and their callers.

use super::syntax::{VPatId, ValueId};
use crate::protocol::{StackProtocol, ValueProtocol};

/// The source protocol at a code entry, after its administrative words are consumed.
#[derive(Clone, Debug, Eq, PartialEq, derive_more::Display)]
pub enum EntryProtocol {
    #[display("closure[{_0}]")]
    Closure(StackProtocol),
    /// The accepted result only; the continuation hides its saved residual stack.
    #[display("continuation[{_0}]")]
    Continuation(ValueProtocol),
}

impl EntryProtocol {
    pub fn unknown(kind: EntryKind) -> Self {
        match kind {
            | EntryKind::Closure => Self::Closure(StackProtocol::Unknown),
            | EntryKind::Continuation => Self::Continuation(ValueProtocol::Unknown),
        }
    }

    pub fn kind(&self) -> EntryKind {
        match self {
            | Self::Closure(_) => EntryKind::Closure,
            | Self::Continuation(_) => EntryKind::Continuation,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EntryKind {
    Closure,
    Continuation,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, derive_more::Display)]
pub enum EntryRole {
    #[display("environment")]
    Environment,
    #[display("result")]
    Result,
}

/// Parameters consumed before the block body sees its residual stack.
/// Each component uses the target's ordinary value-word representation.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EntryParameters {
    Closure { environment: VPatId },
    Continuation { result: VPatId, environment: VPatId },
}

impl EntryParameters {
    pub fn kind(self) -> EntryKind {
        match self {
            | Self::Closure { .. } => EntryKind::Closure,
            | Self::Continuation { .. } => EntryKind::Continuation,
        }
    }

    pub fn environment(self) -> VPatId {
        match self {
            | Self::Closure { environment } | Self::Continuation { environment, .. } => environment,
        }
    }

    /// Stack-consumption order, including the environment restored by a continuation package.
    pub fn words(self) -> impl DoubleEndedIterator<Item = (EntryRole, VPatId)> {
        let result = match self {
            | Self::Closure { .. } => None,
            | Self::Continuation { result, .. } => Some((EntryRole::Result, result)),
        };
        [result, Some((EntryRole::Environment, self.environment()))].into_iter().flatten()
    }
}

/// The word supplied at a transfer. A continuation's environment is already
/// part of the residual stack restored by `OpenContinuation`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EntryArgument {
    Closure { environment: ValueId },
    Continuation { result: ValueId },
}

impl EntryArgument {
    pub fn kind(self) -> EntryKind {
        match self {
            | Self::Closure { .. } => EntryKind::Closure,
            | Self::Continuation { .. } => EntryKind::Continuation,
        }
    }

    pub fn word(self) -> (EntryRole, ValueId) {
        match self {
            | Self::Closure { environment } => (EntryRole::Environment, environment),
            | Self::Continuation { result } => (EntryRole::Result, result),
        }
    }
}
