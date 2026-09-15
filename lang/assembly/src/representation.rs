//! Policies for choosing among locally justified value representations.
//!
//! The analysis establishes an opportunity before asking a policy to select it.
//! Policies cannot change call signatures, tracing rules, or escape evidence.

use std::{fmt, str::FromStr};

/// The analysis that justifies eliminating a residual allocation.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UnboxingReason {
    DirectProduct,
    DirectClosure,
    ProjectedVariable,
    SharedClosure,
}

/// A representation choice already justified by the local analysis.
#[derive(Clone, Copy, Debug)]
pub struct UnboxingOpportunity {
    pub reason: UnboxingReason,
    /// Number of emitted field words; these retain their tagged-word representation.
    pub fields: usize,
}

/// A preference among valid choices, consumed only during compiler analysis.
pub trait RepresentationPolicy {
    fn unbox(&self, opportunity: UnboxingOpportunity) -> bool;

    fn scalar_boxing(&self) -> zydeco_syntax::scalar::ScalarBoxing {
        zydeco_syntax::scalar::ScalarBoxing::Eliminate
    }
}

/// Keep residual products and closures boxed.
#[derive(Clone, Copy, Debug, Default)]
pub struct Boxed;

impl RepresentationPolicy for Boxed {
    fn unbox(&self, _: UnboxingOpportunity) -> bool {
        false
    }

    fn scalar_boxing(&self) -> zydeco_syntax::scalar::ScalarBoxing {
        zydeco_syntax::scalar::ScalarBoxing::Keep
    }
}

/// Fuse adjacent construction/elimination without expanding shared variables.
#[derive(Clone, Copy, Debug, Default)]
pub struct Direct;

impl RepresentationPolicy for Direct {
    fn unbox(&self, opportunity: UnboxingOpportunity) -> bool {
        matches!(opportunity.reason, UnboxingReason::DirectProduct | UnboxingReason::DirectClosure)
    }
}

/// Also expand variables whose uses are all compatible projections.
#[derive(Clone, Copy, Debug, Default)]
pub struct Local;

impl RepresentationPolicy for Local {
    fn unbox(&self, opportunity: UnboxingOpportunity) -> bool {
        opportunity.reason != UnboxingReason::SharedClosure
    }
}

/// Additionally split a local closure whose every use opens that closure.
#[derive(Clone, Copy, Debug, Default)]
pub struct Shared;

impl RepresentationPolicy for Shared {
    fn unbox(&self, _: UnboxingOpportunity) -> bool {
        true
    }
}

/// Runtime selection of the same policies available as Rust types.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub enum RepresentationStrategy {
    Boxed,
    Direct,
    #[default]
    Local,
    Shared,
}

impl RepresentationStrategy {
    pub const ALL: &[Self] = &[Self::Boxed, Self::Direct, Self::Local, Self::Shared];

    pub fn name(self) -> &'static str {
        match self {
            | Self::Boxed => "boxed",
            | Self::Direct => "direct",
            | Self::Local => "local",
            | Self::Shared => "shared",
        }
    }
}

impl RepresentationPolicy for RepresentationStrategy {
    fn unbox(&self, opportunity: UnboxingOpportunity) -> bool {
        match self {
            | Self::Boxed => Boxed.unbox(opportunity),
            | Self::Direct => Direct.unbox(opportunity),
            | Self::Local => Local.unbox(opportunity),
            | Self::Shared => Shared.unbox(opportunity),
        }
    }

    fn scalar_boxing(&self) -> zydeco_syntax::scalar::ScalarBoxing {
        match self {
            | Self::Boxed => zydeco_syntax::scalar::ScalarBoxing::Keep,
            | Self::Direct | Self::Local | Self::Shared => {
                zydeco_syntax::scalar::ScalarBoxing::Eliminate
            }
        }
    }
}

impl fmt::Display for RepresentationStrategy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
#[error("unknown representation strategy `{0}`; expected boxed, direct, local, or shared")]
pub struct InvalidRepresentationStrategy(String);

impl FromStr for RepresentationStrategy {
    type Err = InvalidRepresentationStrategy;

    fn from_str(name: &str) -> Result<Self, Self::Err> {
        Self::ALL
            .iter()
            .copied()
            .find(|strategy| strategy.name() == name)
            .ok_or_else(|| InvalidRepresentationStrategy(name.to_owned()))
    }
}
