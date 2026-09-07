//! Bidirectional checking modes and the common judgment protocol.

use super::*;

pub trait Tyck<'a> {
    type Out;
    type Action;
    /// Entry point for type checking with optional administrative wrapping.
    fn tyck_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out> {
        self.tyck_inner_k(tycker, action)
    }
    /// Core implementation for type checking.
    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out>;
}

/// Synthesis or analysis mode, optionally with an expected annotation.
#[derive(Clone, Copy, Debug)]
pub enum Switch<Ann> {
    Syn,
    Ana(Ann),
}

/// Internal checking trace used to select source-level diagnostic context.
#[derive(Clone, Debug)]
pub enum TyckTask {
    Pat(su::PatId, Switch<AnnId>),
    Term(su::TermId, Switch<AnnId>),
    Lub(AnnId, AnnId),
    SignatureGen(ss::AnnId),
    StructureGen(ss::AnnId),
    MonadicLiftPat(ss::PatId),
    MonadicLiftTerm(ss::TermId),
}

/// Wrapper for passing synthesis/analysis mode into `Tyck`.
///
/// Analytic term checking records the environment under which an expected type was already
/// substituted and normalized. Transparent term wrappers forward that provenance so their
/// source-level nesting and inner lexical extensions do not apply the same environment repeatedly.
pub struct Action<Ann> {
    pub switch: Switch<Ann>,
    pub(super) prepared_environment: Option<ss::TyEnv>,
}

impl<Ann> Action<Ann> {
    pub fn syn() -> Self {
        Self { switch: Switch::Syn, prepared_environment: None }
    }
    pub fn ana(ann: Ann) -> Self {
        Self { switch: Switch::Ana(ann), prepared_environment: None }
    }
    pub(super) fn ana_prepared(ann: Ann, environment: &ss::TyEnv) -> Self {
        Self { switch: Switch::Ana(ann), prepared_environment: Some(environment.clone()) }
    }
    pub(super) fn forward(switch: Switch<Ann>, prepared_environment: Option<&ss::TyEnv>) -> Self {
        Self { switch, prepared_environment: prepared_environment.cloned() }
    }
}
