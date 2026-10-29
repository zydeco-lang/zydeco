//! Bidirectional checking modes and the common judgment protocol.

use super::*;

pub trait Tyck<'a> {
    type Out;
    type Action;
    /// Entry point for type checking with optional administrative wrapping.
    fn tyck(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> Result<Self::Out> {
        self.tyck_inner(tycker, action)
    }
    /// Core implementation for type checking.
    fn tyck_inner(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> Result<Self::Out>;
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

/// Scoped entities whose judgments are keyed on their interned identity.
pub(super) trait JudgmentSubject: Copy {
    type Key<'db>;
    fn intern<'db>(self, db: &'db dyn crate::query::TyckDb) -> Self::Key<'db>;
}

impl JudgmentSubject for su::TermId {
    type Key<'db> = crate::query::InternedTerm<'db>;
    fn intern<'db>(self, db: &'db dyn crate::query::TyckDb) -> Self::Key<'db> {
        crate::query::InternedTerm::new(db, self)
    }
}

impl JudgmentSubject for su::PatId {
    type Key<'db> = crate::query::InternedPat<'db>;
    fn intern<'db>(self, db: &'db dyn crate::query::TyckDb) -> Self::Key<'db> {
        crate::query::InternedPat::new(db, self)
    }
}

impl<'a> Tycker<'a> {
    /// Run a judgment query for the entity under check at its current
    /// allocation occurrence, and return the typed fragment it produces.
    ///
    /// A judgment answers `None` only for an entity of another syntactic shape,
    /// and the checker dispatched on the shape before asking.
    pub(super) fn judge<Subject: JudgmentSubject, Input, Outcome>(
        &self, subject: Subject, input: Input,
        query: impl FnOnce(
            &'a dyn crate::query::TyckDb,
            crate::query::ScopedData<'a>,
            Subject::Key<'a>,
            Input,
            u32,
        ) -> Option<Outcome>,
    ) -> Outcome {
        let key = subject.intern(self.db);
        query(self.db, self.data, key, input, self.site_occurrence()).unwrap_or_else(|| {
            unreachable!("judgments are query-produced for the entity under check")
        })
    }
}
