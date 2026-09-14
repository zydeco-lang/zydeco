//! Facts emitted at the semantic resolver's actual visits.
use super::{CompletionSite, scope::*, syntax::*};
use crate::textual::syntax as t;

/// A successful lookup; consumers never need to repeat name resolution.
pub struct ResolvedReference<'a> {
    pub occurrence: TermId,
    pub definition: DefId,
    pub dependency: Option<BindingSite>,
    pub active_bindings: &'a rpds::VectorSync<BindingSite>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScopeKind {
    Documentation,
    Hole,
}

pub struct ScopeEvent<'a> {
    pub occurrence: TermId,
    pub origin: Option<t::EntityId>,
    pub kind: ScopeKind,
    pub scope: NameScope<'a>,
}

/// Passive observers cannot change resolution, prune descent, or reject syntax.
pub trait ResolutionObserver {
    type Output;
    fn reference(&mut self, _event: &ResolvedReference<'_>) {}
    fn scope(&mut self, _event: &ScopeEvent<'_>) {}
    fn finish(self) -> Self::Output;
}

impl ResolutionObserver for () {
    type Output = ();
    fn finish(self) {}
}

impl<A: ResolutionObserver, B: ResolutionObserver> ResolutionObserver for (A, B) {
    type Output = (A::Output, B::Output);
    fn reference(&mut self, event: &ResolvedReference<'_>) {
        self.0.reference(event);
        self.1.reference(event);
    }
    fn scope(&mut self, event: &ScopeEvent<'_>) {
        self.0.scope(event);
        self.1.scope(event);
    }
    fn finish(self) -> Self::Output {
        (self.0.finish(), self.1.finish())
    }
}

#[derive(Default)]
pub struct ReferenceIndex {
    users: ArenaForth<DefId, TermId>,
}
impl ResolutionObserver for ReferenceIndex {
    type Output = ArenaForth<DefId, TermId>;
    fn reference(&mut self, event: &ResolvedReference<'_>) {
        self.users.insert_new(event.definition, event.occurrence);
    }
    fn finish(self) -> Self::Output {
        self.users
    }
}

#[derive(Default)]
pub struct DocumentationObserver {
    scopes: ArenaAssoc<TermId, ScopeSnapshot>,
}
impl ResolutionObserver for DocumentationObserver {
    type Output = ArenaAssoc<TermId, ScopeSnapshot>;
    fn scope(&mut self, event: &ScopeEvent<'_>) {
        if event.kind == ScopeKind::Documentation {
            self.scopes.insert_new(event.occurrence, event.scope.snapshot());
        }
    }
    fn finish(self) -> Self::Output {
        self.scopes
    }
}

pub struct CompletionObserver {
    target: t::TermId,
    site: Option<CompletionSite>,
}
impl CompletionObserver {
    pub fn new(target: t::TermId) -> Self {
        Self { target, site: None }
    }
}
impl ResolutionObserver for CompletionObserver {
    type Output = Option<CompletionSite>;
    fn scope(&mut self, event: &ScopeEvent<'_>) {
        if event.kind == ScopeKind::Hole && event.origin == Some(self.target.into()) {
            self.site =
                Some(CompletionSite { target: event.occurrence, scope: event.scope.snapshot() });
        }
    }
    fn finish(self) -> Self::Output {
        self.site
    }
}
