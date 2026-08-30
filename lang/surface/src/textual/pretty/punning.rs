//! Canonical pun recognition shared by rendering and source audits.

use crate::textual::syntax::*;

#[derive(Copy, Clone)]
pub(super) enum PunnedTermPayload {
    Variable,
    Annotated { variable: TermId, classifier: TermId },
}

#[derive(Copy, Clone)]
pub(super) enum PunnedPatternPayload {
    Variable,
    Annotated { variable: PatId, classifier: TermId },
}

pub(super) struct Punning<'arena> {
    arena: &'arena TextArena,
}

impl<'arena> Punning<'arena> {
    pub(super) fn new(arena: &'arena TextArena) -> Self {
        Self { arena }
    }

    pub(super) fn term_payload(
        &self, field: &FieldName, inner: TermId,
    ) -> Option<PunnedTermPayload> {
        if !self.is_trivia_free(inner) {
            return None;
        }
        match &self.arena.terms[&inner] {
            | Term::Var(VarName(name)) if name == &field.0 => Some(PunnedTermPayload::Variable),
            | Term::Ann(Ann { tm, ty }) => match &self.arena.terms[tm] {
                | Term::Var(VarName(name)) if name == &field.0 && self.is_trivia_free(*tm) => {
                    Some(PunnedTermPayload::Annotated { variable: *tm, classifier: *ty })
                }
                | _ => None,
            },
            | _ => None,
        }
    }

    pub(super) fn pattern_payload(
        &self, field: &FieldName, inner: PatId,
    ) -> Option<PunnedPatternPayload> {
        if !self.is_trivia_free(inner) {
            return None;
        }
        match &self.arena.pats[&inner] {
            | Pattern::Var(definition) if self.arena.defs[definition].0 == field.0 => {
                Some(PunnedPatternPayload::Variable)
            }
            | Pattern::Ann(Ann { tm, ty }) => match &self.arena.pats[tm] {
                | Pattern::Var(definition)
                    if self.arena.defs[definition].0 == field.0 && self.is_trivia_free(*tm) =>
                {
                    Some(PunnedPatternPayload::Annotated { variable: *tm, classifier: *ty })
                }
                | _ => None,
            },
            | _ => None,
        }
    }

    fn is_trivia_free(&self, entity: impl Into<EntityId>) -> bool {
        self.arena.trivia.leading_comments(entity.into()).is_empty()
    }
}

/// Read-only inventory of explicit named terms that canonical formatting can
/// replace with puns.
pub struct NamedTermPunningAudit<'source, 'arena> {
    source: &'source str,
    spans: &'arena SpanArena,
    punning: Punning<'arena>,
}

impl<'source, 'arena> NamedTermPunningAudit<'source, 'arena> {
    pub fn new(source: &'source str, spans: &'arena SpanArena, arena: &'arena TextArena) -> Self {
        Self { source, spans, punning: Punning::new(arena) }
    }

    pub fn candidates(&self) -> Vec<TermId> {
        self.punning
            .arena
            .terms
            .iter()
            .filter_map(|(term, syntax)| match syntax {
                | Term::Named(Named(field, inner))
                    if self.punning.term_payload(field, *inner).is_some()
                        && !self.source_is_punned(*term) =>
                {
                    Some(*term)
                }
                | _ => None,
            })
            .collect()
    }

    fn source_is_punned(&self, term: TermId) -> bool {
        let span = self.spans[&EntityId::Term(term)].range();
        let (start, end) = (span.start, span.end);
        self.source.get(start..end).is_some_and(|source| source.starts_with('='))
    }
}
