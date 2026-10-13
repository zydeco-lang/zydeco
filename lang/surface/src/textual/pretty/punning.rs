//! Canonical pun recognition for terms and patterns.

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
