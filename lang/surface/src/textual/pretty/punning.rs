//! Canonical pun recognition shared by rendering and source audits.

use crate::textual::syntax::*;

#[derive(Copy, Clone)]
pub(super) enum PunnedTermPayload {
    Variable,
    Annotated(TermId),
}

#[derive(Copy, Clone)]
pub(super) enum PunnedPatternPayload {
    Variable,
    Annotated(TermId),
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
        match &self.arena.terms[&inner] {
            | Term::Var(VarName(name)) if name == &field.0 => Some(PunnedTermPayload::Variable),
            | Term::Ann(Ann { tm, ty }) => match &self.arena.terms[tm] {
                | Term::Var(VarName(name)) if name == &field.0 => {
                    Some(PunnedTermPayload::Annotated(*ty))
                }
                | _ => None,
            },
            | _ => None,
        }
    }

    pub(super) fn pattern_payload(
        &self, field: &FieldName, inner: PatId,
    ) -> Option<PunnedPatternPayload> {
        match &self.arena.pats[&inner] {
            | Pattern::Var(definition) if self.arena.defs[definition].0 == field.0 => {
                Some(PunnedPatternPayload::Variable)
            }
            | Pattern::Ann(Ann { tm, ty }) => match &self.arena.pats[tm] {
                | Pattern::Var(definition) if self.arena.defs[definition].0 == field.0 => {
                    Some(PunnedPatternPayload::Annotated(*ty))
                }
                | _ => None,
            },
            | _ => None,
        }
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
        let (start, end) = self.spans[&EntityId::Term(term)].get_cursor1();
        self.source.get(start..end).is_some_and(|source| source.starts_with('='))
    }
}
