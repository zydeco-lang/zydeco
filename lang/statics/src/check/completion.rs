use super::{AnnotationCompatibility, Switch, Tycker};
use crate::{arena::ArenaAssoc, surface_syntax as su, syntax::*};
use zydeco_utils::arena::ArenaAccess;

/// Incoming analytic constraints and per-definition evidence from one completion check.
#[derive(Clone, Debug)]
pub struct CompletionTyping {
    expectations: Vec<AnnId>,
    compatibility: ArenaAssoc<DefId, AnnotationCompatibility>,
}

impl CompletionTyping {
    pub fn expectations(&self) -> &[AnnId] {
        &self.expectations
    }

    pub fn compatibility(&self, definition: DefId) -> AnnotationCompatibility {
        self.compatibility.get(&definition).copied().unwrap_or_default()
    }
}

pub(super) struct CompletionCapture {
    target: su::TermId,
    expectations: Vec<AnnId>,
}

impl Tycker<'_> {
    pub(crate) fn set_completion_target(&mut self, target: su::TermId) -> &mut Self {
        self.completion = Some(CompletionCapture { target, expectations: Vec::new() });
        self
    }

    pub(super) fn observe_completion(&mut self, term: su::TermId, switch: Switch<AnnId>) {
        if let Some(capture) = &mut self.completion
            && capture.target == term
            && let Switch::Ana(expected) = switch
            && !capture.expectations.contains(&expected)
        {
            capture.expectations.push(expected);
        }
    }

    pub(crate) fn completion_typing(&mut self, definitions: &[DefId]) -> Option<CompletionTyping> {
        let expectations = self.completion.as_ref()?.expectations.clone();
        let compatibility = definitions
            .iter()
            .copied()
            .map(|definition| {
                let evidence = self
                    .statics
                    .annotations_var
                    .get(&definition)
                    .copied()
                    .and_then(|annotation| {
                        expectations
                            .iter()
                            .map(|expected| self.annotation_compatibility(annotation, *expected))
                            .max()
                    })
                    .unwrap_or_default();
                (definition, evidence)
            })
            .collect();
        Some(CompletionTyping { expectations, compatibility })
    }
}
