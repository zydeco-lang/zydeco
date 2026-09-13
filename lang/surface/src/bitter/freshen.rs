//! Occurrence copying for bitter syntax, retaining each node's textual origin.

use super::{Alloc, BitterBuilder, syntax::*};
use crate::fold::Folder;

/// Copies reachable occurrences into the same builder with fresh identities.
///
/// Source and signature boundaries are copied along with their payloads. There
/// is no memoization: repeated edges intentionally produce independent copies,
/// including fresh definitions for binder occurrences. Inputs must be acyclic.
pub struct FreshenFolder<'builder> {
    pub builder: &'builder mut BitterBuilder,
}

impl Folder for FreshenFolder<'_> {
    type Ref = VarName;

    fn fold_def(&mut self, id: DefId) -> DefId {
        let name = self.builder.arena.defs[&id].clone();
        let origin = self.builder.arena.origins.source(&id.into()).unwrap();
        Alloc::alloc(self.builder, name, origin)
    }

    fn fold_pat(&mut self, id: PatId) -> PatId {
        let pattern = self.builder.arena.pats[&id].clone();
        let origin = self.builder.arena.origins.source(&id.into()).unwrap();
        let pattern = pattern.fold_with(self);
        Alloc::alloc(self.builder, pattern, origin)
    }

    fn fold_term(&mut self, id: TermId) -> TermId {
        let term = self.builder.arena.terms[&id].clone();
        let origin = self.builder.arena.origins.source(&id.into()).unwrap();
        let term = term.fold_with(self);
        Alloc::alloc(self.builder, term, origin)
    }
}

#[cfg(test)]
mod tests;
