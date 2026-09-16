//! Scoped syntax construction without name lookup or analysis side effects.
use super::syntax::*;
use zydeco_utils::prelude::IdAllocator;

pub(super) struct ScopedBuilder {
    allocator: IdAllocator<ScopedScope>,
    pub origins: TextualOrigins,
    pub defs: ArenaSparse<ScopedScope, DefId>,
    pub pats: ArenaIndexed<ScopedScope, PatId>,
    pub terms: ArenaIndexed<ScopedScope, TermId>,
    pub blocks: ArenaAssoc<TermId, ContextualTerm<BindingContext, BlockBody>>,
}
impl ScopedBuilder {
    pub fn new(bitter: &BitterArena, origins: TextualOrigins) -> Self {
        let mut pats = ArenaIndexed::default();
        pats.reserve_ids(bitter.pats.iter().map(|(id, _)| id));
        let generated = bitter
            .terms
            .iter()
            .filter(|(_, term)| matches!(term, Term::MobileParam(_) | Term::MobileBind(_)))
            .count();
        let mut terms = ArenaIndexed::default();
        terms.reserve_ids_with_additional(bitter.terms.iter().map(|(id, _)| id), generated);
        Self {
            allocator: IdAllocator::new(),
            origins,
            defs: ArenaSparse::default(),
            pats,
            terms,
            blocks: ArenaAssoc::default(),
        }
    }
    pub fn term(&mut self, source: TermId, term: Term<DefId>) -> TermId {
        let id = self.allocator.alloc();
        self.terms.insert_new(id, term);
        let textual =
            self.origins.source(&source.into()).expect("a source term retains its origin");
        self.origins.insert_new(textual, id.into());
        id
    }
    pub fn finish(self, bitter: BitterArena, users: ArenaForth<DefId, TermId>) -> ScopedArena {
        ScopedArena {
            defs: self.defs,
            pats: self.pats,
            terms: self.terms,
            blocks: self.blocks,
            origins: self.origins,
            users,
            partial_binders: bitter.partial_binders,
        }
    }
}
