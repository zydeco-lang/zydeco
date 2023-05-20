use crate::syntax::*;
use slotmap::SlotMap;

slotmap::new_key_type! {
    pub struct PatternId;
    pub struct TermId;
}

#[derive(Default)]
pub struct Arena {
    pub patterns: SlotMap<PatternId, Pattern>,
    pub terms: SlotMap<TermId, Term>,
}

impl Arena {
    pub fn pattern(&mut self, pattern: Pattern) -> PatternId {
        self.patterns.insert(pattern)
    }
    pub fn term(&mut self, term: Term) -> TermId {
        self.terms.insert(term)
    }
}
