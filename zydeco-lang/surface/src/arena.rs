use crate::syntax::*;
use slotmap::SlotMap;

slotmap::new_key_type! {
    pub struct PatternId;
    pub struct TermId;
}

#[derive(Default)]
pub struct Arena {
    pub patterns: SlotMap<PatternId, Sp<Pattern>>,
    pub terms: SlotMap<TermId, Sp<Term>>,
}

impl Arena {
    pub fn pattern(&mut self, pattern: Sp<Pattern>) -> PatternId {
        self.patterns.insert(pattern)
    }
    pub fn term(&mut self, term: Sp<Term>) -> TermId {
        self.terms.insert(term)
    }
}
