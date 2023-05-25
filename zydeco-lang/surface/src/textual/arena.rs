use super::syntax::*;
use slotmap::SlotMap;
use zydeco_utils::span::FileInfo;

slotmap::new_key_type! {
    pub struct PatternId;
    pub struct TermId;
}

#[derive(Default)]
pub struct Arena {
    pub patterns: SlotMap<PatternId, Sp<Pattern>>,
    pub terms: SlotMap<TermId, Sp<Term>>,
    pub deps: Vec<String>,
    pub project: Option<String>,
}

impl Arena {
    pub fn pattern(&mut self, pattern: Sp<Pattern>) -> PatternId {
        self.patterns.insert(pattern)
    }
    pub fn term(&mut self, term: Sp<Term>) -> TermId {
        self.terms.insert(term)
    }
    pub fn span_map(&mut self, file_info: &FileInfo) {
        for (_, pattern) in self.patterns.iter_mut() {
            pattern.span.set_info(file_info);
        }
        for (_, term) in self.terms.iter_mut() {
            term.span.set_info(file_info);
        }
    }
}
