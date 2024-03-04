use std::{fmt::Debug, ops::AddAssign};
use zydeco_utils::{arena::*, new_key_type, span::Span};

new_key_type! {
    pub struct DefId;
    pub struct PatternId;
    pub struct CoPatternId;
    pub struct TermId;
}

/// keeps all ids and spans, the corresponding source location
#[derive(Debug)]
pub struct SpanArena {
    pub defs: ArenaSparse<DefId, Span>,
    pub pats: ArenaSparse<PatternId, Span>,
    pub copats: ArenaSparse<CoPatternId, Span>,
    pub terms: ArenaSparse<TermId, Span>,
}

impl AddAssign<SpanArena> for SpanArena {
    fn add_assign(&mut self, rhs: SpanArena) {
        self.defs += rhs.defs;
        self.pats += rhs.pats;
        self.copats += rhs.copats;
        self.terms += rhs.terms;
    }
}

mod span_arena_impl {
    use super::*;

    impl SpanArena {
        pub fn new(alloc: &mut GlobalAlloc) -> Self {
            SpanArena {
                defs: ArenaSparse::new(alloc.alloc()),
                pats: ArenaSparse::new(alloc.alloc()),
                copats: ArenaSparse::new(alloc.alloc()),
                terms: ArenaSparse::new(alloc.alloc()),
            }
        }
    }

    impl std::ops::Index<DefId> for SpanArena {
        type Output = Span;

        fn index(&self, id: DefId) -> &Self::Output {
            self.defs.get(id).unwrap()
        }
    }

    impl std::ops::Index<PatternId> for SpanArena {
        type Output = Span;

        fn index(&self, id: PatternId) -> &Self::Output {
            self.pats.get(id).unwrap()
        }
    }

    impl std::ops::Index<CoPatternId> for SpanArena {
        type Output = Span;

        fn index(&self, id: CoPatternId) -> &Self::Output {
            self.copats.get(id).unwrap()
        }
    }

    impl std::ops::Index<TermId> for SpanArena {
        type Output = Span;

        fn index(&self, id: TermId) -> &Self::Output {
            self.terms.get(id).unwrap()
        }
    }
}
