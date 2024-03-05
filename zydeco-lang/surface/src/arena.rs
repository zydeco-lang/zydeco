pub use zydeco_utils::{arena::*, new_key_type, span::Span};

use std::{fmt::Debug, ops::AddAssign};

pub trait DefPtr: IndexLike<Meta = usize> + Eq + std::hash::Hash {}
pub trait PatPtr: IndexLike<Meta = usize> + Eq + std::hash::Hash {}
pub trait CoPatPtr: IndexLike<Meta = usize> + Eq + std::hash::Hash {}
pub trait TermPtr: IndexLike<Meta = usize> + Eq + std::hash::Hash {}

/// keeps all ids and spans, the corresponding source location
#[derive(Debug)]
pub struct SpanArena<DefId: DefPtr, PatId: PatPtr, CoPatId: CoPatPtr, TermId: TermPtr> {
    pub defs: ArenaSparse<DefId, Span>,
    pub pats: ArenaSparse<PatId, Span>,
    pub copats: ArenaSparse<CoPatId, Span>,
    pub terms: ArenaSparse<TermId, Span>,
}

impl<DefId, PatId, CoPatId, TermId> AddAssign<SpanArena<DefId, PatId, CoPatId, TermId>>
    for SpanArena<DefId, PatId, CoPatId, TermId>
where
    DefId: DefPtr,
    PatId: PatPtr,
    CoPatId: CoPatPtr,
    TermId: TermPtr,
{
    fn add_assign(&mut self, rhs: Self) {
        self.defs += rhs.defs;
        self.pats += rhs.pats;
        self.copats += rhs.copats;
        self.terms += rhs.terms;
    }
}

mod span_arena_impl {
    use super::*;

    impl<DefId, PatId, CoPatId, TermId> SpanArena<DefId, PatId, CoPatId, TermId>
    where
        DefId: DefPtr,
        PatId: PatPtr,
        CoPatId: CoPatPtr,
        TermId: TermPtr,
    {
        pub fn new(alloc: &mut GlobalAlloc) -> Self {
            SpanArena {
                defs: ArenaSparse::new(alloc.alloc()),
                pats: ArenaSparse::new(alloc.alloc()),
                copats: ArenaSparse::new(alloc.alloc()),
                terms: ArenaSparse::new(alloc.alloc()),
            }
        }
    }

    impl<DefId, PatId, CoPatId, TermId> std::ops::Index<DefId>
        for SpanArena<DefId, PatId, CoPatId, TermId>
    where
        DefId: DefPtr,
        PatId: PatPtr,
        CoPatId: CoPatPtr,
        TermId: TermPtr,
    {
        type Output = Span;

        fn index(&self, id: DefId) -> &Self::Output {
            self.defs.get(id).unwrap()
        }
    }

    // impl<DefId, PatId, CoPatId, TermId> std::ops::Index<PatId>
    //     for SpanArena<DefId, PatId, CoPatId, TermId>
    // where
    //     DefId: DefPtr,
    //     PatId: PatPtr,
    //     CoPatId: CoPatPtr,
    //     TermId: TermPtr,
    // {
    //     type Output = Span;

    //     fn index(&self, id: PatId) -> &Self::Output {
    //         self.pats.get(id).unwrap()
    //     }
    // }

    // impl<DefId, PatId, CoPatId, TermId> std::ops::Index<CoPatId>
    //     for SpanArena<DefId, PatId, CoPatId, TermId>
    // where
    //     DefId: DefPtr,
    //     PatId: PatPtr,
    //     CoPatId: CoPatPtr,
    //     TermId: TermPtr,
    // {
    //     type Output = Span;

    //     fn index(&self, id: CoPatId) -> &Self::Output {
    //         self.copats.get(id).unwrap()
    //     }
    // }

    // impl<DefId, PatId, CoPatId, TermId> std::ops::Index<TermId>
    //     for SpanArena<DefId, PatId, CoPatId, TermId>
    // where
    //     DefId: DefPtr,
    //     PatId: PatPtr,
    //     CoPatId: CoPatPtr,
    //     TermId: TermPtr,
    // {
    //     type Output = Span;

    //     fn index(&self, id: TermId) -> &Self::Output {
    //         self.terms.get(id).unwrap()
    //     }
    // }
}
