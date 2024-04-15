use super::identifier::*;
use std::{fmt::Debug, ops::AddAssign};
use zydeco_utils::arena::*;

#[derive(Debug)]
pub struct GenArena<T, DefId: DefPtr, PatId: PatPtr, CoPatId: CoPatPtr, TermId: TermPtr> {
    pub defs: ArenaSparse<DefId, T>,
    pub pats: ArenaSparse<PatId, T>,
    pub copats: ArenaSparse<CoPatId, T>,
    pub terms: ArenaSparse<TermId, T>,
}

impl<T, DefId, PatId, CoPatId, TermId> GenArena<T, DefId, PatId, CoPatId, TermId>
where
    DefId: DefPtr,
    PatId: PatPtr,
    CoPatId: CoPatPtr,
    TermId: TermPtr,
{
    pub fn new(alloc: &mut GlobalAlloc) -> Self {
        GenArena {
            defs: ArenaSparse::new(alloc.alloc()),
            pats: ArenaSparse::new(alloc.alloc()),
            copats: ArenaSparse::new(alloc.alloc()),
            terms: ArenaSparse::new(alloc.alloc()),
        }
    }
}

impl<T, DefId, PatId, CoPatId, TermId> AddAssign<GenArena<T, DefId, PatId, CoPatId, TermId>>
    for GenArena<T, DefId, PatId, CoPatId, TermId>
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
