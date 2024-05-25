use super::identifier::*;
use std::{fmt::Debug, ops::AddAssign};
use zydeco_utils::arena::*;

#[derive(Debug)]
pub struct ArenaGen<
    T,
    DefId: DefPtr,
    PatId: PatPtr,
    CoPatId: CoPatPtr,
    TermId: TermPtr,
    DeclId: DeclPtr,
> {
    pub defs: ArenaSparse<DefId, T>,
    pub pats: ArenaSparse<PatId, T>,
    pub copats: ArenaSparse<CoPatId, T>,
    pub terms: ArenaSparse<TermId, T>,
    pub decls: ArenaSparse<DeclId, T>,
}

impl<T, DefId, PatId, CoPatId, TermId, DeclId> ArenaGen<T, DefId, PatId, CoPatId, TermId, DeclId>
where
    DefId: DefPtr,
    PatId: PatPtr,
    CoPatId: CoPatPtr,
    TermId: TermPtr,
    DeclId: DeclPtr,
{
    pub fn new(alloc: &mut GlobalAlloc) -> Self {
        ArenaGen {
            defs: ArenaSparse::new(alloc.alloc()),
            pats: ArenaSparse::new(alloc.alloc()),
            copats: ArenaSparse::new(alloc.alloc()),
            terms: ArenaSparse::new(alloc.alloc()),
            decls: ArenaSparse::new(alloc.alloc()),
        }
    }
}

impl<T, DefId, PatId, CoPatId, TermId, DeclId> AddAssign
    for ArenaGen<T, DefId, PatId, CoPatId, TermId, DeclId>
where
    DefId: DefPtr,
    PatId: PatPtr,
    CoPatId: CoPatPtr,
    TermId: TermPtr,
    DeclId: DeclPtr,
{
    fn add_assign(&mut self, rhs: Self) {
        self.defs += rhs.defs;
        self.pats += rhs.pats;
        self.copats += rhs.copats;
        self.terms += rhs.terms;
        self.decls += rhs.decls;
    }
}
