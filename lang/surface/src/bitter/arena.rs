use super::syntax::*;
use crate::textual::syntax as t;

/* ---------------------------------- Arena --------------------------------- */

#[derive(Clone, Debug, derive_more::AddAssign)]
pub struct BitterArena {
    // arenas
    pub defs: ArenaSparse<DefId, VarName>,
    pub pats: ArenaSparse<PatId, Pattern>,
    pub terms: ArenaSparse<TermId, Term<NameRef<VarName>>>,
    pub decls: ArenaSparse<DeclId, Modifiers<Declaration>>,

    /// entity maps from textural syntax
    pub textual: ArenaForth<t::EntityId, EntityId>,
}

impl BitterArena {
    pub fn new_arc(alloc: ArcGlobalAlloc) -> Self {
        BitterArena {
            defs: ArenaSparse::new(alloc.alloc()),
            pats: ArenaSparse::new(alloc.alloc()),
            terms: ArenaSparse::new(alloc.alloc()),
            decls: ArenaSparse::new(alloc.alloc()),

            textual: ArenaForth::new(),
        }
    }
}
