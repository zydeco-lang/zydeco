use super::syntax::*;

/* ---------------------------------- Arena --------------------------------- */

#[derive(Default, Debug)]
pub struct Arena {
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatId, Pattern>,
    pub copats: ArenaAssoc<CoPatId, CoPattern>,
    pub terms: ArenaAssoc<TermId, Term>,
    pub decls: ArenaAssoc<DeclId, Modifiers<Declaration>>,
}

pub type SpanArena = ArenaSparse<EntityId, Span>;
