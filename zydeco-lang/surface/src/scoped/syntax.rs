pub use crate::bitter::syntax::*;
pub use crate::syntax::*;

use zydeco_utils::arena::ArenaAssoc;

/* -------------------------------- TopLevel -------------------------------- */

#[derive(Clone, Debug)]
pub struct TopLevel(pub Vec<Declaration>);

/* ---------------------------------- Arena --------------------------------- */

#[derive(Debug)]
pub struct Arena {
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatId, Pattern>,
    pub copats: ArenaAssoc<CoPatId, CoPattern>,
    pub terms: ArenaAssoc<TermId, Term<DefId>>,
}
