pub use crate::syntax::*;

use zydeco_utils::arena::ArenaAssoc;

/* ---------------------------------- Kind ---------------------------------- */

#[derive(Debug, Clone)]
pub enum Kind {}

/* ---------------------------------- Type ---------------------------------- */

#[derive(Debug, Clone)]
pub enum Type {}

/* ---------------------------------- Arena --------------------------------- */

#[derive(Debug)]
pub struct SortedArena {
    pub kinds: ArenaAssoc<TermId, Kind>,
    pub types: ArenaAssoc<TermId, Type>,
}
