pub use crate::bitter::syntax::*;
pub use crate::syntax::*;

use zydeco_utils::arena::*;
use zydeco_utils::deps::DepGraph;

/* --------------------------------- Context -------------------------------- */

new_key_type! {
    pub struct CtxtId;
}

#[derive(Clone, Debug)]
pub struct Context<T> {
    pub defs: im::HashMap<DefId, T>,
}
impl<T> Context<T> {
    pub fn new() -> Self {
        Self { defs: im::HashMap::new() }
    }
}

/* ---------------------------------- Arena --------------------------------- */

#[derive(Debug)]
pub struct ScopedArena {
    pub ctxs: ArenaSparse<CtxtId, Context<DefId>>,
    pub term_under_ctx: ArenaAssoc<TermId, CtxtId>,
    pub term_to_def: ArenaAssoc<TermId, DefId>,
    pub deps: DepGraph<DefId>,
}
