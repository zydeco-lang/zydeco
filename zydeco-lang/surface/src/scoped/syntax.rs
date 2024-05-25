pub use crate::bitter::syntax::*;
pub use crate::syntax::*;

use zydeco_utils::{arena::*, deps::DepGraph, multi_cell::MultiCell, scc::SccGraph};

/* -------------------------------- TopLevel -------------------------------- */

/* --------------------------------- Context -------------------------------- */

new_key_type! {
    pub struct CtxtId;
}

#[derive(Clone, Debug)]
pub struct Context<T> {
    pub defs: im::HashMap<DefId, T>,
}
impl<T> Context<T>
where
    T: Clone,
{
    pub fn new() -> Self {
        Self { defs: im::HashMap::new() }
    }
    pub fn extended(self, iter: impl IntoIterator<Item = (DefId, T)>) -> Self {
        let mut res = self;
        res.defs.extend(iter);
        res
    }
}

/* ---------------------------------- Arena --------------------------------- */

#[derive(Debug)]
pub struct ScopedArena {
    // pub ctxs: ArenaSparse<CtxtId, Context<DefId>>,
    // pub term_under_ctx: ArenaAssoc<TermId, CtxtId>,
    /// if a term is a variable, it can be mapped to a def
    pub term_to_def: ArenaAssoc<TermId, DefId>,
    /// dependency graph of the top level declarations
    pub deps: DepGraph<DeclId>,
    /// scc graph of the top level declarations
    pub scc: SccGraph<DeclId>,
}

/* -------------------------------- Primitive ------------------------------- */

#[derive(Default)]
pub struct PrimDef {
    pub vtype: MultiCell<DefId>,
    pub ctype: MultiCell<DefId>,
}
