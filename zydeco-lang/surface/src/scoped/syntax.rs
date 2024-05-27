use std::ops::Add;

pub use crate::bitter::syntax::*;
pub use crate::syntax::*;

use zydeco_utils::{arena::*, deps::DepGraph, multi_cell::MultiCell, scc::SccGraph};

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
impl<T> Add for Context<T>
where
    T: Clone,
{
    type Output = Self;
    fn add(mut self, other: Self) -> Self {
        self.defs.extend(other.defs);
        self
    }
}

/* ---------------------------------- Arena --------------------------------- */

#[derive(Debug)]
pub struct ScopedArena {
    /// arena for contexts (WIP)
    // Note: not implemented yet
    pub ctxs: ArenaSparse<CtxtId, Context<DefId>>,
    /// context for every term (WIP)
    // Note: not implemented yet
    pub term_under_ctx: ArenaAssoc<TermId, CtxtId>,

    // arenas
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatId, Pattern>,
    pub copats: ArenaAssoc<CoPatId, CoPattern>,
    pub terms: ArenaAssoc<TermId, Term<DefId>>,
    pub decls: ArenaAssoc<DeclId, Declaration>,

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
