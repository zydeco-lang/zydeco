pub use crate::bitter::syntax::*;
pub use crate::syntax::*;

use std::ops::Add;
use zydeco_utils::{arena::*, cells::SingCell, deps::DepGraph, scc::SccGraph};

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
    // /// arena for contexts (WIP)
    // // Todo: not implemented yet
    // pub ctxs: ArenaSparse<CtxtId, Context<DefId>>,
    // /// context for every term (WIP)
    // // Todo: not implemented yet
    // pub term_under_ctx: ArenaAssoc<TermId, CtxtId>,

    // arenas
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatId, Pattern>,
    pub terms: ArenaAssoc<TermId, Term<DefId>>,
    pub decls: ArenaAssoc<DeclId, Declaration>,

    /// def user map
    pub users: ArenaForth<DefId, TermId>,
    /// dependency graph of the top level declarations
    pub deps: DepGraph<DeclId>,
    /// scc graph of the top level declarations
    pub top: SccGraph<DeclId>,
}

/* -------------------------------- Primitive ------------------------------- */

#[derive(Default)]
pub struct PrimDef {
    pub vtype: SingCell<DefId>,
    pub ctype: SingCell<DefId>,
    pub thunk: SingCell<DefId>,
    pub ret: SingCell<DefId>,
    pub unit: SingCell<DefId>,
    pub int: SingCell<DefId>,
    pub char: SingCell<DefId>,
    pub string: SingCell<DefId>,
    pub os: SingCell<DefId>,
    pub monad: SingCell<DefId>,
    pub algebra: SingCell<DefId>,
}

mod impls {
    use super::*;
    use crate::scoped::err::*;
    impl PrimDef {
        pub fn check(&self) -> Result<()> {
            self.vtype.once_or_else(|| ResolveError::MissingPrim("VType"))?;
            self.ctype.once_or_else(|| ResolveError::MissingPrim("CType"))?;
            self.thunk.once_or_else(|| ResolveError::MissingPrim("Thunk"))?;
            self.ret.once_or_else(|| ResolveError::MissingPrim("Ret"))?;
            self.unit.once_or_else(|| ResolveError::MissingPrim("Unit"))?;
            self.int.once_or_else(|| ResolveError::MissingPrim("Int"))?;
            self.char.once_or_else(|| ResolveError::MissingPrim("Char"))?;
            self.string.once_or_else(|| ResolveError::MissingPrim("String"))?;
            self.os.once_or_else(|| ResolveError::MissingPrim("OS"))?;
            self.monad.once_or_else(|| ResolveError::MissingPrim("Monad"))?;
            self.algebra.once_or_else(|| ResolveError::MissingPrim("Algebra"))?;
            Ok(())
        }
    }
}
