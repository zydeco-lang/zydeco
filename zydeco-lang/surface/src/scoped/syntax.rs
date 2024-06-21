pub use crate::bitter::syntax::*;
pub use crate::syntax::*;
pub use crate::textual::syntax::SpanArena;

use crate::textual::syntax as t;
use std::collections::HashSet;
use zydeco_utils::{arena::*, cells::SingCell, deps::DepGraph, scc::SccGraph};

/* --------------------------------- Context -------------------------------- */

#[derive(Clone, Debug)]
pub struct Context<T> {
    pub defs: im::HashMap<DefId, T>,
}

mod impls_context {
    use super::*;
    use std::ops::{Add, AddAssign, Index};
    impl<T> Context<T>
    where
        T: Clone,
    {
        pub fn new() -> Self {
            Self { defs: im::HashMap::new() }
        }
        pub fn singleton(def: DefId, t: T) -> Self {
            let mut defs = im::HashMap::new();
            defs.insert(def, t);
            Self { defs }
        }
        pub fn get(&self, def: &DefId) -> Option<&T> {
            self.defs.get(def)
        }
        pub fn extended(&self, iter: impl IntoIterator<Item = (DefId, T)>) -> Self {
            let Context { mut defs } = self.clone();
            defs.extend(iter);
            Self { defs }
        }
    }
    impl<T> Add for Context<T>
    where
        T: Clone,
    {
        type Output = Self;
        fn add(self, other: Self) -> Self {
            let Context { mut defs } = self;
            defs.extend(other.defs);
            Self { defs }
        }
    }
    impl<T> AddAssign<(DefId, T)> for Context<T>
    where
        T: Clone,
    {
        fn add_assign(&mut self, (def, t): (DefId, T)) {
            let Self { defs } = self;
            let mut defs = defs.clone();
            defs.insert(def, t);
            *self = Self { defs };
        }
    }
    impl<T> Index<&DefId> for Context<T>
    where
        T: Clone,
    {
        type Output = T;
        fn index(&self, def: &DefId) -> &T {
            &self.defs[def]
        }
    }
}

/* -------------------------------- TopLevel -------------------------------- */

pub struct SccDeclarations<'decl>(pub &'decl HashSet<DeclId>);

/* ---------------------------------- Arena --------------------------------- */

#[derive(Debug)]
pub struct ScopedArena {
    // arenas
    pub defs: ArenaSparse<DefId, VarName>,
    pub pats: ArenaSparse<PatId, Pattern>,
    pub terms: ArenaSparse<TermId, Term<DefId>>,
    pub decls: ArenaSparse<DeclId, Declaration>,
    /// entity maps from textural syntax
    pub textual: ArenaForth<t::EntityId, EntityId>,

    /// def user map
    pub users: ArenaForth<DefId, TermId>,
    /// contexts upon terms
    pub ctxs: ArenaAssoc<TermId, Context<()>>,
    /// externs to defs
    pub exts: ArenaAssoc<DeclId, (Internal, DefId)>,
    /// dependency graph of the top level declarations
    pub deps: DepGraph<DeclId>,
    /// scc graph of the top level declarations
    pub top: SccGraph<DeclId>,
}

/* -------------------------------- Primitive ------------------------------- */

/// Primitive definitions
///
/// Collects the primitive definitions from the surface syntax.
/// To add a new primitive form:
/// 1. Add a new field to this struct.
/// 2. Check if the form can be introduced during desugaring, e.g. annotations.
///    If so, add it to `crate::bitter::syntax::PrimTerms` too.
/// 3. Implement the `check` method to ensure all fields are filled.
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
            self.vtype.get_or_else(|| ResolveError::MissingPrim("VType"))?;
            self.ctype.get_or_else(|| ResolveError::MissingPrim("CType"))?;
            self.thunk.get_or_else(|| ResolveError::MissingPrim("Thunk"))?;
            self.ret.get_or_else(|| ResolveError::MissingPrim("Ret"))?;
            self.unit.get_or_else(|| ResolveError::MissingPrim("Unit"))?;
            self.int.get_or_else(|| ResolveError::MissingPrim("Int"))?;
            self.char.get_or_else(|| ResolveError::MissingPrim("Char"))?;
            self.string.get_or_else(|| ResolveError::MissingPrim("String"))?;
            self.os.get_or_else(|| ResolveError::MissingPrim("OS"))?;
            self.monad.get_or_else(|| ResolveError::MissingPrim("Monad"))?;
            self.algebra.get_or_else(|| ResolveError::MissingPrim("Algebra"))?;
            Ok(())
        }
    }
}
