pub use crate::bitter::syntax::*;
pub use crate::syntax::*;
pub use crate::textual::syntax::SpanArena;

use crate::textual::syntax as t;
use zydeco_utils::{arena::*, cells::SingCell, deps::DepGraph, scc::SccGraph};

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
