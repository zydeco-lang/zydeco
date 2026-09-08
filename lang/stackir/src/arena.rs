//! Shared builders and layered definition names for Stack IR.

use super::syntax::*;
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::scoped::arena::{ScopedArena, ScopedScope};

/// Read-only layered definition names for an IR phase.
///
/// Each lowering stores only the names it synthesizes. Source and typed-
/// elaboration names stay in their immutable owning arenas and are consulted
/// on a miss, avoiding a clone of the complete source definition table.
pub trait DefinitionNames {
    fn generated_defs(&self) -> &ArenaSparse<ScopedScope, DefId>;

    fn def_name<'a>(
        &'a self, scoped: &'a ScopedArena, statics: &'a StaticsArena, id: &DefId,
    ) -> &'a VarName {
        self.generated_defs().get(id).unwrap_or_else(|| statics.def_name(scoped, id))
    }
}

/// Build a stack IR node and optionally record its source site mapping.
pub trait Construct<S, T, Arena>: Sized + Into<S> {
    type Site;
    /// Allocate the node in the arena, recording a typed-site mapping if provided.
    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> T;
}
