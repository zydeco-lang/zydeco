use super::syntax::*;
use crate::static_syntax as ss;
use derive_more::{AsMut, AsRef};
use zydeco_derive::{AsMutSelf, AsRefSelf};
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::scoped::arena::{ScopedArena, ScopedScope};

/// Allocation scope for stack IR nodes and synthetic scoped definitions.
#[derive(Debug)]
pub enum StackirScope {}

impl Allocates<VPatId> for StackirScope {}
impl Allocates<ValueId> for StackirScope {}
impl Allocates<StackId> for StackirScope {}
impl Allocates<CompuId> for StackirScope {}
impl Allocates<DefId> for StackirScope {}

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

/// Administrative arena for stack IR.
///
/// This arena is used to store the administrative information for the stack IR.
/// This arena is shared by other arenas in the stack IR.
///
/// It is used to store the builtin operators and functions,
/// and the one-to-many Zydeco-to-ZIR provenance maps for patterns and terms.
#[derive(Debug, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct AdminArena {
    /// ID allocator shared by all stack-IR node categories.
    pub(crate) allocator: IdAllocator<StackirScope>,

    /// Names introduced by Stack IR lowering. Source and typed-elaboration
    /// names stay in their immutable phase arenas.
    pub defs: ArenaSparse<ScopedScope, DefId>,

    /// builtin operators and functions
    pub builtins: BuiltinMap,

    /// One source pattern may originate multiple generated ZIR patterns; every
    /// generated pattern has at most one source pattern.
    pub pats: ArenaForth<ss::PatId, VPatId>,
    /// One source term may originate multiple generated ZIR nodes; every
    /// generated node has at most one source term.
    pub terms: ArenaForth<ss::TermId, TermId>,
}

impl AdminArena {
    pub fn new() -> Self {
        Self {
            allocator: IdAllocator::new(),
            defs: ArenaSparse::default(),
            builtins: Builtin::all(),
            pats: ArenaForth::new(),
            terms: ArenaForth::new(),
        }
    }

    pub(crate) fn fresh<Id>(&mut self) -> Id
    where
        Id: ArenaId,
        StackirScope: Allocates<Id>,
    {
        self.allocator.alloc()
    }

    pub(crate) fn insert_def(&mut self, id: DefId, name: VarName) {
        self.defs.insert_new(id, name);
    }
}

impl DefinitionNames for AdminArena {
    fn generated_defs(&self) -> &ArenaSparse<ScopedScope, DefId> {
        &self.defs
    }
}

impl Default for AdminArena {
    fn default() -> Self {
        Self::new()
    }
}
