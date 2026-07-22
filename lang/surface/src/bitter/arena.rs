use super::syntax::*;
use crate::textual::syntax as t;

/* ---------------------------------- Arena --------------------------------- */

/// Allocation and storage scope for desugared surface syntax.
#[derive(Debug)]
pub enum BitterScope {}

impl Allocates<DefId> for BitterScope {}
impl Allocates<PatId> for BitterScope {}
impl Allocates<TermId> for BitterScope {}
impl Allocates<DeclId> for BitterScope {}

impl ArenaSchema<DefId> for BitterScope {
    type Item = VarName;
}
impl ArenaSchema<PatId> for BitterScope {
    type Item = Pattern;
}
impl ArenaSchema<TermId> for BitterScope {
    type Item = Term<NameRef<VarName>>;
}
impl ArenaSchema<DeclId> for BitterScope {
    type Item = Modifiers<Declaration>;
}

/// Storage for all bitter syntax nodes plus a back-map into textual entities.
#[derive(Default, Debug, derive_more::AddAssign)]
pub struct BitterArena {
    // arenas
    pub defs: ArenaSparse<BitterScope, DefId>,
    pub pats: ArenaSparse<BitterScope, PatId>,
    pub terms: ArenaSparse<BitterScope, TermId>,
    pub decls: ArenaSparse<BitterScope, DeclId>,

    /// entity maps from textural syntax
    pub textual: ArenaForth<t::EntityId, EntityId>,
}

impl BitterArena {
    pub fn new() -> Self {
        Self::default()
    }
}
