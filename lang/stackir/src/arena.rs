use super::syntax::*;
use crate::static_syntax as ss;
use derive_more::{AsMut, AsRef};
use zydeco_derive::{AsMutSelf, AsRefSelf};

/// Administrative arena for stack IR.
///
/// This arena is used to store the administrative information for the stack IR.
/// This arena is shared by other arenas in the stack IR.
///
/// It is used to store the builtin operators and functions,
/// and the one-to-many Zydeco-to-ZIR provenance maps for patterns and terms.
#[derive(Debug, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct AdminArena {
    /// Key space shared by all stack-IR node categories.
    pub key_space: KeySpace,

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
    pub fn new(key_space: KeySpace) -> Self {
        Self {
            key_space,
            builtins: Builtin::all(),
            pats: ArenaForth::new(),
            terms: ArenaForth::new(),
        }
    }
}
