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
/// and the Zydeco to ZIR bijective maps for patterns and terms.
#[derive(Debug, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct AdminArena {
    /// arena allocator for associative arenas
    pub allocator: IndexAlloc<usize>,

    /// builtin operators and functions
    pub builtins: BuiltinMap,

    /// Zydeco to ZIR bijective maps for patterns
    pub pats: ArenaBijective<ss::PatId, VPatId>,
    /// Zydeco to ZIR bijective maps for terms
    pub terms: ArenaBijective<ss::TermId, TermId>,
}

impl AdminArena {
    pub fn new(allocator: IndexAlloc<usize>) -> Self {
        Self {
            allocator,
            builtins: Builtin::all(),
            pats: ArenaBijective::new(),
            terms: ArenaBijective::new(),
        }
    }
    pub unsafe fn duplicate(&self) -> Self {
        Self {
            allocator: self.allocator.clone(),
            builtins: self.builtins.clone(),
            pats: self.pats.clone(),
            terms: self.terms.clone(),
        }
    }
}
