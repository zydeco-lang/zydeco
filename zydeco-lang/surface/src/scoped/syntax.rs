pub use crate::bitter::syntax as b;
pub use crate::bitter::syntax::*;
pub use crate::syntax::*;

use std::{
    collections::HashMap,
    // ops::AddAssign,
};
use zydeco_utils::arena::ArenaAssoc;

/* --------------------------------- Context -------------------------------- */

#[derive(Default, Debug)]
pub struct Ctx {
    // arenas
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatId, Pattern>,
    pub copats: ArenaAssoc<CoPatId, CoPattern>,
    pub terms: ArenaAssoc<TermId, Term<DefId>>,
}

// impl AddAssign<Ctx> for Ctx {
//     fn add_assign(&mut self, rhs: Ctx) {
//         self.defs += rhs.defs;
//         self.pats += rhs.pats;
//         self.copats += rhs.copats;
//         self.terms += rhs.terms;
//     }
// }

/* ---------------------------------- Layer --------------------------------- */

/// the layer tree in a package
#[derive(Clone, Debug)]
pub struct LayerTree {
    // pub name: VarName,
    // pub deps: DepGraph<NameRef<()>>,
    pub layer: HashMap<NameRef<()>, HashMap<VarName, DefId>>,
}
impl Default for LayerTree {
    fn default() -> Self {
        LayerTree { layer: HashMap::new() }
    }
}
