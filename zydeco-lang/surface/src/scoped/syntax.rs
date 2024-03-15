pub use crate::bitter::syntax::*;
pub use crate::syntax::*;

use std::collections::HashMap;
use zydeco_utils::arena::{ArenaAssoc, ArenaSparse};

/* -------------------------------- TopLevel -------------------------------- */

#[derive(Clone, Debug)]
pub struct TopLevel(pub Vec<Declaration>);

/* --------------------------------- Context -------------------------------- */

zydeco_utils::new_key_type! {
    pub struct DeclId;
}

#[derive(Debug)]
pub struct Ctx {
    // arenas
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatId, Pattern>,
    pub copats: ArenaAssoc<CoPatId, CoPattern>,
    pub terms: ArenaAssoc<TermId, Term<DefId>>,
    pub decls: ArenaSparse<DeclId, Declaration>,
}

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
impl LayerTree {
    pub fn query(&self, name: NameRef<VarName>) -> Option<DefId> {
        // Todo: search path; handle root
        let NameRef(_root, path, name) = name;
        self.layer.get(&(path.into()))?.get(&name).cloned()
    }
}
