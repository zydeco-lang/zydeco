pub use crate::bitter::syntax as b;
pub use crate::bitter::syntax::*;

use std::{
    collections::HashMap,
    ops::{AddAssign, Index},
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

impl AddAssign<Ctx> for Ctx {
    fn add_assign(&mut self, rhs: Ctx) {
        self.defs += rhs.defs;
        self.pats += rhs.pats;
        self.copats += rhs.copats;
        self.terms += rhs.terms;
    }
}

/* ---------------------------------- Layer --------------------------------- */

#[derive(Clone, Debug)]
pub struct LayerTree {
    pub name: VarName,
    // pub reexport: Vec<NameRef<VarName>>,
    pub inner: HashMap<VarName, LayerTree>,
}

/* --------------------------------- Symbol --------------------------------- */

pub enum Symbol {
    Module(InternalSymbols),
    Def(b::DefId),
}

/// symbols visible in the current package
pub struct InternalSymbols {
    pub map: HashMap<VarName, Symbol>,
}
impl InternalSymbols {
    pub fn new() -> InternalSymbols {
        Self { map: HashMap::new() }
    }
    // fn insert(&mut self, k: VarName, v: Symbol) -> Option<Symbol> {
    //     self.map.insert(k, v)
    // }
}
impl Index<&VarName> for InternalSymbols {
    type Output = Symbol;
    fn index(&self, index: &VarName) -> &Self::Output {
        &self.map[index]
    }
}
