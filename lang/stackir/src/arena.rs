//! Shared builders and layered definition names for Stack IR.

use super::syntax::*;
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};
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

/// How a listing spells definitions.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum NameStyle {
    /// Source names alone. Where one scope binds a name more than once, later
    /// definitions append `'1`, `'2`, … in order of first appearance.
    #[default]
    Readable,
    /// Every name followed by its arena id, as `acc[54#2257]`.
    Identified,
}

/// Rendered names for one listing, unique within each scope.
///
/// The first definition of a plain name in a scope keeps it; later ones append `'n` with
/// the smallest `n` whose spelling no definition in the scope has yet, so a source name
/// that already reads `acc'1` cannot collide with a generated one.
#[derive(Debug)]
pub struct NameTable<Scope> {
    names: HashMap<DefId, String>,
    taken: HashMap<Scope, HashSet<String>>,
}

impl<Scope> Default for NameTable<Scope> {
    fn default() -> Self {
        Self { names: HashMap::new(), taken: HashMap::new() }
    }
}

impl<Scope: Eq + Hash> NameTable<Scope> {
    /// Spell `def` in `scope`; a definition already spelled keeps its first spelling.
    pub fn assign(&mut self, scope: Scope, def: DefId, plain: &str) {
        if self.names.contains_key(&def) {
            return;
        }
        let taken = self.taken.entry(scope).or_default();
        let spelling = if taken.contains(plain) {
            (1..)
                .map(|n| format!("{plain}'{n}"))
                .find(|candidate| !taken.contains(candidate))
                .expect("some suffix is free")
        } else {
            plain.to_owned()
        };
        taken.insert(spelling.clone());
        self.names.insert(def, spelling);
    }

    pub fn get(&self, def: &DefId) -> Option<&str> {
        self.names.get(def).map(String::as_str)
    }
}

/// Build a stack IR node and optionally record its source site mapping.
pub trait Construct<S, T, Arena>: Sized + Into<S> {
    type Site;
    /// Allocate the node in the arena, recording a typed-site mapping if provided.
    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> T;
}

#[cfg(test)]
mod tests {
    use super::*;
    use zydeco_utils::arena::IdAllocator;

    fn defs(count: usize) -> Vec<DefId> {
        let mut allocator = IdAllocator::<crate::high::arena::StackirScope>::new();
        (0..count).map(|_| allocator.alloc()).collect()
    }

    #[test]
    fn first_definition_keeps_the_plain_name_and_later_ones_take_primes() {
        let defs = defs(3);
        let mut table = NameTable::<u8>::default();
        table.assign(0, defs[0], "acc");
        table.assign(0, defs[1], "acc");
        table.assign(0, defs[2], "acc");
        assert_eq!(table.get(&defs[0]), Some("acc"));
        assert_eq!(table.get(&defs[1]), Some("acc'1"));
        assert_eq!(table.get(&defs[2]), Some("acc'2"));
    }

    #[test]
    fn scopes_are_independent() {
        let defs = defs(2);
        let mut table = NameTable::<u8>::default();
        table.assign(0, defs[0], "acc");
        table.assign(1, defs[1], "acc");
        assert_eq!(table.get(&defs[1]), Some("acc"));
    }

    #[test]
    fn generated_primes_skip_spellings_the_source_already_uses() {
        let defs = defs(3);
        let mut table = NameTable::<u8>::default();
        table.assign(0, defs[0], "acc'1");
        table.assign(0, defs[1], "acc");
        table.assign(0, defs[2], "acc");
        assert_eq!(table.get(&defs[1]), Some("acc"));
        assert_eq!(table.get(&defs[2]), Some("acc'2"));
    }

    #[test]
    fn a_definition_is_spelled_once() {
        let defs = defs(2);
        let mut table = NameTable::<u8>::default();
        table.assign(0, defs[0], "acc");
        table.assign(0, defs[0], "acc");
        assert_eq!(table.get(&defs[0]), Some("acc"));
        assert_eq!(table.get(&defs[1]), None);
    }
}
