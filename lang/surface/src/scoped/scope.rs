//! Lexical lookup shared by resolution and scope observers.

use super::syntax::*;
use rustc_hash::FxHashMap;
use std::collections::BTreeSet;
use zydeco_foundation::scoped_map::ScopedMap;

#[derive(Copy, Clone, Debug)]
pub struct BindingSite {
    pub owner: TermId,
    pub id: BindingId,
}

#[derive(Clone, Copy, Debug)]
pub(super) struct LocalDefinition {
    pub(super) definition: DefId,
    pub(super) depth: usize,
}

#[derive(Copy, Clone, Debug)]
pub(super) struct ScopeMark {
    var_to_def: usize,
    under_map: usize,
    depth: usize,
    boundary: Option<TermId>,
    prelude_enabled: bool,
}

impl ScopeMark {
    pub(super) fn depth(&self) -> usize {
        self.depth
    }
}

/// Mutable undo-trail lexical environment shared across lowered syntax resolution.
#[derive(Clone, Debug)]
pub struct ScopeState {
    /// Context bindings whose dependencies are currently being collected,
    /// from outermost to innermost.
    pub(super) under: Vec<BindingSite>,
    /// Map from variable names to their active definitions.
    pub(super) var_to_def: ScopedMap<VarName, LocalDefinition>,
    pub(super) depth: usize,
    /// Context candidates associated with block-wide definitions.
    pub(super) under_map: ScopedMap<DefId, BindingSite>,
    /// The nearest block currently resolving its residual syntax.
    pub(super) boundary: Option<TermId>,
    pub(super) prelude_enabled: bool,
    pub(super) prelude_definitions: FxHashMap<VarName, DefId>,
}

impl Default for ScopeState {
    fn default() -> Self {
        Self::new()
    }
}

impl ScopeState {
    pub(super) fn new() -> Self {
        Self {
            under: Vec::new(),
            var_to_def: ScopedMap::new(),
            depth: 0,
            under_map: ScopedMap::new(),
            boundary: None,
            prelude_enabled: true,
            prelude_definitions: FxHashMap::default(),
        }
    }

    pub(super) fn mark(&self) -> ScopeMark {
        ScopeMark {
            var_to_def: self.var_to_def.watermark(),
            under_map: self.under_map.watermark(),
            depth: self.depth,
            boundary: self.boundary,
            prelude_enabled: self.prelude_enabled,
        }
    }

    pub(super) fn rollback(&mut self, mark: ScopeMark) {
        self.var_to_def.rollback(mark.var_to_def);
        self.under_map.rollback(mark.under_map);
        self.depth = mark.depth;
        self.boundary = mark.boundary;
        self.prelude_enabled = mark.prelude_enabled;
    }

    pub(super) fn bind_group(&mut self, binders: impl IntoIterator<Item = (VarName, DefId)>) {
        self.depth += 1;
        let depth = self.depth;
        for (name, definition) in binders {
            self.var_to_def.insert(name, LocalDefinition { definition, depth });
        }
    }

    pub(super) fn bind_single(&mut self, name: VarName, definition: DefId) {
        self.depth += 1;
        self.var_to_def.insert(name, LocalDefinition { definition, depth: self.depth });
    }

    pub(super) fn view(&self) -> NameScope<'_> {
        NameScope { state: self }
    }
}

/// One definition selected by the resolver's shadowing rules at a completion site.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VisibleDefinition {
    pub definition: DefId,
    pub name: VarName,
    /// Number of subsequent binder groups between this definition and the cursor.
    pub distance: usize,
}

/// An exact lexical environment, independent of whether type checking succeeds.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ScopeSnapshot {
    pub definitions: Vec<VisibleDefinition>,
    pub implicit_os: bool,
}

/// Shared lookup and enumeration over the resolver's environment layers.
pub struct NameScope<'scope> {
    pub(super) state: &'scope ScopeState,
}

pub struct NameLookup {
    pub definition: DefId,
    pub dependency: Option<BindingSite>,
    pub distance: usize,
}

impl NameScope<'_> {
    pub fn lookup(&self, name: &VarName) -> Option<NameLookup> {
        self.state
            .var_to_def
            .get(name)
            .map(|binding| NameLookup {
                definition: binding.definition,
                dependency: self.state.under_map.get(&binding.definition).copied(),
                distance: self.state.depth - binding.depth,
            })
            .or_else(|| {
                self.state
                    .prelude_enabled
                    .then(|| {
                        self.state.prelude_definitions.get(name).map(|definition| NameLookup {
                            definition: *definition,
                            dependency: None,
                            distance: self.state.depth + 2,
                        })
                    })
                    .flatten()
            })
    }

    pub fn snapshot(&self) -> ScopeSnapshot {
        // Lookup, rather than iteration order, chooses the winning definition.
        let names = self
            .state
            .var_to_def
            .keys()
            .chain(self.state.prelude_definitions.keys().filter(|_| self.state.prelude_enabled))
            .collect::<BTreeSet<_>>();
        let definitions = names
            .into_iter()
            .filter_map(|name| {
                let binding = self.lookup(name)?;
                Some(VisibleDefinition {
                    definition: binding.definition,
                    name: *name,
                    distance: binding.distance,
                })
            })
            .collect();
        ScopeSnapshot {
            definitions,
            implicit_os: self.state.prelude_enabled && self.lookup(&VarName::new("OS")).is_none(),
        }
    }
}
