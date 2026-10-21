//! Lexical lookup shared by resolution and scope observers.
use super::syntax::*;
use std::collections::BTreeSet;

/// Global name environment collected from top-level binders.
#[derive(Clone, Debug, Default)]
pub struct Global {
    /// map from variable names to their definitions
    pub(super) var_to_def: rpds::HashTrieMapSync<VarName, DefId>,
    /// map from definitions to their context bindings
    pub(super) under_map: rpds::HashTrieMapSync<DefId, BindingSite>,
}

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

/// Local name environment built from pattern binders.
#[derive(Clone, Debug)]
pub struct Local {
    /// Context bindings whose dependencies are currently being collected,
    /// from outermost to innermost.
    pub(super) under: rpds::VectorSync<BindingSite>,
    /// map from variable names to their definitions
    pub(super) var_to_def: rpds::HashTrieMapSync<VarName, LocalDefinition>,
    pub(super) depth: usize,
    /// Context candidates associated with block-wide definitions.
    pub(super) under_map: rpds::HashTrieMapSync<DefId, BindingSite>,
    /// The nearest block currently resolving its residual syntax.
    pub(super) boundary: Option<TermId>,
    pub(super) prelude: PreludeScope,
}

#[derive(Clone, Debug)]
pub(super) struct PreludeScope {
    pub(super) enabled: bool,
    pub(super) definitions: rpds::HashTrieMapSync<VarName, DefId>,
}

impl Default for PreludeScope {
    fn default() -> Self {
        Self { enabled: true, definitions: rpds::HashTrieMapSync::new_sync() }
    }
}

impl Local {
    pub(super) fn for_body() -> Self {
        Self {
            under: rpds::VectorSync::new_sync(),
            var_to_def: rpds::HashTrieMapSync::new_sync(),
            depth: 0,
            under_map: rpds::HashTrieMapSync::new_sync(),
            boundary: None,
            prelude: PreludeScope::default(),
        }
    }

    pub(super) fn bind_group(
        mut self, binders: impl IntoIterator<Item = (VarName, DefId)>,
    ) -> Self {
        self.depth += 1;
        self.var_to_def = binders.into_iter().fold(self.var_to_def, |scope, (name, definition)| {
            scope.insert(name, LocalDefinition { definition, depth: self.depth })
        });
        self
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
    pub(super) local: &'scope Local,
    pub(super) global: &'scope Global,
}

pub struct NameLookup {
    pub definition: DefId,
    pub dependency: Option<BindingSite>,
    pub distance: usize,
}

impl NameScope<'_> {
    pub fn lookup(&self, name: &VarName) -> Option<NameLookup> {
        self.local
            .var_to_def
            .get(name)
            .map(|binding| NameLookup {
                definition: binding.definition,
                dependency: self.local.under_map.get(&binding.definition).copied(),
                distance: self.local.depth - binding.depth,
            })
            .or_else(|| {
                self.global.var_to_def.get(name).map(|definition| NameLookup {
                    definition: *definition,
                    dependency: Some(self.global.under_map[definition]),
                    distance: self.local.depth + 1,
                })
            })
            .or_else(|| {
                self.local
                    .prelude
                    .enabled
                    .then(|| {
                        self.local.prelude.definitions.get(name).map(|definition| NameLookup {
                            definition: *definition,
                            dependency: None,
                            distance: self.local.depth + 2,
                        })
                    })
                    .flatten()
            })
    }

    pub fn snapshot(&self) -> ScopeSnapshot {
        // Lookup, rather than iteration order, chooses the winning definition.
        let names = self
            .local
            .var_to_def
            .keys()
            .chain(self.global.var_to_def.keys())
            .chain(self.local.prelude.definitions.keys().filter(|_| self.local.prelude.enabled))
            .collect::<BTreeSet<_>>();
        let definitions = names
            .into_iter()
            .filter_map(|name| {
                let binding = self.lookup(name)?;
                Some(VisibleDefinition {
                    definition: binding.definition,
                    name: name.clone(),
                    distance: binding.distance,
                })
            })
            .collect();
        ScopeSnapshot {
            definitions,
            implicit_os: self.local.prelude.enabled && self.lookup(&VarName("OS".into())).is_none(),
        }
    }
}

/// Inherited lexical information; independent children receive independent local values.
#[derive(Clone)]
pub(super) struct ResolveEnv<'a> {
    pub local: Local,
    pub global: &'a Global,
}

impl ResolveEnv<'_> {
    pub fn scope(&self) -> NameScope<'_> {
        NameScope { local: &self.local, global: self.global }
    }
}
