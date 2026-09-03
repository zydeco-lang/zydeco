use super::{Global, Local, ResolveSourceOut, Result, resolver::BindingSite, syntax::*};
use std::collections::BTreeSet;

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
}

/// The resolved cursor node and the lexical environment captured at that node.
pub struct CompletionSite {
    pub target: TermId,
    pub scope: ScopeSnapshot,
}

/// Tooling resolution keeps a captured scope even if another construct is rejected.
pub struct CompletionResolution {
    pub site: Option<CompletionSite>,
    pub unbound: Vec<super::ResolveError>,
    pub program: Result<ResolveSourceOut>,
}

pub(super) struct CompletionCapture {
    pub target: crate::textual::syntax::TermId,
    pub site: Option<CompletionSite>,
    pub unbound: Vec<super::ResolveError>,
}

/// Shared lookup and enumeration over the resolver's environment layers.
pub(super) struct NameScope<'scope> {
    pub local: &'scope Local,
    pub global: &'scope Global,
}

pub(super) struct NameLookup {
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
    }

    pub fn snapshot(&self) -> ScopeSnapshot {
        // Lookup, rather than iteration order, chooses the winning definition.
        let names = self
            .local
            .var_to_def
            .keys()
            .chain(self.global.var_to_def.keys())
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
        ScopeSnapshot { definitions }
    }
}

#[cfg(test)]
mod tests;
