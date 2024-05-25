use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

/// dependency graph
#[derive(Clone, Debug, Default)]
pub struct DepGraph<Id: Hash + Eq + Clone> {
    pub(crate) map: HashMap<Id, HashSet<Id>>,
}

impl<Id: Hash + Eq + Clone> DepGraph<Id> {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }
    /// check if the graph is empty
    pub fn empty(&self) -> bool {
        self.map.is_empty()
    }
    /// add more dependencies to a node
    pub fn add(&mut self, id: Id, deps: impl IntoIterator<Item = Id>) {
        if let Some(ds) = self.map.get_mut(&id) {
            ds.extend(deps);
        } else {
            self.map.insert(id, deps.into_iter().collect());
        }
    }
    /// find the roots of the graph
    pub fn roots(&self) -> Vec<Id> {
        let mut roots = Vec::new();
        for (id, deps) in &self.map {
            if deps.is_empty() {
                roots.push(id.clone());
            }
        }
        roots
    }
    /// get all nodes
    pub fn nodes(&self) -> HashSet<Id> {
        self.map.keys().cloned().collect()
    }
    /// get all nodes in an order
    pub fn order(&self) -> Vec<Id> {
        self.map.keys().cloned().collect()
    }
    /// query the dependencies of a node
    pub fn query(&self, id: &Id) -> Vec<Id> {
        self.map.get(id).map(|s| s.iter().cloned().collect::<Vec<_>>()).unwrap_or_default()
    }
    /// reverse the graph
    pub fn reverse(&self) -> SrcGraph<Id> {
        let mut rdeps = SrcGraph::new();
        for (id, deps) in &self.map {
            rdeps.add(id.clone(), []);
            for dep in deps {
                rdeps.add(dep.clone(), [id.clone()]);
            }
        }
        rdeps
    }
}

/// co-dependency graph
#[derive(Clone, Debug, Default)]
pub struct SrcGraph<Id: Hash + Eq + Clone> {
    pub(crate) map: HashMap<Id, HashSet<Id>>,
}

impl<Id: Hash + Eq + Clone> SrcGraph<Id> {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }
    /// add more dependencies to a node
    pub fn add(&mut self, id: Id, deps: impl IntoIterator<Item = Id>) {
        if let Some(ds) = self.map.get_mut(&id) {
            ds.extend(deps);
        } else {
            self.map.insert(id, deps.into_iter().collect());
        }
    }
    /// query the dependencies of a node
    pub fn query(&self, id: &Id) -> Vec<Id> {
        self.map.get(id).map(|s| s.iter().cloned().collect::<Vec<_>>()).unwrap_or_default()
    }
    /// roots of the graph
    pub fn roots(&self) -> HashSet<Id> {
        let mut roots = self.map.keys().cloned().collect::<HashSet<_>>();
        for (_, src) in &self.map {
            // for s in src {
            //     roots.remove(s);
            // }
            roots = roots.difference(src).cloned().collect();
        }
        roots
    }
}
