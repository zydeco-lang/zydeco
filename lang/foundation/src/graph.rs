use derive_more::{Deref, DerefMut, IntoIterator};
use indexmap::{IndexMap, IndexSet};
use rustc_hash::{FxBuildHasher, FxHashMap};
use std::hash::Hash;

type OrderedMap<K, V> = IndexMap<K, V, FxBuildHasher>;
type OrderedSet<T> = IndexSet<T, FxBuildHasher>;

/// Dependency graph; LHS depends on all RHSs.
#[derive(Clone, Debug)]
pub struct DepGraph<Id: Hash + Eq + Clone> {
    pub(crate) map: OrderedMap<Id, OrderedSet<Id>>,
}

impl<Id: Hash + Eq + Clone> Default for DepGraph<Id> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Id: Hash + Eq + Clone> DepGraph<Id> {
    pub fn new() -> Self {
        Self { map: OrderedMap::default() }
    }
    /// Check if the graph is empty.
    pub fn empty(&self) -> bool {
        self.map.is_empty()
    }
    /// Add more dependencies to a node.
    pub fn add(&mut self, id: Id, deps: impl IntoIterator<Item = Id>) {
        self.map.entry(id).or_default().extend(deps);
    }
    /// Find the roots of the graph.
    pub fn roots(&self) -> Vec<Id> {
        self.map.iter().filter_map(|(id, deps)| deps.is_empty().then(|| id.clone())).collect()
    }
    /// Get all nodes.
    pub fn nodes(&self) -> OrderedSet<Id> {
        self.map.keys().cloned().collect()
    }
    /// Get all nodes in deterministic insertion order.
    pub fn order(&self) -> Vec<Id> {
        self.map.keys().cloned().collect()
    }
    /// Query the dependencies of a node.
    pub fn query(&self, id: &Id) -> Vec<Id> {
        self.map.get(id).map(|set| set.iter().cloned().collect()).unwrap_or_default()
    }
    /// Reverse the graph.
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

/// Co-dependency graph.
#[derive(Clone, Debug)]
pub struct SrcGraph<Id: Hash + Eq + Clone> {
    pub(crate) map: OrderedMap<Id, OrderedSet<Id>>,
}

impl<Id: Hash + Eq + Clone> Default for SrcGraph<Id> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Id: Hash + Eq + Clone> SrcGraph<Id> {
    pub fn new() -> Self {
        Self { map: OrderedMap::default() }
    }
    /// Add more sources to a node.
    pub fn add(&mut self, id: Id, srcs: impl IntoIterator<Item = Id>) {
        if let Some(ss) = self.map.get_mut(&id) {
            ss.extend(srcs);
        } else {
            self.map.insert(id, srcs.into_iter().collect());
        }
    }
    /// Query the sources of a node.
    pub fn query(&self, id: &Id) -> Vec<Id> {
        self.map.get(id).map(|set| set.iter().cloned().collect()).unwrap_or_default()
    }
    /// Roots of the graph.
    pub fn roots(&self) -> OrderedSet<Id> {
        let mut roots: OrderedSet<Id> = self.map.keys().cloned().collect();
        for src in self.map.values() {
            for s in src {
                roots.swap_remove(s);
            }
        }
        roots
    }
    /// Get all nodes.
    pub fn nodes(&self) -> OrderedSet<Id> {
        self.map.keys().cloned().collect()
    }
}

/// Deterministic single-pass Tarjan strongly-connected-components solver.
pub struct Tarjan<'a, Id: Hash + Eq + Clone> {
    deps: &'a DepGraph<Id>,
}

struct TarjanState<'a, Id: Hash + Eq + Clone> {
    deps: &'a DepGraph<Id>,
    index: usize,
    indices: FxHashMap<Id, usize>,
    lowlink: FxHashMap<Id, usize>,
    on_stack: FxHashMap<Id, bool>,
    stack: Vec<Id>,
    next_scc: usize,
    belongs: FxHashMap<Id, usize>,
}

impl<'a, Id: Hash + Eq + Clone> Tarjan<'a, Id> {
    pub fn new(deps: &'a DepGraph<Id>) -> Self {
        Self { deps }
    }

    pub fn run(self) -> SccGraph<Id> {
        let count = self.deps.map.len();
        let mut state = TarjanState {
            deps: self.deps,
            index: 0,
            indices: FxHashMap::with_capacity_and_hasher(count, FxBuildHasher),
            lowlink: FxHashMap::with_capacity_and_hasher(count, FxBuildHasher),
            on_stack: FxHashMap::with_capacity_and_hasher(count, FxBuildHasher),
            stack: Vec::with_capacity(count),
            next_scc: 0,
            belongs: FxHashMap::with_capacity_and_hasher(count, FxBuildHasher),
        };
        for id in self.deps.map.keys() {
            if !state.indices.contains_key(id) {
                state.strongconnect(id.clone());
            }
        }
        SccGraph::new(self.deps, state.belongs)
    }
}

impl<Id: Hash + Eq + Clone> TarjanState<'_, Id> {
    fn strongconnect(&mut self, v: Id) {
        let v_idx = self.index;
        self.index += 1;
        self.indices.insert(v.clone(), v_idx);
        self.lowlink.insert(v.clone(), v_idx);
        self.on_stack.insert(v.clone(), true);
        self.stack.push(v.clone());

        if let Some(neighbors) = self.deps.map.get(&v) {
            for w in neighbors {
                if !self.indices.contains_key(w) {
                    let next = w.clone();
                    crate::stack::maybe_grow(|| self.strongconnect(next));
                    let w_low = self.lowlink[w];
                    let v_low = self.lowlink.get_mut(&v).expect("v lowlink exists");
                    *v_low = (*v_low).min(w_low);
                } else if self.on_stack.get(w).copied().unwrap_or(false) {
                    let w_idx = self.indices[w];
                    let v_low = self.lowlink.get_mut(&v).expect("v lowlink exists");
                    *v_low = (*v_low).min(w_idx);
                }
            }
        }

        if self.lowlink[&v] == v_idx {
            let scc = self.next_scc;
            self.next_scc += 1;
            while let Some(w) = self.stack.pop() {
                self.on_stack.insert(w.clone(), false);
                self.belongs.insert(w.clone(), scc);
                if w == v {
                    break;
                }
            }
        }
    }
}

#[derive(Clone, Deref, DerefMut, IntoIterator, PartialEq, Eq, Debug)]
pub struct SccGroup<Id: Hash + Eq + Clone>(#[into_iterator(owned, ref)] pub OrderedSet<Id>);

impl<Id: Hash + Eq + Clone> Default for SccGroup<Id> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Id: Hash + Eq + Clone> SccGroup<Id> {
    pub fn new() -> Self {
        Self(OrderedSet::default())
    }
}

impl<Id: Hash + Eq + Clone> FromIterator<Id> for SccGroup<Id> {
    fn from_iter<T: IntoIterator<Item = Id>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

/// Strongly Connected Components condensation graph.
#[derive(Clone, Debug)]
pub struct SccGraph<Id: Hash + Eq + Clone> {
    /// Maps scc index to the scc group.
    strongs: OrderedMap<usize, SccGroup<Id>>,
    /// Maps each node to its scc index.
    belongs: FxHashMap<Id, usize>,
    /// Condensation DAG edges.
    srcs: SrcGraph<usize>,
    deps: DepGraph<usize>,
    /// Roots of the DAG (SCCs with no outgoing dependencies).
    roots: OrderedSet<usize>,
}

impl<Id: Hash + Eq + Clone> SccGraph<Id> {
    pub fn new(id_deps: &DepGraph<Id>, belongs: FxHashMap<Id, usize>) -> Self {
        let mut strongs: OrderedMap<usize, SccGroup<Id>> = OrderedMap::default();
        for (id, low) in &belongs {
            strongs.entry(*low).or_default().insert(id.clone());
        }
        let mut srcs = SrcGraph::new();
        let mut deps = DepGraph::new();
        for &component in strongs.keys() {
            srcs.add(component, []);
            deps.add(component, []);
        }
        for (&k, scc) in &strongs {
            for id in scc {
                if let Some(ds) = id_deps.map.get(id) {
                    for d in ds {
                        let repr = belongs[d];
                        if repr != k {
                            srcs.add(repr, [k]);
                            deps.add(k, [repr]);
                        }
                    }
                }
            }
        }
        let roots = srcs.roots();
        Self { strongs, belongs, srcs, deps, roots }
    }

    /// Get ids at the top of the graph (those not dependent on any other remaining ids).
    pub fn top(&self) -> Vec<SccGroup<Id>> {
        self.roots
            .iter()
            .filter_map(|root| self.strongs.get(root).map(|scc| scc.iter().cloned().collect()))
            .collect()
    }

    /// Remove given ids from the graph.
    pub fn release(&mut self, ids: impl IntoIterator<Item = Id>) {
        let ids: OrderedSet<Id> = ids.into_iter().collect();
        for id in ids {
            let Some(scc_id) = self.belongs.remove(&id) else {
                unreachable!("released node missing from SCC belonging map")
            };
            let Some(scc) = self.strongs.get_mut(&scc_id) else {
                unreachable!("released node missing from SCC group")
            };
            scc.swap_remove(&id);
            if scc.is_empty() {
                self.strongs.swap_remove(&scc_id);
                self.roots.swap_remove(&scc_id);
                let Some(mut next) = self.srcs.map.swap_remove(&scc_id) else { continue };
                for n in &next {
                    self.deps.map.get_mut(n).unwrap().swap_remove(&scc_id);
                }
                next.retain(|x| self.deps.map.get(x).is_none_or(OrderedSet::is_empty));
                self.roots.extend(next);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scc_0() {
        let mut deps = DepGraph::new();
        deps.add(1, []);
        let mut scc = Tarjan::new(&deps).run();
        assert_eq!(scc.top(), vec![[1].into_iter().collect()]);
        scc.release([1]);
        assert_eq!(scc.top(), vec![]);
    }
    #[test]
    fn test_scc_1() {
        let mut deps = DepGraph::new();
        deps.add(1, [2]);
        deps.add(2, [3]);
        deps.add(3, [1]);
        deps.add(4, [3]);
        let mut scc = Tarjan::new(&deps).run();
        assert_eq!(scc.top(), vec![[1, 2, 3].into_iter().collect()]);
        scc.release([1]);
        assert_eq!(scc.top(), vec![[2, 3].into_iter().collect()]);
        scc.release([2, 3]);
        assert_eq!(scc.top(), vec![[4].into_iter().collect()]);
        scc.release([4]);
        assert_eq!(scc.top(), vec![]);
    }
    #[test]
    fn test_scc_2() {
        let mut deps = DepGraph::new();
        deps.add(1, [2, 8]);
        deps.add(2, [3, 7]);
        deps.add(3, [4, 5, 6]);
        deps.add(4, []);
        deps.add(5, [6]);
        deps.add(6, []);
        deps.add(7, [1]);
        deps.add(8, [9]);
        deps.add(9, [7]);
        let mut scc = Tarjan::new(&deps).run();
        for s in scc.top() {
            assert_eq!(s.len(), 1);
            let n = s.into_iter().next().unwrap();
            assert!(n == 4 || n == 6);
        }
        scc.release([4]);
        assert_eq!(scc.top(), vec![[6].into_iter().collect()]);
        scc.release([6]);
        assert_eq!(scc.top(), vec![[5].into_iter().collect()]);
        scc.release([5]);
        assert_eq!(scc.top(), vec![[3].into_iter().collect()]);
        scc.release([3]);
        assert_eq!(scc.top(), vec![[1, 2, 7, 8, 9].into_iter().collect()]);
    }

    #[test]
    fn isolated_recursive_components_are_roots() {
        let mut deps = DepGraph::new();
        deps.add(1, [1]);
        deps.add(2, [3]);
        deps.add(3, [2]);

        let scc = Tarjan::new(&deps).run();
        let roots = scc.top();

        assert_eq!(roots.len(), 2);
        assert!(roots.iter().any(|group| group == &[1].into_iter().collect()));
        assert!(roots.iter().any(|group| group == &[2, 3].into_iter().collect()));
    }
}
