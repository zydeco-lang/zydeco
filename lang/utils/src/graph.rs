use derive_more::{Deref, DerefMut, IntoIterator};
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

/// dependency graph; LHS depends on all RHSs
#[derive(Clone, Debug)]
pub struct DepGraph<Id: Hash + Eq + Clone> {
    pub(crate) map: HashMap<Id, HashSet<Id>>,
}

impl<Id: Hash + Eq + Clone> Default for DepGraph<Id> {
    fn default() -> Self {
        Self::new()
    }
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
        self.map.entry(id).or_insert_with(HashSet::new).extend(deps);
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
#[derive(Clone, Debug)]
pub struct SrcGraph<Id: Hash + Eq + Clone> {
    pub(crate) map: HashMap<Id, HashSet<Id>>,
}

impl<Id: Hash + Eq + Clone> Default for SrcGraph<Id> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Id: Hash + Eq + Clone> SrcGraph<Id> {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }
    /// add more sources to a node
    pub fn add(&mut self, id: Id, srcs: impl IntoIterator<Item = Id>) {
        if let Some(ss) = self.map.get_mut(&id) {
            ss.extend(srcs);
        } else {
            self.map.insert(id, srcs.into_iter().collect());
        }
    }
    /// query the sources of a node
    pub fn query(&self, id: &Id) -> Vec<Id> {
        self.map.get(id).map(|s| s.iter().cloned().collect::<Vec<_>>()).unwrap_or_default()
    }
    /// roots of the graph
    pub fn roots(&self) -> HashSet<Id> {
        let mut roots = self.map.keys().cloned().collect::<HashSet<_>>();
        for (_, src) in &self.map {
            for s in src {
                roots.remove(s);
            }
        }
        roots
    }
    /// get all nodes
    pub fn nodes(&self) -> HashSet<Id> {
        self.map.keys().cloned().collect()
    }
}

pub struct Kosaraju<'a, Id: Hash + Eq + Clone> {
    deps: &'a DepGraph<Id>,
    rdeps: SrcGraph<Id>,
}

impl<'a, Id: Hash + Eq + Clone> Kosaraju<'a, Id> {
    pub fn new(deps: &'a DepGraph<Id>) -> Self {
        let rdeps = deps.reverse();
        Self { deps, rdeps }
    }
    pub fn run(self) -> SccGraph<Id> {
        let order = self.deps.order();
        let mut stack = Vec::new();
        {
            let mut visited = HashSet::new();
            for id in &order {
                if !visited.contains(id) {
                    self.dfs_forward(&mut stack, &mut visited, id.clone());
                }
            }
        }
        let mut belongs = HashMap::new();
        {
            let mut next_scc = 0usize;
            for id in stack.iter().rev() {
                if !belongs.contains_key(id) {
                    let idx = next_scc;
                    next_scc += 1;
                    self.dfs_backward(idx, &mut belongs, id.clone());
                }
            }
        }
        SccGraph::new(self.deps, belongs)
    }
    fn dfs_forward(&self, stack: &mut Vec<Id>, visited: &mut HashSet<Id>, id: Id) {
        visited.insert(id.clone());
        for next in self.deps.query(&id) {
            if !visited.contains(&next) {
                self.dfs_forward(stack, visited, next);
            }
        }
        stack.push(id);
    }
    fn dfs_backward(&self, idx: usize, belongs: &mut HashMap<Id, usize>, id: Id) {
        belongs.insert(id.clone(), idx);
        for next in self.rdeps.query(&id) {
            if !belongs.contains_key(&next) {
                self.dfs_backward(idx, belongs, next);
            }
        }
    }
}

#[derive(Clone, Deref, DerefMut, IntoIterator, PartialEq, Eq, Debug)]
pub struct SccGroup<Id: Hash + Eq + Clone>(#[into_iterator(owned, ref)] pub HashSet<Id>);

impl<Id: Hash + Eq + Clone> Default for SccGroup<Id> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Id: Hash + Eq + Clone> SccGroup<Id> {
    pub fn new() -> Self {
        Self(HashSet::new())
    }
}
impl<Id: Hash + Eq + Clone> FromIterator<Id> for SccGroup<Id> {
    fn from_iter<T: IntoIterator<Item = Id>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

/// Strongly Connected Components graph
#[derive(Clone, Debug)]
pub struct SccGraph<Id: Hash + Eq + Clone> {
    /// maps scc index to the scc
    strongs: HashMap<usize, SccGroup<Id>>,
    /// maps each node to its scc index
    belongs: HashMap<Id, usize>,
    /// dags of sccs
    srcs: SrcGraph<usize>,
    deps: DepGraph<usize>,
    /// roots of the dag
    roots: HashSet<usize>,
}

impl<Id: Hash + Eq + Clone> SccGraph<Id> {
    pub fn new(id_deps: &DepGraph<Id>, belongs: HashMap<Id, usize>) -> Self {
        let mut strongs = HashMap::new();
        for (id, low) in &belongs {
            strongs.entry(*low).or_insert_with(SccGroup::new).insert(id.clone());
        }
        let mut srcs = SrcGraph::new();
        let mut deps = DepGraph::new();
        strongs.keys().for_each(|component| {
            srcs.add(*component, []);
            deps.add(*component, []);
        });
        for (k, scc) in &strongs {
            for id in scc {
                let ds = id_deps.query(id);
                for d in ds {
                    let repr = belongs[&d];
                    if repr != *k {
                        srcs.add(repr, [*k]);
                        deps.add(*k, [repr]);
                    }
                }
            }
        }
        let roots = srcs.roots();
        Self { strongs, belongs, srcs, deps, roots }
    }
    /// Get ids at the top of the graph,
    /// i.e. those that are not dependent on any other ids.
    pub fn top(&self) -> Vec<SccGroup<Id>> {
        let mut top = Vec::new();
        for root in &self.roots {
            let Some(scc) = &self.strongs.get(root) else {
                continue;
            };
            top.push(scc.iter().cloned().collect());
        }
        top
    }
    /// Remove given ids from the graph.
    pub fn release(&mut self, ids: impl IntoIterator<Item = Id>) {
        let ids = ids.into_iter().collect::<HashSet<_>>();
        for id in ids {
            let Some(scc_id) = self.belongs.remove(&id) else { unreachable!() };
            let Some(scc) = &mut self.strongs.get_mut(&scc_id) else { unreachable!() };
            scc.remove(&id);
            if scc.is_empty() {
                self.strongs.remove(&scc_id);
                self.roots.remove(&scc_id);
                let Some(mut next) = self.srcs.map.remove(&scc_id) else { continue };
                for n in &next {
                    self.deps.map.get_mut(n).unwrap().remove(&scc_id);
                }
                next.retain(|x| self.deps.query(x).is_empty());
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
        let mut scc = Kosaraju::new(&deps).run();
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
        let mut scc = Kosaraju::new(&deps).run();
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
        let mut scc = Kosaraju::new(&deps).run();
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

        let scc = Kosaraju::new(&deps).run();
        let roots = scc.top();

        assert_eq!(roots.len(), 2);
        assert!(roots.iter().any(|group| group == &[1].into_iter().collect()));
        assert!(roots.iter().any(|group| group == &[2, 3].into_iter().collect()));
    }
}
