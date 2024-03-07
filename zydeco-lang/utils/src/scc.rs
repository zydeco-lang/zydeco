use crate::{arena::IndexAlloc, deps::DepGraph};
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

/// Tarjan's algorithm
pub struct Tarjan<'a, Id: Hash + Eq + Clone> {
    deps: &'a DepGraph<Id>,
    // issueing new targets
    unvisited: HashSet<Id>,
    // stack of nodes
    seen: Vec<Id>,
    // dfs index allocator
    alloc: IndexAlloc<()>,
    // dfs index of nodes
    dfs: HashMap<Id, usize>,
    // low link of nodes
    low: HashMap<Id, usize>,
}

impl<'a, Id: Hash + Eq + Clone> Tarjan<'a, Id> {
    pub fn run(deps: &'a DepGraph<Id>) -> SccGraph<Id> {
        let mut tarjan = Self::new(deps);
        while let Some(id) = tarjan.remove_or_new_target(None) {
            // id should be unvisited
            fn traverse<Id: Hash + Eq + Clone>(tarjan: &mut Tarjan<Id>, id: Id) {
                let idx = tarjan.assign_dfs_index(id.clone());
                tarjan.set_low(id.clone(), idx);
                tarjan.seen.push(id.clone());
                for next in tarjan.deps.query(&id) {
                    if !tarjan.dfs.contains_key(&next) {
                        // not yet visited
                        tarjan.remove_or_new_target(Some(next.clone()));
                        traverse(tarjan, next.clone());
                    }
                    if tarjan.seen.contains(&next) {
                        // cycle detected
                        // roll back along the stack
                        while let Some(top) = tarjan.seen.pop() {
                            // set low
                            let low = tarjan.compute_low(&id, &top);
                            tarjan.set_low(top.clone(), low);
                            if top == next {
                                break;
                            }
                        }
                    }
                }
            }
            traverse(&mut tarjan, id)
        }
        // construct the scc graph
        let strongs = tarjan.strongs();
        // construct the dependency graph of sccs
        let (deps, roots) = tarjan.scc_deps(&strongs);
        let belongs = tarjan.low;
        SccGraph { strongs, belongs, deps, roots }
    }
    fn new(deps: &'a DepGraph<Id>) -> Self {
        Self {
            deps,
            unvisited: deps.nodes(),
            alloc: IndexAlloc::new(),
            seen: Vec::new(),
            dfs: HashMap::new(),
            low: HashMap::new(),
        }
    }
    fn remove_or_new_target(&mut self, id: Option<Id>) -> Option<Id> {
        let id = match id {
            Some(x) => x,
            None => self.unvisited.iter().next().cloned()?,
        };
        let true = self.unvisited.remove(&id) else { unreachable!() };
        Some(id)
    }
    fn assign_dfs_index(&mut self, id: Id) -> usize {
        let idx = self.alloc.next().unwrap().1;
        self.dfs.insert(id, idx);
        idx
    }
    fn compute_low(&mut self, id: &Id, next: &Id) -> usize {
        self.low[id].min(self.low[next])
    }
    fn set_low(&mut self, id: Id, low: usize) {
        self.low.insert(id, low);
    }
    fn strongs(&self) -> HashMap<usize, HashSet<Id>> {
        let mut strongs = HashMap::new();
        for (id, low) in &self.low {
            strongs.entry(*low).or_insert_with(HashSet::new).insert(id.clone());
        }
        strongs
    }
    fn scc_deps(&self, strongs: &HashMap<usize, HashSet<Id>>) -> (DepGraph<usize>, Vec<usize>) {
        let mut deps = DepGraph::new();
        for (k, scc) in strongs {
            let mut scc_deps = HashSet::new();
            for id in scc {
                for dep in self.deps.query(id) {
                    let dep_low = self.low[&dep];
                    if dep_low != *k {
                        scc_deps.insert(dep_low);
                    }
                }
            }
            deps.add(*k, scc_deps);
        }
        let roots = deps.roots();
        (deps, roots)
    }
}

/// Scc graph
#[derive(Debug)]
pub struct SccGraph<Id: Hash + Eq + Clone> {
    /// maps scc index to the scc
    strongs: HashMap<usize, HashSet<Id>>,
    /// maps each node to its scc index
    belongs: HashMap<Id, usize>,
    /// dag of sccs
    deps: DepGraph<usize>,
    /// sccs that are free of deps
    roots: Vec<usize>,
}

impl<Id: Hash + Eq + Clone> SccGraph<Id> {
    pub fn top(&self) -> Vec<HashSet<Id>> {
        let mut top = Vec::new();
        for root in &self.roots {
            let scc = &self.strongs[root];
            top.push(scc.iter().cloned().collect());
        }
        top
    }
    pub fn release(&mut self, ids: impl IntoIterator<Item = Id>) {
        for id in ids {
            let Some(scc_id) = self.belongs.remove(&id) else { unreachable!() };
            let Some(scc) = &mut self.strongs.get_mut(&scc_id) else { unreachable!() };
            scc.remove(&id);
            if scc.is_empty() {
                self.strongs.remove(&scc_id);
                self.deps.map.remove(&scc_id);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_scc_1() {
        let mut deps = DepGraph::new();
        deps.add(1, vec![2]);
        deps.add(2, vec![3]);
        deps.add(3, vec![1]);
        deps.add(4, vec![3]);
        let scc = Tarjan::run(&deps);
        println!("{:?}", scc);
        // assert_eq!(scc.top(), vec![[4].into_iter().collect()]);
    }
}
