use crate::{
    arena::IndexAlloc,
    deps::{DepGraph, SrcGraph},
};
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

// /// Tarjan's algorithm
// pub struct Tarjan<'a, Id: Hash + Eq + Clone> {
//     deps: &'a DepGraph<Id>,
//     // issueing new targets
//     unvisited: HashSet<Id>,
//     // stack of nodes
//     seen: Vec<Id>,
//     // dfs index allocator
//     alloc: IndexAlloc<()>,
//     // dfs index of nodes
//     dfs: HashMap<Id, usize>,
//     // low link of nodes
//     low: HashMap<Id, usize>,
// }

// impl<'a, Id: Hash + Eq + Clone> Tarjan<'a, Id> {
//     pub fn run(deps: &'a DepGraph<Id>) -> SccGraph<Id> {
//         let mut tarjan = Self::new(deps);
//         while let Some(id) = tarjan.remove_or_new_target(None) {
//             // id should be unvisited
//             fn traverse<Id: Hash + Eq + Clone>(tarjan: &mut Tarjan<Id>, id: Id) {
//                 let idx = tarjan.assign_dfs_index(id.clone());
//                 tarjan.set_low(id.clone(), idx);
//                 tarjan.seen.push(id.clone());
//                 for next in tarjan.deps.query(&id) {
//                     if !tarjan.dfs.contains_key(&next) {
//                         // not yet visited
//                         tarjan.remove_or_new_target(Some(next.clone()));
//                         traverse(tarjan, next.clone());
//                     }
//                     if tarjan.seen.contains(&next) {
//                         // cycle detected
//                         // roll back along the stack
//                         while let Some(top) = tarjan.seen.pop() {
//                             // set low
//                             let low = tarjan.compute_low(&id, &top);
//                             tarjan.set_low(top.clone(), low);
//                             if top == next {
//                                 break;
//                             }
//                         }
//                     }
//                 }
//             }
//             traverse(&mut tarjan, id)
//         }
//         SccGraph::new(&tarjan.deps, tarjan.low)
//     }
//     fn new(deps: &'a DepGraph<Id>) -> Self {
//         Self {
//             deps,
//             unvisited: deps.nodes(),
//             alloc: IndexAlloc::new(),
//             seen: Vec::new(),
//             dfs: HashMap::new(),
//             low: HashMap::new(),
//         }
//     }
//     fn remove_or_new_target(&mut self, id: Option<Id>) -> Option<Id> {
//         let id = match id {
//             Some(x) => x,
//             None => self.unvisited.iter().next().cloned()?,
//         };
//         let true = self.unvisited.remove(&id) else { unreachable!() };
//         Some(id)
//     }
//     fn assign_dfs_index(&mut self, id: Id) -> usize {
//         let idx = self.alloc.next().unwrap().1;
//         self.dfs.insert(id, idx);
//         idx
//     }
//     fn compute_low(&mut self, id: &Id, next: &Id) -> usize {
//         self.low[id].min(self.low[next])
//     }
//     fn set_low(&mut self, id: Id, low: usize) {
//         self.low.insert(id, low);
//     }
// }

pub struct Kosaraju<'a, Id: Hash + Eq + Clone> {
    deps: &'a DepGraph<Id>,
    rdeps: SrcGraph<Id>,
}

impl<'a, Id: Hash + Eq + Clone> Kosaraju<'a, Id>
// where
//     Id: std::fmt::Debug,
{
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
            let mut alloc = IndexAlloc::new();
            for id in stack.iter().rev() {
                if !belongs.contains_key(id) {
                    let idx = alloc.next().unwrap().1;
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

/// Scc graph
#[derive(Clone, Debug)]
pub struct SccGraph<Id: Hash + Eq + Clone> {
    /// maps scc index to the scc
    strongs: HashMap<usize, HashSet<Id>>,
    /// maps each node to its scc index
    belongs: HashMap<Id, usize>,
    /// dags of sccs
    srcs: SrcGraph<usize>,
    deps: DepGraph<usize>,
    /// roots of the dag
    roots: HashSet<usize>,
}

impl<Id: Hash + Eq + Clone> SccGraph<Id>
// where
//     Id: std::fmt::Debug,
{
    pub fn new(id_deps: &DepGraph<Id>, belongs: HashMap<Id, usize>) -> Self {
        let mut strongs = HashMap::new();
        for (id, low) in &belongs {
            strongs.entry(*low).or_insert_with(HashSet::new).insert(id.clone());
        }
        let mut srcs = SrcGraph::new();
        let mut deps = DepGraph::new();
        for (k, scc) in &strongs {
            for id in scc {
                let ds = id_deps.query(id);
                if ds.is_empty() {
                    // if one's dependency is empty, it is a root
                    srcs.add(*k, []);
                }
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
    pub fn top(&self) -> Vec<HashSet<Id>> {
        let mut top = Vec::new();
        for root in &self.roots {
            let Some(scc) = &self.strongs.get(root) else {
                // eprintln!("invalid scc id: {:?}", root);
                continue;
            };
            top.push(scc.iter().cloned().collect());
        }
        top
    }
    pub fn release(&mut self, ids: impl IntoIterator<Item = Id>) {
        let ids = ids.into_iter().collect::<HashSet<_>>();
        // println!(">>> releasing: {:?}", ids);
        for id in ids {
            let Some(scc_id) = self.belongs.remove(&id) else { unreachable!() };
            let Some(scc) = &mut self.strongs.get_mut(&scc_id) else { unreachable!() };
            scc.remove(&id);
            // println!("scc_id: {:?}, scc: {:?}", scc_id, scc);
            if scc.is_empty() {
                // println!("empty: {:?}", scc_id);
                self.strongs.remove(&scc_id);
                self.roots.remove(&scc_id);
                let Some(mut next) = self.srcs.map.remove(&scc_id) else { continue };
                // println!("next?: {:?}", next);
                for n in &next {
                    self.deps.map.get_mut(n).unwrap().remove(&scc_id);
                }
                next.retain(|x| self.deps.query(x).is_empty());
                // println!("next: {:?}", next);
                self.roots.extend(next);
            }
        }
        // println!("<<<");
    }
    pub fn obliviate(&mut self, ids: impl IntoIterator<Item = Id>) {
        let ids = ids.into_iter().collect::<HashSet<_>>();
        // initial frontier is all sccs that contain the ids
        let mut frontier = ids.iter().map(|id| self.belongs[id]).collect::<Vec<_>>();
        let mut victims = HashSet::new();
        while let Some(scc_id) = frontier.pop() {
            if victims.insert(scc_id) {
                // if scc_id is not in the victims, extend the frontier
                frontier.extend(self.srcs.query(&scc_id));
            }
            // else, scc_id is already in the victims, just go on
        }
        // with victims converged, remove them from deps and srcs
        for scc_id in &victims {
            self.srcs.map = self
                .srcs
                .map
                .iter()
                .filter_map(|(id, scc)| {
                    if scc_id == id {
                        None
                    } else {
                        Some((
                            id.to_owned(),
                            scc.into_iter().filter(|x| !victims.contains(x)).cloned().collect(),
                        ))
                    }
                })
                .collect();
            self.deps.map = self
                .deps
                .map
                .iter()
                .filter_map(|(id, scc)| {
                    if scc_id == id {
                        None
                    } else {
                        Some((
                            id.to_owned(),
                            scc.into_iter().filter(|x| !victims.contains(x)).cloned().collect(),
                        ))
                    }
                })
                .collect();
        }
        // let Some(scc) = self.strongs.remove(&scc_id) else { unreachable!() };
        // in the end, remove the victims from the strongs and roots
        let mut ids = HashSet::new();
        for scc_id in &victims {
            if let Some(s) = self.strongs.remove(scc_id) {
                ids.extend(s);
            }
            self.roots.remove(scc_id);
        }
        for id in ids {
            self.belongs.remove(&id);
        }
    }
    pub fn keep_only(&mut self, ids: impl IntoIterator<Item = Id>) {
        let mut keep_sccs = ids.into_iter().map(|id| self.belongs[&id]).collect::<HashSet<_>>();
        // all frontiers have already been visited, but not their dependencies
        let mut frontier_sccs: Vec<usize> = keep_sccs.iter().cloned().collect();
        while let Some(scc_id) = frontier_sccs.pop() {
            for next in self.deps.query(&scc_id) {
                if keep_sccs.insert(next) {
                    frontier_sccs.push(next);
                }
            }
        }
        let mut victims = HashSet::new();
        for scc_id in self.strongs.keys().cloned().collect::<Vec<_>>() {
            if !keep_sccs.contains(&scc_id) {
                victims.insert(scc_id);
            }
        }
        for scc_id in victims {
            let ids = self.strongs.remove(&scc_id).unwrap();
            for id in ids {
                self.belongs.remove(&id);
            }
            let Some(next) = self.srcs.map.remove(&scc_id) else { continue };
            for n in &next {
                self.deps.map.get_mut(n).unwrap().remove(&scc_id);
            }
            self.roots.remove(&scc_id);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    #[test]
    fn test_scc_0() {
        let mut deps = DepGraph::new();
        deps.add(1, []);
        let mut scc = Kosaraju::new(&deps).run();
        println!("{:?}", scc);
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
        // let mut scc = Tarjan::run(&deps);
        let mut scc = Kosaraju::new(&deps).run();
        // println!("{:?}", scc);
        for s in scc.top() {
            assert_eq!(s.len(), 1);
            let n = s.into_iter().next().unwrap();
            assert!(n == 4 || n == 6);
        }
        scc.release([4]);
        // println!("{:?}", scc);
        assert_eq!(scc.top(), vec![[6].into_iter().collect()]);
        scc.release([6]);
        // println!("{:?}", scc);
        assert_eq!(scc.top(), vec![[5].into_iter().collect()]);
        scc.release([5]);
        assert_eq!(scc.top(), vec![[3].into_iter().collect()]);
        scc.release([3]);
        assert_eq!(scc.top(), vec![[1, 2, 7, 8, 9].into_iter().collect()]);
    }
}
