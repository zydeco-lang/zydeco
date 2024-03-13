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
    pub fn new<'a>(id_deps: &'a DepGraph<Id>, belongs: HashMap<Id, usize>) -> Self {
        let mut strongs = HashMap::new();
        for (id, low) in &belongs {
            strongs.entry(*low).or_insert_with(HashSet::new).insert(id.clone());
        }
        let mut srcs = SrcGraph::new();
        let mut deps = DepGraph::new();
        for (k, scc) in &strongs {
            for id in scc {
                for d in id_deps.query(id) {
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
            let scc = &self.strongs[root];
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
}

#[cfg(test)]
mod tests {
    use super::*;
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
