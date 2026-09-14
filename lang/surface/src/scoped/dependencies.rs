//! Required dependency facts, consumed before each block is elaborated.
use super::{observers::ResolvedReference, syntax::*};
use zydeco_utils::prelude::DepGraph;

#[derive(Default)]
pub(super) struct DependencyAnalyzer {
    graphs: ArenaAssoc<TermId, DepGraph<BindingId>>,
}

impl DependencyAnalyzer {
    pub fn begin_block(&mut self, block: TermId, candidates: impl IntoIterator<Item = BindingId>) {
        assert!(self.graphs.get(&block).is_none(), "a block begins once per resolution visit");
        let graph = candidates.into_iter().fold(DepGraph::new(), |mut graph, candidate| {
            graph.add(candidate, []);
            graph
        });
        self.graphs.insert_new(block, graph);
    }
    pub fn reference(&mut self, event: &ResolvedReference<'_>) {
        if let Some(dependency) = event.dependency {
            for binding in
                event.active_bindings.iter().filter(|binding| binding.owner == dependency.owner)
            {
                self.graphs[&binding.owner].add(binding.id, [dependency.id]);
            }
        }
    }
    pub fn finish_block(&mut self, block: TermId) -> DepGraph<BindingId> {
        self.graphs.remove(&block).expect("the block dependency graph must be active")
    }
    pub fn abort_block(&mut self, block: TermId) {
        self.finish_block(block);
    }
    pub fn assert_closed(&self) {
        assert!(self.graphs.iter().next().is_none(), "every block dependency graph must be closed");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scoped::BindingSite;

    #[test]
    fn events_update_every_enclosing_binding_with_the_matching_owner() {
        let mut ids = IdAllocator::<BitterScope>::new();
        let outer: TermId = ids.alloc();
        let inner: TermId = ids.alloc();
        let first: TermId = ids.alloc();
        let second: TermId = ids.alloc();
        let dependency: TermId = ids.alloc();
        let nested: TermId = ids.alloc();
        let definition: DefId = ids.alloc();
        let mut analyzer = DependencyAnalyzer::default();
        analyzer.begin_block(outer, [first, second, dependency]);
        analyzer.begin_block(inner, [nested]);
        let active = [
            BindingSite { owner: outer, id: first },
            BindingSite { owner: inner, id: nested },
            BindingSite { owner: outer, id: second },
        ]
        .into_iter()
        .collect();
        analyzer.reference(&ResolvedReference {
            occurrence: first,
            definition,
            dependency: Some(BindingSite { owner: outer, id: dependency }),
            active_bindings: &active,
        });
        let inner_graph = analyzer.finish_block(inner);
        assert_eq!(inner_graph.nodes(), [nested].into());
        assert!(inner_graph.query(&nested).is_empty());
        let graph = analyzer.finish_block(outer);
        assert_eq!(graph.query(&first), [dependency]);
        assert_eq!(graph.query(&second), [dependency]);
        assert!(graph.query(&dependency).is_empty());
        assert_eq!(graph.nodes().len(), 3);
        analyzer.begin_block(outer, [first]);
        analyzer.abort_block(outer);
        analyzer.assert_closed();
    }
}
