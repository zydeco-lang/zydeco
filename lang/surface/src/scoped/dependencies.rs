//! Required dependency facts, consumed before each block is elaborated.
use super::{BindingSite, observers::ResolvedReference, syntax::*};
use zydeco_utils::prelude::DepGraph;

#[derive(Default)]
pub(super) struct DependencyAnalyzer {
    graphs: ArenaAssoc<TermId, DepGraph<BindingId>>,
    implicit_providers: ArenaAssoc<TermId, std::collections::HashSet<BindingId>>,
}

impl DependencyAnalyzer {
    pub fn begin_block(&mut self, block: TermId, candidates: impl IntoIterator<Item = BindingId>) {
        assert!(self.graphs.get(&block).is_none(), "a block begins once per resolution visit");
        let graph = candidates.into_iter().fold(DepGraph::new(), |mut graph, candidate| {
            graph.add(candidate, []);
            graph
        });
        self.graphs.insert_new(block, graph);
        self.implicit_providers.insert_new(block, Default::default());
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
    /// An implicit capability lookup needs the independent portion of its context
    /// checked first. Record this separately until ordinary dependencies are complete.
    pub fn implicit_provider(&mut self, active: &rpds::VectorSync<BindingSite>) {
        for binding in active {
            self.implicit_providers[&binding.owner].insert(binding.id);
        }
    }

    pub fn finish_block(&mut self, block: TermId) -> DepGraph<BindingId> {
        let mut graph =
            self.graphs.remove(&block).expect("the block dependency graph must be active");
        let users = self.implicit_providers.remove(&block).expect("the block must be active");
        if !users.is_empty() {
            let reverse = graph.reverse();
            let mut dependent = users.clone();
            let mut pending = users.iter().copied().collect::<Vec<_>>();
            while let Some(binding) = pending.pop() {
                for user in reverse.query(&binding) {
                    if dependent.insert(user) {
                        pending.push(user);
                    }
                }
            }
            let independent = graph.nodes().difference(&dependent).copied().collect::<Vec<_>>();
            for user in users {
                graph.add(user, independent.iter().copied());
            }
        }
        graph
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

    #[test]
    fn implicit_provider_ordering_excludes_transitive_dependents_and_preserves_cycles() {
        let mut ids = IdAllocator::<BitterScope>::new();
        let block: TermId = ids.alloc();
        let user: TermId = ids.alloc();
        let provider: TermId = ids.alloc();
        let direct: TermId = ids.alloc();
        let indirect: TermId = ids.alloc();
        let definition: DefId = ids.alloc();
        let mut analyzer = DependencyAnalyzer::default();
        analyzer.begin_block(block, [user, provider, direct, indirect]);
        for (binding, dependency) in [(direct, user), (indirect, direct)] {
            analyzer.reference(&ResolvedReference {
                occurrence: binding,
                definition,
                dependency: Some(BindingSite { owner: block, id: dependency }),
                active_bindings: &[BindingSite { owner: block, id: binding }].into_iter().collect(),
            });
        }
        analyzer.implicit_provider(&[BindingSite { owner: block, id: user }].into_iter().collect());
        let graph = analyzer.finish_block(block);
        assert_eq!(graph.query(&user), [provider]);
        assert_eq!(graph.query(&direct), [user]);
        assert_eq!(graph.query(&indirect), [direct]);
        assert!(graph.query(&provider).is_empty());

        analyzer.begin_block(block, [user, provider]);
        for (binding, dependency) in [(user, provider), (provider, user)] {
            analyzer.reference(&ResolvedReference {
                occurrence: binding,
                definition,
                dependency: Some(BindingSite { owner: block, id: dependency }),
                active_bindings: &[BindingSite { owner: block, id: binding }].into_iter().collect(),
            });
        }
        analyzer.implicit_provider(&[BindingSite { owner: block, id: user }].into_iter().collect());
        let cyclic = analyzer.finish_block(block);
        assert_eq!(cyclic.query(&user), [provider]);
        assert_eq!(cyclic.query(&provider), [user], "authored cycles remain visible to validation");
        analyzer.assert_closed();
    }
}
