use super::syntax::*;
use crate::bitter::arena::TextualOrigins;
use zydeco_derive::{AsMutSelf, AsRefSelf};
use zydeco_utils::prelude::{DepGraph, Kosaraju, SccGraph};

pub use crate::arena::*;

/* ---------------------------------- Arena --------------------------------- */

/// Owning storage scope for name-resolved surface syntax.
#[derive(Debug)]
pub enum ScopedScope {}

impl Allocates<ContextNodeId> for ScopedScope {}
impl Allocates<TermId> for ScopedScope {}

impl ArenaSchema<DefId> for ScopedScope {
    type Item = VarName;
}
impl ArenaSchema<PatId> for ScopedScope {
    type Item = Pattern;
}
impl ArenaSchema<TermId> for ScopedScope {
    type Item = Term<DefId>;
}
impl ArenaSchema<ContextNodeId> for ScopedScope {
    type Item = ContextNode;
}

/// The condensation DAG carried by a contextual term.
#[derive(Clone, Debug)]
pub struct BindingContext {
    pub nodes: ArenaSparse<ScopedScope, ContextNodeId>,
    graph: SccGraph<ContextNodeId>,
    dependencies: DepGraph<BindingId>,
}

impl Default for BindingContext {
    fn default() -> Self {
        Self::from_bindings(IdAllocator::new(), ArenaAssoc::default(), DepGraph::default())
    }
}

impl BindingContext {
    pub(crate) fn from_bindings(
        mut allocator: IdAllocator<ScopedScope>, mut bindings: ArenaAssoc<BindingId, Binding>,
        dependencies: DepGraph<BindingId>,
    ) -> Self {
        let mut components = Kosaraju::new(&dependencies).run();
        let mut ready = Vec::new();
        let groups = std::iter::from_fn(|| {
            if let Some(group) = ready.pop() {
                return Some(group);
            }
            ready = components.top();
            if ready.is_empty() {
                return None;
            }
            components.release(ready.iter().flat_map(|group| group.iter()).copied());
            ready.pop()
        })
        .collect::<Vec<_>>();

        let mut nodes = ArenaSparse::default();
        let mut node_for_binding = ArenaAssoc::default();
        groups.into_iter().for_each(|group| {
            let mut ids = group.into_iter().collect::<Vec<_>>();
            ids.sort_by_key(|id| bindings[id].source_order());
            let recursive = ids.len() > 1
                || ids
                    .first()
                    .is_some_and(|id| dependencies.query(id).into_iter().any(|dep| dep == *id));
            let members = ids
                .iter()
                .map(|id| {
                    bindings
                        .remove(id)
                        .expect("each binding belongs to exactly one context component")
                })
                .collect::<Vec<_>>();
            let node = if recursive {
                ContextNode::Recursive(members)
            } else {
                ContextNode::Acyclic(members.into_iter().next().expect("an SCC cannot be empty"))
            };
            let node_id = allocator.alloc();
            ids.into_iter().for_each(|binding| {
                node_for_binding.insert_new(binding, node_id);
            });
            nodes.insert_new(node_id, node);
        });
        assert!(
            bindings.iter().next().is_none(),
            "all context bindings must occur in the dependency graph"
        );

        let mut node_dependencies = DepGraph::new();
        nodes.iter().for_each(|(node, _)| node_dependencies.add(*node, []));
        dependencies.nodes().into_iter().for_each(|binding| {
            let node = node_for_binding[&binding];
            let deps = dependencies
                .query(&binding)
                .into_iter()
                .map(|dependency| node_for_binding[&dependency])
                .filter(|dependency| *dependency != node);
            node_dependencies.add(node, deps);
        });
        let graph = Kosaraju::new(&node_dependencies).run();
        Self { nodes, graph, dependencies }
    }

    /// Create a mutable traversal state without exposing graph representation.
    pub fn traversal(&self) -> SccGraph<ContextNodeId> {
        self.graph.clone()
    }

    /// Return the currently ready DAG nodes in deterministic source order.
    pub fn ready(&self, traversal: &SccGraph<ContextNodeId>) -> Vec<ContextNodeId> {
        let mut ready = traversal
            .top()
            .into_iter()
            .map(|group| {
                let mut members = group.into_iter();
                let node = members.next().expect("a context DAG node cannot be empty");
                assert!(members.next().is_none(), "the context condensation graph must be acyclic");
                node
            })
            .collect::<Vec<_>>();
        ready.sort_by_key(|node| self.nodes[node].source_order());
        ready
    }

    /// Produce a deterministic dependency-respecting order of all context nodes.
    pub fn topological_order(&self) -> Vec<ContextNodeId> {
        let mut traversal = self.traversal();
        let mut ready = Vec::new();
        std::iter::from_fn(|| {
            if let Some(node) = ready.pop() {
                return Some(node);
            }
            ready = self.ready(&traversal);
            if ready.is_empty() {
                return None;
            }
            traversal.release(ready.iter().copied());
            ready.reverse();
            ready.pop()
        })
        .collect()
    }

    /// Query source binding dependencies for downstream lowering.
    pub fn dependencies(&self, binding: &BindingId) -> Vec<BindingId> {
        self.dependencies.query(binding)
    }

    pub fn binding_ids(&self) -> impl Iterator<Item = BindingId> + '_ {
        self.nodes.iter().flat_map(|(_, node)| node.bindings().iter().map(|binding| binding.id))
    }
}

/// Resolved arena plus name-resolution metadata and dependency analysis.
#[derive(Clone, Debug, Default, AsRefSelf, AsMutSelf)]
pub struct ScopedArena {
    // arenas
    pub defs: ArenaSparse<ScopedScope, DefId>,
    pub pats: ArenaIndexed<ScopedScope, PatId>,
    pub terms: ArenaIndexed<ScopedScope, TermId>,
    /// Textual source origin of every resolved entity.
    pub origins: TextualOrigins,
    /// Source binders explicitly opted in to refutable computation binding.
    pub partial_binders: std::collections::HashSet<crate::textual::syntax::PatId>,

    /// def user map
    pub users: ArenaForth<DefId, TermId>,
    /// Context DAGs retained for nested `begin` terms.
    pub blocks: ArenaAssoc<TermId, ContextualTerm<BindingContext, BlockBody>>,
    /// Authoring scopes of documentation annotations, before entering their payload.
    pub documentation_scopes: ArenaAssoc<TermId, super::ScopeSnapshot>,
}

impl ScopedArena {
    pub fn allows_partial_binding(&self, pattern: PatId) -> bool {
        match self.origins.source(&pattern.into()) {
            | Some(crate::textual::syntax::EntityId::Pat(source)) => {
                self.partial_binders.contains(&source)
            }
            | _ => false,
        }
    }

    /// Insert a synthetic definition issued by the pass creating it.
    pub fn insert_def(&mut self, id: DefId, name: VarName) {
        self.defs.insert_new(id, name);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitter::arena::BitterScope;

    struct ContextFixture {
        allocator: IdAllocator<BitterScope>,
        bindings: ArenaAssoc<BindingId, Binding>,
        dependencies: DepGraph<BindingId>,
    }

    impl ContextFixture {
        fn new() -> Self {
            Self {
                allocator: IdAllocator::new(),
                bindings: ArenaAssoc::default(),
                dependencies: DepGraph::default(),
            }
        }

        fn add_binding(&mut self, source_order: usize) -> BindingId {
            let id: TermId = self.allocator.alloc();
            let binder = self.allocator.alloc();
            let bindee = self.allocator.alloc();
            self.bindings.insert_new(
                id,
                Binding {
                    id,
                    inner: BindingForm::Definition(Definition { binder, bindee }),
                    metas: rpds::VectorSync::new_sync(),
                    source_order,
                },
            );
            id
        }

        fn build(self) -> BindingContext {
            BindingContext::from_bindings(IdAllocator::new(), self.bindings, self.dependencies)
        }
    }

    #[test]
    fn context_nodes_preserve_scc_shape_and_condensation_dependencies() {
        let mut fixture = ContextFixture::new();
        let root = fixture.add_binding(0);
        let dependent = fixture.add_binding(1);
        let self_recursive = fixture.add_binding(2);
        let mutual_left = fixture.add_binding(3);
        let mutual_right = fixture.add_binding(4);

        fixture.dependencies.add(root, []);
        fixture.dependencies.add(dependent, [root]);
        fixture.dependencies.add(self_recursive, [self_recursive]);
        fixture.dependencies.add(mutual_left, [mutual_right]);
        fixture.dependencies.add(mutual_right, [mutual_left]);

        let context = fixture.build();
        let node_for = |binding| {
            context
                .nodes
                .iter()
                .find_map(|(node_id, node)| {
                    node.bindings().iter().any(|member| member.id == binding).then_some(*node_id)
                })
                .expect("binding must belong to a context node")
        };

        assert!(matches!(context.nodes[&node_for(root)], ContextNode::Acyclic(_)));
        assert!(matches!(context.nodes[&node_for(dependent)], ContextNode::Acyclic(_)));
        assert!(matches!(
            &context.nodes[&node_for(self_recursive)],
            ContextNode::Recursive(bindings)
                if bindings.iter().map(|binding| binding.id).collect::<Vec<_>>()
                    == vec![self_recursive]
        ));
        assert!(matches!(
            &context.nodes[&node_for(mutual_left)],
            ContextNode::Recursive(bindings)
                if bindings.iter().map(|binding| binding.id).collect::<Vec<_>>()
                    == vec![mutual_left, mutual_right]
        ));

        let mut traversal = context.traversal();
        let initially_ready = context.ready(&traversal);
        assert!(initially_ready.contains(&node_for(root)));
        assert!(initially_ready.contains(&node_for(self_recursive)));
        assert!(initially_ready.contains(&node_for(mutual_left)));
        assert!(!initially_ready.contains(&node_for(dependent)));
        traversal.release(initially_ready);
        assert_eq!(context.ready(&traversal), vec![node_for(dependent)]);
    }
}
