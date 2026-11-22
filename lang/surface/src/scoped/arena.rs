use super::syntax::*;
use crate::textual::syntax as t;
use zydeco_derive::{AsMutSelf, AsRefSelf};
use zydeco_foundation::prelude::{DepGraph, SccGraph, Tarjan};

pub use crate::arena::*;

/* ---------------------------------- Arena --------------------------------- */

/// Owning storage scope for lowered and name-resolved surface syntax.
#[derive(Debug)]
pub enum ScopedScope {}

impl Allocates<DefId> for ScopedScope {}
impl Allocates<PatId> for ScopedScope {}
impl Allocates<TermId> for ScopedScope {}
impl Allocates<ContextNodeId> for ScopedScope {}

impl ArenaSchema<DefId> for ScopedScope {
    type Item = VarName;
}
impl ArenaSchema<PatId> for ScopedScope {
    type Item = Pattern;
}
impl ArenaSchema<TermId> for ScopedScope {
    type Item = Term;
}
impl ArenaSchema<ContextNodeId> for ScopedScope {
    type Item = ContextNode;
}

/* ----------------------------- Textual Origins ---------------------------- */

/// One compact textual origin for every derived surface node.
///
/// Each resolved unit allocates derived IDs sequentially within its own key space,
/// so each page retains the unit's textual key space and compact per-slot origins.
#[derive(Clone, Debug, Default)]
pub struct TextualOrigins {
    pages: ArenaAssoc<KeySpaceId, std::sync::Arc<OriginPage>>,
}

#[derive(Clone, Debug, Default)]
struct OriginPage {
    textual_key_space: Option<KeySpaceId>,
    derived_categories: Vec<DerivedCategory>,
    textual_origins: Vec<CompactTextualOrigin>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
enum DerivedCategory {
    Definition,
    Pattern,
    Term,
}

impl DerivedCategory {
    fn split(entity: EntityId) -> (Self, KeySpaceId, RawIdx) {
        match entity {
            | EntityId::Def(id) => (Self::Definition, id.key_space(), id.raw()),
            | EntityId::Pat(id) => (Self::Pattern, id.key_space(), id.raw()),
            | EntityId::Term(id) => (Self::Term, id.key_space(), id.raw()),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CompactTextualOrigin {
    Source { raw: RawIdx, category: t::EntityCategory },
    Synthetic,
}

impl CompactTextualOrigin {
    fn split(entity: t::EntityId) -> (KeySpaceId, Self) {
        let (category, key_space, raw) = entity.into_parts();
        (key_space, Self::Source { raw, category })
    }

    fn join(self, key_space: Option<KeySpaceId>) -> Option<t::EntityId> {
        match self {
            | Self::Source { raw, category } => Some(
                category
                    .restore(key_space.expect("a source origin page has a textual key space"), raw),
            ),
            | Self::Synthetic => None,
        }
    }
}

impl TextualOrigins {
    pub fn insert_new(&mut self, textual: t::EntityId, derived: EntityId) {
        let (textual_key_space, textual) = CompactTextualOrigin::split(textual);
        let (category, derived_key_space, raw) = DerivedCategory::split(derived);
        let entry = self.pages.entry(derived_key_space).or_default();
        let page = std::sync::Arc::make_mut(entry);
        match page.textual_key_space {
            | Some(existing) => assert_eq!(
                existing, textual_key_space,
                "textual origins must belong to one source program",
            ),
            | None => page.textual_key_space = Some(textual_key_space),
        }
        assert_eq!(
            raw.into_u32() as usize,
            page.textual_origins.len(),
            "derived IDs must follow their allocation sequence",
        );
        page.derived_categories.push(category);
        page.textual_origins.push(textual);
    }

    pub fn insert_synthetic(&mut self, derived: EntityId) {
        let (category, derived_key_space, raw) = DerivedCategory::split(derived);
        let entry = self.pages.entry(derived_key_space).or_default();
        let page = std::sync::Arc::make_mut(entry);
        assert_eq!(
            raw.into_u32() as usize,
            page.textual_origins.len(),
            "derived IDs must follow their allocation sequence",
        );
        page.derived_categories.push(category);
        page.textual_origins.push(CompactTextualOrigin::Synthetic);
    }

    pub fn textual_key_space(&self) -> Option<KeySpaceId> {
        let mut spaces = self.pages.iter().filter_map(|(_, page)| page.textual_key_space);
        let first = spaces.next()?;
        assert!(spaces.next().is_none(), "unit origins have at most one textual key space");
        Some(first)
    }

    pub fn merge(&mut self, other: &TextualOrigins) {
        for (key_space, page) in other.pages.iter() {
            self.pages.entry(*key_space).or_insert_with(|| std::sync::Arc::clone(page));
        }
    }

    pub fn source(&self, derived: &EntityId) -> Option<t::EntityId> {
        let (category, key_space, raw) = DerivedCategory::split(*derived);
        let page = self.pages.get(&key_space)?;
        let index = raw.into_u32() as usize;
        let origin = *page.textual_origins.get(index)?;
        (page.derived_categories.get(index) == Some(&category))
            .then_some(origin)
            .and_then(|origin| origin.join(page.textual_key_space))
    }
}

/* ----------------------------- Binding Context ---------------------------- */

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
        let mut components = Tarjan::new(&dependencies).run();
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
        let graph = Tarjan::new(&node_dependencies).run();
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
    pub partial_binders: std::collections::HashSet<t::PatId>,
    /// Default prelude definitions synthesized per unit without an editable token.
    pub default_prelude_defs: rustc_hash::FxHashSet<DefId>,

    /// def user map
    pub users: ArenaForth<DefId, TermId>,
    /// Context DAGs retained for nested `begin` terms.
    pub blocks: ArenaAssoc<TermId, ContextualTerm<BindingContext, BlockBody>>,
}

impl ScopedArena {
    pub fn is_prelude_definition(&self, definition: DefId) -> bool {
        self.is_default_prelude_definition(definition)
            || matches!(self.origins.source(&definition.into()), Some(t::EntityId::Meta(_)))
    }

    pub fn is_default_prelude_definition(&self, definition: DefId) -> bool {
        self.default_prelude_defs.contains(&definition)
    }

    pub fn allows_partial_binding(&self, pattern: PatId) -> bool {
        match self.origins.source(&pattern.into()) {
            | Some(t::EntityId::Pat(source)) => self.partial_binders.contains(&source),
            | _ => false,
        }
    }

    /// Insert a synthetic definition issued by the pass creating it.
    pub fn insert_def(&mut self, id: DefId, name: VarName) {
        self.defs.insert_new(id, name);
    }

    /// Merge an independently resolved unit arena (disjoint key space) into this arena.
    pub fn merge_unit(&mut self, other: &ScopedArena) {
        self.defs.reserve(other.defs.len());
        for (id, name) in other.defs.iter() {
            self.defs.insert_new(*id, *name);
        }
        for (id, pat) in other.pats.iter() {
            self.pats.insert_new(id, pat.clone());
        }
        for (id, term) in other.terms.iter() {
            self.terms.insert_new(id, term.clone());
        }
        self.origins.merge(&other.origins);
        self.partial_binders.extend(other.partial_binders.iter().copied());
        self.default_prelude_defs.extend(other.default_prelude_defs.iter().copied());
        for (def, users) in other.users.iter() {
            for user in users {
                self.users.insert_new(*def, *user);
            }
        }
        self.blocks.reserve(other.blocks.len());
        for (id, block) in other.blocks.iter() {
            self.blocks.insert_new(*id, block.clone());
        }
    }

    /// Replace an existing placeholder term (such as a `SourceBoundary`) when linking units.
    pub fn patch_term(&mut self, id: TermId, term: Term) {
        self.terms.replace_existing(id, term);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::textual::arena::TextualScope;
    use std::mem::size_of;

    #[test]
    fn textual_origins_round_trip_dense_typed_ids() {
        let mut textual = IdAllocator::<TextualScope>::new();
        let source_definition: t::DefId = textual.alloc();
        let source_pattern: t::PatId = textual.alloc();
        let source_copattern: t::CoPatId = textual.alloc();
        let source_term: t::TermId = textual.alloc();
        let mut derived = IdAllocator::<ScopedScope>::new();
        let definition: DefId = derived.alloc();
        let pattern: PatId = derived.alloc();
        let copattern_term: TermId = derived.alloc();
        let source_term_term: TermId = derived.alloc();
        let mut origins = TextualOrigins::default();

        origins.insert_new(source_definition.into(), definition.into());
        origins.insert_new(source_pattern.into(), pattern.into());
        origins.insert_new(source_copattern.into(), copattern_term.into());
        origins.insert_new(source_term.into(), source_term_term.into());

        assert_eq!(size_of::<CompactTextualOrigin>(), 8);
        assert_eq!(size_of::<DerivedCategory>(), 1);
        assert_eq!(origins.source(&definition.into()), Some(source_definition.into()));
        assert_eq!(origins.source(&pattern.into()), Some(source_pattern.into()));
        assert_eq!(origins.source(&copattern_term.into()), Some(source_copattern.into()));
        assert_eq!(origins.source(&source_term_term.into()), Some(source_term.into()));

        let wrong_category: PatId = restore_id(definition.key_space(), definition.raw());
        assert_eq!(origins.source(&wrong_category.into()), None);
    }

    #[test]
    #[should_panic(expected = "derived IDs must follow their allocation sequence")]
    fn textual_origins_reject_allocation_gaps() {
        let mut textual = IdAllocator::<TextualScope>::new();
        let source: t::TermId = textual.alloc();
        let mut derived = IdAllocator::<ScopedScope>::new();
        let _: TermId = derived.alloc();
        let term: TermId = derived.alloc();
        TextualOrigins::default().insert_new(source.into(), term.into());
    }

    #[test]
    #[should_panic(expected = "textual origins must belong to one source program")]
    fn textual_origins_reject_multiple_source_key_spaces() {
        let mut first_textual = IdAllocator::<TextualScope>::new();
        let mut second_textual = IdAllocator::<TextualScope>::new();
        let first_source: t::DefId = first_textual.alloc();
        let second_source: t::PatId = second_textual.alloc();
        let mut derived = IdAllocator::<ScopedScope>::new();
        let definition: DefId = derived.alloc();
        let pattern: PatId = derived.alloc();
        let mut origins = TextualOrigins::default();

        origins.insert_new(first_source.into(), definition.into());
        origins.insert_new(second_source.into(), pattern.into());
    }

    struct ContextFixture {
        allocator: IdAllocator<ScopedScope>,
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
