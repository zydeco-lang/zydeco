use super::syntax::*;
use crate::textual::syntax as t;
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

/// Item projectors out of the scoped arena.
#[auto_impl::auto_impl(&, &mut, Box, Rc, Arc)]
pub trait ArenaScoped {
    fn def(&self, id: &DefId) -> VarName;
    fn pat(&self, id: &PatId) -> Pattern;
    fn term(&self, id: &TermId) -> Term<DefId>;
}

/// Resolved arena plus name-resolution metadata and dependency/context analysis.
#[derive(Clone, Debug, Default, AsRefSelf, AsMutSelf)]
pub struct ScopedArena {
    // arenas
    pub defs: ArenaSparse<ScopedScope, DefId>,
    pub pats: ArenaSparse<ScopedScope, PatId>,
    pub terms: ArenaSparse<ScopedScope, TermId>,
    /// entity maps from textural syntax
    pub textual: ArenaForth<t::EntityId, EntityId>,

    /// def user map
    pub users: ArenaForth<DefId, TermId>,
    /// variables that are free within the term
    pub coctxs_term_local: ArenaAssoc<TermId, CoContext>,
    /// Context DAGs retained for nested `begin` terms.
    pub blocks: ArenaAssoc<TermId, ContextualTerm<BindingContext, BlockBody>>,
}

impl ScopedArena {
    /// Insert a synthetic definition issued by the pass creating it.
    pub fn insert_def(&mut self, id: DefId, name: VarName) {
        self.defs.insert_new(id, name);
    }
}

impl ArenaScoped for ScopedArena {
    fn def(&self, id: &DefId) -> VarName {
        self.defs[id].to_owned()
    }
    fn pat(&self, id: &PatId) -> Pattern {
        self.pats[id].to_owned()
    }
    fn term(&self, id: &TermId) -> Term<DefId> {
        self.terms[id].to_owned()
    }
}

use super::Collector;

impl ArenaScoped for Collector {
    fn def(&self, id: &DefId) -> VarName {
        self.defs[id].to_owned()
    }
    fn pat(&self, id: &PatId) -> Pattern {
        self.pats[id].to_owned()
    }
    fn term(&self, id: &TermId) -> Term<DefId> {
        self.terms[id].to_owned()
    }
}

/* -------------------------------- LocalFold ------------------------------- */

/// A set of local actions on scoped arena items.
#[auto_impl::auto_impl(&mut, Box)]
pub trait LocalFoldScoped<Cx>: ArenaScoped {
    fn action_def(&mut self, def: DefId, ctx: &Cx);
    fn action_pat(&mut self, pat: PatId, ctx: &Cx);
    fn action_term(&mut self, term: TermId, ctx: &Cx);
}

impl LocalFoldScoped<()> for Collector {
    fn action_def(&mut self, _def: DefId, _ctx: &()) {}

    /// Updates [`Self::ctxs_pat_local`] and [`Self::coctxs_pat_local`].
    fn action_pat(&mut self, pat: PatId, _ctx: &()) {
        let item = self.pat(&pat);
        match item {
            | Pattern::Ann(inner) => {
                let Ann { tm, ty } = inner;
                self.ctxs_pat_local.insert_new(pat, self.ctxs_pat_local[&tm].to_owned());
                self.coctxs_pat_local.insert_new(pat, {
                    let co_tm = self.coctxs_pat_local[&tm].to_owned();
                    let co_ty = self.coctxs_term_local[&ty].to_owned();
                    co_tm + co_ty
                });
            }
            | Pattern::Hole(inner) => {
                let Hole = inner;
                self.ctxs_pat_local.insert_new(pat, Context::new());
                self.coctxs_pat_local.insert_new(pat, CoContext::new());
            }
            | Pattern::Var(inner) => {
                let def = inner;
                self.ctxs_pat_local.insert_new(pat, Context::singleton(def));
                self.coctxs_pat_local.insert_new(pat, CoContext::new());
            }
            | Pattern::Named(inner) => {
                let Named(_name, inner) = inner;
                self.ctxs_pat_local.insert_new(pat, self.ctxs_pat_local[&inner].to_owned());
                self.coctxs_pat_local.insert_new(pat, self.coctxs_pat_local[&inner].to_owned());
            }
            | Pattern::Triv(Triv) => {
                self.ctxs_pat_local.insert_new(pat, Context::new());
                self.coctxs_pat_local.insert_new(pat, CoContext::new());
            }
            | Pattern::Ctor(inner) => {
                let Ctor(_ctorv, body) = inner;
                self.ctxs_pat_local.insert_new(pat, self.ctxs_pat_local[&body].to_owned());
                self.coctxs_pat_local.insert_new(pat, self.coctxs_pat_local[&body].to_owned());
            }
            | Pattern::Project(ProjectionPattern(_, inner)) => {
                self.ctxs_pat_local.insert_new(pat, self.ctxs_pat_local[&inner].to_owned());
                self.coctxs_pat_local.insert_new(pat, self.coctxs_pat_local[&inner].to_owned());
            }
            | Pattern::Alias(Alias(inner)) | Pattern::Cons(inner) => {
                let local = inner
                    .iter()
                    .fold(Context::new(), |ctx, item| ctx + self.ctxs_pat_local[item].to_owned());
                let colocal = inner.iter().fold(CoContext::new(), |ctx, item| {
                    ctx + self.coctxs_pat_local[item].to_owned()
                });
                self.ctxs_pat_local.insert_new(pat, local);
                self.coctxs_pat_local.insert_new(pat, colocal);
            }
        }
    }

    /// Updates [`Self::coctxs_term_local`].
    fn action_term(&mut self, term: TermId, _ctx: &()) {
        let item = self.term(&term);
        match item {
            | Term::Meta(inner) => {
                let MetaT(_meta, inner) = inner;
                let co_term = self.coctxs_term_local[&inner].to_owned();
                self.coctxs_term_local.insert_new(term, co_term);
            }
            | Term::SourceBoundary(inner) => {
                let SourceBoundary(inner) = inner;
                let co_inner = self.coctxs_term_local[&inner].to_owned();
                self.coctxs_term_local.insert_new(term, co_inner);
            }
            | Term::SignatureBoundary(inner) => {
                let SignatureBoundary(inner) = inner;
                let co_inner = self.coctxs_term_local[&inner].to_owned();
                self.coctxs_term_local.insert_new(term, co_inner);
            }
            | Term::Internal(_) => {
                self.coctxs_term_local.insert_new(term, CoContext::new());
            }
            | Term::Sealed(inner) => {
                let Sealed(inner) = inner;
                let co_inner = self.coctxs_term_local[&inner].to_owned();
                self.coctxs_term_local.insert_new(term, co_inner);
            }
            | Term::Ann(inner) => {
                let Ann { tm, ty } = inner;
                self.coctxs_term_local.insert_new(term, {
                    let co_tm = self.coctxs_term_local[&tm].to_owned();
                    let co_ty = self.coctxs_term_local[&ty].to_owned();
                    co_tm + co_ty
                });
            }
            | Term::Hole(inner) => {
                let Hole = inner;
                self.coctxs_term_local.insert_new(term, CoContext::new());
            }
            | Term::Var(inner) => {
                let def = inner;
                self.coctxs_term_local.insert_new(term, CoContext::singleton(def));
            }
            | Term::Named(inner) => {
                let Named(_name, inner) = inner;
                let co_inner = self.coctxs_term_local[&inner].to_owned();
                self.coctxs_term_local.insert_new(term, co_inner);
            }
            | Term::Label(inner) => {
                let Label(_name, inner) = inner;
                let co_inner = self.coctxs_term_local[&inner].to_owned();
                self.coctxs_term_local.insert_new(term, co_inner);
            }
            | Term::Triv(Triv) => {
                self.coctxs_term_local.insert_new(term, CoContext::new());
            }
            | Term::Cons(inner) => {
                let colocal = inner.iter().fold(CoContext::new(), |ctx, item| {
                    ctx + self.coctxs_term_local[item].to_owned()
                });
                self.coctxs_term_local.insert_new(term, colocal);
            }
            | Term::Abs(inner) => {
                let Abs(pat, body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                let cx_pat = self.ctxs_pat_local[&pat].to_owned();
                let co_pat = self.coctxs_pat_local[&pat].to_owned();
                self.coctxs_term_local.insert_new(term, co_body - cx_pat + co_pat);
            }
            | Term::App(inner) => {
                let App(a, b) = inner;
                let co_a = self.coctxs_term_local[&a].to_owned();
                let co_b = self.coctxs_term_local[&b].to_owned();
                self.coctxs_term_local.insert_new(term, co_a + co_b);
            }
            | Term::Fix(inner) => {
                let Fix(pat, body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                let cx_pat = self.ctxs_pat_local[&pat].to_owned();
                let co_pat = self.coctxs_pat_local[&pat].to_owned();
                self.coctxs_term_local.insert_new(term, co_body - cx_pat + co_pat);
            }
            | Term::Pi(inner) => {
                let Pi(pat, body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                let cx_pat = self.ctxs_pat_local[&pat].to_owned();
                let co_pat = self.coctxs_pat_local[&pat].to_owned();
                self.coctxs_term_local.insert_new(term, co_body - cx_pat + co_pat);
            }
            | Term::Sigma(inner) => {
                let Sigma(pat, body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                let cx_pat = self.ctxs_pat_local[&pat].to_owned();
                let co_pat = self.coctxs_pat_local[&pat].to_owned();
                self.coctxs_term_local.insert_new(term, co_body - cx_pat + co_pat);
            }
            | Term::ManifestExists(inner) => {
                let ManifestExists { binder, definition, body } = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                let cx_binder = self.ctxs_pat_local[&binder].to_owned();
                let co_binder = self.coctxs_pat_local[&binder].to_owned();
                let co_definition = self.coctxs_term_local[&definition].to_owned();
                self.coctxs_term_local
                    .insert_new(term, co_body - cx_binder + co_binder + co_definition);
            }
            | Term::Thunk(inner) => {
                let Thunk(body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                self.coctxs_term_local.insert_new(term, co_body);
            }
            | Term::Force(inner) => {
                let Force(body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                self.coctxs_term_local.insert_new(term, co_body);
            }
            | Term::Ret(inner) => {
                let Return(body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                self.coctxs_term_local.insert_new(term, co_body);
            }
            | Term::Do(inner) => {
                let Bind { binder, bindee, tail } = inner;
                let co_tail = self.coctxs_term_local[&tail].to_owned();
                let cx_binder = self.ctxs_pat_local[&binder].to_owned();
                let co_binder = self.coctxs_pat_local[&binder].to_owned();
                let co_bindee = self.coctxs_term_local[&bindee].to_owned();
                self.coctxs_term_local
                    .insert_new(term, co_tail - cx_binder + co_binder + co_bindee);
            }
            | Term::Let(inner) => {
                let Let { binder, bindee, tail } = inner;
                let co_tail = self.coctxs_term_local[&tail].to_owned();
                let cx_binder = self.ctxs_pat_local[&binder].to_owned();
                let co_binder = self.coctxs_pat_local[&binder].to_owned();
                let co_bindee = self.coctxs_term_local[&bindee].to_owned();
                self.coctxs_term_local
                    .insert_new(term, co_tail - cx_binder + co_binder + co_bindee);
            }
            | Term::MobileParam(_) | Term::MobileBind(_) => {
                unreachable!("mobile syntax must be eliminated during name resolution")
            }
            | Term::Residual(inner) => {
                let Residual(body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                self.coctxs_term_local.insert_new(term, co_body);
            }
            | Term::Block(inner) => {
                let Block(body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                self.coctxs_term_local.insert_new(term, co_body);
            }
            | Term::RecGroup(inner) => {
                let RecGroup { definitions, tail } = inner;
                let bound = definitions.iter().fold(Context::new(), |ctx, definition| {
                    ctx + self.ctxs_pat_local[&definition.binder].to_owned()
                });
                let free_definitions =
                    definitions.iter().fold(CoContext::new(), |ctx, definition| {
                        ctx + self.coctxs_pat_local[&definition.binder].to_owned()
                            + self.coctxs_term_local[&definition.bindee].to_owned()
                    });
                let free_tail = self.coctxs_term_local[&tail].to_owned();
                self.coctxs_term_local.insert_new(term, free_definitions + free_tail - bound);
            }
            | Term::MoBlock(inner) => {
                let MoBlock { body, basis } = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                let co_basis = self.coctxs_term_local[&basis.monad].to_owned()
                    + self.coctxs_term_local[&basis.algebra].to_owned();
                self.coctxs_term_local.insert_new(term, co_body + co_basis);
            }
            | Term::Data(inner) => {
                let Data { arms } = inner;
                let co_arms = CoContext::from_iter(arms.into_iter().flat_map(
                    |DataArm { name: _, param }| self.coctxs_term_local[&param].to_owned(),
                ));
                self.coctxs_term_local.insert_new(term, co_arms);
            }
            | Term::CoData(inner) => {
                let CoData { arms } = inner;
                let co_arms = CoContext::from_iter(arms.into_iter().flat_map(
                    |CoDataArm { name: _, out }| self.coctxs_term_local[&out].to_owned(),
                ));
                self.coctxs_term_local.insert_new(term, co_arms);
            }
            | Term::Ctor(inner) => {
                let Ctor(_name, body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                self.coctxs_term_local.insert_new(term, co_body);
            }
            | Term::Match(inner) => {
                let Match { scrut, arms } = inner;
                let co_arms =
                    CoContext::from_iter(arms.into_iter().flat_map(|Matcher { binder, tail }| {
                        let cx_binder = self.ctxs_pat_local[&binder].to_owned();
                        let co_binder = self.coctxs_pat_local[&binder].to_owned();
                        let co_tail = self.coctxs_term_local[&tail].to_owned();
                        co_tail - cx_binder + co_binder
                    }));
                let co_scrut = self.coctxs_term_local[&scrut].to_owned();
                self.coctxs_term_local.insert_new(term, co_arms + co_scrut);
            }
            | Term::CoMatchClauses(inner) => {
                let CoMatchClauses { clauses } = inner;
                let co_clauses = CoContext::from_iter(clauses.into_iter().flat_map(
                    |CoPatternClause { spine, tail }| {
                        spine.iter().rev().fold(
                            self.coctxs_term_local[&tail].to_owned(),
                            |free, item| match item {
                                | CoPatternItem::Pat(pattern) => {
                                    free - self.ctxs_pat_local[pattern].to_owned()
                                        + self.coctxs_pat_local[pattern].to_owned()
                                }
                                | CoPatternItem::Dtor(_) => free,
                            },
                        )
                    },
                ));
                self.coctxs_term_local.insert_new(term, co_clauses);
            }
            | Term::CoMatch(inner) => {
                let CoMatch { arms } = inner;
                let co_arms = CoContext::from_iter(arms.into_iter().flat_map(
                    |CoMatcher { dtor: _, tail }| self.coctxs_term_local[&tail].to_owned(),
                ));
                self.coctxs_term_local.insert_new(term, co_arms);
            }
            | Term::Dtor(inner) => {
                let Dtor(body, _name) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                self.coctxs_term_local.insert_new(term, co_body);
            }
            | Term::Proj(inner) => {
                let Proj(head, _name) = inner;
                let co_head = self.coctxs_term_local[&head].to_owned();
                self.coctxs_term_local.insert_new(term, co_head);
            }
            | Term::Lit(inner) => {
                let _lit = inner;
                self.coctxs_term_local.insert_new(term, CoContext::new());
            }
        }
    }
}

/* ------------------------------ LocalPostFold ----------------------------- */

/// A forward fold w/ context. Reader + State monad.
pub trait ObverseLocalPostFold {
    fn obverse_local_post<C, F>(self, f: &mut F, ctx: &C)
    where
        F: LocalFoldScoped<C>;
}

mod impl_obverse_local_post {
    use super::*;

    impl<T> ObverseLocalPostFold for Option<T>
    where
        T: ObverseLocalPostFold,
    {
        fn obverse_local_post<C, F>(self, f: &mut F, ctx: &C)
        where
            F: LocalFoldScoped<C>,
        {
            match self {
                | Some(item) => item.obverse_local_post(f, ctx),
                | None => {}
            }
        }
    }

    impl ObverseLocalPostFold for DefId {
        fn obverse_local_post<C, F>(self, f: &mut F, ctx: &C)
        where
            F: LocalFoldScoped<C>,
        {
            f.action_def(self, ctx)
        }
    }

    impl ObverseLocalPostFold for PatId {
        fn obverse_local_post<C, F>(self, f: &mut F, ctx: &C)
        where
            F: LocalFoldScoped<C>,
        {
            let item = f.pat(&self);
            match item {
                | Pattern::Ann(inner) => {
                    let Ann { tm, ty } = inner;
                    tm.obverse_local_post(f, ctx);
                    ty.obverse_local_post(f, ctx);
                }
                | Pattern::Hole(inner) => {
                    let Hole = inner;
                }
                | Pattern::Var(inner) => {
                    let def = inner;
                    def.obverse_local_post(f, ctx);
                }
                | Pattern::Named(inner) => {
                    let Named(_name, inner) = inner;
                    inner.obverse_local_post(f, ctx);
                }
                | Pattern::Triv(Triv) => {}
                | Pattern::Ctor(inner) => {
                    let Ctor(_ctorv, body) = inner;
                    body.obverse_local_post(f, ctx);
                }
                | Pattern::Project(ProjectionPattern(_, inner)) => {
                    inner.obverse_local_post(f, ctx);
                }
                | Pattern::Alias(Alias(inner)) | Pattern::Cons(inner) => {
                    for item in inner {
                        item.obverse_local_post(f, ctx);
                    }
                }
            }
            f.action_pat(self, ctx)
        }
    }

    impl ObverseLocalPostFold for TermId {
        fn obverse_local_post<C, F>(self, f: &mut F, ctx: &C)
        where
            F: LocalFoldScoped<C>,
        {
            let item = f.term(&self);
            match item {
                | Term::Meta(inner) => {
                    let MetaT(_meta, term) = inner;
                    term.obverse_local_post(f, ctx);
                }
                | Term::SourceBoundary(inner) => {
                    let SourceBoundary(inner) = inner;
                    inner.obverse_local_post(f, ctx);
                }
                | Term::SignatureBoundary(inner) => {
                    let SignatureBoundary(inner) = inner;
                    inner.obverse_local_post(f, ctx);
                }
                | Term::Internal(_) => {}
                | Term::Sealed(inner) => {
                    let Sealed(inner) = inner;
                    inner.obverse_local_post(f, ctx);
                }
                | Term::Ann(inner) => {
                    let Ann { tm, ty } = inner;
                    tm.obverse_local_post(f, ctx);
                    ty.obverse_local_post(f, ctx);
                }
                | Term::Hole(inner) => {
                    let Hole = inner;
                }
                | Term::Var(inner) => {
                    let def = inner;
                    def.obverse_local_post(f, ctx);
                }
                | Term::Named(inner) => {
                    let Named(_name, inner) = inner;
                    inner.obverse_local_post(f, ctx);
                }
                | Term::Label(inner) => {
                    let Label(_name, inner) = inner;
                    inner.obverse_local_post(f, ctx);
                }
                | Term::Triv(Triv) => {}
                | Term::Cons(inner) => {
                    for item in inner {
                        item.obverse_local_post(f, ctx);
                    }
                }
                | Term::Abs(inner) => {
                    let Abs(pat, body) = inner;
                    pat.obverse_local_post(f, ctx);
                    body.obverse_local_post(f, ctx);
                }
                | Term::App(inner) => {
                    let App(a, b) = inner;
                    a.obverse_local_post(f, ctx);
                    b.obverse_local_post(f, ctx);
                }
                | Term::Fix(inner) => {
                    let Fix(pat, body) = inner;
                    pat.obverse_local_post(f, ctx);
                    body.obverse_local_post(f, ctx);
                }
                | Term::Pi(inner) => {
                    let Pi(pat, body) = inner;
                    pat.obverse_local_post(f, ctx);
                    body.obverse_local_post(f, ctx);
                }
                | Term::Sigma(inner) => {
                    let Sigma(pat, body) = inner;
                    pat.obverse_local_post(f, ctx);
                    body.obverse_local_post(f, ctx);
                }
                | Term::ManifestExists(inner) => {
                    let ManifestExists { binder, definition, body } = inner;
                    definition.obverse_local_post(f, ctx);
                    binder.obverse_local_post(f, ctx);
                    body.obverse_local_post(f, ctx);
                }
                | Term::Thunk(inner) => {
                    let Thunk(body) = inner;
                    body.obverse_local_post(f, ctx);
                }
                | Term::Force(inner) => {
                    let Force(body) = inner;
                    body.obverse_local_post(f, ctx);
                }
                | Term::Ret(inner) => {
                    let Return(body) = inner;
                    body.obverse_local_post(f, ctx);
                }
                | Term::Do(inner) => {
                    let Bind { binder, bindee, tail } = inner;
                    bindee.obverse_local_post(f, ctx);
                    binder.obverse_local_post(f, ctx);
                    tail.obverse_local_post(f, ctx);
                }
                | Term::Let(inner) => {
                    let Let { binder, bindee, tail } = inner;
                    bindee.obverse_local_post(f, ctx);
                    binder.obverse_local_post(f, ctx);
                    tail.obverse_local_post(f, ctx);
                }
                | Term::MobileParam(_) | Term::MobileBind(_) => {
                    unreachable!("mobile syntax must be eliminated during name resolution")
                }
                | Term::Residual(inner) => {
                    let Residual(body) = inner;
                    body.obverse_local_post(f, ctx);
                }
                | Term::Block(inner) => {
                    let Block(body) = inner;
                    body.obverse_local_post(f, ctx);
                }
                | Term::RecGroup(inner) => {
                    let RecGroup { definitions, tail } = inner;
                    for RecursiveDefinition { binder, bindee } in definitions {
                        binder.obverse_local_post(f, ctx);
                        bindee.obverse_local_post(f, ctx);
                    }
                    tail.obverse_local_post(f, ctx);
                }
                | Term::MoBlock(inner) => {
                    let MoBlock { body, basis } = inner;
                    basis.monad.obverse_local_post(f, ctx);
                    basis.algebra.obverse_local_post(f, ctx);
                    body.obverse_local_post(f, ctx);
                }
                | Term::Data(inner) => {
                    let Data { arms } = inner;
                    for DataArm { name: _, param } in arms {
                        param.obverse_local_post(f, ctx);
                    }
                }
                | Term::CoData(inner) => {
                    let CoData { arms } = inner;
                    for CoDataArm { name: _, out } in arms {
                        out.obverse_local_post(f, ctx);
                    }
                }
                | Term::Ctor(inner) => {
                    let Ctor(_name, body) = inner;
                    body.obverse_local_post(f, ctx);
                }
                | Term::Match(inner) => {
                    let Match { scrut, arms } = inner;
                    scrut.obverse_local_post(f, ctx);
                    for Matcher { binder, tail } in arms {
                        binder.obverse_local_post(f, ctx);
                        tail.obverse_local_post(f, ctx);
                    }
                }
                | Term::CoMatchClauses(inner) => {
                    let CoMatchClauses { clauses } = inner;
                    for CoPatternClause { spine, tail } in clauses {
                        for item in spine.into_items() {
                            if let CoPatternItem::Pat(pattern) = item {
                                pattern.obverse_local_post(f, ctx);
                            }
                        }
                        tail.obverse_local_post(f, ctx);
                    }
                }
                | Term::CoMatch(inner) => {
                    let CoMatch { arms } = inner;
                    for CoMatcher { dtor: _, tail } in arms {
                        tail.obverse_local_post(f, ctx);
                    }
                }
                | Term::Dtor(inner) => {
                    let Dtor(body, _name) = inner;
                    body.obverse_local_post(f, ctx);
                }
                | Term::Proj(inner) => {
                    let Proj(head, _name) = inner;
                    head.obverse_local_post(f, ctx);
                }
                | Term::Lit(inner) => {
                    let _lit = inner;
                }
            }
            f.action_term(self, ctx)
        }
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
                    metas: im::Vector::new(),
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
