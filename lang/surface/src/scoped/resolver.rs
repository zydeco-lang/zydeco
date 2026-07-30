use crate::scoped::{syntax::*, *};
use crate::textual::syntax as t;
use zydeco_utils::prelude::DepGraph;

/// Global name environment collected from top-level binders.
#[derive(Clone, Debug, Default)]
pub struct Global {
    /// map from variable names to their definitions
    pub(super) var_to_def: im::HashMap<VarName, DefId>,
    /// map from definitions to their context bindings
    pub(super) under_map: im::HashMap<DefId, BindingSite>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum ContextOwner {
    Root,
    Block(TermId),
}

#[derive(Copy, Clone, Debug)]
pub(super) struct BindingSite {
    pub(super) owner: ContextOwner,
    pub(super) id: BindingId,
}

/// Local name environment built from pattern binders.
#[derive(Clone, Debug)]
pub struct Local {
    /// Context bindings whose dependencies are currently being collected,
    /// from outermost to innermost.
    pub(super) under: im::Vector<BindingSite>,
    /// map from variable names to their definitions
    pub(super) var_to_def: im::HashMap<VarName, DefId>,
    /// Context candidates associated with block-wide definitions.
    pub(super) under_map: im::HashMap<DefId, BindingSite>,
    /// The nearest block currently resolving its residual syntax.
    pub(super) boundary: Option<TermId>,
}

impl Local {
    fn for_binding(under: DeclId) -> Self {
        Self {
            under: im::vector![BindingSite {
                owner: ContextOwner::Root,
                id: BindingId::Declaration(under),
            }],
            var_to_def: im::HashMap::new(),
            under_map: im::HashMap::new(),
            boundary: None,
        }
    }

    fn for_body() -> Self {
        Self {
            under: im::Vector::new(),
            var_to_def: im::HashMap::new(),
            under_map: im::HashMap::new(),
            boundary: None,
        }
    }
}

/// Name-resolution state and accumulators.
pub struct Resolver<'a> {
    pub(super) allocator: IdAllocator<ScopedScope>,
    pub spans: &'a SpanArena,
    pub bitter: BitterArena,
    pub prim_term: PrimTerms,
    pub prim_def: PrimDefs,
    /// all internal definitions mapped to a corresponding def
    pub internal_to_def: ArenaAssoc<TermId, DefId>,

    // arenas
    pub defs: ArenaSparse<ScopedScope, DefId>,
    pub pats: ArenaSparse<ScopedScope, PatId>,
    pub terms: ArenaSparse<ScopedScope, TermId>,
    pub bindings: ArenaAssoc<BindingId, Binding>,
    pub body: Option<ContextBody>,
    pub blocks: ArenaAssoc<TermId, ContextualTerm<BindingContext, BlockBody>>,

    pub users: ArenaForth<DefId, TermId>,
    pub metas: ArenaAssoc<DeclId, im::Vector<Meta>>,
    pub exts: ArenaAssoc<BindingId, (Internal, DefId)>,
    pub deps: DepGraph<BindingId>,
    pub(super) block_deps: ArenaAssoc<TermId, DepGraph<BindingId>>,
    source_order: ArenaAssoc<BindingId, usize>,
}

/// Output of the name-resolution pass.
pub struct ResolveOut {
    pub prim: PrimDefs,
    pub arena: ScopedArena,
}

impl<'a> Resolver<'a> {
    pub fn new(spans: &'a SpanArena, bitter: BitterArena, prim_term: PrimTerms) -> Self {
        Self {
            allocator: IdAllocator::new(),
            spans,
            bitter,
            prim_term,
            prim_def: PrimDefs::default(),
            internal_to_def: ArenaAssoc::default(),

            defs: ArenaSparse::default(),
            pats: ArenaSparse::default(),
            terms: ArenaSparse::default(),
            bindings: ArenaAssoc::default(),
            body: None,
            blocks: ArenaAssoc::default(),

            users: ArenaForth::default(),
            metas: ArenaAssoc::default(),
            exts: ArenaAssoc::default(),
            deps: DepGraph::default(),
            block_deps: ArenaAssoc::default(),
            source_order: ArenaAssoc::default(),
        }
    }
    /// Run name resolution and context collection over the top-level program.
    pub fn run(mut self, top: &TopLevel) -> Result<ResolveOut> {
        top.resolve(&mut self, ())?;
        let Resolver {
            allocator,
            spans: _,
            bitter,
            prim_term: _,
            prim_def: prim,
            internal_to_def: _,

            defs,
            pats,
            terms,
            bindings,
            body,
            blocks,

            users,
            metas: _,
            exts,
            deps,
            block_deps,
            source_order: _,
        } = self;
        assert!(block_deps.iter().next().is_none(), "every block dependency graph must be closed");
        let BitterArena { defs: _, pats: _, terms: _, decls: _, textual } = bitter;
        let ctxs_term = ArenaAssoc::default();
        let ctxs_pat_local = ArenaAssoc::default();
        let coctxs_pat_local = ArenaAssoc::default();
        let coctxs_term_local = ArenaAssoc::default();
        let root = ContextualTerm {
            context: BindingContext::from_bindings(allocator, bindings, deps),
            body,
        };
        let Collector {
            defs,
            pats,
            terms,
            textual,
            users,
            ctxs_term,
            ctxs_pat_local,
            coctxs_pat_local,
            coctxs_term_local,
            blocks,
            root,
        } = Collector {
            defs,
            pats,
            terms,
            textual,
            users,
            ctxs_term,
            ctxs_pat_local,
            coctxs_pat_local,
            coctxs_term_local,
            blocks,
            root,
        }
        .run()?;
        Ok(ResolveOut {
            prim,
            arena: ScopedArena {
                defs,
                pats,
                terms,
                textual,
                users,
                ctxs_term,
                ctxs_pat_local,
                coctxs_pat_local,
                coctxs_term_local,
                exts,
                blocks,
                root,
            },
        })
    }

    fn add_dependency(&mut self, local: &Local, dependency: BindingSite) {
        local.under.iter().copied().filter(|binding| binding.owner == dependency.owner).for_each(
            |binding| match binding.owner {
                | ContextOwner::Root => self.deps.add(binding.id, [dependency.id]),
                | ContextOwner::Block(block) => {
                    self.block_deps[&block].add(binding.id, [dependency.id])
                }
            },
        );
    }

    fn source_items(&self, declaration: DeclId) -> Vec<DeclId> {
        let Modifiers { inner, .. } = &self.bitter.decls[&declaration];
        match inner {
            | Declaration::Meta(MetaT(_, inner)) => self.source_items(*inner),
            | Declaration::AliasBody(_) | Declaration::AliasHead(_) | Declaration::Exec(_) => {
                vec![declaration]
            }
        }
    }

    fn record_source_order(&mut self, top: &TopLevel) {
        let TopLevel(declarations) = top;
        let bindings = declarations
            .iter()
            .flat_map(|declaration| self.source_items(*declaration))
            .filter(|declaration| {
                matches!(
                    self.bitter.decls[declaration].inner,
                    Declaration::AliasBody(_) | Declaration::AliasHead(_)
                )
            })
            .collect::<Vec<_>>();
        bindings.into_iter().enumerate().for_each(|(order, binding)| {
            self.source_order.insert_new(BindingId::Declaration(binding), order)
        });
    }
}

/// Performs name resolution, turning `VarName`s into `DefId`s with dependency tracking.
pub trait Resolve {
    type Out;
    type Lookup<'a>;
    fn resolve(&self, resolver: &mut Resolver, lookup: Self::Lookup<'_>) -> Result<Self::Out>;
}

impl Resolve for TopLevel {
    type Out = ();
    type Lookup<'a> = ();
    fn resolve(&self, resolver: &mut Resolver, (): Self::Lookup<'_>) -> Result<Self::Out> {
        let TopLevel(decls) = self;
        resolver.record_source_order(self);
        // collect all top-level binders and ...
        // 1. check for duplicates
        // 2. update primitives to internal_to_def
        let global = resolver.collect_global_binders(decls, Global::default())?;
        // within each term (when we also count types as terms),
        // we introduce local binders.
        // since we'll resolve variables in the order of
        // 1. local binders (introduced eagerly),
        // 2. global binders (introduced lazily).
        // therefore, we shall introduce all local binders,
        // but introduce global binders in local scopes only if needed.
        for decl in decls {
            decl.resolve(resolver, &global)?;
        }
        // check all primitives are defined
        resolver.prim_def.check()?;
        Ok(())
    }
}

impl Resolve for DeclId {
    type Out = ();
    type Lookup<'a> = &'a Global;
    fn resolve(&self, resolver: &mut Resolver, global: Self::Lookup<'_>) -> Result<Self::Out> {
        let decl = resolver.bitter.decls[self].clone();
        let Modifiers { public: _, external: _, inner } = decl;
        match inner.clone() {
            | Declaration::Meta(decl) => {
                let MetaT(meta, decl) = decl;
                let mut metas = im::Vector::new();
                if let Some(old) = resolver.metas.remove(self) {
                    metas.extend(old);
                }
                metas.push_back(meta);
                resolver.metas.insert_new(decl, metas);
                let () = decl.resolve(resolver, global)?;
            }
            | Declaration::AliasBody(decl) => {
                let id = BindingId::Declaration(*self);
                resolver.deps.add(id, []);
                let local = Local::for_binding(*self);
                let AliasBody { binder, bindee } = decl;
                // resolve bindee first
                let () = bindee.resolve(resolver, (local.clone(), global))?;
                // and then binder, though we don't need the context yielded by binder
                // since it's global and has been collected already
                let _ = binder.resolve(resolver, (local.clone(), global))?;
                resolver.bindings.insert_new(
                    id,
                    Binding {
                        id,
                        inner: BindingForm::Definition(Definition { binder, bindee }),
                        metas: resolver.metas.remove(self).unwrap_or_default(),
                        source_order: resolver.source_order[&id],
                    },
                );
            }
            | Declaration::AliasHead(decl) => {
                let id = BindingId::Declaration(*self);
                resolver.deps.add(id, []);
                let local = Local::for_binding(*self);
                let AliasHead { binder, ty } = decl;
                // no more bindee, but we still need to resolve the binders just for the type mentioned
                if let Some(ty) = ty {
                    let () = ty.resolve(resolver, (local.clone(), global))?;
                }
                let _ = binder.resolve(resolver, (local.clone(), global))?;
                resolver.bindings.insert_new(
                    id,
                    Binding {
                        id,
                        inner: BindingForm::External(External { binder, classifier: ty }),
                        metas: resolver.metas.remove(self).unwrap_or_default(),
                        source_order: resolver.source_order[&id],
                    },
                );
            }
            | Declaration::Exec(decl) => {
                let local = Local::for_body();
                let Exec(term) = decl;
                let () = term.resolve(resolver, (local.clone(), global))?;
                let body = ContextBody {
                    id: *self,
                    term,
                    metas: resolver.metas.remove(self).unwrap_or_default(),
                };
                if let Some(previous) = resolver.body.replace(body) {
                    Err(ResolveError::DuplicateEntry(
                        previous.id.span(resolver).clone(),
                        self.span(resolver).clone(),
                    ))?
                }
            }
        };
        Ok(())
    }
}
impl Resolve for DefId {
    type Out = ();

    type Lookup<'a> = ();

    fn resolve(&self, resolver: &mut Resolver, _lookup: Self::Lookup<'_>) -> Result<Self::Out> {
        resolver.defs.insert_new(*self, resolver.bitter.defs[self].clone());
        Ok(())
    }
}
impl Resolve for PatId {
    // Note: returns the context yielded **after** the pattern
    type Out = Local;
    type Lookup<'a> = (Local, &'a Global);
    fn resolve(
        &self, resolver: &mut Resolver, (mut local, global): Self::Lookup<'_>,
    ) -> Result<Self::Out> {
        let pat = resolver.bitter.pats[self].clone();
        let local = match &pat {
            | Pattern::Ann(pat) => {
                let Ann { tm, ty } = pat;
                let () = ty.resolve(resolver, (local.clone(), global))?;
                tm.resolve(resolver, (local, global))?
            }
            | Pattern::Hole(pat) => {
                let Hole = pat;
                local
            }
            | Pattern::Triv(Triv) => local,
            | Pattern::Var(def) => {
                let () = def.resolve(resolver, ())?;
                local.var_to_def.insert(resolver.bitter.defs[def].clone(), *def);
                local
            }
            | Pattern::Named(pat) => {
                let Named(_name, inner) = pat;
                inner.resolve(resolver, (local, global))?
            }
            | Pattern::Ctor(pat) => {
                let Ctor(_ctor, args) = pat;
                args.resolve(resolver, (local, global))?
            }
            | Pattern::Cons(pat) => {
                // Later items can depend on binders introduced by earlier items.
                for item in pat {
                    local = item.resolve(resolver, (local, global))?;
                }
                local
            }
        };
        // no id changed, reuse old inner pat structure
        resolver.pats.insert_new(*self, pat);
        Ok(local)
    }
}
impl Resolve for TermId {
    type Out = ();
    type Lookup<'a> = (Local, &'a Global);
    fn resolve(
        &self, resolver: &mut Resolver, (mut local, global): Self::Lookup<'_>,
    ) -> Result<Self::Out> {
        let term = resolver.bitter.terms[self].clone();
        let res: Term<DefId> = match term {
            | Term::Meta(term) => {
                let MetaT(_, inner) = term;
                let () = inner.resolve(resolver, (local, global))?;
                term.into()
            }
            | Term::Internal(_) => {
                // internal terms should be resolved by looking up internal_to_def
                // which has already been updated by primitives when collecting top level
                let def = resolver.internal_to_def[self];
                // now the only thing left is to add the dependency
                let binding = global.under_map[&def];
                resolver.add_dependency(&local, binding);
                // no need update the term as def
                resolver.terms.insert_new(*self, Term::Var(def));
                return Ok(());
            }
            | Term::Sealed(term) => {
                let Sealed(inner) = &term;
                let () = inner.resolve(resolver, (local, global))?;
                term.into()
            }
            | Term::Ann(term) => {
                let Ann { tm, ty } = &term;
                let () = ty.resolve(resolver, (local.clone(), global))?;
                let () = tm.resolve(resolver, (local, global))?;
                term.into()
            }
            | Term::Hole(term) => {
                let Hole = &term;
                term.into()
            }
            | Term::Var(var) => {
                // first, try to find the variable locally
                if let Some(def) = local.var_to_def.get(&var) {
                    // if found, we're done
                    resolver.terms.insert_new(*self, Term::Var(*def));
                    resolver.users.insert_new(*def, *self);
                    if let Some(binding) = local.under_map.get(def) {
                        resolver.add_dependency(&local, *binding);
                    }
                    return Ok(());
                }
                // otherwise, try to find the variable globally
                if let Some(def) = global.var_to_def.get(&var) {
                    // if found, also add dependency
                    resolver.terms.insert_new(*self, Term::Var(*def));
                    resolver.users.insert_new(*def, *self);
                    resolver.add_dependency(&local, global.under_map[def]);
                    return Ok(());
                }
                // if not found, report an error
                let span = &self.span(resolver);
                Err(ResolveError::UnboundVar(span.make(var.clone())))?
            }
            | Term::Named(term) => {
                let Named(_name, inner) = &term;
                let () = inner.resolve(resolver, (local, global))?;
                term.into()
            }
            | Term::Label(term) => {
                let Label(_name, inner) = &term;
                let () = inner.resolve(resolver, (local, global))?;
                term.into()
            }
            | Term::Triv(term) => {
                let Triv = &term;
                term.into()
            }
            | Term::Cons(term) => {
                for item in &term {
                    let () = item.resolve(resolver, (local.clone(), global))?;
                }
                term.into()
            }
            | Term::Abs(term) => {
                let Abs(copat, body) = &term;
                local = copat.resolve(resolver, (local.clone(), global))?;
                let () = body.resolve(resolver, (local, global))?;
                term.into()
            }
            | Term::App(term) => {
                let App(a, b) = &term;
                let () = a.resolve(resolver, (local.clone(), global))?;
                let () = b.resolve(resolver, (local.clone(), global))?;
                term.into()
            }
            | Term::Fix(term) => {
                let Fix(pat, body) = &term;
                local = pat.resolve(resolver, (local.clone(), global))?;
                let () = body.resolve(resolver, (local, global))?;
                term.into()
            }
            | Term::Pi(term) => {
                let Pi(copat, body) = &term;
                local = copat.resolve(resolver, (local.clone(), global))?;
                let () = body.resolve(resolver, (local, global))?;
                term.into()
            }
            | Term::Sigma(term) => {
                let Sigma(copat, body) = &term;
                local = copat.resolve(resolver, (local.clone(), global))?;
                let () = body.resolve(resolver, (local, global))?;
                term.into()
            }
            | Term::ManifestExists(term) => {
                let ManifestExists { binder, definition, body } = &term;
                let () = definition.resolve(resolver, (local.clone(), global))?;
                local = binder.resolve(resolver, (local, global))?;
                let () = body.resolve(resolver, (local, global))?;
                term.into()
            }
            | Term::Thunk(term) => {
                let Thunk(body) = &term;
                let () = body.resolve(resolver, (local.clone(), global))?;
                term.into()
            }
            | Term::Force(term) => {
                let Force(body) = &term;
                let () = body.resolve(resolver, (local.clone(), global))?;
                term.into()
            }
            | Term::Ret(term) => {
                let Return(body) = &term;
                let () = body.resolve(resolver, (local.clone(), global))?;
                term.into()
            }
            | Term::Do(term) => {
                let Bind { binder, bindee, tail } = &term;
                let () = bindee.resolve(resolver, (local.clone(), global))?;
                local = binder.resolve(resolver, (local.clone(), global))?;
                let () = tail.resolve(resolver, (local, global))?;
                term.into()
            }
            | Term::Let(term) => {
                let Let { binder, bindee, tail } = &term;
                let () = bindee.resolve(resolver, (local.clone(), global))?;
                local = binder.resolve(resolver, (local.clone(), global))?;
                let () = tail.resolve(resolver, (local, global))?;
                term.into()
            }
            | Term::MobileParam(term) => {
                let MobileParam { binder: _, tail } = term;
                if local.boundary.is_none() {
                    Err(ResolveError::UnenclosedThat(self.span(resolver).clone()))?
                }
                tail.resolve(resolver, (local, global))?;
                Residual(tail).into()
            }
            | Term::MobileBind(term) => {
                let MobileBind { binder: _, bindee: _, tail } = term;
                if local.boundary.is_none() {
                    Err(ResolveError::UnenclosedThat(self.span(resolver).clone()))?
                }
                tail.resolve(resolver, (local, global))?;
                Residual(tail).into()
            }
            | Term::Residual(_) => {
                unreachable!("residual nodes are introduced only during name resolution")
            }
            | Term::Block(term) => {
                let Block(body) = term;
                resolver.resolve_block(*self, body, local, global)?
            }
            | Term::RecGroup(_) => {
                unreachable!("recursive groups are introduced only after name resolution")
            }
            | Term::MoBlock(term) => {
                let MoBlock(body) = &term;
                let () = body.resolve(resolver, (local.clone(), global))?;
                let mo_def = resolver.prim_def.monad.get().to_owned();
                resolver.add_dependency(&local, global.under_map[&mo_def]);
                term.into()
            }
            | Term::Data(term) => {
                let Data { arms } = &term;
                for arm in arms {
                    let DataArm { name: _, param } = arm;
                    let () = param.resolve(resolver, (local.clone(), global))?;
                }
                term.into()
            }
            | Term::CoData(term) => {
                let CoData { arms } = &term;
                for arm in arms {
                    let CoDataArm { name: _, out } = arm;
                    let () = out.resolve(resolver, (local.clone(), global))?;
                }
                term.into()
            }
            | Term::Ctor(term) => {
                let Ctor(_ctor, body) = &term;
                let () = body.resolve(resolver, (local.clone(), global))?;
                term.into()
            }
            | Term::Match(term) => {
                let Match { scrut, arms } = &term;
                let () = scrut.resolve(resolver, (local.clone(), global))?;
                for arm in arms {
                    let mut local = local.clone();
                    let Matcher { binder, tail } = arm;
                    local = binder.resolve(resolver, (local.clone(), global))?;
                    let () = tail.resolve(resolver, (local.clone(), global))?;
                }
                term.into()
            }
            | Term::CoMatch(term) => {
                let CoMatch { arms } = &term;
                for arm in arms {
                    let CoMatcher { dtor: _, tail } = arm;
                    let () = tail.resolve(resolver, (local.clone(), global))?;
                }
                term.into()
            }
            | Term::Dtor(term) => {
                let Dtor(body, _dtor) = &term;
                let () = body.resolve(resolver, (local.clone(), global))?;
                term.into()
            }
            | Term::Proj(term) => {
                let Proj(head, _name) = &term;
                let () = head.resolve(resolver, (local, global))?;
                term.into()
            }
            | Term::Lit(term) => term.into(),
        };
        // save the new term structure
        resolver.terms.insert_new(*self, res);
        Ok(())
    }
}

/// See [`ScopedArena`] for more detail.
pub struct Collector {
    pub defs: ArenaSparse<ScopedScope, DefId>,
    pub pats: ArenaSparse<ScopedScope, PatId>,
    pub terms: ArenaSparse<ScopedScope, TermId>,
    pub textual: ArenaForth<t::EntityId, EntityId>,

    pub users: ArenaForth<DefId, TermId>,
    pub ctxs_term: ArenaAssoc<TermId, Context>,
    pub ctxs_pat_local: ArenaAssoc<PatId, Context>,
    pub coctxs_pat_local: ArenaAssoc<PatId, CoContext>,
    pub coctxs_term_local: ArenaAssoc<TermId, CoContext>,

    pub blocks: ArenaAssoc<TermId, ContextualTerm<BindingContext, BlockBody>>,
    pub root: ContextualTerm<BindingContext>,
}

impl Collector {
    pub fn run(mut self) -> Result<Self> {
        let ctx = self
            .root
            .context
            .topological_order()
            .into_iter()
            .try_fold(Context::new(), |ctx, node| {
                self.root.context.nodes[&node].clone().collect(&mut self, ctx)
            })?;
        if let Some(body) = self.root.body.clone() {
            body.term.collect(&mut self, ctx)?;
        }
        Ok(self)
    }
}

/// Collect contexts on every pattern and term site in a contextual term.
trait Collect {
    type Out;
    fn collect(&self, collector: &mut Collector, ctx: Context) -> Result<Self::Out>;
}

impl Collect for ContextNode {
    type Out = Context;
    fn collect(&self, collector: &mut Collector, ctx: Context) -> Result<Self::Out> {
        match self {
            | ContextNode::Acyclic(binding) => match &binding.inner {
                | BindingForm::Parameter(_) => {
                    unreachable!("the root context cannot contain parameters")
                }
                | BindingForm::Definition(Definition { binder, bindee }) => {
                    bindee.collect(collector, ctx.clone())?;
                    binder.collect(collector, ctx)
                }
                | BindingForm::External(External { binder, classifier }) => {
                    if let Some(classifier) = classifier {
                        classifier.collect(collector, ctx.clone())?;
                    }
                    binder.collect(collector, ctx)
                }
            },
            | ContextNode::Recursive(bindings) => {
                let ctx = bindings.iter().try_fold(ctx, |ctx, binding| match &binding.inner {
                    | BindingForm::Parameter(_) => {
                        unreachable!("the root context cannot contain parameters")
                    }
                    | BindingForm::Definition(Definition { binder, .. }) => {
                        binder.collect(collector, ctx)
                    }
                    | BindingForm::External(_) => {
                        unreachable!("external bindings cannot participate in a recursive group")
                    }
                })?;
                bindings.iter().try_for_each(|binding| match &binding.inner {
                    | BindingForm::Parameter(_) => {
                        unreachable!("the root context cannot contain parameters")
                    }
                    | BindingForm::Definition(Definition { bindee, .. }) => {
                        bindee.collect(collector, ctx.clone())
                    }
                    | BindingForm::External(_) => {
                        unreachable!("external bindings cannot participate in a recursive group")
                    }
                })?;
                Ok(ctx)
            }
        }
    }
}

impl Collect for PatId {
    type Out = Context;
    fn collect(&self, collector: &mut Collector, ctx: Context) -> Result<Self::Out> {
        let () = self.obverse_local_post(collector, &ctx);
        Ok(ctx + collector.ctxs_pat_local[self].to_owned())
    }
}

impl Collect for TermId {
    type Out = ();
    fn collect(&self, collector: &mut Collector, ctx: Context) -> Result<Self::Out> {
        // very important! this is where we update term contexts.
        self.obverse_local_post(collector, &ctx);
        Ok(())
    }
}
