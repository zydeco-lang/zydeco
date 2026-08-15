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

#[derive(Copy, Clone, Debug)]
pub(super) struct BindingSite {
    pub(super) owner: TermId,
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
    pub prim_def: PrimDefs,

    // arenas
    pub defs: ArenaSparse<ScopedScope, DefId>,
    pub pats: ArenaSparse<ScopedScope, PatId>,
    pub terms: ArenaSparse<ScopedScope, TermId>,
    pub blocks: ArenaAssoc<TermId, ContextualTerm<BindingContext, BlockBody>>,

    pub users: ArenaForth<DefId, TermId>,
    pub(super) block_deps: ArenaAssoc<TermId, DepGraph<BindingId>>,
}

/// Output of name resolution for one complete source term.
pub struct ResolveSourceOut {
    pub prim: PrimDefs,
    pub arena: ScopedArena,
    pub root: TermId,
}

struct ResolvedProgram {
    prim: PrimDefs,
    arena: ScopedArena,
}

impl<'a> Resolver<'a> {
    pub fn new(spans: &'a SpanArena, bitter: BitterArena, _prim_term: PrimTerms) -> Self {
        Self {
            allocator: IdAllocator::new(),
            spans,
            bitter,
            prim_def: PrimDefs::default(),

            defs: ArenaSparse::default(),
            pats: ArenaSparse::default(),
            terms: ArenaSparse::default(),
            blocks: ArenaAssoc::default(),

            users: ArenaForth::default(),
            block_deps: ArenaAssoc::default(),
        }
    }

    /// Run name resolution and context collection over one complete source term.
    pub fn run_source(mut self, root: TermId) -> Result<ResolveSourceOut> {
        root.resolve(&mut self, (Local::for_body(), &Global::default()))?;
        let ResolvedProgram { prim, arena } = self.finish(root)?;
        Ok(ResolveSourceOut { prim, arena, root })
    }

    fn finish(self, root: TermId) -> Result<ResolvedProgram> {
        let Resolver {
            allocator,
            spans: _,
            bitter,
            prim_def: prim,

            defs,
            pats,
            terms,
            blocks,

            users,
            block_deps,
        } = self;
        let _ = allocator;
        assert!(block_deps.iter().next().is_none(), "every block dependency graph must be closed");
        let BitterArena { defs: _, pats: _, terms: _, textual } = bitter;
        let ctxs_pat_local = ArenaAssoc::default();
        let coctxs_pat_local = ArenaAssoc::default();
        let coctxs_term_local = ArenaAssoc::default();
        let collector = Collector {
            defs,
            pats,
            terms,
            textual,
            users,
            ctxs_pat_local,
            coctxs_pat_local,
            coctxs_term_local,
            blocks,
        };
        let Collector {
            defs,
            pats,
            terms,
            textual,
            users,
            ctxs_pat_local: _,
            coctxs_pat_local: _,
            coctxs_term_local,
            blocks,
        } = collector.run_source(root);
        Ok(ResolvedProgram {
            prim,
            arena: ScopedArena { defs, pats, terms, textual, users, coctxs_term_local, blocks },
        })
    }

    fn add_dependency(&mut self, local: &Local, dependency: BindingSite) {
        local.under.iter().copied().filter(|binding| binding.owner == dependency.owner).for_each(
            |binding| {
                self.block_deps[&binding.owner].add(binding.id, [dependency.id]);
            },
        );
    }

    fn resolve_reference(
        &mut self, user: TermId, name: &VarName, local: &Local, global: &Global,
    ) -> Result<DefId> {
        let (definition, dependency) = local
            .var_to_def
            .get(name)
            .map(|definition| (*definition, local.under_map.get(definition).copied()))
            .or_else(|| {
                global
                    .var_to_def
                    .get(name)
                    .map(|definition| (*definition, Some(global.under_map[definition])))
            })
            .ok_or_else(|| ResolveError::UnboundVar(user.span(self).make(name.clone())))?;
        self.users.insert_new(definition, user);
        if let Some(dependency) = dependency {
            self.add_dependency(local, dependency);
        }
        Ok(definition)
    }
}

/// Performs name resolution, turning `VarName`s into `DefId`s with dependency tracking.
pub trait Resolve {
    type Out;
    type Lookup<'a>;
    fn resolve(&self, resolver: &mut Resolver, lookup: Self::Lookup<'_>) -> Result<Self::Out>;
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
            | Pattern::Project(ProjectionPattern(_, pattern)) => {
                pattern.resolve(resolver, (local, global))?
            }
            | Pattern::Alias(Alias(pat)) | Pattern::Cons(pat) => {
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
            | Term::SourceBoundary(term) => {
                let SourceBoundary(inner) = term;
                let global = Global::default();
                let () = inner.resolve(resolver, (Local::for_body(), &global))?;
                SourceBoundary(inner).into()
            }
            | Term::SignatureBoundary(term) => {
                let SignatureBoundary(inner) = term;
                let global = Global::default();
                let () = inner.resolve(resolver, (Local::for_body(), &global))?;
                SignatureBoundary(inner).into()
            }
            | Term::Internal(internal) => internal.into(),
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
                let definition = resolver.resolve_reference(*self, &var, &local, global)?;
                resolver.terms.insert_new(*self, Term::Var(definition));
                return Ok(());
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
                let MoBlock { body, basis } = &term;
                basis.monad.resolve(resolver, (local.clone(), global))?;
                basis.algebra.resolve(resolver, (local.clone(), global))?;
                let () = body.resolve(resolver, (local.clone(), global))?;
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
            | Term::CoMatchClauses(term) => {
                let CoMatchClauses { clauses } = &term;
                for CoPatternClause { spine, tail } in clauses {
                    let mut clause_local = local.clone();
                    for item in spine.iter() {
                        if let CoPatternItem::Pat(pattern) = item {
                            clause_local = pattern.resolve(resolver, (clause_local, global))?;
                        }
                    }
                    let () = tail.resolve(resolver, (clause_local, global))?;
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
    pub ctxs_pat_local: ArenaAssoc<PatId, Context>,
    pub coctxs_pat_local: ArenaAssoc<PatId, CoContext>,
    pub coctxs_term_local: ArenaAssoc<TermId, CoContext>,

    pub blocks: ArenaAssoc<TermId, ContextualTerm<BindingContext, BlockBody>>,
}

impl Collector {
    pub fn run_source(mut self, root: TermId) -> Self {
        root.obverse_local_post(&mut self, &());
        self
    }
}
