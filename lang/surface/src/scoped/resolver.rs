use crate::scoped::{syntax::*, *};
use crate::textual::syntax as t;
use zydeco_utils::{deps::DepGraph, imc::*, scc::Kosaraju, scc::SccGraph};

#[derive(Clone, Debug, Default)]
pub struct Global {
    /// map from variable names to their definitions
    pub(super) var_to_def: im::HashMap<VarName, DefId>,
    /// map from definitions to their global declarations
    pub(super) under_map: im::HashMap<DefId, DeclId>,
}
#[derive(Clone, Debug, Default)]
pub struct Local {
    /// which global declaration is the local scope checking in
    under: DeclId,
    /// map from variable names to their definitions
    var_to_def: im::HashMap<VarName, DefId>,
}

pub struct Resolver {
    pub spans: SpanArena,
    pub bitter: Arena,
    pub prim_term: PrimTerms,
    pub prim_def: PrimDefs,
    /// all internal definitions mapped to a corresponding def
    pub internal_to_def: ArenaAssoc<TermId, DefId>,

    // arenas
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatId, Pattern>,
    pub terms: ArenaAssoc<TermId, Term<DefId>>,
    pub decls: ArenaAssoc<DeclId, Declaration>,

    pub users: ArenaForth<DefId, TermId>,
    pub exts: ArenaAssoc<DeclId, (Internal, DefId)>,
    pub deps: DepGraph<DeclId>,
}

pub struct ResolveOut {
    pub spans: SpanArena,
    pub prim: PrimDefs,
    pub arena: ScopedArena,
}

impl Resolver {
    pub fn run(mut self, top: &TopLevel) -> Result<ResolveOut> {
        top.resolve(&mut self, ())?;
        let Resolver {
            spans,
            bitter,
            prim_term: _,
            prim_def: prim,
            internal_to_def: _,

            defs,
            pats,
            terms,
            decls,

            users,
            exts,
            deps,
        } = self;
        let defs = bitter.defs.filter_map_id(|id| defs.get(&id).cloned());
        let pats = bitter.pats.filter_map_id(|id| pats.get(&id).cloned());
        let terms = bitter.terms.filter_map_id(|id| terms.get(&id).cloned());
        let decls = bitter.decls.filter_map_id(|id| decls.get(&id).cloned());
        let textual = bitter.textual;
        let ctxs_term = ArenaAssoc::default();
        let ctxs_pat_local = ArenaAssoc::default();
        let coctxs_pat_local = ArenaAssoc::default();
        let coctxs_term_local = ArenaAssoc::default();
        let unis = ArenaAssoc::default();
        let top = Kosaraju::new(&deps).run();
        let Collector {
            defs,
            pats,
            terms,
            decls,
            textual,
            users,
            ctxs_term,
            ctxs_pat_local,
            coctxs_pat_local,
            coctxs_term_local,
            unis,
            deps,
            top,
        } = Collector {
            defs,
            pats,
            terms,
            decls,
            textual,
            users,
            ctxs_term,
            ctxs_pat_local,
            coctxs_pat_local,
            coctxs_term_local,
            unis,
            deps,
            top,
        }
        .run()?;
        Ok(ResolveOut {
            spans,
            prim,
            arena: ScopedArena {
                defs,
                pats,
                terms,
                decls,
                textual,
                users,
                ctxs_term,
                ctxs_pat_local,
                coctxs_pat_local,
                coctxs_term_local,
                exts,
                unis,
                deps,
                top,
            },
        })
    }
}

/// Performs name resolution, effectively turning all `VarName`s back to `DefId`s.
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
        // register the global binder in deps
        resolver.deps.add(*self, []);
        let decl = resolver.bitter.decls[self].clone();
        let local = Local { under: *self, ..Local::default() };
        let Modifiers { public: _, external: _, inner } = decl;
        match inner.clone() {
            | Declaration::AliasBody(decl) => {
                let AliasBody { binder, bindee } = decl;
                // resolve bindee first
                let () = bindee.resolve(resolver, (local.clone(), global))?;
                // and then binder, though we don't need the context yielded by binder
                // since it's global and has been collected already
                let _ = binder.resolve(resolver, (local.clone(), global))?;
            }
            | Declaration::AliasHead(decl) => {
                let AliasHead { binder, ty } = decl;
                // no more bindee, but we still need to resolve the binders just for the type mentioned
                if let Some(ty) = ty {
                    let () = ty.resolve(resolver, (local.clone(), global))?;
                }
                let _ = binder.resolve(resolver, (local.clone(), global))?;
            }
            | Declaration::Module(_) => unreachable!(),
            | Declaration::Exec(decl) => {
                let Exec(term) = decl;
                let () = term.resolve(resolver, (local.clone(), global))?;
            }
        };
        // no id changed, reuse old inner decl structure
        resolver.decls.insert(*self, inner);
        Ok(())
    }
}
impl Resolve for DefId {
    type Out = ();

    type Lookup<'a> = ();

    fn resolve(&self, resolver: &mut Resolver, _lookup: Self::Lookup<'_>) -> Result<Self::Out> {
        resolver.defs.insert(*self, resolver.bitter.defs[self].clone());
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
            | Pattern::Var(def) => {
                let () = def.resolve(resolver, ())?;
                local.var_to_def.insert(resolver.bitter.defs[def].clone(), *def);
                local
            }
            | Pattern::Ctor(pat) => {
                let Ctor(_ctor, args) = pat;
                args.resolve(resolver, (local, global))?
            }
            | Pattern::Triv(pat) => {
                let Triv = pat;
                local
            }
            | Pattern::Cons(pat) => {
                let Cons(a, b) = pat;
                // can be dependent on the previous binders
                local = a.resolve(resolver, (local, global))?;
                local = b.resolve(resolver, (local, global))?;
                local
            }
        };
        // no id changed, reuse old inner pat structure
        resolver.pats.insert(*self, pat);
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
            | Term::Internal(_) => {
                // internal terms should be resolved by looking up internal_to_def
                // which has already been updated by primitives when collecting top level
                let def = resolver.internal_to_def[self];
                // now the only thing left is to add the dependency
                let decl = global.under_map[&def];
                resolver.deps.add(local.under, [decl]);
                // no need update the term as def
                resolver.terms.insert(*self, Term::Var(def));
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
                if let Some(def) = local.var_to_def.get(var.leaf()) {
                    // if found, we're done
                    resolver.terms.insert(*self, Term::Var(*def));
                    resolver.users.insert(*def, *self);
                    return Ok(());
                }
                // otherwise, try to find the variable globally
                if let Some(def) = global.var_to_def.get(var.leaf()) {
                    // if found, also add dependency
                    resolver.terms.insert(*self, Term::Var(*def));
                    resolver.users.insert(*def, *self);
                    resolver.deps.add(local.under, [global.under_map[def]]);
                    return Ok(());
                }
                // if not found, report an error
                let span = &self.span(resolver);
                Err(ResolveError::UnboundVar(span.make(var.clone())))?
            }
            | Term::Triv(term) => {
                let Triv = &term;
                term.into()
            }
            | Term::Cons(term) => {
                let Cons(a, b) = &term;
                let () = a.resolve(resolver, (local.clone(), global))?;
                let () = b.resolve(resolver, (local.clone(), global))?;
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
            | Term::MoBlock(term) => {
                let MoBlock(body) = &term;
                let () = body.resolve(resolver, (local.clone(), global))?;
                let mo_def = resolver.prim_def.monad.get().to_owned();
                resolver.deps.add(local.under, [global.under_map[&mo_def]]);
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
            | Term::Lit(term) => term.into(),
        };
        // save the new term structure
        resolver.terms.insert(*self, res);
        Ok(())
    }
}

/// See [`ScopedArena`] for more detail.
pub struct Collector {
    pub defs: ArenaSparse<DefId, VarName>,
    pub pats: ArenaSparse<PatId, Pattern>,
    pub terms: ArenaSparse<TermId, Term<DefId>>,
    pub decls: ArenaSparse<DeclId, Declaration>,
    pub textual: ArenaForth<t::EntityId, EntityId>,

    pub users: ArenaForth<DefId, TermId>,
    pub ctxs_term: ArenaAssoc<TermId, Context<()>>,
    pub ctxs_pat_local: ArenaAssoc<PatId, Context<()>>,
    pub coctxs_pat_local: ArenaAssoc<PatId, CoContext<()>>,
    pub coctxs_term_local: ArenaAssoc<TermId, CoContext<()>>,

    pub unis: ArenaAssoc<DeclId, ()>,
    pub deps: DepGraph<DeclId>,
    pub top: SccGraph<DeclId>,
}

impl Collector {
    pub fn run(mut self) -> Result<Self> {
        let mut scc = self.top.clone();
        let mut ctx = Context::new();
        loop {
            let groups = scc.top();
            if groups.is_empty() {
                break;
            }
            for group in groups {
                ctx = SccDeclarations(&group).collect(&mut self, ctx.clone())?;
                scc.release(group)
            }
        }
        Ok(self)
    }
}

/// Collects contexts on all term sites.
/// Additionally, collects whether top level declarations are recursive.
///
/// 1. Gradually collects variables from the topologically sorted top level declarations.
/// 2. Within each declaration, collects variables according to the declaration's recursiveness.
trait Collect {
    type Out;
    fn collect(&self, collector: &mut Collector, ctx: Context<()>) -> Result<Self::Out>;
}

impl Collect for SccDeclarations<'_> {
    type Out = Context<()>;
    fn collect(&self, collector: &mut Collector, mut ctx: Context<()>) -> Result<Self::Out> {
        let SccDeclarations(decls) = self;
        match decls.len() {
            | 0 => Ok(ctx),
            | 1 => {
                use std::collections::HashSet;
                let id = decls.iter().next().unwrap();
                let self_ref =
                    collector.deps.query(id).into_iter().collect::<HashSet<_>>().contains(id);
                if self_ref {
                    // collect context for the binder before collecting for the bindee
                    let decl = collector.decls[id].clone();
                    match decl {
                        | Declaration::AliasBody(decl) => {
                            let AliasBody { binder, bindee } = decl;
                            ctx = binder.collect(collector, ctx)?;
                            let () = bindee.collect(collector, ctx.to_owned())?;
                        }
                        | Declaration::AliasHead(_)
                        | Declaration::Module(_)
                        | Declaration::Exec(_) => {
                            unreachable!()
                        }
                    }
                } else {
                    // add the declaration to unis
                    collector.unis.insert(*id, ());
                    // collect context for bindee before collecting for the binder
                    let decl = collector.decls[id].clone();
                    match decl {
                        | Declaration::AliasBody(decl) => {
                            let AliasBody { binder, bindee } = decl;
                            let () = bindee.collect(collector, ctx.to_owned())?;
                            ctx = binder.collect(collector, ctx)?;
                        }
                        | Declaration::AliasHead(decl) => {
                            let AliasHead { binder, ty } = decl;
                            if let Some(ty) = ty {
                                let () = ty.collect(collector, ctx.clone())?;
                            }
                            ctx = binder.collect(collector, ctx)?;
                        }
                        | Declaration::Module(_) => unreachable!(),
                        | Declaration::Exec(decl) => {
                            let Exec(term) = decl;
                            let () = term.collect(collector, ctx.to_owned())?;
                        }
                    }
                }
                Ok(ctx)
            }
            | _ => {
                // collect context for all binders before collecting all bindees
                for decl in decls.iter() {
                    let decl = collector.decls[decl].clone();
                    match decl {
                        | Declaration::AliasBody(decl) => {
                            let AliasBody { binder, bindee: _ } = decl;
                            ctx = binder.collect(collector, ctx)?;
                        }
                        | Declaration::AliasHead(_)
                        | Declaration::Module(_)
                        | Declaration::Exec(_) => {
                            unreachable!()
                        }
                    }
                }
                for decl in decls.iter() {
                    let decl = collector.decls[decl].clone();
                    match decl {
                        | Declaration::AliasBody(decl) => {
                            let AliasBody { binder: _, bindee } = decl;
                            let () = bindee.collect(collector, ctx.to_owned())?;
                        }
                        | Declaration::AliasHead(_)
                        | Declaration::Module(_)
                        | Declaration::Exec(_) => {
                            unreachable!()
                        }
                    }
                }
                Ok(ctx)
            }
        }
    }
}

impl Collect for PatId {
    type Out = Context<()>;
    fn collect(&self, collector: &mut Collector, ctx: Context<()>) -> Result<Self::Out> {
        let () = self.obverse_local_post(collector, &ctx);
        Ok(ctx + collector.ctxs_pat_local[self].to_owned())
    }
}

impl Collect for TermId {
    type Out = ();
    fn collect(&self, collector: &mut Collector, ctx: Context<()>) -> Result<Self::Out> {
        // very important! this is where we update term contexts.
        self.obverse_local_post(collector, &ctx);
        Ok(())
    }
}
