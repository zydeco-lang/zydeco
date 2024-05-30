use crate::scoped::{err::*, syntax::*};
use zydeco_utils::{
    arena::*,
    cells::{MultiCell, SingCell},
    deps::DepGraph,
    scc::Kosaraju,
};

#[derive(Clone, Debug, Default)]
pub struct Global {
    /// map from variable names to their definitions
    var_to_def: im::HashMap<VarName, DefId>,
    /// map from definitions to their global declarations
    under_map: im::HashMap<DefId, DeclId>,
}
#[derive(Clone, Debug, Default)]
pub struct Local {
    /// which global declaration is the local scope checking in
    under: DeclId,
    /// map from variable names to their definitions
    var_to_def: im::HashMap<VarName, DefId>,
}

trait Binders {
    type Arena;
    fn binders(&self, arena: &Self::Arena) -> im::HashMap<VarName, DefId>;
}

impl Binders for PatId {
    type Arena = Arena;
    fn binders(&self, arena: &Self::Arena) -> im::HashMap<VarName, DefId> {
        let pat = &arena.pats[self];
        match pat {
            | Pattern::Ann(pat) => {
                let Ann { tm, ty: _ } = pat;
                tm.binders(arena)
            }
            | Pattern::Hole(pat) => {
                let Hole = pat;
                im::HashMap::new()
            }
            | Pattern::Var(pat) => {
                let def = pat;
                im::hashmap! { arena.defs[def].clone() => *def }
            }
            | Pattern::Ctor(pat) => {
                let Ctor(_ctor, args) = pat;
                args.binders(arena)
            }
            | Pattern::Triv(pat) => {
                let Triv = pat;
                im::HashMap::new()
            }
            | Pattern::Cons(pat) => {
                let Cons(a, b) = pat;
                let mut res = im::HashMap::new();
                res = res.union(a.binders(arena));
                res = res.union(b.binders(arena));
                res
            }
        }
    }
}

pub struct Resolver {
    pub spans: SpanArena,
    pub bitter: Arena,
    pub prim_term: PrimTerms,
    pub prim_def: PrimDef,
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
    pub prim: PrimDef,
    pub arena: ScopedArena,
}

impl Resolver {
    pub fn run(mut self, top: &TopLevel) -> Result<ResolveOut> {
        top.resolve(&mut self, ())?;
        let Resolver {
            spans,
            bitter: _,
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
        let top = Kosaraju::new(&deps).run();
        Ok(ResolveOut {
            spans,
            prim,
            arena: ScopedArena { defs, pats, terms, decls, users, exts, deps, top },
        })
    }
}

impl Resolver {
    fn collect_global_binders(&mut self, decls: &[DeclId], mut global: Global) -> Result<Global> {
        for id in decls {
            let Modifiers { public: _, external, inner } = &self.bitter.decls[id];
            match inner {
                | Declaration::AliasBody(decl) => {
                    let AliasBody { binder, bindee: _ } = decl;
                    let binders = binder.binders(&self.bitter);
                    // check if it's a primitive and (later in terms) update the internal_to_def
                    // Note: currently this is a bit hacky; we should really have a marker of some sort
                    // maybe macros can help?
                    'out: {
                        if binders.len() != 1 {
                            break 'out;
                        }
                        if !external {
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Monad".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &mut self.prim_def.monad,
                                &self.prim_term.monad,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                id,
                                def,
                                "Monad",
                                Internal::Monad,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Algebra".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &mut self.prim_def.algebra,
                                &self.prim_term.algebra,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                id,
                                def,
                                "Algebra",
                                Internal::Algebra,
                            )?;
                            break 'out;
                        }
                    }
                    self.check_duplicate_and_update_global(id, binders, &mut global)?;
                }
                | Declaration::AliasHead(decl) => {
                    let AliasHead { binder, ty: _ } = decl;
                    let binders = binder.binders(&self.bitter);
                    // check if it's a primitive and (later in terms) update the internal_to_def
                    'out: {
                        if binders.len() != 1 {
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("VType".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &mut self.prim_def.vtype,
                                &self.prim_term.vtype,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                id,
                                def,
                                "VType",
                                Internal::VType,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("CType".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &mut self.prim_def.ctype,
                                &self.prim_term.ctype,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                id,
                                def,
                                "CType",
                                Internal::CType,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Thunk".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &mut self.prim_def.thunk,
                                &self.prim_term.thunk,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                id,
                                def,
                                "Thunk",
                                Internal::Thunk,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Ret".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &mut self.prim_def.ret,
                                &self.prim_term.ret,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                id,
                                def,
                                "Ret",
                                Internal::Ret,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Unit".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &mut self.prim_def.unit,
                                &self.prim_term.unit,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                id,
                                def,
                                "Unit",
                                Internal::Unit,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Int".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &mut self.prim_def.int,
                                &self.prim_term.int,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                id,
                                def,
                                "Int",
                                Internal::Int,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Char".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &mut self.prim_def.char,
                                &self.prim_term.char,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                id,
                                def,
                                "Char",
                                Internal::Char,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("String".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &mut self.prim_def.string,
                                &self.prim_term.string,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                id,
                                def,
                                "String",
                                Internal::String,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("OS".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &mut self.prim_def.os,
                                &self.prim_term.os,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                id,
                                def,
                                "OS",
                                Internal::OS,
                            )?;
                            break 'out;
                        }
                        // Note: the rest may be valid, but we don't know yet; no error is given here
                        // Err(ResolveError::UndefinedPrimitive({
                        //     let (name, def) = binders.iter().next().unwrap();
                        //     self.spans.defs[def].clone().make(name.clone())
                        // }))?
                    }
                    self.check_duplicate_and_update_global(id, binders, &mut global)?;
                }
                | Declaration::Main(_) => {}
            }
        }
        Ok(global)
    }
    fn check_duplicate_and_update_global(
        &self, under: &DeclId, binders: im::HashMap<VarName, DefId>, global: &mut Global,
    ) -> Result<()> {
        for (name, def) in binders.iter() {
            if let Some(prev) = global.var_to_def.get(name) {
                let span1 = &self.spans.defs[prev];
                let span2 = &self.spans.defs[def];
                Err(ResolveError::DuplicateDefinition(
                    span1.make(name.clone()),
                    span2.make(name.clone()),
                ))?;
            }
        }
        // update names
        global.under_map =
            global.under_map.clone().union(binders.values().map(|def| (*def, *under)).collect());
        global.var_to_def = global.var_to_def.clone().union(binders);
        Ok(())
    }
    fn alloc_prim(
        spans: &SpanArena, sc: &mut SingCell<DefId>, mc: &MultiCell<TermId>,
        exts: &mut ArenaAssoc<DeclId, (Internal, DefId)>,
        internal_to_def: &mut ArenaAssoc<TermId, DefId>, decl: &DeclId, def: &DefId,
        name: &'static str, internal: Internal,
    ) -> Result<DefId> {
        let prim = sc
            .init_or_else(
                || *def,
                |id| {
                    ResolveError::DuplicatePrimitive(
                        spans.defs[def].clone().make(VarName(name.into())),
                        spans.defs[id].clone().make(VarName(name.into())),
                    )
                },
            )
            .cloned()?;
        exts.insert(*decl, (internal, prim));
        internal_to_def.extend(mc.all().into_iter().map(|term| (*term, prim)));
        Ok(prim)
    }
}

pub trait Resolve {
    type Out;
    type Lookup<'a>;
    fn resolve(&self, resolver: &mut Resolver, lookup: Self::Lookup<'_>) -> Result<Self::Out>;
}

impl Resolve for TopLevel {
    type Out = ();
    type Lookup<'a> = ();
    fn resolve(&self, resolver: &mut Resolver, global: Self::Lookup<'_>) -> Result<Self::Out> {
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
            | Declaration::Main(decl) => {
                let Main(term) = decl;
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
                let span = &resolver.spans.terms[self];
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
            | Term::Rec(term) => {
                let Rec(pat, body) = &term;
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
                let Ret(body) = &term;
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
                let PureBind { binder, bindee, tail } = &term;
                let () = bindee.resolve(resolver, (local.clone(), global))?;
                local = binder.resolve(resolver, (local.clone(), global))?;
                let () = tail.resolve(resolver, (local, global))?;
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
