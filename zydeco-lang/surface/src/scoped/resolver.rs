use crate::scoped::{err::*, syntax::*};
use zydeco_utils::{arena::*, cells::SingCell, deps::DepGraph, scc::Kosaraju};

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
            | Pattern::Unit(pat) => {
                let Unit = pat;
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
    pub prim_term: PrimTerm,
    pub prim_def: PrimDef,
    /// all internal definitions mapped to a corresponding def
    pub internal_to_def: ArenaAssoc<TermId, DefId>,

    // pub ctxs: ArenaSparse<CtxtId, Context<DefId>>,
    // pub term_under_ctx: ArenaAssoc<TermId, CtxtId>,

    // arenas
    pub defs: ArenaAssoc<DefId, VarName>,
    pub pats: ArenaAssoc<PatId, Pattern>,
    pub terms: ArenaAssoc<TermId, Term<DefId>>,
    pub decls: ArenaAssoc<DeclId, Declaration>,

    pub users: ArenaForth<DefId, TermId>,
    pub deps: DepGraph<DeclId>,
}

pub struct ResolveOut {
    pub spans: SpanArena,
    pub prim: PrimDef,
    pub arena: ScopedArena,
}

impl Resolver {
    pub fn run(mut self, top: &TopLevel) -> Result<ResolveOut> {
        top.resolve(&mut self, Global::default())?;
        let Resolver {
            spans,
            bitter: _,
            prim_term: _,
            prim_def: prim,
            internal_to_def: _,

            // ctxs,
            // term_under_ctx,

            //
            defs,
            pats,
            terms,
            decls,

            users,
            deps,
        } = self;
        let top = Kosaraju::new(&deps).run();
        Ok(ResolveOut {
            spans,
            prim,
            arena: ScopedArena {
                // ctxs,
                // term_under_ctx,

                //
                defs,
                pats,
                terms,
                decls,

                users,
                deps,
                top,
            },
        })
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
        span: &SpanArena, mc: &mut SingCell<DefId>, def: DefId, name: &'static str,
    ) -> Result<DefId> {
        mc.init_or_else(
            || def,
            |id| {
                ResolveError::DuplicatePrimitive(
                    span.defs[&def].clone().make(VarName(name.into())),
                    span.defs[id].clone().make(VarName(name.into())),
                )
            },
        )
        .cloned()
    }
}

pub trait Resolve {
    type Out;
    type Lookup<'a>;
    fn resolve(&self, resolver: &mut Resolver, lookup: Self::Lookup<'_>) -> Result<Self::Out>;
}

impl Resolve for TopLevel {
    type Out = ();
    type Lookup<'a> = Global;
    fn resolve(&self, resolver: &mut Resolver, mut global: Self::Lookup<'_>) -> Result<Self::Out> {
        let TopLevel(decls) = self;
        // collect all top-level binders and ...
        // 1. check for duplicates
        // 2. update primitives to internal_to_def
        for id in decls {
            let Modifiers { public: _, inner } = &resolver.bitter.decls[id];
            match inner {
                | Declaration::Alias(decl) => {
                    let Alias { binder, bindee: _ } = decl;
                    let binders = binder.binders(&resolver.bitter);
                    // check if it's a primitive and (later in terms) update the internal_to_def
                    // Note: currently this is a bit hacky; we should really have a marker of some sort
                    // maybe macros can help?
                    'out: {
                        if binders.len() != 1 {
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Monad".into())) {
                            let monad = Resolver::alloc_prim(
                                &resolver.spans,
                                &mut resolver.prim_def.monad,
                                *def,
                                "Monad",
                            )?;
                            resolver.internal_to_def.extend(
                                resolver
                                    .prim_term
                                    .monad
                                    .all()
                                    .into_iter()
                                    .map(|term| (*term, monad)),
                            );
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Algebra".into())) {
                            let algebra = Resolver::alloc_prim(
                                &resolver.spans,
                                &mut resolver.prim_def.algebra,
                                *def,
                                "Algebra",
                            )?;
                            resolver.internal_to_def.extend(
                                resolver
                                    .prim_term
                                    .algebra
                                    .all()
                                    .into_iter()
                                    .map(|term| (*term, algebra)),
                            );
                            break 'out;
                        }
                    }
                    resolver.check_duplicate_and_update_global(id, binders, &mut global)?;
                }
                | Declaration::Extern(decl) => {
                    let Extern { comp: _, binder, params: _, ty: _ } = decl;
                    let binders = binder.binders(&resolver.bitter);
                    // check if it's a primitive and (later in terms) update the internal_to_def
                    'out: {
                        if binders.len() != 1 {
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("VType".into())) {
                            let vtype = Resolver::alloc_prim(
                                &resolver.spans,
                                &mut resolver.prim_def.vtype,
                                *def,
                                "VType",
                            )?;
                            resolver.internal_to_def.extend(
                                resolver
                                    .prim_term
                                    .vtype
                                    .all()
                                    .into_iter()
                                    .map(|term| (*term, vtype)),
                            );
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("CType".into())) {
                            let ctype = Resolver::alloc_prim(
                                &resolver.spans,
                                &mut resolver.prim_def.ctype,
                                *def,
                                "CType",
                            )?;
                            resolver.internal_to_def.extend(
                                resolver
                                    .prim_term
                                    .ctype
                                    .all()
                                    .into_iter()
                                    .map(|term| (*term, ctype)),
                            );
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Thunk".into())) {
                            let thunk = Resolver::alloc_prim(
                                &resolver.spans,
                                &mut resolver.prim_def.thunk,
                                *def,
                                "Thunk",
                            )?;
                            resolver.internal_to_def.extend(
                                resolver
                                    .prim_term
                                    .thunk
                                    .all()
                                    .into_iter()
                                    .map(|term| (*term, thunk)),
                            );
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Ret".into())) {
                            let ret = Resolver::alloc_prim(
                                &resolver.spans,
                                &mut resolver.prim_def.ret,
                                *def,
                                "Ret",
                            )?;
                            resolver.internal_to_def.extend(
                                resolver.prim_term.ret.all().into_iter().map(|term| (*term, ret)),
                            );
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Unit".into())) {
                            let unit = Resolver::alloc_prim(
                                &resolver.spans,
                                &mut resolver.prim_def.unit,
                                *def,
                                "Unit",
                            )?;
                            resolver.internal_to_def.extend(
                                resolver.prim_term.unit.all().into_iter().map(|term| (*term, unit)),
                            );
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Int".into())) {
                            let int = Resolver::alloc_prim(
                                &resolver.spans,
                                &mut resolver.prim_def.int,
                                *def,
                                "Int",
                            )?;
                            resolver.internal_to_def.extend(
                                resolver.prim_term.int.all().into_iter().map(|term| (*term, int)),
                            );
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Char".into())) {
                            let char = Resolver::alloc_prim(
                                &resolver.spans,
                                &mut resolver.prim_def.char,
                                *def,
                                "Char",
                            )?;
                            resolver.internal_to_def.extend(
                                resolver.prim_term.char.all().into_iter().map(|term| (*term, char)),
                            );
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("String".into())) {
                            let string = Resolver::alloc_prim(
                                &resolver.spans,
                                &mut resolver.prim_def.string,
                                *def,
                                "String",
                            )?;
                            resolver.internal_to_def.extend(
                                resolver
                                    .prim_term
                                    .string
                                    .all()
                                    .into_iter()
                                    .map(|term| (*term, string)),
                            );
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("OS".into())) {
                            let os = Resolver::alloc_prim(
                                &resolver.spans,
                                &mut resolver.prim_def.os,
                                *def,
                                "OS",
                            )?;
                            resolver.internal_to_def.extend(
                                resolver.prim_term.os.all().into_iter().map(|term| (*term, os)),
                            );
                            break 'out;
                        }
                        Err(ResolveError::UndefinedPrimitive({
                            let (name, def) = binders.iter().next().unwrap();
                            resolver.spans.defs[def].clone().make(name.clone())
                        }))?
                    }
                    resolver.check_duplicate_and_update_global(id, binders, &mut global)?;
                }
                | Declaration::Main(_) => {}
            }
        }
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
        let Modifiers { public: _, inner } = decl;
        match inner.clone() {
            | Declaration::Alias(decl) => {
                let Alias { binder, bindee } = decl;
                // resolve bindee first
                let () = bindee.resolve(resolver, (local.clone(), global))?;
                // and then binder, though we don't need the context yielded by binder
                // since it's global and has been collected already
                let _ = binder.resolve(resolver, (local.clone(), global))?;
            }
            | Declaration::Extern(decl) => {
                let Extern { comp: _, binder, params, ty } = decl;
                // no more bindee, but we still need to resolve the binders just for the type mentioned
                if let Some(ty) = ty {
                    let () = ty.resolve(resolver, (local.clone(), global))?;
                }
                for param in params {
                    let _ = param.resolve(resolver, (local.clone(), global))?;
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
            | Pattern::Unit(pat) => {
                let Unit = pat;
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
            | Term::Unit(term) => {
                let Unit = &term;
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
