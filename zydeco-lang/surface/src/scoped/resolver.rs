use crate::scoped::{err::*, syntax::*};
use zydeco_utils::{arena::ArenaAssoc, deps::DepGraph, multi_cell::MultiCell, scc::Kosaraju};

#[derive(Clone, Debug, Default)]
pub struct Global {
    /// map from variable names to their definitions
    map: im::HashMap<VarName, DefId>,
    under_map: im::HashMap<DefId, DeclId>,
}
#[derive(Clone, Debug, Default)]
pub struct Local {
    /// which global declaration is the local scope checking in
    under: DeclId,
    /// map from variable names to their definitions
    map: im::HashMap<VarName, DefId>,
}

trait Binders {
    type Arena;
    fn binders<'f>(&self, arena: &'f Self::Arena) -> im::HashMap<VarName, DefId>;
}

impl Binders for PatId {
    type Arena = Arena;
    fn binders<'f>(&self, arena: &'f Self::Arena) -> im::HashMap<VarName, DefId> {
        let pat = &arena.pats[*self];
        match pat {
            Pattern::Ann(pat) => {
                let Ann { tm, ty: _ } = pat;
                tm.binders(arena)
            }
            Pattern::Hole(pat) => {
                let Hole = pat;
                im::HashMap::new()
            }
            Pattern::Var(pat) => {
                let def = pat;
                im::hashmap! { arena.defs[*def].clone() => *def }
            }
            Pattern::Ctor(pat) => {
                let Ctor(_ctor, args) = pat;
                args.binders(arena)
            }
            Pattern::Paren(pat) => {
                let Paren(pats) = pat;
                let mut res = im::HashMap::new();
                for binder in pats {
                    res = res.union(binder.binders(arena));
                }
                res
            }
        }
    }
}

pub struct Resolver {
    pub spans: SpanArenaBitter,
    pub arena: Arena,
    pub prim_term: PrimTerm,
    pub prim_def: PrimDef,
    pub term_to_def: ArenaAssoc<TermId, DefId>,
    /// the dependency of top level definitions
    pub deps: DepGraph<DeclId>,
}

pub struct ResolveOut {
    pub scoped: ScopedArena,
    pub arena: Arena,
    pub spans: SpanArenaBitter,
}

impl Resolver {
    pub fn run(mut self, top: &TopLevel) -> Result<ResolveOut> {
        top.resolve(&mut self, Global::default())?;
        let Resolver { spans, arena, prim_term: _, prim_def: _, term_to_def, deps } = self;
        Ok(ResolveOut {
            spans,
            arena,
            scoped: ScopedArena {
                term_to_def: term_to_def.clone(),
                scc: Kosaraju::new(&deps).run(),
                deps,
            },
        })
    }
    fn check_duplicate_and_update_global(
        &self, under: &DeclId, binders: im::HashMap<VarName, DefId>, global: &mut Global,
    ) -> Result<()> {
        for (name, def) in binders.iter() {
            if let Some(prev) = global.map.get(name) {
                let span1 = &self.spans.defs[*prev];
                let span2 = &self.spans.defs[*def];
                Err(ResolveError::DuplicateDefinition(
                    span1.make(name.clone()),
                    span2.make(name.clone()),
                ))?;
            }
        }
        // update names
        global.under_map =
            global.under_map.clone().union(binders.values().map(|def| (*def, *under)).collect());
        global.map = global.map.clone().union(binders);
        Ok(())
    }
    /// register a term to its definition
    fn def(&mut self, term: TermId, def: DefId) {
        self.term_to_def.insert(term, def);
    }
    fn alloc_prim(
        span: &SpanArenaBitter, mc: &mut MultiCell<DefId>, def: DefId, name: &'static str,
    ) -> Result<DefId> {
        if mc.is_empty() {
            Ok(*mc.init(def))
        } else {
            let var = VarName(name.into());
            Err(ResolveError::DuplicatePrim(
                span.defs[def].clone().make(var.clone()),
                span.defs[*mc.get()].clone().make(var),
            ))
        }
    }
    fn alloc_vtype(&mut self, vtype: DefId) -> Result<DefId> {
        Self::alloc_prim(&self.spans, &mut self.prim_def.vtype, vtype, "VType")
    }
    fn alloc_ctype(&mut self, ctype: DefId) -> Result<DefId> {
        Self::alloc_prim(&self.spans, &mut self.prim_def.ctype, ctype, "CType")
    }
}

pub trait Resolve {
    type Out;
    type Lookup<'a>;
    fn resolve<'f>(&self, resolver: &mut Resolver, lookup: Self::Lookup<'f>) -> Result<Self::Out>;
}

impl Resolve for TopLevel {
    type Out = ();
    type Lookup<'a> = Global;
    fn resolve<'f>(
        &self, resolver: &mut Resolver, mut global: Self::Lookup<'f>,
    ) -> Result<Self::Out> {
        let TopLevel(decls) = self;
        // collect all top-level binders and ...
        // 1. check for duplicates
        // 2. update primitives to term_to_def
        for id in decls {
            let Modifiers { public: _, inner } = &resolver.arena.decls[*id];
            match inner {
                Declaration::Alias(decl) => {
                    let Alias { binder, bindee: _ } = decl;
                    resolver.check_duplicate_and_update_global(
                        id,
                        binder.binders(&resolver.arena),
                        &mut global,
                    )?;
                }
                Declaration::Extern(decl) => {
                    let Extern { comp: _, binder, params: _, ty: _ } = decl;
                    let binders = binder.binders(&resolver.arena);
                    // check if it's a primitive and probably update the term_to_def
                    if binders.len() == 1 {
                        if let Some(def) = binders.get(&VarName("VType".into())) {
                            resolver.alloc_vtype(*def)?;
                        }
                        if let Some(def) = binders.get(&VarName("CType".into())) {
                            resolver.alloc_ctype(*def)?;
                        }
                    }
                    resolver.check_duplicate_and_update_global(id, binders, &mut global)?;
                }
                Declaration::Main(_) => {}
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
        resolver.prim_def.check()?;
        Ok(())
    }
}

impl PrimDef {
    pub fn check(&self) -> Result<()> {
        if self.vtype.is_empty() {
            return Err(ResolveError::MissingPrim("VType"));
        }
        if self.ctype.is_empty() {
            return Err(ResolveError::MissingPrim("CType"));
        }
        Ok(())
    }
}

impl Resolve for DeclId {
    type Out = ();
    type Lookup<'a> = &'a Global;
    fn resolve<'f>(&self, resolver: &mut Resolver, global: Self::Lookup<'f>) -> Result<Self::Out> {
        // register the global binder in deps
        resolver.deps.add(*self, []);
        let decl = resolver.arena.decls[*self].clone();
        let local = Local { under: *self, ..Local::default() };
        let Modifiers { public: _, inner } = decl;
        match inner {
            Declaration::Alias(decl) => {
                let Alias { binder, bindee } = decl;
                // resolve bindee first
                let () = bindee.resolve(resolver, (local.clone(), global))?;
                // and then binder, though we don't need the context yielded by binder
                // since it's global and has been collected already
                let _ = binder.resolve(resolver, (local.clone(), global))?;
                Ok(())
            }
            Declaration::Extern(decl) => {
                let Extern { comp: _, binder, params, ty } = decl;
                // no more bindee, but we still need to resolve the binders just for the type mentioned
                if let Some(ty) = ty {
                    let () = ty.resolve(resolver, (local.clone(), global))?;
                }
                if let Some(params) = params {
                    let _ = params.resolve(resolver, (local.clone(), global))?;
                }
                let _ = binder.resolve(resolver, (local.clone(), global))?;
                Ok(())
            }
            Declaration::Main(decl) => {
                let Main(term) = decl;
                let () = term.resolve(resolver, (local.clone(), global))?;
                Ok(())
            }
        }
    }
}

impl Resolve for PatId {
    // Note: returns the context yielded **after** the pattern
    type Out = Local;
    type Lookup<'a> = (Local, &'a Global);
    fn resolve<'f>(
        &self, resolver: &mut Resolver, (mut local, global): Self::Lookup<'f>,
    ) -> Result<Self::Out> {
        let pat = resolver.arena.pats[*self].clone();
        match pat {
            Pattern::Ann(pat) => {
                let Ann { tm, ty } = pat;
                let () = ty.resolve(resolver, (local.clone(), global))?;
                tm.resolve(resolver, (local, global))
            }
            Pattern::Hole(pat) => {
                let Hole = pat;
                Ok(local)
            }
            Pattern::Var(def) => {
                local.map.insert(resolver.arena.defs[def].clone(), def);
                Ok(local)
            }
            Pattern::Ctor(pat) => {
                let Ctor(_ctor, args) = pat;
                args.resolve(resolver, (local, global))
            }
            Pattern::Paren(pat) => {
                let Paren(inner) = pat;
                for binder in inner {
                    // can be dependent on the previous binders
                    local = binder.resolve(resolver, (local, global))?;
                }
                Ok(local)
            }
        }
    }
}
impl Resolve for CoPatId {
    type Out = Local;
    type Lookup<'a> = (Local, &'a Global);
    fn resolve<'f>(
        &self, resolver: &mut Resolver, (mut local, global): Self::Lookup<'f>,
    ) -> Result<Self::Out> {
        let copat = resolver.arena.copats[*self].clone();
        match copat {
            CoPattern::Pat(pat) => pat.resolve(resolver, (local, global)),
            CoPattern::Dtor(_dtor) => Ok(local),
            CoPattern::App(copat) => {
                let App(args) = copat;
                for arg in args {
                    // can be dependent on the previous binders
                    local = arg.resolve(resolver, (local, global))?;
                }
                Ok(local)
            }
        }
    }
}
impl Resolve for TermId {
    type Out = ();
    type Lookup<'a> = (Local, &'a Global);
    fn resolve<'f>(
        &self, resolver: &mut Resolver, (mut local, global): Self::Lookup<'f>,
    ) -> Result<Self::Out> {
        let term = resolver.arena.terms[*self].clone();
        match term {
            Term::Internal(_) => {
                // internal terms will be resolved by looking up primitives
            }
            Term::Sealed(term) => {
                let Sealed(inner) = term;
                let () = inner.resolve(resolver, (local, global))?;
            }
            Term::Ann(term) => {
                let Ann { tm, ty } = term;
                let () = ty.resolve(resolver, (local.clone(), global))?;
                let () = tm.resolve(resolver, (local, global))?;
            }
            Term::Hole(term) => {
                let Hole = term;
            }
            Term::Var(var) => {
                // first, try to find the variable locally
                if let Some(def) = local.map.get(var.leaf()) {
                    // if found, we're done
                    resolver.def(*self, *def);
                    return Ok(());
                }
                // otherwise, try to find the variable globally
                if let Some(def) = global.map.get(var.leaf()) {
                    // if found, also add dependency
                    resolver.def(*self, *def);
                    resolver.deps.add(local.under, [global.under_map[def]]);
                    return Ok(());
                }
                // if not found, report an error
                let span = &resolver.spans.terms[*self];
                Err(ResolveError::UnboundVar(span.make(var.clone())))?;
            }
            Term::Paren(term) => {
                let Paren(terms) = term;
                for term in terms {
                    let () = term.resolve(resolver, (local.clone(), global))?;
                }
            }
            Term::Abs(term) => {
                let Abs(copat, body) = term;
                local = copat.resolve(resolver, (local.clone(), global))?;
                let () = body.resolve(resolver, (local, global))?;
            }
            Term::App(term) => {
                let App(terms) = term;
                for term in terms {
                    let () = term.resolve(resolver, (local.clone(), global))?;
                }
            }
            Term::Rec(term) => {
                let Rec(pat, body) = term;
                local = pat.resolve(resolver, (local.clone(), global))?;
                let () = body.resolve(resolver, (local, global))?;
            }
            Term::Pi(term) => {
                let Pi(copat, body) = term;
                local = copat.resolve(resolver, (local.clone(), global))?;
                let () = body.resolve(resolver, (local, global))?;
            }
            Term::Sigma(term) => {
                let Sigma(copat, body) = term;
                local = copat.resolve(resolver, (local.clone(), global))?;
                let () = body.resolve(resolver, (local, global))?;
            }
            Term::Thunk(term) => {
                let Thunk(term) = term;
                let () = term.resolve(resolver, (local.clone(), global))?;
            }
            Term::Force(term) => {
                let Force(term) = term;
                let () = term.resolve(resolver, (local.clone(), global))?;
            }
            Term::Ret(term) => {
                let Return(term) = term;
                let () = term.resolve(resolver, (local.clone(), global))?;
            }
            Term::Do(term) => {
                let Bind { binder, bindee, tail } = term;
                let () = bindee.resolve(resolver, (local.clone(), global))?;
                local = binder.resolve(resolver, (local.clone(), global))?;
                let () = tail.resolve(resolver, (local, global))?;
            }
            Term::Let(term) => {
                let PureBind { binder, bindee, tail } = term;
                let () = bindee.resolve(resolver, (local.clone(), global))?;
                local = binder.resolve(resolver, (local.clone(), global))?;
                let () = tail.resolve(resolver, (local, global))?;
            }
            Term::Data(term) => {
                let Data { arms } = term;
                for arm in arms {
                    let DataArm { name: _, param } = arm;
                    let () = param.resolve(resolver, (local.clone(), global))?;
                }
            }
            Term::CoData(term) => {
                let CoData { arms } = term;
                for arm in arms {
                    let mut local = local.clone();
                    let CoDataArm { name: _, params, out } = arm;
                    if let Some(params) = params {
                        local = params.resolve(resolver, (local.clone(), global))?;
                    }
                    let () = out.resolve(resolver, (local.clone(), global))?;
                }
            }
            Term::Ctor(term) => {
                let Ctor(_ctor, term) = term;
                let () = term.resolve(resolver, (local.clone(), global))?;
            }
            Term::Match(term) => {
                let Match { scrut, arms } = term;
                let () = scrut.resolve(resolver, (local.clone(), global))?;
                for arm in arms {
                    let mut local = local.clone();
                    let Matcher { binder, tail } = arm;
                    local = binder.resolve(resolver, (local.clone(), global))?;
                    let () = tail.resolve(resolver, (local.clone(), global))?;
                }
            }
            Term::CoMatch(term) => {
                let CoMatch { arms } = term;
                for arm in arms {
                    let mut local = local.clone();
                    let CoMatcher { params, tail } = arm;
                    local = params.resolve(resolver, (local.clone(), global))?;
                    let () = tail.resolve(resolver, (local.clone(), global))?;
                }
            }
            Term::Dtor(term) => {
                let Dtor(term, _dtor) = term;
                let () = term.resolve(resolver, (local.clone(), global))?;
            }
            Term::Lit(_) => {}
        }
        Ok(())
    }
}
