use crate::scoped::{err::*, syntax::*};

#[derive(Clone, Debug, Default)]
pub struct Global {
    map: im::HashMap<VarName, DefId>,
    set: im::HashMap<DefId, ()>,
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
                let Paren(inner) = pat;
                let mut res = im::HashMap::new();
                for binder in inner {
                    res = res.union(binder.binders(arena));
                }
                res
            }
        }
    }
}

pub struct Resolver {
    pub scoped: ScopedArena,
    pub bitter: Arena,
    pub spans: SpanArenaBitter,
}

impl Resolver {
    pub fn run(&mut self, top: TopLevel) -> Result<()> {
        top.resolve(self, Global::default())
    }
    fn check_duplicate_and_update_global(
        &self, binders: im::HashMap<VarName, DefId>, global: &mut Global,
    ) -> Result<()> {
        let dup = binders.clone().intersection(global.map.clone());
        if !dup.is_empty() {
            // get first duplicate
            let (name, def1) = dup.into_iter().next().unwrap();
            let def2 = global.map[&name];
            // get spans
            let span1 = &self.spans.defs[def1];
            let span2 = &self.spans.defs[def2];
            Err(ResolveError::DuplicateDefinition(span1.make(name.clone()), span2.make(name)))?;
        }
        // update names
        global.set = global.set.clone().union(binders.values().map(|def| (*def, ())).collect());
        global.map = global.map.clone().union(binders);
        Ok(())
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
        // collect all top-level binders and check for duplicates
        for Modifiers { public: _, inner } in decls {
            match inner {
                Declaration::Alias(decl) => {
                    let Alias { binder, bindee: _ } = decl;
                    resolver.check_duplicate_and_update_global(
                        binder.binders(&resolver.bitter),
                        &mut global,
                    )?;
                }
                Declaration::Extern(decl) => {
                    let Extern { comp: _, binder, params: _, ty: _ } = decl;
                    resolver.check_duplicate_and_update_global(
                        binder.binders(&resolver.bitter),
                        &mut global,
                    )?;
                }
                Declaration::Main(decl) => {
                    let Main(_) = decl;
                }
            }
        }
        // within each term (when we also count types as terms),
        // we introduce local binders.
        // since we'll resolve variables in the order of
        // 1. local binders (introduced eagerly),
        // 2. global binders (introduced lazily).
        // therefore, we shall introduce all local binders,
        // but introduce global binders in local scopes only if needed.
        for Modifiers { public: _, inner } in decls {
            inner.resolve(resolver, &global)?;
        }
        Ok(())
    }
}

impl Resolve for Declaration {
    type Out = ();
    type Lookup<'a> = &'a Global;
    fn resolve<'f>(&self, resolver: &mut Resolver, global: Self::Lookup<'f>) -> Result<Self::Out> {
        match self {
            Declaration::Alias(decl) => {
                let Alias { binder, bindee } = decl;
                // resolve bindee first
                let () = bindee.resolve(resolver, (Context::new(), global))?;
                // and then binder, though we don't need the context yielded by binder
                // since it's global and has been collected already
                let _ = binder.resolve(resolver, (Context::new(), global))?;
                Ok(())
            }
            Declaration::Extern(decl) => {
                let Extern { comp: _, binder, params, ty } = decl;
                // no more bindee, but we still need to resolve the binders just for the type mentioned
                if let Some(ty) = ty {
                    let () = ty.resolve(resolver, (Context::new(), global))?;
                }
                if let Some(params) = params {
                    let _ = params.resolve(resolver, (Context::new(), global))?;
                }
                let _ = binder.resolve(resolver, (Context::new(), global))?;
                Ok(())
            }
            Declaration::Main(decl) => {
                let Main(term) = decl;
                let () = term.resolve(resolver, (Context::new(), global))?;
                Ok(())
            }
        }
    }
}

impl Resolve for PatId {
    type Out = Context<()>;
    type Lookup<'a> = (Context<()>, &'a Global);
    fn resolve<'f>(&self, resolver: &mut Resolver, lookup: Self::Lookup<'f>) -> Result<Self::Out> {
        todo!()
    }
}
impl Resolve for CoPatId {
    type Out = Context<()>;
    type Lookup<'a> = (Context<()>, &'a Global);
    fn resolve<'f>(&self, resolver: &mut Resolver, lookup: Self::Lookup<'f>) -> Result<Self::Out> {
        todo!()
    }
}
impl Resolve for TermId {
    type Out = ();
    type Lookup<'a> = (Context<()>, &'a Global);
    fn resolve<'f>(&self, resolver: &mut Resolver, lookup: Self::Lookup<'f>) -> Result<Self::Out> {
        todo!()
    }
}
