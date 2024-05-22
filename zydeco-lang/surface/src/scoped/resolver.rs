//! Name resolution for the surface language.
//! All Ids will remain the same.
//! TermId -> Term<NameRef<VarName>> will be resolved to Term<DefId>.
//! All the other Ids remain the same.
//!
//! During resolution, patterns will register the binders to the context;
//! and terms will lookup the context and resolve accordingly.

use super::err::*;
use super::syntax as r;
use crate::bitter::syntax as b;
use zydeco_utils::arena::ArenaAssoc;

pub struct Resolver {
    pub spans: b::SpanArenaBitter,
    pub terms_old: ArenaAssoc<b::TermId, b::Term<b::NameRef<b::VarName>>>,
    pub arena: r::Arena,
    pub local: im::HashMap<b::VarName, r::DefId>,
    pub searching: Vec<b::NameRef<()>>,
}

impl Resolver {
    pub fn new(
        spans: b::SpanArenaBitter,
        terms_old: ArenaAssoc<b::TermId, b::Term<b::NameRef<b::VarName>>>, arena: r::Arena,
    ) -> Resolver {
        Resolver { spans, terms_old, arena, local: im::HashMap::new(), searching: vec![] }
    }
}

impl Resolver {
    pub fn save(&self) -> im::HashMap<b::VarName, r::DefId> {
        self.local.clone()
    }
    pub fn restore(&mut self, local: im::HashMap<b::VarName, r::DefId>) {
        self.local = local;
    }
}

pub trait Resolve {
    type Out;
    fn resolve(self, resolver: &mut Resolver) -> Result<Self::Out>;
}

impl Resolve for b::TopLevel {
    type Out = r::TopLevel;
    fn resolve(self, resolver: &mut Resolver) -> Result<Self::Out> {
        let b::TopLevel(decls) = self;
        let mut decls_new = vec![];
        for b::Modifiers { public: _, inner } in decls {
            // no local as of now
            let local = resolver.save();
            use b::Declaration as Decl;
            let decl_new = match inner {
                Decl::Alias(d) => {
                    let b::Alias { binder, bindee } = d;
                    bindee.resolve(resolver)?;
                    // no need to resolve the binder
                    // because it has already been detected
                    r::Alias { binder, bindee }.into()
                }
                Decl::Extern(d) => {
                    let b::Extern { comp, binder, params, ty } = d;
                    r::Extern { comp, binder, params, ty }.into()
                }
                Decl::Main(d) => {
                    let b::Main(m) = d;
                    m.resolve(resolver)?;
                    r::Main(m).into()
                }
            };
            decls_new.push(decl_new);
            // restore empty local
            resolver.restore(local);
        }
        Ok(r::TopLevel(decls_new))
    }
}

impl Resolve for &b::PatId {
    type Out = ();
    fn resolve(self, resolver: &mut Resolver) -> Result<Self::Out> {
        let pat = resolver.arena.pats[*self].clone();
        match pat {
            b::Pattern::Ann(p) => {
                let b::Ann { tm, ty } = p;
                // type should be resolved first
                ty.resolve(resolver)?;
                // ..and then the term registers
                tm.resolve(resolver)?;
            }
            b::Pattern::Hole(p) => {
                let b::Hole = p;
            }
            b::Pattern::Var(p) => {
                let name = resolver.arena.defs[p].clone();
                resolver.local.insert(name, p);
            }
            b::Pattern::Ctor(p) => {
                let b::Ctor(_, arg) = p;
                arg.resolve(resolver)?;
            }
            b::Pattern::Paren(p) => {
                let b::Paren(pats) = p;
                for pat in pats {
                    pat.resolve(resolver)?;
                }
            }
        }
        Ok(())
    }
}

impl Resolve for &b::CoPatId {
    type Out = ();
    fn resolve(self, resolver: &mut Resolver) -> Result<Self::Out> {
        let copat = resolver.arena.copats[*self].clone();
        match copat {
            b::CoPattern::Pat(cp) => {
                cp.resolve(resolver)?;
            }
            b::CoPattern::Dtor(cp) => {
                let b::DtorName(_) = cp;
            }
            b::CoPattern::App(cp) => {
                let b::App(copats) = cp;
                for copat in copats {
                    copat.resolve(resolver)?;
                }
            }
        }
        Ok(())
    }
}

impl Resolve for &b::TermId {
    type Out = ();
    fn resolve(self, resolver: &mut Resolver) -> Result<Self::Out> {
        let term = resolver.terms_old[*self].clone();
        match term {
            r::Term::Sealed(t) => {
                let b::Sealed(t) = t;
                t.resolve(resolver)?;
            }
            r::Term::Ann(t) => {
                let b::Ann { tm, ty } = t;
                ty.resolve(resolver)?;
                tm.resolve(resolver)?;
            }
            r::Term::Hole(t) => {
                let b::Hole = t;
            }
            r::Term::Var(t) => {
                if let Some(name) = t.clone().syntactic_local() {
                    if let Some(def) = resolver.local.get(&name).cloned() {
                        resolver.arena.terms.insert(*self, r::Term::Var(def));
                        return Ok(());
                    }
                }
                let span = &resolver.spans.terms[*self];
                Err(ResolveError::UnboundVar(span.make(t)))?
            }
            r::Term::Paren(t) => {
                let b::Paren(ts) = t;
                for t in ts {
                    t.resolve(resolver)?;
                }
            }
            r::Term::Abs(t) => {
                let b::Abs(params, body) = t;
                params.resolve(resolver)?;
                body.resolve(resolver)?;
            }
            r::Term::App(t) => {
                let b::App(ts) = t;
                for t in ts {
                    t.resolve(resolver)?;
                }
            }
            r::Term::Rec(t) => {
                let b::Rec(pat, t) = t;
                pat.resolve(resolver)?;
                t.resolve(resolver)?;
            }
            r::Term::Pi(t) => {
                let b::Pi(params, body) = t;
                params.resolve(resolver)?;
                body.resolve(resolver)?;
            }
            r::Term::Sigma(t) => {
                let b::Sigma(params, body) = t;
                params.resolve(resolver)?;
                body.resolve(resolver)?;
            }
            r::Term::Thunk(t) => {
                let b::Thunk(t) = t;
                t.resolve(resolver)?;
            }
            r::Term::Force(t) => {
                let b::Force(t) = t;
                t.resolve(resolver)?;
            }
            r::Term::Ret(t) => {
                let b::Return(t) = t;
                t.resolve(resolver)?;
            }
            r::Term::Do(t) => {
                let b::Bind { binder, bindee, tail } = t;
                let local = resolver.save();
                bindee.resolve(resolver)?;
                // recover the context after exiting bindee
                resolver.restore(local);
                binder.resolve(resolver)?;
                tail.resolve(resolver)?;
            }
            r::Term::Let(t) => {
                let b::PureBind { binder, bindee, tail } = t;
                let local = resolver.save();
                bindee.resolve(resolver)?;
                // recover the context after exiting bindee
                resolver.restore(local);
                binder.resolve(resolver)?;
                tail.resolve(resolver)?;
            }
            r::Term::Data(t) => {
                let b::Data { arms } = t;
                for b::DataArm { name: _, param } in arms {
                    param.resolve(resolver)?;
                }
            }
            r::Term::CoData(t) => {
                let b::CoData { arms } = t;
                for b::CoDataArm { name: _, params, out } in arms {
                    if let Some(params) = params {
                        params.resolve(resolver)?;
                    }
                    out.resolve(resolver)?;
                }
            }
            r::Term::Ctor(t) => {
                let b::Ctor(_, t) = t;
                t.resolve(resolver)?;
            }
            r::Term::Match(t) => {
                let b::Match { scrut, arms } = t;
                scrut.resolve(resolver)?;
                for b::Matcher { binder, tail } in arms {
                    let local = resolver.save();
                    binder.resolve(resolver)?;
                    tail.resolve(resolver)?;
                    // recover the context after exiting branch
                    resolver.restore(local);
                }
            }
            r::Term::CoMatch(t) => {
                let b::CoMatch { arms } = t;
                for b::CoMatcher { params, tail } in arms {
                    let local = resolver.save();
                    params.resolve(resolver)?;
                    tail.resolve(resolver)?;
                    // recover the context after exiting branch
                    resolver.restore(local);
                }
            }
            r::Term::Dtor(t) => {
                let b::Dtor(t, _) = t;
                t.resolve(resolver)?;
            }
            r::Term::Lit(_t) => {}
        }
        Ok(())
    }
}
