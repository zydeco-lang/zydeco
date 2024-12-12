use super::syntax::*;
use crate::textual::syntax as t;
use zydeco_utils::{deps::DepGraph, scc::SccGraph};

/// Item projectors out of the scoped arena.
#[auto_impl::auto_impl(&, &mut, Box, Rc, Arc)]
pub trait ArenaScoped {
    fn def(&self, id: &DefId) -> VarName;
    fn pat(&self, id: &PatId) -> Pattern;
    fn term(&self, id: &TermId) -> Term<DefId>;
    fn decl(&self, id: &DeclId) -> Declaration;
}

/// A set of actions on scoped arena items.
#[auto_impl::auto_impl(&mut, Box)]
pub trait AccumFoldScoped<C, R>: ArenaScoped {
    fn action_def(&mut self, item: VarName, ctx: &C, res: R) -> R;
    fn action_pat(&mut self, item: Pattern, ctx: &C, res: R) -> R;
    fn action_term(&mut self, item: Term<DefId>, ctx: &C, res: R) -> R;
    fn action_decl(&mut self, item: Declaration, ctx: &C, res: R) -> R;
}

/// A forward fold w/ context and accumulator. Reader + State.
pub trait ObverseAccumFold {
    fn obverse_accum<C, R, F>(self, f: F, ctx: &C, res: R) -> R
    where
        F: AccumFoldScoped<C, R> + Clone;
}

/// A backward fold w/ context and accumulator. Reader + State.
pub trait ReverseAccumFold {
    fn reverse_accum<C, R, F>(self, f: F, ctx: &C, res: R) -> R
    where
        F: AccumFoldScoped<C, R> + Clone;
}

/* ---------------------------------- Arena --------------------------------- */

#[derive(Debug)]
pub struct ScopedArena {
    // arenas
    pub defs: ArenaSparse<DefId, VarName>,
    pub pats: ArenaSparse<PatId, Pattern>,
    pub terms: ArenaSparse<TermId, Term<DefId>>,
    pub decls: ArenaSparse<DeclId, Declaration>,
    /// entity maps from textural syntax
    pub textual: ArenaForth<t::EntityId, EntityId>,

    /// def user map
    pub users: ArenaForth<DefId, TermId>,
    /// contexts upon terms
    pub ctxs: ArenaAssoc<TermId, Context<()>>,
    /// co-contexts upon terms
    pub coctxs: ArenaAssoc<TermId, CoContext<()>>,
    /// externs to defs
    pub exts: ArenaAssoc<DeclId, (Internal, DefId)>,
    /// non-(optionally-mutual-)recursive declarations
    pub unis: ArenaAssoc<DeclId, ()>,
    /// dependency graph of the top level declarations
    pub deps: DepGraph<DeclId>,
    /// scc graph of the top level declarations
    pub top: SccGraph<DeclId>,
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
    fn decl(&self, id: &DeclId) -> Declaration {
        self.decls[id].to_owned()
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
    fn decl(&self, id: &DeclId) -> Declaration {
        self.decls[id].to_owned()
    }
}

mod impl_obverse_accum {
    use super::*;

    impl ObverseAccumFold for DefId {
        fn obverse_accum<C, R, F>(self, mut f: F, ctx: &C, res: R) -> R
        where
            F: AccumFoldScoped<C, R>,
        {
            let item = f.def(&self);
            f.action_def(item, ctx, res)
        }
    }

    impl ObverseAccumFold for PatId {
        fn obverse_accum<C, R, F>(self, mut f: F, ctx: &C, mut res: R) -> R
        where
            F: AccumFoldScoped<C, R> + Clone,
        {
            let item = f.pat(&self);
            match item.to_owned() {
                | Pattern::Ann(pat) => {
                    let Ann { tm, ty } = pat;
                    res = ty.obverse_accum(f.to_owned(), ctx, res);
                    res = tm.obverse_accum(f.to_owned(), ctx, res);
                }
                | Pattern::Hole(pat) => {
                    let Hole = pat;
                }
                | Pattern::Var(pat) => {
                    let def = pat;
                    res = def.obverse_accum(f.to_owned(), ctx, res);
                }
                | Pattern::Ctor(pat) => {
                    let Ctor(_ctorv, body) = pat;
                    res = body.obverse_accum(f.to_owned(), ctx, res);
                }
                | Pattern::Triv(pat) => {
                    let Triv = pat;
                }
                | Pattern::Cons(pat) => {
                    let Cons(a, b) = pat;
                    res = a.obverse_accum(f.to_owned(), ctx, res);
                    res = b.obverse_accum(f.to_owned(), ctx, res);
                }
            };
            f.action_pat(item, ctx, res)
        }
    }

    impl ObverseAccumFold for TermId {
        fn obverse_accum<C, R, F>(self, mut f: F, ctx: &C, mut res: R) -> R
        where
            F: AccumFoldScoped<C, R> + Clone,
        {
            let item = f.term(&self);
            match item.to_owned() {
                | Term::Internal(_) => unreachable!(),
                | Term::Sealed(term) => {
                    let Sealed(inner) = term;
                    res = inner.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Ann(term) => {
                    let Ann { tm, ty } = term;
                    res = tm.obverse_accum(f.to_owned(), ctx, res);
                    res = ty.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Hole(term) => {
                    let Hole = term;
                }
                | Term::Var(term) => {
                    let def = term;
                    res = def.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Triv(term) => {
                    let Triv = term;
                }
                | Term::Cons(term) => {
                    let Cons(a, b) = term;
                    res = a.obverse_accum(f.to_owned(), ctx, res);
                    res = b.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Abs(term) => {
                    let Abs(pat, body) = term;
                    res = pat.obverse_accum(f.to_owned(), ctx, res);
                    res = body.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::App(term) => {
                    let App(a, b) = term;
                    res = a.obverse_accum(f.to_owned(), ctx, res);
                    res = b.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Fix(term) => {
                    let Fix(pat, body) = term;
                    res = pat.obverse_accum(f.to_owned(), ctx, res);
                    res = body.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Pi(term) => {
                    let Pi(pat, body) = term;
                    res = pat.obverse_accum(f.to_owned(), ctx, res);
                    res = body.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Sigma(term) => {
                    let Sigma(pat, body) = term;
                    res = pat.obverse_accum(f.to_owned(), ctx, res);
                    res = body.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Thunk(term) => {
                    let Thunk(body) = term;
                    res = body.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Force(term) => {
                    let Force(body) = term;
                    res = body.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Ret(term) => {
                    let Ret(body) = term;
                    res = body.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Do(term) => {
                    let Bind { binder, bindee, tail } = term;
                    res = bindee.obverse_accum(f.to_owned(), ctx, res);
                    res = binder.obverse_accum(f.to_owned(), ctx, res);
                    res = tail.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Let(term) => {
                    let PureBind { binder, bindee, tail } = term;
                    res = bindee.obverse_accum(f.to_owned(), ctx, res);
                    res = binder.obverse_accum(f.to_owned(), ctx, res);
                    res = tail.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::MoBlock(term) => {
                    let MoBlock(body) = term;
                    res = body.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Data(term) => {
                    let Data { arms } = term;
                    for DataArm { name: _, param } in arms {
                        res = param.obverse_accum(f.to_owned(), ctx, res);
                    }
                }
                | Term::CoData(term) => {
                    let CoData { arms } = term;
                    for CoDataArm { name: _, out } in arms {
                        res = out.obverse_accum(f.to_owned(), ctx, res);
                    }
                }
                | Term::Ctor(term) => {
                    let Ctor(_name, body) = term;
                    res = body.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Match(term) => {
                    let Match { scrut, arms } = term;
                    res = scrut.obverse_accum(f.to_owned(), ctx, res);
                    for Matcher { binder, tail } in arms {
                        res = binder.obverse_accum(f.to_owned(), ctx, res);
                        res = tail.obverse_accum(f.to_owned(), ctx, res);
                    }
                }
                | Term::CoMatch(term) => {
                    let CoMatch { arms } = term;
                    for CoMatcher { dtor: _, tail } in arms {
                        res = tail.obverse_accum(f.to_owned(), ctx, res);
                    }
                }
                | Term::Dtor(term) => {
                    let Dtor(body, _name) = term;
                    res = body.obverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Lit(term) => {
                    let _lit = term;
                }
            };
            f.action_term(item, ctx, res)
        }
    }
}
mod impl_reverse_accum {
    use super::*;

    impl ReverseAccumFold for DefId {
        fn reverse_accum<C, R, F>(self, mut f: F, ctx: &C, res: R) -> R
        where
            F: AccumFoldScoped<C, R>,
        {
            let item = f.def(&self);
            f.action_def(item, ctx, res)
        }
    }

    impl ReverseAccumFold for PatId {
        fn reverse_accum<C, R, F>(self, mut f: F, ctx: &C, mut res: R) -> R
        where
            F: AccumFoldScoped<C, R> + Clone,
        {
            let item = f.pat(&self);
            match item.to_owned() {
                | Pattern::Ann(pat) => {
                    let Ann { tm, ty } = pat;
                    res = ty.reverse_accum(f.to_owned(), ctx, res);
                    res = tm.reverse_accum(f.to_owned(), ctx, res);
                }
                | Pattern::Hole(pat) => {
                    let Hole = pat;
                }
                | Pattern::Var(pat) => {
                    let def = pat;
                    res = def.reverse_accum(f.to_owned(), ctx, res);
                }
                | Pattern::Ctor(pat) => {
                    let Ctor(_ctorv, body) = pat;
                    res = body.reverse_accum(f.to_owned(), ctx, res);
                }
                | Pattern::Triv(pat) => {
                    let Triv = pat;
                }
                | Pattern::Cons(pat) => {
                    let Cons(a, b) = pat;
                    res = a.reverse_accum(f.to_owned(), ctx, res);
                    res = b.reverse_accum(f.to_owned(), ctx, res);
                }
            };
            f.action_pat(item, ctx, res)
        }
    }

    impl ReverseAccumFold for TermId {
        fn reverse_accum<C, R, F>(self, mut f: F, ctx: &C, mut res: R) -> R
        where
            F: AccumFoldScoped<C, R> + Clone,
        {
            let item = f.term(&self);
            match item.to_owned() {
                | Term::Internal(_) => unreachable!(),
                | Term::Sealed(term) => {
                    let Sealed(inner) = term;
                    res = inner.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Ann(term) => {
                    let Ann { tm, ty } = term;
                    res = tm.reverse_accum(f.to_owned(), ctx, res);
                    res = ty.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Hole(term) => {
                    let Hole = term;
                }
                | Term::Var(term) => {
                    let def = term;
                    res = def.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Triv(term) => {
                    let Triv = term;
                }
                | Term::Cons(term) => {
                    let Cons(a, b) = term;
                    res = a.reverse_accum(f.to_owned(), ctx, res);
                    res = b.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Abs(term) => {
                    let Abs(pat, body) = term;
                    res = body.reverse_accum(f.to_owned(), ctx, res);
                    res = pat.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::App(term) => {
                    let App(a, b) = term;
                    res = a.reverse_accum(f.to_owned(), ctx, res);
                    res = b.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Fix(term) => {
                    let Fix(pat, body) = term;
                    res = body.reverse_accum(f.to_owned(), ctx, res);
                    res = pat.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Pi(term) => {
                    let Pi(pat, body) = term;
                    res = body.reverse_accum(f.to_owned(), ctx, res);
                    res = pat.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Sigma(term) => {
                    let Sigma(pat, body) = term;
                    res = body.reverse_accum(f.to_owned(), ctx, res);
                    res = pat.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Thunk(term) => {
                    let Thunk(body) = term;
                    res = body.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Force(term) => {
                    let Force(body) = term;
                    res = body.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Ret(term) => {
                    let Ret(body) = term;
                    res = body.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Do(term) => {
                    let Bind { binder, bindee, tail } = term;
                    res = tail.reverse_accum(f.to_owned(), ctx, res);
                    res = binder.reverse_accum(f.to_owned(), ctx, res);
                    res = bindee.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Let(term) => {
                    let PureBind { binder, bindee, tail } = term;
                    res = tail.reverse_accum(f.to_owned(), ctx, res);
                    res = binder.reverse_accum(f.to_owned(), ctx, res);
                    res = bindee.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::MoBlock(term) => {
                    let MoBlock(body) = term;
                    res = body.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Data(term) => {
                    let Data { arms } = term;
                    for DataArm { name: _, param } in arms {
                        res = param.reverse_accum(f.to_owned(), ctx, res);
                    }
                }
                | Term::CoData(term) => {
                    let CoData { arms } = term;
                    for CoDataArm { name: _, out } in arms {
                        res = out.reverse_accum(f.to_owned(), ctx, res);
                    }
                }
                | Term::Ctor(term) => {
                    let Ctor(_name, body) = term;
                    res = body.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Match(term) => {
                    let Match { scrut, arms } = term;
                    for Matcher { binder, tail } in arms {
                        res = tail.reverse_accum(f.to_owned(), ctx, res);
                        res = binder.reverse_accum(f.to_owned(), ctx, res);
                    }
                    res = scrut.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::CoMatch(term) => {
                    let CoMatch { arms } = term;
                    for CoMatcher { dtor: _, tail } in arms {
                        res = tail.reverse_accum(f.to_owned(), ctx, res);
                    }
                }
                | Term::Dtor(term) => {
                    let Dtor(body, _name) = term;
                    res = body.reverse_accum(f.to_owned(), ctx, res);
                }
                | Term::Lit(term) => {
                    let _lit = term;
                }
            };
            f.action_term(item, ctx, res)
        }
    }
}
