use super::syntax::*;
use crate::textual::syntax as t;
use zydeco_derive::{AsMutSelf, AsRefSelf};
use zydeco_utils::prelude::{DepGraph, SccGraph};

pub use crate::arena::*;

/* ---------------------------------- Arena --------------------------------- */

/// Item projectors out of the scoped arena.
#[auto_impl::auto_impl(&, &mut, Box, Rc, Arc)]
pub trait ArenaScoped {
    fn def(&self, id: &DefId) -> VarName;
    fn pat(&self, id: &PatId) -> Pattern;
    fn term(&self, id: &TermId) -> Term<DefId>;
    fn decl(&self, id: &DeclId) -> Declaration;
}

/// Resolved arena plus name-resolution metadata and dependency/context analysis.
// Clone is derived only for coping with wf in driver
#[derive(Debug, Clone, AsRefSelf, AsMutSelf)]
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
    /// variables available upon the term
    pub ctxs_term: ArenaAssoc<TermId, Context>,
    /// variables that are introduced by the pattern
    pub ctxs_pat_local: ArenaAssoc<PatId, Context>,
    /// variables that are free within the pattern (e.g. unbound type variable in annotations)
    pub coctxs_pat_local: ArenaAssoc<PatId, CoContext>,
    /// variables that are free within the term
    pub coctxs_term_local: ArenaAssoc<TermId, CoContext>,
    // meta annotations to declarations
    pub metas: ArenaAssoc<DeclId, im::Vector<Meta>>,
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

/* -------------------------------- LocalFold ------------------------------- */

/// A set of local actions on scoped arena items.
#[auto_impl::auto_impl(&mut, Box)]
pub trait LocalFoldScoped<Cx>: ArenaScoped {
    fn action_def(&mut self, def: DefId, ctx: &Cx);
    fn action_pat(&mut self, pat: PatId, ctx: &Cx);
    fn action_term(&mut self, term: TermId, ctx: &Cx);
    fn action_decl(&mut self, decl: DeclId, ctx: &Cx);
}

impl LocalFoldScoped<Context> for Collector {
    fn action_def(&mut self, _def: DefId, _ctx: &Context) {}

    /// Updates [`Self::ctxs_pat_local`] and [`Self::coctxs_pat_local`].
    fn action_pat(&mut self, pat: PatId, _ctx: &Context) {
        let item = self.pat(&pat);
        match item {
            | Pattern::Ann(inner) => {
                let Ann { tm, ty } = inner;
                self.ctxs_pat_local.insert(pat, self.ctxs_pat_local[&tm].to_owned());
                self.coctxs_pat_local.insert(pat, {
                    let co_tm = self.coctxs_pat_local[&tm].to_owned();
                    let co_ty = self.coctxs_term_local[&ty].to_owned();
                    co_tm + co_ty
                });
            }
            | Pattern::Hole(inner) => {
                let Hole = inner;
                self.ctxs_pat_local.insert(pat, Context::new());
                self.coctxs_pat_local.insert(pat, CoContext::new());
            }
            | Pattern::Var(inner) => {
                let def = inner;
                self.ctxs_pat_local.insert(pat, Context::singleton(def));
                self.coctxs_pat_local.insert(pat, CoContext::new());
            }
            | Pattern::Ctor(inner) => {
                let Ctor(_ctorv, body) = inner;
                self.ctxs_pat_local.insert(pat, self.ctxs_pat_local[&body].to_owned());
                self.coctxs_pat_local.insert(pat, self.coctxs_pat_local[&body].to_owned());
            }
            | Pattern::Triv(inner) => {
                let Triv = inner;
                self.ctxs_pat_local.insert(pat, Context::new());
                self.coctxs_pat_local.insert(pat, CoContext::new());
            }
            | Pattern::Cons(inner) => {
                let Cons(a, b) = inner;
                self.ctxs_pat_local.insert(pat, {
                    let ctx_a = self.ctxs_pat_local[&a].to_owned();
                    let ctx_b = self.ctxs_pat_local[&b].to_owned();
                    ctx_a + ctx_b
                });
                self.coctxs_pat_local.insert(pat, {
                    let co_a = self.coctxs_pat_local[&a].to_owned();
                    let co_b = self.coctxs_pat_local[&b].to_owned();
                    co_a + co_b
                });
            }
        }
    }

    /// Updates [`Self::ctxs_term`] and [`Self::coctxs_term_local`].
    fn action_term(&mut self, term: TermId, ctx: &Context) {
        self.ctxs_term.insert(term, ctx.to_owned());
        let item = self.term(&term);
        match item {
            | Term::Meta(inner) => {
                let MetaT(_meta, inner) = inner;
                let co_term = self.coctxs_term_local[&inner].to_owned();
                self.coctxs_term_local.insert(term, co_term);
            }
            | Term::Internal(_) => {
                unreachable!()
            }
            | Term::Sealed(inner) => {
                let Sealed(inner) = inner;
                let co_inner = self.coctxs_term_local[&inner].to_owned();
                self.coctxs_term_local.insert(term, co_inner);
            }
            | Term::Ann(inner) => {
                let Ann { tm, ty } = inner;
                self.coctxs_term_local.insert(term, {
                    let co_tm = self.coctxs_term_local[&tm].to_owned();
                    let co_ty = self.coctxs_term_local[&ty].to_owned();
                    co_tm + co_ty
                });
            }
            | Term::Hole(inner) => {
                let Hole = inner;
                self.coctxs_term_local.insert(term, CoContext::new());
            }
            | Term::Var(inner) => {
                let def = inner;
                self.coctxs_term_local.insert(term, CoContext::singleton(def));
            }
            | Term::Triv(inner) => {
                let Triv = inner;
                self.coctxs_term_local.insert(term, CoContext::new());
            }
            | Term::Cons(inner) => {
                let Cons(a, b) = inner;
                self.coctxs_term_local.insert(term, {
                    let co_a = self.coctxs_term_local[&a].to_owned();
                    let co_b = self.coctxs_term_local[&b].to_owned();
                    co_a + co_b
                });
            }
            | Term::Abs(inner) => {
                let Abs(pat, body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                let cx_pat = self.ctxs_pat_local[&pat].to_owned();
                let co_pat = self.coctxs_pat_local[&pat].to_owned();
                self.coctxs_term_local.insert(term, co_body - cx_pat + co_pat);
            }
            | Term::App(inner) => {
                let App(a, b) = inner;
                let co_a = self.coctxs_term_local[&a].to_owned();
                let co_b = self.coctxs_term_local[&b].to_owned();
                self.coctxs_term_local.insert(term, co_a + co_b);
            }
            | Term::Fix(inner) => {
                let Fix(pat, body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                let cx_pat = self.ctxs_pat_local[&pat].to_owned();
                let co_pat = self.coctxs_pat_local[&pat].to_owned();
                self.coctxs_term_local.insert(term, co_body - cx_pat + co_pat);
            }
            | Term::Pi(inner) => {
                let Pi(pat, body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                let cx_pat = self.ctxs_pat_local[&pat].to_owned();
                let co_pat = self.coctxs_pat_local[&pat].to_owned();
                self.coctxs_term_local.insert(term, co_body - cx_pat + co_pat);
            }
            | Term::Sigma(inner) => {
                let Sigma(pat, body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                let cx_pat = self.ctxs_pat_local[&pat].to_owned();
                let co_pat = self.coctxs_pat_local[&pat].to_owned();
                self.coctxs_term_local.insert(term, co_body - cx_pat + co_pat);
            }
            | Term::Thunk(inner) => {
                let Thunk(body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                self.coctxs_term_local.insert(term, co_body);
            }
            | Term::Force(inner) => {
                let Force(body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                self.coctxs_term_local.insert(term, co_body);
            }
            | Term::Ret(inner) => {
                let Return(body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                self.coctxs_term_local.insert(term, co_body);
            }
            | Term::Do(inner) => {
                let Bind { binder, bindee, tail } = inner;
                let co_tail = self.coctxs_term_local[&tail].to_owned();
                let cx_binder = self.ctxs_pat_local[&binder].to_owned();
                let co_binder = self.coctxs_pat_local[&binder].to_owned();
                let co_bindee = self.coctxs_term_local[&bindee].to_owned();
                self.coctxs_term_local.insert(term, co_tail - cx_binder + co_binder + co_bindee);
            }
            | Term::Let(inner) => {
                let Let { binder, bindee, tail } = inner;
                let co_tail = self.coctxs_term_local[&tail].to_owned();
                let cx_binder = self.ctxs_pat_local[&binder].to_owned();
                let co_binder = self.coctxs_pat_local[&binder].to_owned();
                let co_bindee = self.coctxs_term_local[&bindee].to_owned();
                self.coctxs_term_local.insert(term, co_tail - cx_binder + co_binder + co_bindee);
            }
            | Term::MoBlock(inner) => {
                let MoBlock(body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                self.coctxs_term_local.insert(term, co_body);
            }
            | Term::Data(inner) => {
                let Data { arms } = inner;
                let co_arms = CoContext::from_iter(arms.into_iter().flat_map(
                    |DataArm { name: _, param }| self.coctxs_term_local[&param].to_owned(),
                ));
                self.coctxs_term_local.insert(term, co_arms);
            }
            | Term::CoData(inner) => {
                let CoData { arms } = inner;
                let co_arms = CoContext::from_iter(arms.into_iter().flat_map(
                    |CoDataArm { name: _, out }| self.coctxs_term_local[&out].to_owned(),
                ));
                self.coctxs_term_local.insert(term, co_arms);
            }
            | Term::Ctor(inner) => {
                let Ctor(_name, body) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                self.coctxs_term_local.insert(term, co_body);
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
                self.coctxs_term_local.insert(term, co_arms + co_scrut);
            }
            | Term::CoMatch(inner) => {
                let CoMatch { arms } = inner;
                let co_arms = CoContext::from_iter(arms.into_iter().flat_map(
                    |CoMatcher { dtor: _, tail }| self.coctxs_term_local[&tail].to_owned(),
                ));
                self.coctxs_term_local.insert(term, co_arms);
            }
            | Term::Dtor(inner) => {
                let Dtor(body, _name) = inner;
                let co_body = self.coctxs_term_local[&body].to_owned();
                self.coctxs_term_local.insert(term, co_body);
            }
            | Term::Lit(inner) => {
                let _lit = inner;
                self.coctxs_term_local.insert(term, CoContext::new());
            }
        }
    }

    fn action_decl(&mut self, _decl: DeclId, _ctx: &Context) {}
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
                | Pattern::Ctor(inner) => {
                    let Ctor(_ctorv, body) = inner;
                    body.obverse_local_post(f, ctx);
                }
                | Pattern::Triv(inner) => {
                    let Triv = inner;
                }
                | Pattern::Cons(inner) => {
                    let Cons(a, b) = inner;
                    a.obverse_local_post(f, ctx);
                    b.obverse_local_post(f, ctx);
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
                | Term::Internal(_) => unreachable!(),
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
                | Term::Triv(inner) => {
                    let Triv = inner;
                }
                | Term::Cons(inner) => {
                    let Cons(a, b) = inner;
                    a.obverse_local_post(f, ctx);
                    b.obverse_local_post(f, ctx);
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
                | Term::MoBlock(inner) => {
                    let MoBlock(body) = inner;
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
                | Term::Lit(inner) => {
                    let _lit = inner;
                }
            }
            f.action_term(self, ctx)
        }
    }
}
