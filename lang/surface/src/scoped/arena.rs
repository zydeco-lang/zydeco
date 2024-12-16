pub use crate::arena::*;
use super::syntax::*;
use crate::textual::syntax as t;
use zydeco_utils::{deps::DepGraph, imc::ImmutableMonoidMap, monoid::Monoid, scc::SccGraph};

/* ---------------------------------- Arena --------------------------------- */

/// Item projectors out of the scoped arena.
#[auto_impl::auto_impl(&, &mut, Box, Rc, Arc)]
pub trait ArenaScoped {
    fn def(&self, id: &DefId) -> VarName;
    fn pat(&self, id: &PatId) -> Pattern;
    fn term(&self, id: &TermId) -> Term<DefId>;
    fn decl(&self, id: &DeclId) -> Declaration;
}

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
    /// variables available upon the term
    pub ctxs_term: ArenaAssoc<TermId, Context<()>>,
    /// variables that are introduced by the pattern
    pub ctxs_pat_local: ArenaAssoc<PatId, Context<()>>,
    /// variables that are free within the pattern
    pub coctxs_pat_local: ArenaAssoc<PatId, CoContext<()>>,
    /// variables that are free within the term
    pub coctxs_term_local: ArenaAssoc<TermId, CoContext<()>>,
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

impl LocalFoldScoped<Context<()>> for Collector {
    fn action_def(&mut self, _def: DefId, _ctx: &Context<()>) {}

    /// Updates [`Self::ctxs_pat_local`] and [`Self::coctxs_pat_local`].
    fn action_pat(&mut self, pat: PatId, _ctx: &Context<()>) {
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
                self.ctxs_pat_local.insert(pat, Context::singleton(def, ()));
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
    fn action_term(&mut self, term: TermId, ctx: &Context<()>) {
        self.ctxs_term.insert(term, ctx.to_owned());
        let item = self.term(&term);
        match item {
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
                self.coctxs_term_local.insert(term, CoContext::singleton(def, ()));
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
                let Ret(body) = inner;
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
                let PureBind { binder, bindee, tail } = inner;
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
                let co_arms =
                    CoContext::concat(arms.into_iter().map(|DataArm { name: _, param }| {
                        self.coctxs_term_local[&param].to_owned()
                    }));
                self.coctxs_term_local.insert(term, co_arms);
            }
            | Term::CoData(inner) => {
                let CoData { arms } = inner;
                let co_arms = CoContext::concat(
                    arms.into_iter()
                        .map(|CoDataArm { name: _, out }| self.coctxs_term_local[&out].to_owned()),
                );
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
                    CoContext::concat(arms.into_iter().map(|Matcher { binder, tail }| {
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
                let co_arms =
                    CoContext::concat(arms.into_iter().map(|CoMatcher { dtor: _, tail }| {
                        self.coctxs_term_local[&tail].to_owned()
                    }));
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

    fn action_decl(&mut self, _decl: DeclId, _ctx: &Context<()>) {
        todo!()
    }
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
                    let Ret(body) = inner;
                    body.obverse_local_post(f, ctx);
                }
                | Term::Do(inner) => {
                    let Bind { binder, bindee, tail } = inner;
                    bindee.obverse_local_post(f, ctx);
                    binder.obverse_local_post(f, ctx);
                    tail.obverse_local_post(f, ctx);
                }
                | Term::Let(inner) => {
                    let PureBind { binder, bindee, tail } = inner;
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

    // impl ObverseLocalPostFold for DeclId {
    //     fn obverse_local_post<C, F>(self, f: &mut F, ctx: &C)
    //     where
    //         F: LocalFoldScoped<C>,
    //     {
    //         let item = f.decl(&self);
    //         match item {
    //             | Declaration::AliasBody(decl) => {
    //                 let AliasBody { binder, bindee } = decl;
    //                 bindee.obverse_local_post(f, ctx);
    //                 binder.obverse_local_post(f, ctx);
    //             }
    //             | Declaration::AliasHead(decl) => {
    //                 let AliasHead { binder, ty } = decl;
    //                 ty.obverse_local_post(f, ctx);
    //                 binder.obverse_local_post(f, ctx);
    //             }
    //             | Declaration::Module(_) => unreachable!(),
    //             | Declaration::Exec(decl) => {
    //                 let Exec(body) = decl;
    //                 body.obverse_local_post(f, ctx);
    //             }
    //         }
    //         f.action_decl(self, ctx)
    //     }
    // }

    // impl ObverseLocalPostFold for SccDeclarations<'_> {
    //     fn obverse_local_post<C, F>(self, f: &mut F, ctx: &C)
    //     where
    //         F: LocalFoldScoped<C>,
    //     {
    //         todo!()
    //     }
    // }
}

// /* -------------------------------- AccumFold ------------------------------- */
// /// A set of actions on scoped arena items.
// #[auto_impl::auto_impl(&mut, Box)]
// pub trait AccumFoldScoped<C, R>: ArenaScoped {
//     fn action_def(&mut self, item: VarName, ctx: &C, res: R) -> R;
//     fn action_pat(&mut self, item: Pattern, ctx: &C, res: R) -> R;
//     fn action_term(&mut self, item: Term<DefId>, ctx: &C, res: R) -> R;
//     fn action_decl(&mut self, item: Declaration, ctx: &C, res: R) -> R;
// }

// /// A forward fold w/ context and accumulator. Reader + State.
// pub trait ObverseAccumFold {
//     fn obverse_accum<C, R, F>(self, f: &mut F, ctx: &C, res: R) -> R
//     where
//         F: AccumFoldScoped<C, R>;
// }

// /// A backward fold w/ context and accumulator. Reader + State.
// pub trait ReverseAccumFold {
//     fn reverse_accum<C, R, F>(self, f: &mut F, ctx: &C, res: R) -> R
//     where
//         F: AccumFoldScoped<C, R>;
// }

// mod impl_obverse_accum {
//     use super::*;

//     impl<T> ObverseAccumFold for Option<T>
//     where
//         T: ObverseAccumFold,
//     {
//         fn obverse_accum<C, R, F>(self, f: &mut F, ctx: &C, res: R) -> R
//         where
//             F: AccumFoldScoped<C, R>,
//         {
//             match self {
//                 | Some(item) => item.obverse_accum(f, ctx, res),
//                 | None => res,
//             }
//         }
//     }

//     impl ObverseAccumFold for DefId {
//         fn obverse_accum<C, R, F>(self, f: &mut F, ctx: &C, res: R) -> R
//         where
//             F: AccumFoldScoped<C, R>,
//         {
//             let item = f.def(&self);
//             f.action_def(item, ctx, res)
//         }
//     }

//     impl ObverseAccumFold for PatId {
//         fn obverse_accum<C, R, F>(self, f: &mut F, ctx: &C, mut res: R) -> R
//         where
//             F: AccumFoldScoped<C, R>,
//         {
//             let item = f.pat(&self);
//             match item.to_owned() {
//                 | Pattern::Ann(pat) => {
//                     let Ann { tm, ty } = pat;
//                     res = ty.obverse_accum(f, ctx, res);
//                     res = tm.obverse_accum(f, ctx, res);
//                 }
//                 | Pattern::Hole(pat) => {
//                     let Hole = pat;
//                 }
//                 | Pattern::Var(pat) => {
//                     let def = pat;
//                     res = def.obverse_accum(f, ctx, res);
//                 }
//                 | Pattern::Ctor(pat) => {
//                     let Ctor(_ctorv, body) = pat;
//                     res = body.obverse_accum(f, ctx, res);
//                 }
//                 | Pattern::Triv(pat) => {
//                     let Triv = pat;
//                 }
//                 | Pattern::Cons(pat) => {
//                     let Cons(a, b) = pat;
//                     res = a.obverse_accum(f, ctx, res);
//                     res = b.obverse_accum(f, ctx, res);
//                 }
//             };
//             f.action_pat(item, ctx, res)
//         }
//     }

//     impl ObverseAccumFold for TermId {
//         fn obverse_accum<C, R, F>(self, f: &mut F, ctx: &C, mut res: R) -> R
//         where
//             F: AccumFoldScoped<C, R>,
//         {
//             let item = f.term(&self);
//             match item.to_owned() {
//                 | Term::Internal(_) => unreachable!(),
//                 | Term::Sealed(term) => {
//                     let Sealed(inner) = term;
//                     res = inner.obverse_accum(f, ctx, res);
//                 }
//                 | Term::Ann(term) => {
//                     let Ann { tm, ty } = term;
//                     res = tm.obverse_accum(f, ctx, res);
//                     res = ty.obverse_accum(f, ctx, res);
//                 }
//                 | Term::Hole(term) => {
//                     let Hole = term;
//                 }
//                 | Term::Var(term) => {
//                     let def = term;
//                     res = def.obverse_accum(f, ctx, res);
//                 }
//                 | Term::Triv(term) => {
//                     let Triv = term;
//                 }
//                 | Term::Cons(term) => {
//                     let Cons(a, b) = term;
//                     res = a.obverse_accum(f, ctx, res);
//                     res = b.obverse_accum(f, ctx, res);
//                 }
//                 | Term::Abs(term) => {
//                     let Abs(pat, body) = term;
//                     res = pat.obverse_accum(f, ctx, res);
//                     res = body.obverse_accum(f, ctx, res);
//                 }
//                 | Term::App(term) => {
//                     let App(a, b) = term;
//                     res = a.obverse_accum(f, ctx, res);
//                     res = b.obverse_accum(f, ctx, res);
//                 }
//                 | Term::Fix(term) => {
//                     let Fix(pat, body) = term;
//                     res = pat.obverse_accum(f, ctx, res);
//                     res = body.obverse_accum(f, ctx, res);
//                 }
//                 | Term::Pi(term) => {
//                     let Pi(pat, body) = term;
//                     res = pat.obverse_accum(f, ctx, res);
//                     res = body.obverse_accum(f, ctx, res);
//                 }
//                 | Term::Sigma(term) => {
//                     let Sigma(pat, body) = term;
//                     res = pat.obverse_accum(f, ctx, res);
//                     res = body.obverse_accum(f, ctx, res);
//                 }
//                 | Term::Thunk(term) => {
//                     let Thunk(body) = term;
//                     res = body.obverse_accum(f, ctx, res);
//                 }
//                 | Term::Force(term) => {
//                     let Force(body) = term;
//                     res = body.obverse_accum(f, ctx, res);
//                 }
//                 | Term::Ret(term) => {
//                     let Ret(body) = term;
//                     res = body.obverse_accum(f, ctx, res);
//                 }
//                 | Term::Do(term) => {
//                     let Bind { binder, bindee, tail } = term;
//                     res = bindee.obverse_accum(f, ctx, res);
//                     res = binder.obverse_accum(f, ctx, res);
//                     res = tail.obverse_accum(f, ctx, res);
//                 }
//                 | Term::Let(term) => {
//                     let PureBind { binder, bindee, tail } = term;
//                     res = bindee.obverse_accum(f, ctx, res);
//                     res = binder.obverse_accum(f, ctx, res);
//                     res = tail.obverse_accum(f, ctx, res);
//                 }
//                 | Term::MoBlock(term) => {
//                     let MoBlock(body) = term;
//                     res = body.obverse_accum(f, ctx, res);
//                 }
//                 | Term::Data(term) => {
//                     let Data { arms } = term;
//                     for DataArm { name: _, param } in arms {
//                         res = param.obverse_accum(f, ctx, res);
//                     }
//                 }
//                 | Term::CoData(term) => {
//                     let CoData { arms } = term;
//                     for CoDataArm { name: _, out } in arms {
//                         res = out.obverse_accum(f, ctx, res);
//                     }
//                 }
//                 | Term::Ctor(term) => {
//                     let Ctor(_name, body) = term;
//                     res = body.obverse_accum(f, ctx, res);
//                 }
//                 | Term::Match(term) => {
//                     let Match { scrut, arms } = term;
//                     res = scrut.obverse_accum(f, ctx, res);
//                     for Matcher { binder, tail } in arms {
//                         res = binder.obverse_accum(f, ctx, res);
//                         res = tail.obverse_accum(f, ctx, res);
//                     }
//                 }
//                 | Term::CoMatch(term) => {
//                     let CoMatch { arms } = term;
//                     for CoMatcher { dtor: _, tail } in arms {
//                         res = tail.obverse_accum(f, ctx, res);
//                     }
//                 }
//                 | Term::Dtor(term) => {
//                     let Dtor(body, _name) = term;
//                     res = body.obverse_accum(f, ctx, res);
//                 }
//                 | Term::Lit(term) => {
//                     let _lit = term;
//                 }
//             };
//             f.action_term(item, ctx, res)
//         }
//     }

//     impl ObverseAccumFold for DeclId {
//         fn obverse_accum<C, R, F>(self, f: &mut F, ctx: &C, mut res: R) -> R
//         where
//             F: AccumFoldScoped<C, R>,
//         {
//             let item = f.decl(&self);
//             match item.to_owned() {
//                 | Declaration::AliasBody(decl) => {
//                     let AliasBody { binder, bindee } = decl;
//                     res = bindee.obverse_accum(f, ctx, res);
//                     res = binder.obverse_accum(f, ctx, res);
//                 }
//                 | Declaration::AliasHead(decl) => {
//                     let AliasHead { binder, ty } = decl;
//                     res = ty.obverse_accum(f, ctx, res);
//                     res = binder.obverse_accum(f, ctx, res);
//                 }
//                 | Declaration::Module(_) => unreachable!(),
//                 | Declaration::Exec(decl) => {
//                     let Exec(body) = decl;
//                     res = body.obverse_accum(f, ctx, res);
//                 }
//             }
//             f.action_decl(item, ctx, res)
//         }
//     }
// }

// mod impl_reverse_accum {
//     use super::*;

//     impl<T> ReverseAccumFold for Option<T>
//     where
//         T: ReverseAccumFold,
//     {
//         fn reverse_accum<C, R, F>(self, f: &mut F, ctx: &C, res: R) -> R
//         where
//             F: AccumFoldScoped<C, R>,
//         {
//             match self {
//                 | Some(item) => item.reverse_accum(f, ctx, res),
//                 | None => res,
//             }
//         }
//     }

//     impl ReverseAccumFold for DefId {
//         fn reverse_accum<C, R, F>(self, f: &mut F, ctx: &C, res: R) -> R
//         where
//             F: AccumFoldScoped<C, R>,
//         {
//             let item = f.def(&self);
//             f.action_def(item, ctx, res)
//         }
//     }

//     impl ReverseAccumFold for PatId {
//         fn reverse_accum<C, R, F>(self, f: &mut F, ctx: &C, mut res: R) -> R
//         where
//             F: AccumFoldScoped<C, R>,
//         {
//             let item = f.pat(&self);
//             match item.to_owned() {
//                 | Pattern::Ann(pat) => {
//                     let Ann { tm, ty } = pat;
//                     res = ty.reverse_accum(f, ctx, res);
//                     res = tm.reverse_accum(f, ctx, res);
//                 }
//                 | Pattern::Hole(pat) => {
//                     let Hole = pat;
//                 }
//                 | Pattern::Var(pat) => {
//                     let def = pat;
//                     res = def.reverse_accum(f, ctx, res);
//                 }
//                 | Pattern::Ctor(pat) => {
//                     let Ctor(_ctorv, body) = pat;
//                     res = body.reverse_accum(f, ctx, res);
//                 }
//                 | Pattern::Triv(pat) => {
//                     let Triv = pat;
//                 }
//                 | Pattern::Cons(pat) => {
//                     let Cons(a, b) = pat;
//                     res = a.reverse_accum(f, ctx, res);
//                     res = b.reverse_accum(f, ctx, res);
//                 }
//             };
//             f.action_pat(item, ctx, res)
//         }
//     }

//     impl ReverseAccumFold for TermId {
//         fn reverse_accum<C, R, F>(self, f: &mut F, ctx: &C, mut res: R) -> R
//         where
//             F: AccumFoldScoped<C, R>,
//         {
//             let item = f.term(&self);
//             match item.to_owned() {
//                 | Term::Internal(_) => unreachable!(),
//                 | Term::Sealed(term) => {
//                     let Sealed(inner) = term;
//                     res = inner.reverse_accum(f, ctx, res);
//                 }
//                 | Term::Ann(term) => {
//                     let Ann { tm, ty } = term;
//                     res = tm.reverse_accum(f, ctx, res);
//                     res = ty.reverse_accum(f, ctx, res);
//                 }
//                 | Term::Hole(term) => {
//                     let Hole = term;
//                 }
//                 | Term::Var(term) => {
//                     let def = term;
//                     res = def.reverse_accum(f, ctx, res);
//                 }
//                 | Term::Triv(term) => {
//                     let Triv = term;
//                 }
//                 | Term::Cons(term) => {
//                     let Cons(a, b) = term;
//                     res = a.reverse_accum(f, ctx, res);
//                     res = b.reverse_accum(f, ctx, res);
//                 }
//                 | Term::Abs(term) => {
//                     let Abs(pat, body) = term;
//                     res = body.reverse_accum(f, ctx, res);
//                     res = pat.reverse_accum(f, ctx, res);
//                 }
//                 | Term::App(term) => {
//                     let App(a, b) = term;
//                     res = a.reverse_accum(f, ctx, res);
//                     res = b.reverse_accum(f, ctx, res);
//                 }
//                 | Term::Fix(term) => {
//                     let Fix(pat, body) = term;
//                     res = body.reverse_accum(f, ctx, res);
//                     res = pat.reverse_accum(f, ctx, res);
//                 }
//                 | Term::Pi(term) => {
//                     let Pi(pat, body) = term;
//                     res = body.reverse_accum(f, ctx, res);
//                     res = pat.reverse_accum(f, ctx, res);
//                 }
//                 | Term::Sigma(term) => {
//                     let Sigma(pat, body) = term;
//                     res = body.reverse_accum(f, ctx, res);
//                     res = pat.reverse_accum(f, ctx, res);
//                 }
//                 | Term::Thunk(term) => {
//                     let Thunk(body) = term;
//                     res = body.reverse_accum(f, ctx, res);
//                 }
//                 | Term::Force(term) => {
//                     let Force(body) = term;
//                     res = body.reverse_accum(f, ctx, res);
//                 }
//                 | Term::Ret(term) => {
//                     let Ret(body) = term;
//                     res = body.reverse_accum(f, ctx, res);
//                 }
//                 | Term::Do(term) => {
//                     let Bind { binder, bindee, tail } = term;
//                     res = tail.reverse_accum(f, ctx, res);
//                     res = binder.reverse_accum(f, ctx, res);
//                     res = bindee.reverse_accum(f, ctx, res);
//                 }
//                 | Term::Let(term) => {
//                     let PureBind { binder, bindee, tail } = term;
//                     res = tail.reverse_accum(f, ctx, res);
//                     res = binder.reverse_accum(f, ctx, res);
//                     res = bindee.reverse_accum(f, ctx, res);
//                 }
//                 | Term::MoBlock(term) => {
//                     let MoBlock(body) = term;
//                     res = body.reverse_accum(f, ctx, res);
//                 }
//                 | Term::Data(term) => {
//                     let Data { arms } = term;
//                     for DataArm { name: _, param } in arms {
//                         res = param.reverse_accum(f, ctx, res);
//                     }
//                 }
//                 | Term::CoData(term) => {
//                     let CoData { arms } = term;
//                     for CoDataArm { name: _, out } in arms {
//                         res = out.reverse_accum(f, ctx, res);
//                     }
//                 }
//                 | Term::Ctor(term) => {
//                     let Ctor(_name, body) = term;
//                     res = body.reverse_accum(f, ctx, res);
//                 }
//                 | Term::Match(term) => {
//                     let Match { scrut, arms } = term;
//                     for Matcher { binder, tail } in arms {
//                         res = tail.reverse_accum(f, ctx, res);
//                         res = binder.reverse_accum(f, ctx, res);
//                     }
//                     res = scrut.reverse_accum(f, ctx, res);
//                 }
//                 | Term::CoMatch(term) => {
//                     let CoMatch { arms } = term;
//                     for CoMatcher { dtor: _, tail } in arms {
//                         res = tail.reverse_accum(f, ctx, res);
//                     }
//                 }
//                 | Term::Dtor(term) => {
//                     let Dtor(body, _name) = term;
//                     res = body.reverse_accum(f, ctx, res);
//                 }
//                 | Term::Lit(term) => {
//                     let _lit = term;
//                 }
//             };
//             f.action_term(item, ctx, res)
//         }
//     }

//     impl ReverseAccumFold for DeclId {
//         fn reverse_accum<C, R, F>(self, f: &mut F, ctx: &C, mut res: R) -> R
//         where
//             F: AccumFoldScoped<C, R>,
//         {
//             let item = f.decl(&self);
//             match item.to_owned() {
//                 | Declaration::AliasBody(decl) => {
//                     let AliasBody { binder, bindee } = decl;
//                     res = bindee.reverse_accum(f, ctx, res);
//                     res = binder.reverse_accum(f, ctx, res);
//                 }
//                 | Declaration::AliasHead(decl) => {
//                     let AliasHead { binder, ty } = decl;
//                     res = ty.reverse_accum(f, ctx, res);
//                     res = binder.reverse_accum(f, ctx, res);
//                 }
//                 | Declaration::Module(_) => unreachable!(),
//                 | Declaration::Exec(decl) => {
//                     let Exec(body) = decl;
//                     res = body.reverse_accum(f, ctx, res);
//                 }
//             }
//             f.action_decl(item, ctx, res)
//         }
//     }
// }

// /* --------------------------------- CtxFold -------------------------------- */
// #[auto_impl::auto_impl(&mut, Box)]
// pub trait CtxFoldScoped<Cx>: ArenaScoped {
//     fn action_pat(&mut self, pat: PatId, ctx: Cx) -> Cx;
//     fn action_term(&mut self, term: TermId, ctx: Cx);
//     fn action_decl(&mut self, decl: DeclId, ctx: Cx) -> Cx;
// }

// pub trait ObverseCtxAccumFold {
//     fn obverse_ctx_accum<Cx, F>(self, f: &mut F, ctx: Cx) -> Cx
//     where
//         F: CtxFoldScoped<Cx>,
//         Cx: Clone;
// }

// pub trait ObverseCtxFinalFold {
//     fn obverse_ctx_final<Cx, F>(self, f: &mut F, ctx: &Cx)
//     where
//         F: CtxFoldScoped<Cx>,
//         Cx: Clone;
// }

// mod impl_obverse_ctx {
//     use super::*;

//     impl<T> ObverseCtxAccumFold for Option<T>
//     where
//         T: ObverseCtxAccumFold,
//     {
//         fn obverse_ctx_accum<Cx, F>(self, f: &mut F, ctx: Cx) -> Cx
//         where
//             F: CtxFoldScoped<Cx>,
//             Cx: Clone,
//         {
//             match self {
//                 | Some(item) => item.obverse_ctx_accum(f, ctx),
//                 | None => ctx,
//             }
//         }
//     }

//     impl<T> ObverseCtxFinalFold for Option<T>
//     where
//         T: ObverseCtxFinalFold,
//     {
//         fn obverse_ctx_final<Cx, F>(self, f: &mut F, ctx: &Cx)
//         where
//             F: CtxFoldScoped<Cx>,
//             Cx: Clone,
//         {
//             match self {
//                 | Some(item) => item.obverse_ctx_final(f, ctx),
//                 | None => {}
//             }
//         }
//     }

//     impl ObverseCtxAccumFold for PatId {
//         fn obverse_ctx_accum<Cx, F>(self, f: &mut F, mut ctx: Cx) -> Cx
//         where
//             F: CtxFoldScoped<Cx>,
//             Cx: Clone,
//         {
//             let item = f.pat(&self);
//             match item.to_owned() {
//                 | Pattern::Ann(pat) => {
//                     let Ann { tm, ty } = pat;
//                     ty.obverse_ctx_final(f, &ctx);
//                     ctx = tm.obverse_ctx_accum(f, ctx);
//                 }
//                 | Pattern::Hole(pat) => {
//                     let Hole = pat;
//                 }
//                 | Pattern::Var(pat) => {
//                     let _ = pat;
//                 }
//                 | Pattern::Ctor(pat) => {
//                     let Ctor(_ctorv, body) = pat;
//                     ctx = body.obverse_ctx_accum(f, ctx)
//                 }
//                 | Pattern::Triv(pat) => {
//                     let Triv = pat;
//                 }
//                 | Pattern::Cons(pat) => {
//                     let Cons(a, b) = pat;
//                     ctx = a.obverse_ctx_accum(f, ctx);
//                     ctx = b.obverse_ctx_accum(f, ctx)
//                 }
//             }
//             f.action_pat(self, ctx)
//         }
//     }

//     impl ObverseCtxFinalFold for TermId {
//         fn obverse_ctx_final<Cx, F>(self, f: &mut F, ctx: &Cx)
//         where
//             F: CtxFoldScoped<Cx>,
//             Cx: Clone,
//         {
//             let item = f.term(&self);
//             match item.to_owned() {
//                 | Term::Internal(_) => unreachable!(),
//                 | Term::Sealed(term) => {
//                     let Sealed(inner) = term;
//                     inner.obverse_ctx_final(f, ctx);
//                 }
//                 | Term::Ann(term) => {
//                     let Ann { tm, ty } = term;
//                     ty.obverse_ctx_final(f, ctx);
//                     tm.obverse_ctx_final(f, ctx);
//                 }
//                 | Term::Hole(term) => {
//                     let Hole = term;
//                 }
//                 | Term::Var(term) => {
//                     let _ = term;
//                 }
//                 | Term::Triv(term) => {
//                     let Triv = term;
//                 }
//                 | Term::Cons(term) => {
//                     let Cons(a, b) = term;
//                     a.obverse_ctx_final(f, ctx);
//                     b.obverse_ctx_final(f, ctx);
//                 }
//                 | Term::Abs(term) => {
//                     let Abs(pat, body) = term;
//                     let ctx_ = &pat.obverse_ctx_accum(f, ctx.to_owned());
//                     body.obverse_ctx_final(f, ctx_);
//                 }
//                 | Term::App(term) => {
//                     let App(a, b) = term;
//                     a.obverse_ctx_final(f, ctx);
//                     b.obverse_ctx_final(f, ctx);
//                 }
//                 | Term::Fix(term) => {
//                     let Fix(pat, body) = term;
//                     let ctx_ = &pat.obverse_ctx_accum(f, ctx.to_owned());
//                     body.obverse_ctx_final(f, ctx_);
//                 }
//                 | Term::Pi(term) => {
//                     let Pi(pat, body) = term;
//                     let ctx_ = &pat.obverse_ctx_accum(f, ctx.to_owned());
//                     body.obverse_ctx_final(f, ctx_);
//                 }
//                 | Term::Sigma(term) => {
//                     let Sigma(pat, body) = term;
//                     let ctx_ = &pat.obverse_ctx_accum(f, ctx.to_owned());
//                     body.obverse_ctx_final(f, ctx_);
//                 }
//                 | Term::Thunk(term) => {
//                     let Thunk(body) = term;
//                     body.obverse_ctx_final(f, ctx);
//                 }
//                 | Term::Force(term) => {
//                     let Force(body) = term;
//                     body.obverse_ctx_final(f, ctx);
//                 }
//                 | Term::Ret(term) => {
//                     let Ret(body) = term;
//                     body.obverse_ctx_final(f, ctx);
//                 }
//                 | Term::Do(term) => {
//                     let Bind { binder, bindee, tail } = term;
//                     bindee.obverse_ctx_final(f, ctx);
//                     let ctx_ = &binder.obverse_ctx_accum(f, ctx.to_owned());
//                     tail.obverse_ctx_final(f, ctx_);
//                 }
//                 | Term::Let(term) => {
//                     let PureBind { binder, bindee, tail } = term;
//                     bindee.obverse_ctx_final(f, ctx);
//                     let ctx_ = &binder.obverse_ctx_accum(f, ctx.to_owned());
//                     tail.obverse_ctx_final(f, ctx_);
//                 }
//                 | Term::MoBlock(term) => {
//                     let MoBlock(body) = term;
//                     body.obverse_ctx_final(f, ctx);
//                 }
//                 | Term::Data(term) => {
//                     let Data { arms } = term;
//                     for DataArm { name: _, param } in arms {
//                         param.obverse_ctx_final(f, ctx);
//                     }
//                 }
//                 | Term::CoData(term) => {
//                     let CoData { arms } = term;
//                     for CoDataArm { name: _, out } in arms {
//                         out.obverse_ctx_final(f, ctx);
//                     }
//                 }
//                 | Term::Ctor(term) => {
//                     let Ctor(_name, body) = term;
//                     body.obverse_ctx_final(f, ctx);
//                 }
//                 | Term::Match(term) => {
//                     let Match { scrut, arms } = term;
//                     for Matcher { binder, tail } in arms {
//                         let ctx = &binder.obverse_ctx_accum(f, ctx.to_owned());
//                         tail.obverse_ctx_final(f, ctx);
//                     }
//                     scrut.obverse_ctx_final(f, ctx);
//                 }
//                 | Term::CoMatch(term) => {
//                     let CoMatch { arms } = term;
//                     for CoMatcher { dtor: _, tail } in arms {
//                         tail.obverse_ctx_final(f, ctx);
//                     }
//                 }
//                 | Term::Dtor(term) => {
//                     let Dtor(body, _name) = term;
//                     body.obverse_ctx_final(f, ctx);
//                 }
//                 | Term::Lit(term) => {
//                     let _lit = term;
//                 }
//             };
//             f.action_term(self, ctx.to_owned())
//         }
//     }

//     impl ObverseCtxAccumFold for DeclId {
//         fn obverse_ctx_accum<Cx, F>(self, f: &mut F, mut ctx: Cx) -> Cx
//         where
//             F: CtxFoldScoped<Cx>,
//             Cx: Clone,
//         {
//             let item = f.decl(&self);
//             match item.to_owned() {
//                 | Declaration::AliasBody(decl) => {
//                     let AliasBody { binder, bindee } = decl;
//                     ctx = binder.obverse_ctx_accum(f, ctx.to_owned());
//                     bindee.obverse_ctx_final(f, &ctx);
//                 }
//                 | Declaration::AliasHead(decl) => {
//                     let AliasHead { binder, ty } = decl;
//                     ctx = binder.obverse_ctx_accum(f, ctx.to_owned());
//                     ty.obverse_ctx_final(f, &ctx);
//                 }
//                 | Declaration::Module(_) => unreachable!(),
//                 | Declaration::Exec(decl) => {
//                     let Exec(body) = decl;
//                     body.obverse_ctx_final(f, &ctx);
//                 }
//             }
//             f.action_decl(self, ctx)
//         }
//     }
// }

// impl CtxFoldScoped<Context<()>> for Collector {
//     fn action_pat(&mut self, pat: PatId, ctx: Context<()>) -> Context<()> {
//         let item = self.pat(&pat);
//         match item {
//             | Pattern::Var(def) => ctx + (def, ()),
//             | Pattern::Ann(_)
//             | Pattern::Hole(_)
//             | Pattern::Ctor(_)
//             | Pattern::Triv(_)
//             | Pattern::Cons(_) => ctx,
//         }
//     }

//     fn action_term(&mut self, term: TermId, ctx: Context<()>) {
//         self.ctxs.insert(term, ctx)
//     }

//     fn action_decl(&mut self, _decl: DeclId, ctx: Context<()>) -> Context<()> {
//         ctx
//     }
// }

// /* -------------------------------- CoCtxFold ------------------------------- */
// #[auto_impl::auto_impl(&mut, Box)]
// pub trait CoCtxFoldScoped<Co>: ArenaScoped {
//     fn action_pat(&mut self, pat: PatId, co: Co) -> Co;
//     fn action_term(&mut self, term: TermId, co: Co);
//     fn action_decl(&mut self, decl: DeclId, co: Co) -> Co;
// }

// pub trait ReverseCoCtxReduceFold {
//     fn reverse_coctx_reduce<Co, F>(self, f: &mut F, co: Co) -> Co
//     where
//         F: CoCtxFoldScoped<Co>,
//         Co: Monoid + Clone;
// }

// pub trait ReverseCoCtxInitFold {
//     fn reverse_coctx_init<Co, F>(self, f: &mut F) -> Co
//     where
//         F: CoCtxFoldScoped<Co>,
//         Co: Monoid + Clone;
// }

// mod impl_reverse_coctx {
//     use super::*;

//     impl<T> ReverseCoCtxReduceFold for Option<T>
//     where
//         T: ReverseCoCtxReduceFold,
//     {
//         fn reverse_coctx_reduce<Co, F>(self, f: &mut F, co: Co) -> Co
//         where
//             F: CoCtxFoldScoped<Co>,
//             Co: Monoid + Clone,
//         {
//             match self {
//                 | Some(item) => item.reverse_coctx_reduce(f, co),
//                 | None => co,
//             }
//         }
//     }

//     impl<T> ReverseCoCtxInitFold for Option<T>
//     where
//         T: ReverseCoCtxInitFold,
//     {
//         fn reverse_coctx_init<Co, F>(self, f: &mut F) -> Co
//         where
//             F: CoCtxFoldScoped<Co>,
//             Co: Monoid + Clone,
//         {
//             match self {
//                 | Some(item) => item.reverse_coctx_init(f),
//                 | None => Co::default(),
//             }
//         }
//     }

//     impl ReverseCoCtxReduceFold for PatId {
//         fn reverse_coctx_reduce<Co, F>(self, f: &mut F, mut co: Co) -> Co
//         where
//             F: CoCtxFoldScoped<Co>,
//             Co: Monoid + Clone,
//         {
//             let item = f.pat(&self);
//             match item.to_owned() {
//                 | Pattern::Ann(pat) => {
//                     let Ann { tm, ty } = pat;
//                     co = tm.reverse_coctx_reduce(f, co);
//                     co = co + ty.reverse_coctx_init(f);
//                 }
//                 | Pattern::Hole(pat) => {
//                     let Hole = pat;
//                 }
//                 | Pattern::Var(pat) => {
//                     let _ = pat;
//                 }
//                 | Pattern::Ctor(pat) => {
//                     let Ctor(_ctorv, body) = pat;
//                     co = body.reverse_coctx_reduce(f, co);
//                 }
//                 | Pattern::Triv(pat) => {
//                     let Triv = pat;
//                 }
//                 | Pattern::Cons(pat) => {
//                     let Cons(a, b) = pat;
//                     co = b.reverse_coctx_reduce(f, co);
//                     co = a.reverse_coctx_reduce(f, co);
//                 }
//             }
//             f.action_pat(self, co)
//         }
//     }

//     impl ReverseCoCtxInitFold for TermId {
//         fn reverse_coctx_init<Co, F>(self, f: &mut F) -> Co
//         where
//             F: CoCtxFoldScoped<Co>,
//             Co: Monoid + Clone,
//         {
//             let item = f.term(&self);
//             let co = match item.to_owned() {
//                 | Term::Internal(_) => unreachable!(),
//                 | Term::Sealed(term) => {
//                     let Sealed(inner) = term;
//                     inner.reverse_coctx_init(f)
//                 }
//                 | Term::Ann(term) => {
//                     let Ann { tm, ty } = term;
//                     tm.reverse_coctx_init(f) + ty.reverse_coctx_init(f)
//                 }
//                 | Term::Hole(term) => {
//                     let Hole = term;
//                     Co::default()
//                 }
//                 | Term::Var(term) => {
//                     let _ = term;
//                     Co::default()
//                 }
//                 | Term::Triv(term) => {
//                     let Triv = term;
//                     Co::default()
//                 }
//                 | Term::Cons(term) => {
//                     let Cons(a, b) = term;
//                     a.reverse_coctx_init(f) + b.reverse_coctx_init(f)
//                 }
//                 | Term::Abs(term) => {
//                     let Abs(pat, body) = term;
//                     let co = body.reverse_coctx_init(f);
//                     pat.reverse_coctx_reduce(f, co)
//                 }
//                 | Term::App(term) => {
//                     let App(a, b) = term;
//                     a.reverse_coctx_init(f) + b.reverse_coctx_init(f)
//                 }
//                 | Term::Fix(term) => {
//                     let Fix(pat, body) = term;
//                     let co = body.reverse_coctx_init(f);
//                     pat.reverse_coctx_reduce(f, co)
//                 }
//                 | Term::Pi(term) => {
//                     let Pi(pat, body) = term;
//                     let co = body.reverse_coctx_init(f);
//                     pat.reverse_coctx_reduce(f, co)
//                 }
//                 | Term::Sigma(term) => {
//                     let Sigma(pat, body) = term;
//                     let co = body.reverse_coctx_init(f);
//                     pat.reverse_coctx_reduce(f, co)
//                 }
//                 | Term::Thunk(term) => {
//                     let Thunk(body) = term;
//                     body.reverse_coctx_init(f)
//                 }
//                 | Term::Force(term) => {
//                     let Force(body) = term;
//                     body.reverse_coctx_init(f)
//                 }
//                 | Term::Ret(term) => {
//                     let Ret(body) = term;
//                     body.reverse_coctx_init(f)
//                 }
//                 | Term::Do(term) => {
//                     let Bind { binder, bindee, tail } = term;
//                     let co = tail.reverse_coctx_init(f);
//                     binder.reverse_coctx_reduce(f, co) + bindee.reverse_coctx_init(f)
//                 }
//                 | Term::Let(term) => {
//                     let PureBind { binder, bindee, tail } = term;
//                     let co = tail.reverse_coctx_init(f);
//                     binder.reverse_coctx_reduce(f, co) + bindee.reverse_coctx_init(f)
//                 }
//                 | Term::MoBlock(term) => {
//                     let MoBlock(body) = term;
//                     body.reverse_coctx_init(f)
//                 }
//                 | Term::Data(term) => {
//                     let Data { arms } = term;
//                     Co::concat(
//                         arms.into_iter()
//                             .map(|DataArm { name: _, param }| param.reverse_coctx_init(f)),
//                     )
//                 }
//                 | Term::CoData(term) => {
//                     let CoData { arms } = term;
//                     Co::concat(
//                         arms.into_iter()
//                             .map(|CoDataArm { name: _, out }| out.reverse_coctx_init(f)),
//                     )
//                 }
//                 | Term::Ctor(term) => {
//                     let Ctor(_name, body) = term;
//                     body.reverse_coctx_init(f)
//                 }
//                 | Term::Match(term) => {
//                     let Match { scrut, arms } = term;
//                     let co = Co::concat(arms.into_iter().map(|Matcher { binder, tail }| {
//                         let co = tail.reverse_coctx_init(f);
//                         binder.reverse_coctx_reduce(f, co)
//                     }));
//                     scrut.reverse_coctx_init(f) + co
//                 }
//                 | Term::CoMatch(term) => {
//                     let CoMatch { arms } = term;
//                     Co::concat(
//                         arms.into_iter()
//                             .map(|CoMatcher { dtor: _, tail }| tail.reverse_coctx_init(f)),
//                     )
//                 }
//                 | Term::Dtor(term) => {
//                     let Dtor(body, _name) = term;
//                     body.reverse_coctx_init(f)
//                 }
//                 | Term::Lit(term) => {
//                     let _lit = term;
//                     Co::default()
//                 }
//             };
//             f.action_term(self, co.to_owned());
//             co
//         }
//     }
// }

// impl CoCtxFoldScoped<CoContext<()>> for Collector {
//     fn action_pat(&mut self, pat: PatId, co: CoContext<()>) -> CoContext<()> {
//         let item = self.pat(&pat);
//         match item {
//             | Pattern::Var(def) => co - &def,
//             | Pattern::Ann(_)
//             | Pattern::Hole(_)
//             | Pattern::Ctor(_)
//             | Pattern::Triv(_)
//             | Pattern::Cons(_) => co,
//         }
//     }

//     fn action_term(&mut self, term: TermId, co: CoContext<()>) {
//         self.coctxs.insert(term, co)
//     }

//     fn action_decl(&mut self, _decl: DeclId, _co: CoContext<()>) -> CoContext<()> {
//         unreachable!()
//     }
// }
