use std::ops::Index;

use crate::{
    arena::*,
    bitter::syntax as b,
    textual::syntax::{self as t},
};

pub trait Desugar {
    type Out;
    fn desugar(self) -> Self::Out;
}

pub struct Desugarer {
    pub ctx: b::Ctx,
    pub top: b::TopLevel,
}
struct With<'ctx, Id, T> {
    spans: &'ctx mut SpanArena,
    tctx: &'ctx t::Ctx,
    bctx: &'ctx mut b::Ctx,
    id: Id,
    t: T,
}

// pub trait Lookup<'ctx, 'f, Id: Copy>
// where
//     'ctx: 'f,
// {
//     type Out;
//     fn lookup(&'ctx mut self, id: Id) -> Self::Out;
// }

mod impls {
    use super::*;

    impl<'ctx, Id: Copy, T> With<'ctx, Id, T> {
        pub fn take(self) -> (With<'ctx, Id, ()>, Id, T) {
            let With { spans, tctx, bctx, id, t } = self;
            (With { spans, tctx, bctx, id, t: () }, id, t)
        }
        pub fn map<U>(self, f: impl FnOnce(T) -> U) -> With<'ctx, Id, U> {
            let With { spans, tctx, bctx, id, t } = self;
            With { spans, tctx, bctx, id, t: f(t) }
        }
    }
    impl<'ctx, Id: Copy> With<'ctx, Id, ()> {
        pub fn mk<U>(self, f: impl FnOnce() -> U) -> With<'ctx, Id, U> {
            let With { spans, tctx, bctx, id, t: () } = self;
            With { spans, tctx, bctx, id, t: f() }
        }
    }

    // impl<'ctx, 'f> Lookup<'ctx, 'f, DefId> for With<'ctx, (), ()>
    // where
    //     'ctx: 'f,
    // {
    //     type Out = With<'f, DefId, t::VarName>;
    //     fn lookup(&'ctx mut self, id: DefId) -> Self::Out
    //     where
    //         'ctx: 'f,
    //     {
    //         let With { spans, tctx, bctx, id: (), t: () } = self;
    //         With { spans, tctx, bctx, id, t: tctx.defs[id].clone() }
    //     }
    // }
    // impl<'ctx, 'f> Lookup<'ctx, 'f, PatternId> for With<'ctx, (), ()>
    // where
    //     'ctx: 'f,
    // {
    //     type Out = With<'f, PatternId, t::Pattern>;
    //     fn lookup(&'ctx mut self, id: PatternId) -> Self::Out
    //     where
    //         'ctx: 'f,
    //     {
    //         let With { spans, tctx, bctx, id: (), t: () } = self;
    //         With { spans, tctx, bctx, id, t: tctx.pats[id].clone() }
    //     }
    // }
    // impl<'ctx, 'f> Lookup<'ctx, 'f, CoPatternId> for With<'ctx, (), ()>
    // where
    //     'ctx: 'f,
    // {
    //     type Out = With<'f, CoPatternId, t::CoPattern>;
    //     fn lookup(&'ctx mut self, id: CoPatternId) -> Self::Out
    //     where
    //         'ctx: 'f,
    //     {
    //         let With { spans, tctx, bctx, id: (), t: () } = self;
    //         With { spans, tctx, bctx, id, t: tctx.copats[id].clone() }
    //     }
    // }
    // impl<'ctx, 'f> Lookup<'ctx, 'f, TermId> for With<'ctx, (), ()>
    // where
    //     'ctx: 'f,
    // {
    //     type Out = With<'f, TermId, t::Term<t::NameRef<t::VarName>>>
    //     where
    //         'ctx: 'f,
    //         Self: 'ctx;
    //     fn lookup(&'ctx mut self, id: TermId) -> Self::Out
    //     where
    //         'ctx: 'f,
    //     {
    //         let With { spans, tctx, bctx, id: (), t: () } = self;
    //         With { spans, tctx, bctx, id, t: tctx.terms[id].clone() }
    //     }
    // }

    impl<'ctx> With<'ctx, (), ()> {
        pub fn lookup_def<'f>(&'ctx mut self, id: DefId) -> With<'f, DefId, t::VarName>
        where
            'ctx: 'f,
        {
            let With { spans, tctx, bctx, id: (), t: () } = self;
            With { spans, tctx, bctx, id, t: tctx.defs[id].clone() }
        }
    }
    impl<'ctx> With<'ctx, (), ()> {
        pub fn lookup_pat<'f>(&'ctx mut self, id: PatternId) -> With<'f, PatternId, t::Pattern>
        where
            'ctx: 'f,
        {
            let With { spans, tctx, bctx, id: (), t: () } = self;
            With { spans, tctx, bctx, id, t: tctx.pats[id].clone() }
        }
    }
    impl<'ctx> With<'ctx, (), ()> {
        pub fn lookup_copat<'f>(
            &'ctx mut self, id: CoPatternId,
        ) -> With<'f, CoPatternId, t::CoPattern>
        where
            'ctx: 'f,
        {
            let With { spans, tctx, bctx, id: (), t: () } = self;
            With { spans, tctx, bctx, id, t: tctx.copats[id].clone() }
        }
    }
    impl<'ctx> With<'ctx, (), ()> {
        pub fn lookup_term<'f>(
            &'ctx mut self, id: TermId,
        ) -> With<'f, TermId, t::Term<t::NameRef<t::VarName>>>
        where
            'ctx: 'f,
        {
            let With { spans, tctx, bctx, id: (), t: () } = self;
            With { spans, tctx, bctx, id, t: tctx.terms[id].clone() }
        }
    }

    impl<'ctx> With<'ctx, DefId, t::VarName> {
        pub fn def(&'ctx mut self) -> DefId {
            self.bctx.defs.insert(self.id, self.t.clone());
            self.id
        }
    }
    impl<'ctx> With<'ctx, PatternId, b::Pattern> {
        pub fn pat(&'ctx mut self) -> PatternId {
            self.bctx.pats.insert(self.id, self.t.clone());
            self.id
        }
    }
    impl<'ctx> With<'ctx, CoPatternId, b::CoPattern> {
        pub fn copat(&'ctx mut self) -> CoPatternId {
            self.bctx.copats.insert(self.id, self.t.clone());
            self.id
        }
    }
    impl<'ctx> With<'ctx, TermId, b::Term<t::NameRef<t::VarName>>> {
        pub fn term(&'ctx mut self) -> TermId {
            self.bctx.terms.insert(self.id, self.t.clone());
            self.id
        }
    }
}

pub struct RunDesugar {
    pub spans: SpanArena,
    pub ctx: t::Ctx,
    pub top: t::TopLevel,
}

impl RunDesugar {
    pub fn run(mut self) -> Desugarer {
        let mut ctx = b::Ctx::default();
        let top =
            With { spans: &mut self.spans, tctx: &self.ctx, bctx: &mut ctx, id: (), t: self.top }
                .desugar();
        Desugarer { ctx, top }
    }
}

impl<'ctx> Desugar for With<'ctx, (), t::TopLevel> {
    type Out = b::TopLevel;
    fn desugar(self) -> Self::Out {
        let (mut with, (), t) = self.take();
        let t::TopLevel(decls) = t;
        let mut decls_new = Vec::new();
        for t::Modifiers { public, inner } in decls {
            use t::Declaration as Decl;
            let inner = match inner {
                Decl::DataDef(decl) => {
                    let t::DataDef { name, params, def } = decl;
                    let name = { with.lookup_def(name).def() };
                    // with.spans.defs[name];
                    // let mut params_new = Vec::new();
                    // for param in params {
                    //     let param = with.lookup_pat(param).map(|param| todo!()).pat();
                    //     params_new.push(param);
                    // }
                    todo!()
                }
                Decl::CoDataDef(_) => todo!(),
                Decl::Define(_) => todo!(),
                Decl::Alias(_) => todo!(),
                Decl::Extern(_) => todo!(),
                Decl::Module(_) => todo!(),
                Decl::UseDef(_) => todo!(),
                Decl::UseBlock(_) => todo!(),
                Decl::Main(_) => todo!(),
            };
            decls_new.push(b::Modifiers { public, inner })
        }
        b::TopLevel(decls_new)
    }
}

impl<'ctx> Desugar for With<'ctx, TermId, t::Term<t::NameRef<t::VarName>>> {
    type Out = ();
    fn desugar(self) {
        let (with, id, t) = self.take();
        use t::Term as Tm;
        match t {
            Tm::Ann(term) => {
                let t::Ann { tm, ty } = term;
                // Todo: rec
                with.mk(|| b::Ann { tm, ty }.into()).term();
            }
            Tm::Hole(term) => {
                let t::Hole = term;
                with.mk(|| b::Hole.into()).term();
            }
            Tm::Var(name) => {
                with.mk(|| b::Term::Var(name).into()).term();
            }
            Tm::Paren(term) => {
                let t::Paren(terms) = term;
                // Todo: rec
                with.mk(|| b::Paren(terms).into()).term();
            }
            Tm::Abs(term) => {
                let t::Abs(params, tail) = term;
                // Todo: rec
                with.mk(|| b::Abs(params, tail).into()).term();
            }
            Tm::App(term) => {
                let t::App(terms) = term;
                // Todo: rec
                with.mk(|| b::App(terms).into()).term();
            }
            Tm::Rec(term) => {
                let t::Rec(pat, term) = term;
                // Todo: rec
                with.mk(|| b::Rec(pat, term).into()).term();
            }
            Tm::Pi(term) => {
                let t::Pi(params, ty) = term;
                // Todo: rec
                with.mk(|| b::Pi(params, ty).into()).term();
            }
            Tm::Arrow(term) => {
                let t::Arrow(params, ty) = term;
                // Todo: rec
                with.mk(|| b::Arrow(params, ty).into()).term();
            }
            Tm::Forall(term) => {
                let t::Forall(params, ty) = term;
                // Todo: rec
                with.mk(|| b::Forall(params, ty).into()).term();
            }
            Tm::Sigma(term) => {
                let t::Sigma(params, ty) = term;
                // Todo: rec
                with.mk(|| b::Sigma(params, ty).into()).term();
            }
            Tm::Prod(term) => {
                let t::Prod(terms) = term;
                // Todo: rec
                with.mk(|| b::Prod(terms).into()).term();
            }
            Tm::Exists(term) => {
                let t::Exists(params, ty) = term;
                // Todo: rec
                with.mk(|| b::Exists(params, ty).into()).term();
            }
            Tm::Thunk(term) => {
                let t::Thunk(term) = term;
                // Todo: rec
                with.mk(|| b::Thunk(term).into()).term();
            }
            Tm::Force(term) => {
                let t::Force(term) = term;
                // Todo: rec
                with.mk(|| b::Force(term).into()).term();
            }
            Tm::Ret(term) => {
                let t::Return(term) = term;
                // Todo: rec
                with.mk(|| b::Return(term).into()).term();
            }
            Tm::Do(term) => {
                let t::Bind { binder, bindee, tail } = term;
                // Todo: rec
                with.mk(|| b::Bind { binder, bindee, tail }.into()).term();
            }
            Tm::Let(term) => {
                let t::PureBind {
                    binding: t::GenBind { rec, comp, binder, params, ty, bindee },
                    tail,
                } = term;
                // Fixme: xxx
            }
            Tm::UseLet(term) => {
                let t::UseBind { uses, tail } = term;
                // Todo: rec
                with.mk(|| b::UseBind { uses, tail }.into()).term();
            }
            Tm::Data(term) => {
                let t::Data { arms } = term;
                // Todo: rec
                // with.mk(|| b::Data { arms }.into()).term();
            }
            Tm::CoData(term) => {
                let t::CoData { arms } = term;
                // Todo: rec
                // with.mk(|| b::CoData { arms }.into()).term();
            }
            Tm::Ctor(term) => {
                let t::Ctor(name, term) = term;
                // Todo: rec
                with.mk(|| b::Ctor(name, term).into()).term();
            }
            Tm::Match(term) => {
                let t::Match { scrut, arms } = term;
                // Fixme: xxx
                // with.mk(|| b::Match { scrut, arms }.into()).term();
            }
            Tm::CoMatch(term) => {
                let t::CoMatch { arms } = term;
                // Fixme: xxx
                // with.mk(|| b::CoMatch { arms }.into()).term();
            }
            Tm::Dtor(term) => {
                let t::Dtor(term, name) = term;
                // Todo: rec
                with.mk(|| b::Dtor(term, name).into()).term();
            }
            Tm::Lit(term) => {
                with.mk(|| term.into()).term();
            }
        }
    }
}
