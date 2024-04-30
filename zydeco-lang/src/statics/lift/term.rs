use super::*;

// Note: no type level transformation is performed here since we're gonna remove them right after anyway

impl MonadTransTerm for Sp<TermValue> {
    type Out = TermValue;
    fn lift(&self, m: RcValue, ctx: Ctx) -> Self::Out {
        match self.inner_ref() {
            TermValue::Annotation(Annotation { term, ty }) => {
                let span = term.span();
                let term = span.make_rc(term.lift(m, ctx.clone()));
                Annotation { term, ty: ty.clone() }.into()
            }
            TermValue::Var(x) => x.clone().into(),
            TermValue::Thunk(Thunk(body)) => {
                let span = body.span();
                let body = span.make_rc(body.lift(m, ctx.clone()));
                Thunk(body).into()
            }
            TermValue::Ctor(Ctor { ctorv, args }) => {
                let args = args
                    .iter()
                    .map(|v| {
                        let span = v.span();
                        span.make_rc(v.lift(m.clone(), ctx.clone()))
                    })
                    .collect();
                Ctor { ctorv: ctorv.clone(), args }.into()
            }
            TermValue::Literal(lit) => lit.clone().into(),
            TermValue::Pack(Pack { ty, body }) => {
                let span = body.span();
                let body = span.make_rc(body.lift(m, ctx.clone()));
                Pack { ty: ty.clone(), body }.into()
            }
        }
    }
}

impl MonadTransTerm for Sp<TermComputation> {
    type Out = TermComputation;
    fn lift(&self, m: RcValue, ctx: Ctx) -> Self::Out {
        match self.inner_ref() {
            TermComputation::Annotation(Annotation { term, ty }) => {
                let span = term.span();
                let term = span.make_rc(term.lift(m, ctx.clone()));
                Annotation { term, ty: ty.clone() }.into()
            }
            TermComputation::Abs(Abs { param, body }) => {
                let span = body.span();
                let body = span.make_rc(body.lift(m, ctx.clone()));
                Abs { param: param.clone(), body }.into()
            }
            TermComputation::App(App { body, arg }) => {
                let span = body.span();
                let body = span.make_rc(body.lift(m.clone(), ctx.clone()));
                let arg = span.make_rc(arg.lift(m, ctx.clone()));
                App { body, arg }.into()
            }
            TermComputation::Ret(Ret(arg)) => {
                // the return case uses the monad's return
                let span = arg.span();
                let arg = span.make_rc(arg.lift(m.clone(), ctx.clone()));
                let monad = span.make_rc(Force(m).into());
                let mret = span.make_rc(
                    Dtor { body: monad, dtorv: DtorV::new(format!("return"), span.clone()) }.into(),
                );
                App { body: mret, arg }.into()
            }
            TermComputation::Force(Force(body)) => {
                let span = body.span();
                let body = span.make_rc(body.lift(m, ctx.clone()));
                Force(body).into()
            }
            TermComputation::Let(Let { var, def, body }) => {
                let span = def.span();
                let def = span.make_rc(def.lift(m.clone(), ctx.clone()));
                let span = body.span();
                let body = span.make_rc(body.lift(m, ctx.clone()));
                Let { var: var.clone(), def, body }.into()
            }
            TermComputation::Do(Do { var, comp, body }) => {
                let span = comp.span();
                let ty_body = self.syn(ctx.clone()).expect("synthesis of do body failed");
                let alg =
                    ty_body.alg(m.clone(), ctx.clone(), span).expect("failed generating algebra");
                let alg_app = span.make_rc(
                    Dtor {
                        body: span.make_rc(alg),
                        dtorv: DtorV::new(format!("bindA"), span.clone()),
                    }
                    .into(),
                );
                let comp = span.make_rc(comp.lift(m.clone(), ctx.clone()));
                let thunk_comp: RcValue = span.make_rc(Thunk(comp).into());
                let span = body.span();
                let body = span.make_rc(body.lift(m, ctx.clone()));
                let thunk_body: RcValue = span
                    .make_rc(Thunk(span.make_rc(Abs { param: var.clone(), body }.into())).into());
                App {
                    body: self.span().make_rc(App { body: alg_app, arg: thunk_comp }.into()),
                    arg: thunk_body,
                }
                .into()
            }
            TermComputation::Rec(Rec { var, body }) => {
                let span = body.span();
                let body = span.make_rc(body.lift(m, ctx.clone()));
                Rec { var: var.clone(), body }.into()
            }
            TermComputation::Match(Match { scrut, arms }) => {
                let span = scrut.span();
                let scrut = span.make_rc(scrut.lift(m.clone(), ctx.clone()));
                let arms = arms
                    .iter()
                    .map(|arm| {
                        let span = arm.body.span();
                        let body = span.make_rc(arm.body.lift(m.clone(), ctx.clone()));
                        Matcher { ctorv: arm.ctorv.clone(), vars: arm.vars.clone(), body }
                    })
                    .collect();
                Match { scrut, arms }.into()
            }
            TermComputation::Comatch(Comatch { arms }) => {
                let arms = arms
                    .iter()
                    .map(|arm| {
                        let span = arm.body.span();
                        let body = span.make_rc(arm.body.lift(m.clone(), ctx.clone()));
                        Comatcher { dtorv: arm.dtorv.clone(), body }
                    })
                    .collect();
                Comatch { arms }.into()
            }
            TermComputation::Dtor(Dtor { body, dtorv }) => {
                let span = body.span();
                let body = span.make_rc(body.lift(m, ctx.clone()));
                Dtor { body, dtorv: dtorv.clone() }.into()
            }
            TermComputation::BeginBlock(BeginBlock { monad, body }) => {
                let span = monad.span();
                let monad = span.make_rc(monad.lift(m.clone(), ctx.clone()));
                let span = body.span();
                let body = span.make_rc(body.lift(m, ctx.clone()));
                BeginBlock { monad, body }.into()
            }
            TermComputation::TyAbsTerm(Abs { param, body }) => {
                let span = body.span();
                let body = span.make_rc(body.lift(m, ctx.clone()));
                Abs { param: param.clone(), body }.into()
            }
            TermComputation::TyAppTerm(App { body, arg }) => {
                let span = body.span();
                let body = span.make_rc(body.lift(m, ctx.clone()));
                App { body, arg: arg.clone() }.into()
            }
            TermComputation::MatchPack(MatchPack { scrut, tvar, var, body }) => {
                let span = scrut.span();
                let scrut = span.make_rc(scrut.lift(m.clone(), ctx.clone()));
                let span = body.span();
                let body = span.make_rc(body.lift(m, ctx.clone()));
                MatchPack { scrut, tvar: tvar.clone(), var: var.clone(), body }.into()
            }
        }
    }
}
