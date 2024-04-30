use super::*;

// Note: no type level transformation is performed here since we're gonna remove them right after anyway

impl MonadTransTerm for TermValue {
    fn lift(&self, m: &TermValue) -> Self {
        match self {
            TermValue::Annotation(Annotation { term, ty }) => {
                let span = term.span();
                let term = span.make_rc(term.inner_ref().lift(m));
                Annotation { term, ty: ty.clone() }.into()
            }
            TermValue::Var(x) => x.clone().into(),
            TermValue::Thunk(Thunk(body)) => {
                let span = body.span();
                let body = span.make_rc(body.inner_ref().lift(m));
                Thunk(body).into()
            }
            TermValue::Ctor(Ctor { ctorv, args }) => {
                let args = args
                    .iter()
                    .map(|v| {
                        let span = v.span();
                        span.make_rc(v.inner_ref().lift(m))
                    })
                    .collect();
                Ctor { ctorv: ctorv.clone(), args }.into()
            }
            TermValue::Literal(lit) => lit.clone().into(),
            TermValue::Pack(Pack { ty, body }) => {
                let span = body.span();
                let body = span.make_rc(body.inner_ref().lift(m));
                Pack { ty: ty.clone(), body }.into()
            }
        }
    }
}

impl MonadTransTerm for TermComputation {
    fn lift(&self, m: &TermValue) -> Self {
        match self {
            TermComputation::Annotation(Annotation { term, ty }) => {
                let span = term.span();
                let term = span.make_rc(term.inner_ref().lift(m));
                Annotation { term, ty: ty.clone() }.into()
            }
            TermComputation::Abs(Abs { param, body }) => {
                let span = body.span();
                let body = span.make_rc(body.inner_ref().lift(m));
                Abs { param: param.clone(), body }.into()
            }
            TermComputation::App(App { body, arg }) => {
                let span = body.span();
                let body = span.make_rc(body.inner_ref().lift(m));
                let arg = span.make_rc(arg.inner_ref().lift(m));
                App { body, arg }.into()
            }
            TermComputation::Ret(Ret(arg)) => {
                // the return case uses the monad's return
                let span = arg.span();
                let arg = span.make_rc(arg.inner_ref().lift(m));
                let monad = span.make_rc(Force(span.make_rc(m.clone())).into());
                let mret = span.make_rc(
                    Dtor { body: monad, dtorv: DtorV::new(format!("return"), span.clone()) }.into(),
                );
                App { body: mret, arg }.into()
            }
            TermComputation::Force(Force(body)) => {
                let span = body.span();
                let body = span.make_rc(body.inner_ref().lift(m));
                Force(body).into()
            }
            TermComputation::Let(Let { var, def, body }) => {
                let span = def.span();
                let def = span.make_rc(def.inner_ref().lift(m));
                let span = body.span();
                let body = span.make_rc(body.inner_ref().lift(m));
                Let { var: var.clone(), def, body }.into()
            }
            TermComputation::Do(Do { var, comp, body }) => {
                // Todo: ...
                let span = comp.span();
                let comp = span.make_rc(comp.inner_ref().lift(m));
                let span = body.span();
                let body = span.make_rc(body.inner_ref().lift(m));
                Do { var: var.clone(), comp, body }.into()
            }
            TermComputation::Rec(Rec { var, body }) => {
                let span = body.span();
                let body = span.make_rc(body.inner_ref().lift(m));
                Rec { var: var.clone(), body }.into()
            }
            TermComputation::Match(Match { scrut, arms }) => {
                let span = scrut.span();
                let scrut = span.make_rc(scrut.inner_ref().lift(m));
                let arms = arms
                    .iter()
                    .map(|arm| {
                        let span = arm.body.span();
                        let body = span.make_rc(arm.body.inner_ref().lift(m));
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
                        let body = span.make_rc(arm.body.inner_ref().lift(m));
                        Comatcher { dtorv: arm.dtorv.clone(), body }
                    })
                    .collect();
                Comatch { arms }.into()
            }
            TermComputation::Dtor(Dtor { body, dtorv }) => {
                let span = body.span();
                let body = span.make_rc(body.inner_ref().lift(m));
                Dtor { body, dtorv: dtorv.clone() }.into()
            }
            TermComputation::BeginBlock(BeginBlock { monad, body }) => {
                let span = monad.span();
                let monad = span.make_rc(monad.inner_ref().lift(m));
                let span = body.span();
                let body = span.make_rc(body.inner_ref().lift(m));
                BeginBlock { monad, body }.into()
            }
            TermComputation::TyAbsTerm(Abs { param, body }) => {
                let span = body.span();
                let body = span.make_rc(body.inner_ref().lift(m));
                Abs { param: param.clone(), body }.into()
            }
            TermComputation::TyAppTerm(App { body, arg }) => {
                let span = body.span();
                let body = span.make_rc(body.inner_ref().lift(m));
                App { body, arg: arg.clone() }.into()
            }
            TermComputation::MatchPack(MatchPack { scrut, tvar, var, body }) => {
                let span = scrut.span();
                let scrut = span.make_rc(scrut.inner_ref().lift(m));
                let span = body.span();
                let body = span.make_rc(body.inner_ref().lift(m));
                MatchPack { scrut, tvar: tvar.clone(), var: var.clone(), body }.into()
            }
        }
    }
}
