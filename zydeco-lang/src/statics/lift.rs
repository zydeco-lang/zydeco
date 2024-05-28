use crate::prelude::*;
use crate::statics::{err::*, syntax::*, tyck::*};

/// monad transformers
mod r#type;
mod term;

pub trait MonadTransType {
    fn lift(&self, ty_m: Type, ctx: Ctx, span: &Span) -> Result<Type, TyckError>;
    fn alg(&self, m: RcValue, ctx: Ctx, span: &Span) -> Result<TermComputation, TyckError>;
}

pub trait MonadTransTerm: Clone {
    type Out;
    fn lift(&self, m: RcValue, ctx: Ctx) -> Self::Out;
}

pub trait MonadTrans {
    fn trans(self, ctx: Ctx) -> Self;
}

impl MonadTrans for Sp<Program> {
    fn trans(self, ctx: Ctx) -> Self {
        let Program { module, entry } = self.inner;
        let module = module.trans(ctx.clone());
        let entry = (*rc!(entry).trans(ctx)).clone();
        self.info.make(Program { module, entry })
    }
}

impl MonadTrans for Sp<Module> {
    fn trans(self, ctx: Ctx) -> Self {
        let Module { name, data, codata, alias, define: old_define, define_ext } = self.inner;
        let mut define = Vec::new();
        for DeclSymbol { inner: Define { name, def }, public, external } in old_define {
            let def = def.trans(ctx.clone());
            define.push(DeclSymbol { inner: Define { name, def }, public, external });
        }
        self.info.make(Module { name, data, codata, alias, define, define_ext })
    }
}

impl MonadTrans for RcValue {
    fn trans(self, ctx: Ctx) -> Self {
        let span = self.span().clone();
        match self.inner_ref() {
            | TermValue::Annotation(Annotation { term, ty }) => {
                let term = term.clone().trans(ctx.clone());
                span.make_rc(Annotation { term, ty: ty.clone() }.into())
            }
            | TermValue::Var(x) => {
                let x = x.clone();
                span.make_rc(TermValue::Var(x))
            }
            | TermValue::Thunk(Thunk(body)) => {
                let body = body.clone().trans(ctx.clone());
                span.make_rc(Thunk(body).into())
            }
            | TermValue::Ctor(Ctor { ctorv, args }) => {
                let args = args.iter().map(|v| v.clone().trans(ctx.clone()));
                span.make_rc(Ctor { ctorv: ctorv.clone(), args: args.collect() }.into())
            }
            | TermValue::Literal(lit) => {
                let lit = lit.clone();
                span.make_rc(TermValue::Literal(lit))
            }
            | TermValue::Pack(Pack { ty, body }) => {
                let body = body.clone().trans(ctx.clone());
                span.make_rc(Pack { ty: ty.clone(), body }.into())
            }
        }
    }
}

impl MonadTrans for RcComp {
    fn trans(self, ctx: Ctx) -> Self {
        let span = self.span().clone();
        // find only begin-block and call lift
        match self.inner_ref() {
            | TermComputation::Annotation(Annotation { term, ty }) => {
                let term = term.clone().trans(ctx.clone());
                let ty = ty.clone();
                span.make_rc(Annotation { term, ty }.into())
            }
            | TermComputation::Abs(Abs { param, body }) => {
                let body = body.clone().trans(ctx.clone());
                span.make_rc(Abs { param: param.clone(), body }.into())
            }
            | TermComputation::App(App { body, arg }) => {
                let body = body.clone().trans(ctx.clone());
                let arg = arg.clone().trans(ctx.clone());
                span.make_rc(App { body, arg }.into())
            }
            | TermComputation::Ret(Ret(body)) => {
                let body = body.clone().trans(ctx.clone());
                span.make_rc(Ret(body).into())
            }
            | TermComputation::Do(Do { var, comp, body }) => {
                let comp = comp.clone().trans(ctx.clone());
                let body = body.clone().trans(ctx.clone());
                span.make_rc(Do { var: var.clone(), comp, body }.into())
            }
            | TermComputation::Force(Force(body)) => {
                let body = body.clone().trans(ctx.clone());
                span.make_rc(Force(body).into())
            }
            | TermComputation::Let(Let { var, def, body }) => {
                let def = def.clone().trans(ctx.clone());
                let body = body.clone().trans(ctx.clone());
                span.make_rc(Let { var: var.clone(), def, body }.into())
            }
            | TermComputation::Rec(Rec { var, body }) => {
                let body = body.clone().trans(ctx.clone());
                span.make_rc(Rec { var: var.clone(), body }.into())
            }
            | TermComputation::Match(Match { scrut, arms }) => {
                let scrut = scrut.clone().trans(ctx.clone());
                let arms = arms.iter().map(|arm| {
                    let Matcher { ctorv, vars, body } = arm;
                    let body = body.clone().trans(ctx.clone());
                    Matcher { ctorv: ctorv.clone(), vars: vars.clone(), body }
                });
                span.make_rc(Match { scrut, arms: arms.collect() }.into())
            }
            | TermComputation::Comatch(Comatch { arms }) => {
                let arms = arms.iter().map(|arm| {
                    let Comatcher { dtorv, body } = arm;
                    let body = body.clone().trans(ctx.clone());
                    Comatcher { dtorv: dtorv.clone(), body }
                });
                span.make_rc(Comatch { arms: arms.collect() }.into())
            }
            | TermComputation::Dtor(Dtor { body, dtorv }) => {
                let body = body.clone().trans(ctx.clone());
                span.make_rc(Dtor { body, dtorv: dtorv.clone() }.into())
            }
            | TermComputation::BeginBlock(BeginBlock { monad, body }) => {
                span.make_rc(body.clone().lift(monad.clone(), ctx))
            }
            | TermComputation::TyAbsTerm(Abs { param, body }) => {
                let body = body.clone().trans(ctx.clone());
                span.make_rc(Abs { param: param.clone(), body }.into())
            }
            | TermComputation::TyAppTerm(App { body, arg }) => {
                let body = body.clone().trans(ctx.clone());
                let arg = arg.clone();
                span.make_rc(App { body, arg }.into())
            }
            | TermComputation::MatchPack(MatchPack { scrut, tvar, var, body }) => {
                let scrut = scrut.clone().trans(ctx.clone());
                let body = body.clone().trans(ctx.clone());
                span.make_rc(MatchPack { scrut, tvar: tvar.clone(), var: var.clone(), body }.into())
            }
        }
    }
}
