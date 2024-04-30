use crate::prelude::*;
use crate::statics::{err::*, syntax::*, tyck::*};

pub trait MonadTransType {
    fn lift(&self, ty_m: Type, ctx: Ctx, span: &Span) -> Result<Type, TyckError>;
    fn alg(&self, m: RcValue, ctx: Ctx, span: &Span) -> Result<TermComputation, TyckError>;
}

impl MonadTransType for Type {
    fn lift(&self, ty_m: Type, ctx: Ctx, span: &Span) -> Result<Type, TyckError> {
        let synty = match &self.synty {
            SynType::TypeAbs(_) => {
                // Type abstraction would not be of kind "CType"
                Err(ctx.err(
                    span,
                    TyckErrorItem::KindMismatch {
                        context: format!("begin-block-lift"),
                        expected: KindBase::CType.into(),
                        found: span.make(self.clone()).syn(ctx.clone())?.into(),
                    },
                ))?
            }
            SynType::TypeApp(TypeApp { tvar, args: old_args }) => {
                let mut args = Vec::new();
                for arg in old_args {
                    args.push(span.make_rc(arg.inner_ref().lift(
                        ty_m.clone(),
                        ctx.clone(),
                        span,
                    )?));
                }
                let tvar = match tvar {
                    NeutralVar::Var(tv) if tv.name() == "Ret" => {
                        // Substitute if it's Ret
                        return ty_m.apply(args, &ctx);
                    }
                    _ => tvar.clone(),
                };
                SynType::TypeApp(TypeApp { tvar, args })
            }
            SynType::Arrow(Arrow(ty_in, ty_out)) => {
                let ty_in =
                    span.make_rc(ty_in.inner_ref().lift(ty_m.clone(), ctx.clone(), span)?);
                let ty_out = span.make_rc(ty_out.inner_ref().lift(ty_m, ctx, span)?);
                SynType::Arrow(Arrow(ty_in, ty_out))
            }
            SynType::Forall(Forall { param, ty }) => {
                // Hack: shadowing not considered
                let param = param.clone();
                let ty = span.make_rc(ty.inner_ref().lift(ty_m, ctx.clone(), span)?);
                SynType::Forall(Forall { param, ty })
            }
            SynType::Exists(Exists { param, ty }) => {
                // Hack: shadowing not considered
                let param = param.clone();
                let ty = span.make_rc(ty.inner_ref().lift(ty_m, ctx.clone(), span)?);
                SynType::Forall(Forall { param, ty })
            }
            SynType::AbstVar(_) | SynType::Hole(_) => self.synty.clone(),
        };
        Ok(Type { synty })
    }

    fn alg(&self, m: RcValue, mut ctx: Ctx, span: &Span) -> Result<TermComputation, TyckError> {
        let mk_var_value = |name| {
            let var = TermV::new(name, span.clone());
            (var.clone(), span.make_rc(TermValue::Var(var)))
        };
        let (var_f, value_f) = mk_var_value(format!("$f"));
        let (var_m, value_m) = mk_var_value(format!("$m"));
        let dtorv_binda = DtorV::new(format!("bindA"), span.clone());
        let alg_template = |body| -> TermComputation {
            // fn .bindA m f -> ...
            Comatch {
                arms: vec![Comatcher {
                    dtorv: dtorv_binda.clone(),
                    body: span.make_rc(
                        Abs {
                            param: var_m.clone(),
                            body: span.make_rc(Abs { param: var_f.clone(), body }.into()),
                        }
                        .into(),
                    ),
                }],
            }
            .into()
        };
        match &self.synty {
            SynType::TypeAbs(_) => unreachable!(),
            SynType::TypeApp(app) => {
                // using return guard pattern...
                let NeutralVar::Var(tvar) = app.tvar.clone() else {
                    unreachable!("Encounter abstract type on generating algebra of relative monad")
                };
                if let Some(_ty_v) = app.elim_ret_syntax() {
                    // Ret: use monad bind
                    // fn .bindA -> ! m .bind
                    let monad = span.make_rc(Force(m.clone()).into());
                    let mbind = span.make_rc(
                        Dtor { body: monad, dtorv: DtorV::new(format!("bind"), span.clone()) }
                            .into(),
                    );
                    // Hack: ignore adding type arguments since type checking is done
                    let alg = Comatch {
                        arms: vec![Comatcher { dtorv: dtorv_binda.clone(), body: mbind }],
                    }
                    .into();
                    return Ok(alg);
                }
                if let Some(coda) = ctx.codata_env.get(&tvar) {
                    // Codata: penetrate into the comatch arms
                    for (tv, kd) in &coda.params {
                        ctx.type_ctx.insert(tv.clone(), kd.inner_clone());
                    }
                    let mut arms = Vec::new();
                    for CodataBr { dtorv, ty } in &coda.dtors {
                        let dtorv = dtorv.clone();
                        let span = dtorv.span();
                        let alg_br =
                            span.make_rc(ty.inner_ref().alg(m.clone(), ctx.clone(), span)?);
                        let alg_app = span.make_rc(
                            App {
                                body: span.make_rc(
                                    Dtor { body: alg_br, dtorv: dtorv_binda.clone() }.into(),
                                ),
                                arg: value_m.clone(),
                            }
                            .into(),
                        );
                        let thunk_fn_dtor: RcValue = span.make_rc(
                            Thunk({
                                let (var_arg, value_arg) = mk_var_value(format!("$arg"));
                                let force_f = span.make_rc(Force(value_f.clone()).into());
                                let dtorv = dtorv.clone();
                                let app_arg =
                                    span.make_rc(App { body: force_f, arg: value_arg }.into());
                                let body = span.make_rc(Dtor { body: app_arg, dtorv }.into());
                                span.make_rc(Abs { param: var_arg, body }.into())
                            })
                            .into(),
                        );
                        let body = span.make_rc(App { body: alg_app, arg: thunk_fn_dtor }.into());
                        arms.push(Comatcher { dtorv, body });
                    }
                    let comatch = span.make_rc(Comatch { arms }.into());
                    return Ok(alg_template(comatch));
                }
                // there shouldn't be any other cases for type applications
                unreachable!()
            }
            SynType::Arrow(Arrow(_ty_in, ty_out)) => {
                // Arrow: penetrate into the fn abstraction
                let (var_fn_arg, value_fn_arg) = mk_var_value(format!("$fn_arg"));
                let alg_body =
                    span.make_rc(ty_out.inner_ref().alg(m.clone(), ctx.clone(), span)?);
                let alg_app = span.make_rc(
                    App {
                        body: span
                            .make_rc(Dtor { body: alg_body, dtorv: dtorv_binda.clone() }.into()),
                        arg: value_m.clone(),
                    }
                    .into(),
                );
                let thunk_fn_app: RcValue = span.make_rc(
                    Thunk({
                        let (var_arg, value_arg) = mk_var_value(format!("$arg"));
                        let force_f = span.make_rc(Force(value_f.clone()).into());
                        let app_arg = span.make_rc(App { body: force_f, arg: value_arg }.into());
                        let body = span.make_rc(App { body: app_arg, arg: value_fn_arg }.into());
                        span.make_rc(Abs { param: var_arg, body }.into())
                    })
                    .into(),
                );
                let body = span.make_rc(App { body: alg_app, arg: thunk_fn_app }.into());
                let abs = span.make_rc(Abs { param: var_fn_arg, body }.into());
                Ok(alg_template(abs))
            }
            SynType::Forall(Forall { param: _, ty }) => {
                // Hack: since types are erased, we can ignore the type parameter
                ty.inner_ref().alg(m, ctx, span)
            }
            // there shouldn't be any other cases since we're only dealing with:
            // && concrete types
            // && types of kind CType
            SynType::Exists(_) => unreachable!(),
            SynType::AbstVar(_) => unreachable!(),
            SynType::Hole(_) => unreachable!(),
        }
    }
}
