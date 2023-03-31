use std::collections::{HashMap, HashSet};

use super::*;

impl TypeCheck for Span<TermComputation> {
    type Ctx = Ctx;
    type Out = Type;
    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        let span = self.span();
        Ok(match self.inner_ref() {
            TermComputation::TermAnn(TermAnn { term, ty }) => {
                ty.ana(Kind::CType, ctx.clone())?;
                Step::AnaMode((ctx, term), ty.inner_ref().clone())
            }
            TermComputation::Ret(Ret(v)) => {
                // Err(span
                // .make(NeedAnnotation { content: format!("ret") }))?
                let ty_body: Type = v.syn(ctx.clone())?;
                span.make(ty_body.clone()).ana(Kind::VType, ctx)?;
                Step::Done(Type::make_ret(rc!(span.make(ty_body))))
            }
            TermComputation::Force(Force(v)) => {
                let ty_val = v.syn(ctx.clone())?;
                let SynType::TypeApp(ty_app) = &ty_val.synty else {
                    Err(span.make(TypeExpected {
                        context: format!("force"),
                        expected: format!("{{b}}"),
                        found: ty_val,
                    }))?
                };
                let ty_body = ty_app.elim_thunk().ok_or_else(|| {
                    span.make(TypeExpected {
                        context: format!("force"),
                        expected: format!("{{a}}"),
                        found: ty_app.clone().into(),
                    })
                })?;
                span.make(ty_body.to_owned()).ana(Kind::CType, ctx)?;
                Step::Done(ty_body)
            }
            TermComputation::Let(Let { var, def, body }) => {
                let ty_def = def.syn(ctx.clone())?;
                span.make(ty_def.clone()).ana(Kind::VType, ctx.clone())?;
                ctx.term_ctx.insert(var.to_owned(), ty_def.clone());
                Step::SynMode((ctx, body))
            }
            TermComputation::Do(Do { var, comp, body }) => {
                let ty_comp = comp.syn(ctx.clone())?;
                span.make(ty_comp.clone()).ana(Kind::CType, ctx.clone())?;
                let SynType::TypeApp(ty_app) = &ty_comp.synty else {
                    Err(span.make(TypeExpected {
                        context: format!("do"),
                        expected: format!("ret a"),
                        found: ty_comp,
                    }))?
                };
                let ty_val = ty_app.elim_ret().ok_or_else(|| {
                    span.make(TypeExpected {
                        context: format!("do"),
                        expected: format!("ret a"),
                        found: ty_comp.clone(),
                    })
                })?;
                ctx.term_ctx.insert(var.to_owned(), ty_val);
                Step::SynMode((ctx, body))
            }
            TermComputation::Rec(Rec { var: _, body: _ }) => {
                Err(span.make(NeedAnnotation { content: format!("rec") }))?
            }
            TermComputation::Match(Match { scrut, arms }) => {
                let ty_scrut = scrut.syn(ctx.clone())?;
                span.make(ty_scrut.clone()).ana(Kind::VType, ctx.clone())?;
                let SynType::TypeApp(ty_app) = ty_scrut.synty else {
                    Err(span.make(TypeExpected {
                        context: format!("match"),
                        expected: format!("data type"),
                        found: ty_scrut,
                    }))?
                };
                let tvar = ty_app.tvar;
                let Data { name, params, ctors } =
                    ctx.data_env.get(&tvar).cloned().ok_or_else(|| {
                        span.make(
                            NameResolveError::UnboundTypeVariable { tvar }
                                .into(),
                        )
                    })?;
                // arity check on data type
                let diff = Env::init(&params, &ty_app.args, || {
                    span.make(ArityMismatch {
                        context: format!("data type `{}` instiantiation", name),
                        expected: params.len(),
                        found: ty_app.args.len(),
                    })
                })?;
                let ctors: HashMap<_, _> = ctors
                    .into_iter()
                    .map(|DataBr(ctor, tys)| (ctor, tys))
                    .collect();
                let mut unexpected = Vec::new();
                let mut ctorv_set_arm: HashSet<CtorV> = HashSet::new();
                let mut ty_arms = Vec::new();
                for Matcher { ctor, vars, body } in arms {
                    let Some(tys) = ctors.get(ctor) else {
                        unexpected.push(ctor.to_owned());
                        continue;
                    };
                    ctorv_set_arm.insert(ctor.to_owned());
                    let tys = tys.into_iter().map(|ty| {
                        ty.inner_ref().to_owned().subst(diff.clone())
                    });
                    let mut ctx = ctx.clone();
                    for (var, ty) in vars.iter().zip(tys) {
                        ctx.term_ctx.insert(var.to_owned(), ty?);
                    }
                    let ty = body.syn(ctx.clone())?;
                    let span = body.span();
                    span.make(ty.clone()).ana(Kind::CType, ctx)?;
                    ty_arms.push(ty);
                }
                let ctorv_set_data: HashSet<CtorV> =
                    ctors.keys().cloned().collect();
                let missing: Vec<_> = ctorv_set_data
                    .difference(&ctorv_set_arm)
                    .cloned()
                    .collect();
                bool_test(unexpected.is_empty() && missing.is_empty(), || {
                    span.make(InconsistentMatchers { unexpected, missing })
                })?;
                // branch consistency check
                let mut ty_opt: Option<Type> = None;
                for ty in &ty_arms {
                    if let Some(ty_opt) = &ty_opt {
                        ty_opt.eqv(ty, ctx.clone(), || {
                            span.make(InconsistentBranches(ty_arms.clone()))
                        })?;
                    } else {
                        ty_opt = Some(ty.clone());
                    }
                }
                // empty match
                let Some(ty) = ty_opt else {
                            Err(span.make(InconsistentBranches(vec![])))?
                        };
                Step::Done(ty)
            }
            TermComputation::CoMatch(_) => {
                Err(span.make(NeedAnnotation { content: format!("comatch") }))?
            }
            TermComputation::Dtor(Dtor { body, dtor, args }) => {
                let ty_body = body.syn(ctx.clone())?;
                let SynType::TypeApp(ty_app) = ty_body.synty else {
                    Err(span.make(TypeExpected {
                        context: format!("dtor"),
                        expected: format!("codata type"),
                        found: ty_body,
                    }))?
                };
                let tvar = ty_app.tvar;
                let Codata { name, params, dtors } =
                    ctx.coda_env.get(&tvar).cloned().ok_or_else(|| {
                        span.make(
                            NameResolveError::UnboundTypeVariable { tvar }
                                .into(),
                        )
                    })?;
                // arity check on codata type
                let diff = Env::init(&params, &ty_app.args, || {
                    span.make(ArityMismatch {
                        context: format!(
                            "codata type `{}` instiantiation",
                            name
                        ),
                        expected: params.len(),
                        found: ty_app.args.len(),
                    })
                })?;
                let CodataBr(_, tys, ty) = dtors
                    .into_iter()
                    .find(|CodataBr(dtorv, _, _)| dtorv == dtor)
                    .ok_or_else(|| {
                        body.span().make(
                            NameResolveError::UnknownDestructor {
                                context: format!("codata type `{}`", name),
                                dtor: dtor.clone(),
                            }
                            .into(),
                        )
                    })?;
                bool_test(args.len() == tys.len(), || {
                    span.make(ArityMismatch {
                        context: format!("dtor"),
                        expected: tys.len(),
                        found: args.len(),
                    })
                })?;
                for (arg, ty) in args.iter().zip(tys.iter()) {
                    arg.ana(
                        ty.inner_ref().to_owned().subst(diff.clone())?,
                        ctx.clone(),
                    )?;
                }
                Step::Done(ty.inner_ref().to_owned().subst(diff)?)
            }
            TermComputation::TypAbs(_) => {
                Err(span.make(NeedAnnotation { content: format!("typabs") }))?
            }
            TermComputation::TypApp(TypApp { body, arg }) => {
                let ty_body = body.syn(ctx.clone())?;
                let SynType::Forall(Forall { param, kd, ty }) = ty_body.synty else {
                    Err(span.make(TypeExpected {
                        context: format!("term-typ-application"),
                        expected: format!("forall,"),
                        found: ty_body,
                    }))?
                };
                arg.ana(kd.clone(), ctx.clone())?;
                let diff = Env::init(&[(param, kd)], &[arg.clone()], || {
                    span.make(ArityMismatch {
                        context: format!("typapp"),
                        expected: 1,
                        found: 1,
                    })
                })?;
                Step::Done(ty.inner_ref().clone().subst(diff)?)
            }
            TermComputation::MatchPack(MatchPack {
                scrut,
                tvar,
                var,
                body,
            }) => {
                let ty_scrut = scrut.syn(ctx.clone())?;
                let SynType::Exists(Exists { param, kd, ty }) = &ty_scrut.synty else {
                        Err(span.make(TypeExpected {
                            context: format!("match-pack"),
                            expected: format!("exists"),
                            found: ty_scrut,
                        }))?
                    };
                ctx.type_ctx.insert(tvar.clone(), kd.clone().into());
                let ty = ty.inner_ref().clone().subst(Env::from_iter([(
                    param.clone(),
                    tvar.clone().into(),
                )]))?;
                ctx.term_ctx.insert(var.clone(), ty);
                let ty_body = body.syn(ctx.clone())?;
                span.make(ty_body.clone()).ana(Kind::CType, ctx)?;
                Step::Done(ty_body)
            }
        })
    }
    fn ana_step(
        &self, typ: Self::Out, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        if let SynType::Hole(_) = typ.synty {
            return Ok(Step::SynMode((ctx, self)));
        }
        let span = self.span();
        span.make(typ.clone()).ana(Kind::CType, ctx.clone())?;
        Ok(match self.inner_ref() {
            TermComputation::TermAnn(TermAnn { term, ty }) => {
                let ty_lub = Type::lub(
                    ty.inner_ref().clone(),
                    typ.clone(),
                    ctx.clone(),
                    || {
                        span.make(Subsumption {
                            sort: "computation annotation",
                            tycker_src: format!(
                                "{}:{}:{}",
                                file!(),
                                line!(),
                                column!()
                            ),
                        })
                    },
                )?;
                Step::AnaMode((ctx, term), ty_lub)
            }
            TermComputation::Ret(Ret(v)) => {
                let SynType::TypeApp(ty_app) = &typ.synty else {
                    Err(span.make(TypeExpected {
                        context: format!("ret"),
                        expected: format!("ret a"),
                        found: typ,
                    }))?
                };
                let ty_body = ty_app.elim_ret().ok_or_else(|| {
                    span.make(TypeExpected {
                        context: format!("ret"),
                        expected: format!("ret a"),
                        found: typ.clone(),
                    })
                })?;
                v.ana(ty_body, ctx)?;
                Step::Done(typ)
            }
            TermComputation::Force(Force(v)) => {
                span.make(typ.clone()).ana(Kind::CType, ctx.clone())?;
                v.ana(Type::make_thunk(rc!(span.make(typ.clone()))), ctx)?;
                Step::Done(typ)
            }
            TermComputation::Let(Let { var, def, body }) => {
                let ty_def = def.syn(ctx.clone())?;
                span.make(ty_def.clone()).ana(Kind::VType, ctx.clone())?;
                ctx.term_ctx.insert(var.to_owned(), ty_def.clone());
                Step::AnaMode((ctx, body), typ)
            }
            TermComputation::Do(Do { var, comp, body }) => {
                let ty_comp = comp.syn(ctx.clone())?;
                span.make(ty_comp.clone()).ana(Kind::CType, ctx.clone())?;
                let SynType::TypeApp(ty_app) = &ty_comp.synty else {
                    Err(span.make(TypeExpected {
                        context: format!("do"),
                        expected: format!("ret a"),
                        found: ty_comp,
                    }))?
                };
                let ty_val = ty_app.elim_ret().ok_or_else(|| {
                    span.make(TypeExpected {
                        context: format!("do"),
                        expected: format!("ret a"),
                        found: ty_comp.clone(),
                    })
                })?;
                ctx.term_ctx.insert(var.to_owned(), ty_val);
                Step::AnaMode((ctx, body), typ)
            }
            TermComputation::Rec(Rec { var, body }) => {
                ctx.term_ctx.insert(
                    var.to_owned(),
                    Type::make_thunk(rc!(span.make(typ.clone()))),
                );
                Step::AnaMode((ctx, body), typ)
            }
            TermComputation::Match(Match { scrut, arms }) => {
                let ty_scrut = scrut.syn(ctx.clone())?;
                span.make(ty_scrut.clone()).ana(Kind::VType, ctx.clone())?;
                let SynType::TypeApp(ty_app) = ty_scrut.synty else {
                    Err(span.make(TypeExpected {
                        context: format!("match"),
                        expected: format!("data type"),
                        found: ty_scrut,
                    }))?
                };
                let tvar = ty_app.tvar;
                let Data { name, params, ctors } =
                    ctx.data_env.get(&tvar).cloned().ok_or_else(|| {
                        span.make(
                            NameResolveError::UnboundTypeVariable { tvar }
                                .into(),
                        )
                    })?;
                // arity check on data type
                let diff = Env::init(&params, &ty_app.args, || {
                    span.make(ArityMismatch {
                        context: format!("data type `{}` instiantiation", name),
                        expected: params.len(),
                        found: ty_app.args.len(),
                    })
                })?;
                let ctors: HashMap<_, _> = ctors
                    .into_iter()
                    .map(|DataBr(ctor, tys)| (ctor, tys))
                    .collect();
                let mut unexpected = Vec::new();
                let mut ctorv_set_arm: HashSet<CtorV> = HashSet::new();
                for Matcher { ctor, vars, body } in arms {
                    let Some(tys) = ctors.get(ctor) else {
                unexpected.push(ctor.to_owned());
                continue;
            };
                    ctorv_set_arm.insert(ctor.to_owned());
                    let tys = tys.into_iter().map(|ty| {
                        ty.inner_ref().to_owned().subst(diff.clone())
                    });
                    let mut ctx = ctx.clone();
                    for (var, ty) in vars.iter().zip(tys) {
                        ctx.term_ctx.insert(var.to_owned(), ty?);
                    }
                    body.ana(typ.clone(), ctx.clone())?;
                }
                let ctorv_set_data: HashSet<CtorV> =
                    ctors.keys().cloned().collect();
                let missing: Vec<_> = ctorv_set_data
                    .difference(&ctorv_set_arm)
                    .cloned()
                    .collect();
                bool_test(unexpected.is_empty() && missing.is_empty(), || {
                    span.make(InconsistentMatchers { unexpected, missing })
                })?;
                Step::Done(typ)
            }
            TermComputation::CoMatch(CoMatch { arms }) => {
                let SynType::TypeApp(ty_app) = &typ.synty else {
                    Err(span.make(TypeExpected {
                        context: format!("comatch"),
                        expected: format!("codata type"),
                        found: typ,
                    }))?
                };
                let tvar = &ty_app.tvar;
                let Codata { name, params, dtors } =
                    ctx.coda_env.get(&tvar).cloned().ok_or_else(|| {
                        span.make(
                            NameResolveError::UnboundTypeVariable {
                                tvar: tvar.clone(),
                            }
                            .into(),
                        )
                    })?;
                // arity check on codata type
                let diff = Env::init(&params, &ty_app.args, || {
                    span.make(ArityMismatch {
                        context: format!(
                            "codata type `{}` instantiation",
                            name
                        ),
                        expected: params.len(),
                        found: ty_app.args.len(),
                    })
                })?;
                let dtors: HashMap<_, _> = dtors
                    .into_iter()
                    .map(|CodataBr(dtor, tys, ty)| (dtor, (tys, ty)))
                    .collect();
                let mut unexpected = Vec::new();
                let mut dtorv_set_arm: HashSet<DtorV> = HashSet::new();
                for CoMatcher { dtor, vars, body } in arms {
                    let Some((tys, ty)) = dtors.get(dtor).cloned() else {
                        unexpected.push(dtor.to_owned());
                        continue;
                    };
                    dtorv_set_arm.insert(dtor.to_owned());
                    let tys = tys.into_iter().map(|ty| {
                        ty.inner_ref().to_owned().subst(diff.clone())
                    });
                    let ty = ty.inner_ref().to_owned().subst(diff.clone());
                    let mut ctx = ctx.clone();
                    for (var, ty) in vars.iter().zip(tys) {
                        ctx.term_ctx.insert(var.to_owned(), ty?);
                    }
                    body.ana(ty?, ctx)?;
                }
                let dtorv_set_coda: HashSet<DtorV> =
                    dtors.keys().cloned().collect();
                let missing: Vec<_> = dtorv_set_coda
                    .difference(&dtorv_set_arm)
                    .cloned()
                    .collect();
                bool_test(unexpected.is_empty() && missing.is_empty(), || {
                    span.make(InconsistentCoMatchers { unexpected, missing })
                })?;
                Step::Done(typ)
            }
            TermComputation::TypAbs(TypAbs { tvar, kd, body }) => {
                let SynType::Forall(Forall { param, kd: kd_, ty }) = &typ.synty else {
                    Err(span.make(TypeExpected {
                        context: format!("typabs"),
                        expected: format!("forall"),
                        found: typ.clone(),
                    }))?
                };
                bool_test(kd == kd_, || {
                    span.make(KindMismatch {
                        context: format!("typabs"),
                        expected: kd_.clone(),
                        found: kd.clone(),
                    })
                })?;
                let abst_var = ctx.fresh(kd.clone());
                ctx.type_env.insert(tvar.clone(), abst_var.clone().into());
                ty.inner_ref().clone().subst(Env::from_iter([(
                    param.clone(),
                    abst_var.into(),
                )]))?;
                body.ana(ty.inner_ref().clone(), ctx)?;
                Step::Done(typ)
            }
            TermComputation::Dtor(_)
            | TermComputation::TypApp(_)
            | TermComputation::MatchPack(_) => {
                // subsumption
                let typ_syn = self.syn(ctx.clone())?;
                let typ_lub = Type::lub(typ, typ_syn, ctx, || {
                    span.make(Subsumption {
                        sort: "computation",
                        tycker_src: format!(
                            "{}:{}:{}",
                            file!(),
                            line!(),
                            column!()
                        ),
                    })
                })?;
                Step::Done(typ_lub)
            }
        })
    }
}
