use std::collections::{HashMap, HashSet};

use super::*;

impl TypeCheck for Span<TermComputation> {
    type Ctx = Ctx;
    type Out = Type;
    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        ctx.trace.push(Frame {
            tycker_src: format!("{}:{}:{}", file!(), line!(), column!()),
            sort: format!("synthezing computation"),
            term: format!("{}", self.inner_ref().fmt_truncate(40)),
            info: self.span().clone(),
        });
        let span = self.span();
        Ok(match self.inner_ref() {
            TermComputation::Annotation(Annotation { term, ty }) => {
                ty.ana(KindBase::CType.into(), ctx.clone())?;
                Step::AnaMode((ctx, term), ty.inner_clone())
            }
            TermComputation::Ret(_) => {
                Err(ctx.err(span, NeedAnnotation { content: format!("ret") }))?
            }
            TermComputation::Force(Force(v)) => {
                let ty_val = v.syn(ctx.clone())?;
                let ty_body = ty_val.clone().elim_thunk(ctx.clone(), span).ok_or_else(|| {
                    ctx.err(
                        span,
                        TypeExpected {
                            context: format!("force"),
                            expected: format!("{{a}}"),
                            found: ty_val,
                        },
                    )
                })?;
                span.make(ty_body.to_owned()).ana(KindBase::CType.into(), ctx)?;
                Step::Done(ty_body)
            }
            TermComputation::TailGroup(TailGroup { group, body }) => {
                for item in group {
                    match item {
                        TailTerm::Let(Let { var, def, body: () }) => {
                            let ty_def = def.syn(ctx.clone())?;
                            span.make(ty_def.clone()).ana(KindBase::VType.into(), ctx.clone())?;
                            ctx.term_ctx.insert(var.to_owned(), ty_def);
                        }
                        TailTerm::Do(Do { var, comp, body: () }) => {
                            let ty_comp = comp.syn(ctx.clone())?;
                            span.make(ty_comp.clone()).ana(KindBase::CType.into(), ctx.clone())?;
                            let ty_val =
                                ty_comp.clone().elim_ret(ctx.clone(), span).ok_or_else(|| {
                                    ctx.err(
                                        span,
                                        TypeExpected {
                                            context: format!("do"),
                                            expected: format!("Ret _?"),
                                            found: ty_comp.clone(),
                                        },
                                    )
                                })?;
                            ctx.term_ctx.insert(var.to_owned(), ty_val);
                        }
                    }
                }
                Step::SynMode((ctx, body))
            }
            TermComputation::Rec(Rec { var: _, body: _ }) => {
                Err(ctx.err(span, NeedAnnotation { content: format!("rec") }))?
            }
            TermComputation::Match(Match { scrut, arms }) => {
                let ty_scrut = scrut.syn(ctx.clone())?;
                span.make(ty_scrut.clone()).ana(KindBase::VType.into(), ctx.clone())?;
                let (Data { name, params, ctors }, args) = ctx.resolve_data(ty_scrut, span)?;
                // arity check on data type
                let diff = Env::init(&params, &args, || {
                    ctx.err(
                        span,
                        ArityMismatch {
                            context: format!("data type `{}` instiantiation", name),
                            expected: params.len(),
                            found: args.len(),
                        },
                    )
                })?;
                let ctors: HashMap<_, _> =
                    ctors.into_iter().map(|DataBr(ctor, tys)| (ctor, tys)).collect();
                let mut unexpected = Vec::new();
                let mut ctorv_set_arm: HashSet<CtorV> = HashSet::new();
                let mut ty_arms = Vec::new();
                for Matcher { ctorv: ctor, vars, body } in arms {
                    let Some(tys) = ctors.get(ctor) else {
                        unexpected.push(ctor.to_owned());
                        continue;
                    };
                    ctorv_set_arm.insert(ctor.to_owned());
                    let tys = tys.into_iter().map(|ty| ty.inner_clone().subst(diff.clone(), &ctx));
                    let mut ctx = ctx.clone();
                    for (var, ty) in vars.iter().zip(tys) {
                        ctx.term_ctx.insert(var.to_owned(), ty?);
                    }
                    let ty = body.syn(ctx.clone())?;
                    let span = body.span();
                    span.make(ty.clone()).ana(KindBase::CType.into(), ctx)?;
                    ty_arms.push(ty);
                }
                let ctorv_set_data: HashSet<CtorV> = ctors.keys().cloned().collect();
                let missing: Vec<_> = ctorv_set_data.difference(&ctorv_set_arm).cloned().collect();
                bool_test(unexpected.is_empty() && missing.is_empty(), || {
                    ctx.err(span, InconsistentMatchers { unexpected, missing })
                })?;
                // branch consistency check
                let mut ty_opt: Option<Type> = None;
                for ty in &ty_arms {
                    if let Some(ty_opt) = &ty_opt {
                        ty_opt.clone().lub(ty.clone(), ctx.clone(), span).map_err(|_| {
                            ctx.err(span, InconsistentBranches { tys: ty_arms.clone() })
                        })?;
                    } else {
                        ty_opt = Some(ty.clone());
                    }
                }
                // empty match
                let Some(ty) = ty_opt else {
                    Err(ctx.err(span, InconsistentBranches{tys:vec![]}))?
                };
                Step::Done(ty)
            }
            TermComputation::Comatch(_) => {
                Err(ctx.err(span, NeedAnnotation { content: format!("comatch") }))?
            }
            TermComputation::Dtor(Dtor { body, dtorv: dtor, args }) => {
                let ty_body = body.syn(ctx.clone())?;
                let (Codata { name, params, dtors }, ty_args) =
                    ctx.resolve_codata(ty_body, span)?;
                // arity check on codata type
                let diff = Env::init(&params, &ty_args, || {
                    ctx.err(
                        span,
                        ArityMismatch {
                            context: format!("codata type `{}` instiantiation", name),
                            expected: params.len(),
                            found: ty_args.len(),
                        },
                    )
                })?;
                let CodataBr(_, tys, ty) = dtors
                    .into_iter()
                    .find(|CodataBr(dtorv, _, _)| dtorv == dtor)
                    .ok_or_else(|| {
                        ctx.err(
                            body.span(),
                            NameResolveError::UnknownDestructor {
                                context: format!("codata type `{}`", name),
                                dtor: dtor.clone(),
                            }
                            .into(),
                        )
                    })?;
                bool_test(args.len() == tys.len(), || {
                    ctx.err(
                        span,
                        ArityMismatch {
                            context: format!("dtor"),
                            expected: tys.len(),
                            found: args.len(),
                        },
                    )
                })?;
                for (arg, ty) in args.iter().zip(tys.iter()) {
                    arg.ana(ty.inner_clone().subst(diff.clone(), &ctx)?, ctx.clone())?;
                }
                Step::Done(ty.inner_clone().subst(diff, &ctx)?)
            }
            TermComputation::TyAbsTerm(_) => {
                Err(ctx.err(span, NeedAnnotation { content: format!("typabs") }))?
            }
            TermComputation::TyAppTerm(TyAppTerm { body, arg }) => {
                let ty_body = body.syn(ctx.clone())?;
                let ty_body = ctx.resolve_alias(ty_body, span)?;
                let SynType::Forall(Forall { param: (param, kd), ty }) = ty_body.resolve()? else {
                    Err(ctx.err(span, TypeExpected {
                        context: format!("term-typ-application"),
                        expected: format!("forall"),
                        found: ty_body,
                    }))?
                };
                arg.ana(kd.inner_clone(), ctx.clone())?;
                let diff = Env::init(&[(param, kd)], &[arg.clone()], || {
                    ctx.err(
                        span,
                        ArityMismatch { context: format!("typapp"), expected: 1, found: 1 },
                    )
                })?;
                Step::Done(ty.inner_clone().subst(diff, &ctx)?)
            }
            TermComputation::MatchPack(MatchPack { scrut, tvar, var, body }) => {
                let ty_scrut = scrut.syn(ctx.clone())?;
                let ty_scrut = ctx.resolve_alias(ty_scrut, span)?;
                let SynType::Exists(Exists { param: (param, kd), ty }) = ty_scrut.resolve()? else {
                    Err(ctx.err(span, TypeExpected {
                        context: format!("match-pack"),
                        expected: format!("exists"),
                        found: ty_scrut,
                    }))?
                };
                ctx.type_ctx.insert(tvar.clone(), kd.inner_clone());
                let ty =
                    ty.inner_clone().subst(Env::from_iter([(param, tvar.clone().into())]), &ctx)?;
                ctx.term_ctx.insert(var.clone(), ty);
                let ty_body = body.syn(ctx.clone())?;
                span.make(ty_body.clone()).ana(KindBase::CType.into(), ctx)?;
                Step::Done(ty_body)
            }
        })
    }
    fn ana_step(
        &self, typ: Self::Out, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        let span = self.span();
        ctx.trace.push(Frame {
            tycker_src: format!("{}:{}:{}", file!(), line!(), column!()),
            sort: format!("analyzing computation against type {}", typ.fmt()),
            term: format!("{}", self.fmt_truncate(40)),
            info: self.span().clone(),
        });
        let typ = ctx.resolve_alias(typ, span)?;
        let typ_syn = typ.resolve()?;
        if let SynType::Hole(_) = typ_syn {
            return Ok(Step::SynMode((ctx, self)));
        }
        span.make(typ.clone()).ana(KindBase::CType.into(), ctx.clone())?;
        Ok(match self.inner_ref() {
            TermComputation::Annotation(Annotation { term, ty }) => {
                let ty_lub = Type::lub(ty.inner_clone(), typ, ctx.clone(), span)?;
                Step::AnaMode((ctx, term), ty_lub)
            }
            TermComputation::Ret(Ret(v)) => {
                let ty_body = typ.clone().elim_ret(ctx.clone(), span).ok_or_else(|| {
                    ctx.err(
                        span,
                        TypeExpected {
                            context: format!("ret"),
                            expected: format!("Ret a"),
                            found: typ.clone(),
                        },
                    )
                })?;
                let ty = Type::make_ret(rc!(span.make(v.ana(ty_body, ctx.clone())?)));
                let typ_lub = Type::lub(ty, typ, ctx.clone(), span)?;
                Step::Done(typ_lub)
            }
            TermComputation::Force(Force(v)) => {
                v.ana(Type::make_thunk(rc!(span.make(typ.clone()))), ctx)?;
                Step::Done(typ)
            }
            TermComputation::TailGroup(TailGroup { group, body }) => {
                for item in group {
                    match item {
                        TailTerm::Let(Let { var, def, body: () }) => {
                            let ty_def = def.syn(ctx.clone())?;
                            span.make(ty_def.clone()).ana(KindBase::VType.into(), ctx.clone())?;
                            ctx.term_ctx.insert(var.to_owned(), ty_def);
                        }
                        TailTerm::Do(Do { var, comp, body: () }) => {
                            let ty_comp = comp.syn(ctx.clone())?;
                            span.make(ty_comp.clone()).ana(KindBase::CType.into(), ctx.clone())?;
                            let ty_val =
                                ty_comp.clone().elim_ret(ctx.clone(), span).ok_or_else(|| {
                                    ctx.err(
                                        span,
                                        TypeExpected {
                                            context: format!("do"),
                                            expected: format!("Ret _?"),
                                            found: ty_comp.clone(),
                                        },
                                    )
                                })?;
                            ctx.term_ctx.insert(var.to_owned(), ty_val);
                        }
                    }
                }
                Step::AnaMode((ctx, body), typ)
            }
            TermComputation::Rec(Rec { var, body }) => {
                ctx.term_ctx.insert(var.to_owned(), Type::make_thunk(rc!(span.make(typ.clone()))));
                Step::AnaMode((ctx, body), typ)
            }
            TermComputation::Match(Match { scrut, arms }) => {
                let ty_scrut = scrut.syn(ctx.clone())?;
                span.make(ty_scrut.clone()).ana(KindBase::VType.into(), ctx.clone())?;
                let (Data { name, params, ctors }, args) = ctx.resolve_data(ty_scrut, span)?;
                // arity check on data type
                let diff = Env::init(&params, &args, || {
                    ctx.err(
                        span,
                        ArityMismatch {
                            context: format!("data type `{}` instiantiation", name),
                            expected: params.len(),
                            found: args.len(),
                        },
                    )
                })?;
                let ctors: HashMap<_, _> =
                    ctors.into_iter().map(|DataBr(ctor, tys)| (ctor, tys)).collect();
                let mut unexpected = Vec::new();
                let mut ctorv_set_arm: HashSet<CtorV> = HashSet::new();
                for Matcher { ctorv: ctor, vars, body } in arms {
                    let Some(tys) = ctors.get(ctor) else {
                        unexpected.push(ctor.to_owned());
                        continue;
                    };
                    ctorv_set_arm.insert(ctor.to_owned());
                    let tys = tys.into_iter().map(|ty| ty.inner_clone().subst(diff.clone(), &ctx));
                    let mut ctx = ctx.clone();
                    for (var, ty) in vars.iter().zip(tys) {
                        ctx.term_ctx.insert(var.to_owned(), ty?);
                    }
                    body.ana(typ.clone(), ctx.clone())?;
                }
                let ctorv_set_data: HashSet<CtorV> = ctors.keys().cloned().collect();
                let missing: Vec<_> = ctorv_set_data.difference(&ctorv_set_arm).cloned().collect();
                bool_test(unexpected.is_empty() && missing.is_empty(), || {
                    ctx.err(span, InconsistentMatchers { unexpected, missing })
                })?;
                Step::Done(typ)
            }
            TermComputation::Comatch(Comatch { arms }) => {
                let (Codata { name, params, dtors }, ty_args) =
                    ctx.resolve_codata(typ.clone(), span)?;
                // arity check on codata type
                let diff = Env::init(&params, &ty_args, || {
                    ctx.err(
                        span,
                        ArityMismatch {
                            context: format!("codata type `{}` instantiation", name),
                            expected: params.len(),
                            found: ty_args.len(),
                        },
                    )
                })?;
                let dtors: HashMap<_, _> =
                    dtors.into_iter().map(|CodataBr(dtor, tys, ty)| (dtor, (tys, ty))).collect();
                let mut unexpected = Vec::new();
                let mut dtorv_set_arm: HashSet<DtorV> = HashSet::new();
                for Comatcher { dtorv: dtor, vars, body } in arms {
                    let Some((tys, ty)) = dtors.get(dtor).cloned() else {
                        unexpected.push(dtor.to_owned());
                        continue;
                    };
                    dtorv_set_arm.insert(dtor.to_owned());
                    let tys = tys.into_iter().map(|ty| ty.inner_clone().subst(diff.clone(), &ctx));
                    let ty = ty.inner_clone().subst(diff.clone(), &ctx)?;
                    let mut ctx = ctx.clone();
                    for (var, ty) in vars.iter().zip(tys) {
                        ctx.term_ctx.insert(var.to_owned(), ty?);
                    }
                    body.ana(ty, ctx)?;
                }
                let dtorv_set_coda: HashSet<DtorV> = dtors.keys().cloned().collect();
                let missing: Vec<_> = dtorv_set_coda.difference(&dtorv_set_arm).cloned().collect();
                bool_test(unexpected.is_empty() && missing.is_empty(), || {
                    ctx.err(span, InconsistentComatchers { unexpected, missing })
                })?;
                Step::Done(typ)
            }
            TermComputation::TyAbsTerm(TyAbsTerm { param: (tvar_, kd_), body }) => {
                let SynType::Forall(Forall { param: (tvar, kd), ty }) = &typ_syn else {
                    Err(ctx.err(span, TypeExpected {
                        context: format!("type abstraction"),
                        expected: format!("forall"),
                        found: typ,
                    }))?
                };
                let mut kd = kd.clone();
                if let Some(kd_) = kd_ {
                    let kd_ = kd_.inner.clone();
                    kd = kd.try_map(|kd| kd.lub(kd_, ctx.clone(), span))?;
                }
                let abst_var = ctx.fresh(kd.inner_clone());
                ctx.type_env.insert(tvar_.clone(), abst_var.clone().into());
                ty.inner_clone().subst(Env::from_iter([(tvar.clone(), abst_var.into())]), &ctx)?;
                body.ana(ty.inner_clone(), ctx)?;
                Step::Done(typ)
            }
            TermComputation::Dtor(_)
            | TermComputation::TyAppTerm(_)
            | TermComputation::MatchPack(_) => {
                // subsumption
                let typ_syn = self.syn(ctx.clone())?;
                // println!("{} /\\ {}", typ.fmt(), typ_syn.fmt());
                let typ_lub = Type::lub(typ, typ_syn, ctx.clone(), span)?;
                Step::Done(typ_lub)
            }
        })
    }
}
