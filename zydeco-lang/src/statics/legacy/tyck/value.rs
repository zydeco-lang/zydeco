use super::*;

impl TypeCheck for Span<&Literal> {
    type Ctx = ();
    type Out = Type;
    fn syn_step(&self, _ctx: Self::Ctx) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        Ok(Step::Done(match self.inner_ref() {
            Literal::Int(_) => Type::internal("Int", vec![]),
            Literal::String(_) => Type::internal("String", vec![]),
            Literal::Char(_) => Type::internal("Char", vec![]),
        }))
    }
}

impl TypeCheck for Span<TermValue> {
    type Ctx = Ctx;
    type Out = Type;
    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        ctx.trace.push(Frame {
            tycker_src: format!("{}", std::panic::Location::caller()),
            sort: "synthesizing value".to_string(),
            term: format!("{}", self.inner_ref().fmt_truncate(40)),
            info: self.span().clone(),
        });
        let span = self.span();
        Ok(match self.inner_ref() {
            TermValue::Annotation(Annotation { term, ty }) => {
                ty.ana(KindBase::VType.into(), ctx.clone())?;
                Step::AnaMode((ctx, term), ty.inner_clone())
            }
            TermValue::Var(x) => Step::Done(
                ctx.term_ctx.get(x).cloned().ok_or(ctx.err(span, UnboundVar { var: x.clone() }))?,
            ),
            TermValue::Thunk(_) => {
                Err(ctx.err(span, NeedAnnotation { content: format!("thunk") }))?
            }
            TermValue::Ctor(_) => Err(ctx.err(span, NeedAnnotation { content: format!("ctor") }))?,
            TermValue::Literal(l) => Step::Done(span.make(l).syn(())?),
            TermValue::Pack(_) => Err(ctx.err(span, NeedAnnotation { content: format!("pack") }))?,
        })
    }
    fn ana_step(
        &self, typ: Self::Out, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        let span = self.span();
        ctx.trace.push(Frame {
            tycker_src: format!("{}", std::panic::Location::caller()),
            sort: format!("analyzing value against type {}", typ.fmt()),
            term: format!("{}", self.inner_ref().fmt_truncate(40)),
            info: self.span().clone(),
        });
        let typ = ctx.resolve_alias(typ, span)?;
        let typ_syn = typ.resolve()?;
        if let SynType::Hole(_) = typ_syn {
            return Ok(Step::SynMode((ctx, self)));
        }
        span.make(typ.clone()).ana(KindBase::VType.into(), ctx.clone())?;
        Ok(match self.inner_ref() {
            TermValue::Annotation(Annotation { term, ty }) => {
                let ty_lub = Type::lub(ty.inner_clone(), typ, ctx.clone(), span)?;
                Step::AnaMode((ctx, term), ty_lub)
            }
            TermValue::Thunk(Thunk(c)) => {
                let typ_comp = typ.clone().elim_thunk(ctx.clone(), span).ok_or_else(|| {
                    ctx.err(
                        span,
                        TypeExpected {
                            context: format!("thunk"),
                            expected: format!("Thunk a"),
                            found: typ.to_owned(),
                        },
                    )
                })?;
                let ty = Type::make_thunk(rc!(span.make(c.ana(typ_comp, ctx.clone())?)));
                let typ_lub = Type::lub(ty, typ, ctx.clone(), span)?;
                Step::Done(typ_lub)
            }
            TermValue::Ctor(Ctor { ctorv: ctor, args }) => {
                let (Data { name, params, ctors }, ty_args) =
                    ctx.resolve_data(typ.clone(), span)?;
                let diff = Env::init(&params, &ty_args, || {
                    ctx.err(
                        span,
                        ArityMismatch {
                            context: format!("data type `{}` instiantiation", name),
                            expected: params.len(),
                            found: ty_args.len(),
                        },
                    )
                })?;
                let DataBr { ctorv: _, tys } = ctors
                    .into_iter()
                    .find(|DataBr { ctorv, tys: _ }| ctorv == ctor)
                    .ok_or_else(|| {
                        ctx.err(
                            span,
                            NameResolve(NameResolveError::UnknownConstructor {
                                context: format!("data type `{}`", name),
                                ctor: ctor.to_owned(),
                            }),
                        )
                    })?;
                bool_test(args.len() == tys.len(), || {
                    ctx.err(
                        span,
                        ArityMismatch {
                            context: format!("ctor"),
                            expected: tys.len(),
                            found: args.len(),
                        },
                    )
                })?;
                for (arg, ty) in args.iter().zip(tys.iter()) {
                    arg.ana(ty.inner_clone().subst(diff.clone(), &ctx)?, ctx.clone())?;
                }
                Step::Done(typ)
            }
            TermValue::Pack(Pack { ty, body }) => {
                let SynType::Exists(
                    Exists { param: (param, kd), ty: ty_body }
                ) = typ_syn else {
                    Err(ctx.err(span,TypeExpected {
                        context: format!("pack"),
                        expected: format!("exists"),
                        found: typ,
                    }))?
                };
                ty.ana(kd.inner_clone(), ctx.clone())?;
                let ty_body = ty_body
                    .inner_clone()
                    .subst(Env::from_iter([(param, ty.inner_clone())]), &ctx)?;
                body.ana(ty_body, ctx)?;
                Step::Done(typ)
            }
            TermValue::Var(_) | TermValue::Literal(_) => {
                // subsumption
                let typ_syn = self.syn(ctx.clone())?;
                // println!("{} /\\ {}", typ.fmt(), typ_syn.fmt());
                let typ_lub = Type::lub(typ, typ_syn, ctx.clone(), span)?;
                Step::Done(typ_lub)
            }
        })
    }
}
