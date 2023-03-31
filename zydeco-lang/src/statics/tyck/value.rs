use super::*;

impl TypeCheck for Span<&Literal> {
    type Ctx = ();
    type Out = Type;
    fn syn_step(
        &self, _ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
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
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        ctx.trace.push(Frame {
            tycker_src: format!("{}:{}:{}", file!(), line!(), column!()),
            sort: "syn value".to_string(),
            term: format!("{}", self.inner_ref().fmt()),
            info: self.span().clone(),
        });
        let span = self.span();
        Ok(match self.inner_ref() {
            TermValue::Annotation(Annotation { term, ty }) => {
                ty.ana(Kind::VType, ctx.clone())?;
                Step::AnaMode((ctx, term), ty.inner_ref().clone())
            }
            TermValue::Var(x) => Step::Done(
                ctx.term_ctx
                    .get(x)
                    .cloned()
                    .ok_or(span.make(UnboundVar { var: x.clone() }))?,
            ),
            TermValue::Thunk(_) => {
                Err(span.make(NeedAnnotation { content: format!("thunk") }))?
            }
            TermValue::Ctor(_) => {
                Err(span.make(NeedAnnotation { content: format!("ctor") }))?
            }
            TermValue::Literal(l) => Step::Done(span.make(l).syn(())?),
            TermValue::Pack(_) => {
                Err(span.make(NeedAnnotation { content: format!("pack") }))?
            }
        })
    }
    fn ana_step(
        &self, typ: Self::Out, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        ctx.trace.push(Frame {
            tycker_src: format!("{}:{}:{}", file!(), line!(), column!()),
            sort: format!("ana value with type {}", typ.fmt()),
            term: format!("{}", self.inner_ref().fmt()),
            info: self.span().clone(),
        });
        if let SynType::Hole(_) = typ.synty {
            return Ok(Step::SynMode((ctx, self)));
        }
        let span = self.span();
        span.make(typ.clone()).ana(Kind::VType, ctx.clone())?;
        Ok(match self.inner_ref() {
            TermValue::Annotation(Annotation { term, ty }) => {
                let ty_lub = Type::lub(
                    ty.inner_ref().clone(),
                    typ.clone(),
                    ctx.clone(),
                    || span.make(Subsumption { sort: "value annotation" }),
                )?;
                Step::AnaMode((ctx, term), ty_lub)
            }
            TermValue::Thunk(Thunk(c)) => {
                let SynType::TypeApp(ty_app) = &typ.synty else {
                    Err(span.make(TypeExpected {
                        context: format!("thunk"),
                        expected: format!("{{a}}"),
                        found: typ.to_owned(),
                    }))?
                };
                let typ_comp = ty_app.elim_thunk().ok_or_else(|| {
                    span.make(TypeExpected {
                        context: format!("thunk"),
                        expected: format!("{{a}}"),
                        found: typ.to_owned(),
                    })
                })?;
                let ty = Type::make_thunk(rc!(
                    span.make(c.ana(typ_comp, ctx.clone())?)
                ));
                let typ_lub = Type::lub(ty, typ, ctx, || {
                    span.make(Subsumption { sort: "thunk" })
                })?;
                Step::Done(typ_lub)
            }
            TermValue::Ctor(Ctor { ctor, args }) => {
                let SynType::TypeApp(ty_app) = &typ.synty else {
                    Err(span.make(TypeExpected {
                        context: format!("ctor"),
                        expected: format!("{{a}}"),
                        found: typ.to_owned(),
                    }))?
                };
                let tvar = &ty_app.tvar;
                let Data { name, params, ctors } =
                    ctx.data_env.get(tvar).cloned().ok_or_else(|| {
                        span.make(NameResolve(
                            NameResolveError::UnboundTypeVariable {
                                tvar: tvar.to_owned(),
                            },
                        ))
                    })?;
                let diff = Env::init(&params, &ty_app.args, || {
                    span.make(ArityMismatch {
                        context: format!("data type `{}` instiantiation", name),
                        expected: params.len(),
                        found: ty_app.args.len(),
                    })
                })?;
                let DataBr(_, tys) = ctors
                    .into_iter()
                    .find(|DataBr(ctorv, _)| ctorv == ctor)
                    .ok_or_else(|| {
                        span.make(NameResolve(
                            NameResolveError::UnknownConstructor {
                                context: format!("data type `{}`", name),
                                ctor: ctor.to_owned(),
                            },
                        ))
                    })?;
                bool_test(args.len() == tys.len(), || {
                    span.make(ArityMismatch {
                        context: format!("ctor"),
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
                Step::Done(typ)
            }
            TermValue::Pack(Pack { ty, body }) => {
                let SynType::Exists(
                    Exists { param: (param, kd), ty: ty_body }
                ) = &typ.synty else {
                    Err(span.make(TypeExpected {
                        context: format!("pack"),
                        expected: format!("{{a}}"),
                        found: typ.to_owned(),
                    }))?
                };
                ty.ana(kd.clone(), ctx.clone())?;
                let ty_body = ty_body.inner_ref().clone().subst(
                    Env::from_iter([(param.clone(), ty.inner_ref().clone())]),
                )?;
                body.ana(ty_body, ctx)?;
                Step::Done(typ)
            }
            TermValue::Var(_) | TermValue::Literal(_) => {
                // subsumption
                let typ_syn = self.syn(ctx.clone())?;
                // println!("{} /\\ {}", typ.fmt(), typ_syn.fmt());
                let typ_lub = Type::lub(typ, typ_syn, ctx, || {
                    span.make(Subsumption { sort: "value" })
                })?;
                Step::Done(typ_lub)
            }
        })
    }
}
