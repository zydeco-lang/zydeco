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
        &self, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        Ok(match self.inner_ref() {
            TermValue::TermAnn(TermAnn { term, ty }) => {
                ty.span()
                    .make(ty.syn(ctx.clone())?)
                    .ensure(&Kind::VType, "value term annotation")?;
                Step::AnaMode((ctx, term), ty.inner_ref().clone())
            }
            TermValue::Var(x) => Step::Done(
                ctx.term_ctx
                    .get(x)
                    .cloned()
                    .ok_or(self.span().make(UnboundVar { var: x.clone() }))?,
            ),
            TermValue::Thunk(Thunk(c)) => {
                let c = c.syn(ctx)?.head_reduction()?;
                Step::Done(
                    TypeApp {
                        tctor: TCtor::Thunk,
                        args: vec![rc!(self.span().make(c.into()))],
                    }
                    .into(),
                )
                // Err(self
                //     .span()
                //     .make(NeedAnnotation { content: format!("thunk") }))?
            }
            TermValue::Ctor(_) => Err(self
                .span()
                .make(NeedAnnotation { content: format!("ctor") }))?,
            TermValue::Literal(l) => Step::Done(self.span().make(l).syn(())?),
        })
    }
    fn ana_step(
        &self, typ: Self::Out, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        let span = self.span();
        span.make(span.make(typ.clone()).syn(ctx.clone())?)
            .ensure(&Kind::VType, "ana value")?;
        Ok(match self.inner_ref() {
            TermValue::Thunk(Thunk(c)) => {
                let app = typ.head_reduction()?;
                bool_test(app.tctor == TCtor::Thunk, || {
                    self.span().make(TypeExpected {
                        context: format!("thunk"),
                        expected: format!("{{a}}"),
                        found: typ.to_owned(),
                    })
                })?;
                bool_test(app.args.len() == 1, || {
                    self.span().make(ArityMismatch {
                        context: format!("thunk"),
                        expected: 1,
                        found: typ.app.args.len(),
                    })
                })?;
                let typ_comp = app.args[0].inner_ref().to_owned();
                c.ana(typ_comp, ctx)?;
                Step::Done(typ)
            }
            TermValue::Ctor(Ctor { ctor, args }) => {
                let ty_app = typ.head_reduction()?;
                let TCtor::Var(tvar) = &ty_app.tctor else {
                    Err(self.span().make(TypeExpected {
                        context: format!("ctor"),
                        expected: format!("{{a}}"),
                        found: typ.to_owned(),
                    }))?
                };
                let Data { name, params, ctors } =
                    ctx.data_ctx.get(tvar).cloned().ok_or_else(|| {
                        self.span().make(NameResolve(
                            NameResolveError::UnboundTypeVariable {
                                tvar: tvar.to_owned(),
                            },
                        ))
                    })?;
                let diff = Env::init(&params, &ty_app.args, || {
                    self.span().make(ArityMismatch {
                        context: format!("data type `{}` instiantiation", name),
                        expected: params.len(),
                        found: ty_app.args.len(),
                    })
                })?;
                let DataBr(_, tys) = ctors
                    .into_iter()
                    .find(|DataBr(ctorv, _)| ctorv == ctor)
                    .ok_or_else(|| {
                        self.span().make(NameResolve(
                            NameResolveError::UnknownConstructor {
                                ctor: ctor.to_owned(),
                            },
                        ))
                    })?;
                bool_test(args.len() == tys.len(), || {
                    self.span().make(ArityMismatch {
                        context: format!("ctor"),
                        expected: tys.len(),
                        found: args.len(),
                    })
                })?;
                for (arg, ty) in args.iter().zip(tys.iter()) {
                    arg.ana(
                        ty.inner_ref().to_owned().subst(diff.clone()),
                        ctx.clone(),
                    )?;
                }
                Step::Done(typ)
            }
            TermValue::TermAnn(_)
            | TermValue::Var(_)
            | TermValue::Literal(_) => {
                let typ_syn = self.syn(ctx)?;
                typ.eqv(&typ_syn, || self.span().make(Subsumption))?;
                Step::Done(typ)
            }
        })
    }
}
