use super::*;

impl TypeCheck for Span<Type> {
    type Ctx = Ctx;
    type Out = Kind;
    fn syn_step(
        &self, ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        let app = self.inner_ref().head_reduction()?;
        Ok(match &app.tctor {
            TCtor::Var(x) => {
                // type constructor
                let Some(TypeArity { params, kd }) = ctx.type_ctx.get(&x) else {
                    Err(self.span().make(
                        NameResolveError::UnboundTypeVariable {
                            tvar: x.to_owned(),
                        }.into()
                    ))?
                };
                bool_test(app.args.len() == params.len(), || {
                    self.span().make(ArityMismatch {
                        context: format!("{}", self.inner_ref().fmt()),
                        expected: params.len(),
                        found: app.args.len(),
                    })
                })?;
                for (arg, kd) in app.args.iter().zip(params.iter()) {
                    self.span()
                        .make(arg.syn(ctx.clone())?)
                        .ensure(kd, "type argument")?;
                }
                if let Some(kd_self) = self.inner_ref().kd {
                    self.span().make(kd_self).ensure(kd, "kind subsumption")?;
                }
                Step::Done(kd.clone())
            }
            TCtor::Thunk => match &app.args.as_slice() {
                &[arg] => {
                    self.span()
                        .make(arg.syn(ctx.clone())?)
                        .ensure(&Kind::CType, "thunk argument")?;
                    Step::Done(Kind::VType)
                }
                _ => Err(self.span().make(ArityMismatch {
                    context: format!("{}", self.inner_ref().fmt()),
                    expected: 1,
                    found: app.args.len(),
                }))?,
            },
            TCtor::Ret => match &app.args.as_slice() {
                &[arg] => {
                    self.span()
                        .make(arg.syn(ctx.clone())?)
                        .ensure(&Kind::VType, "return argument")?;
                    Step::Done(Kind::CType)
                }
                _ => Err(self.span().make(ArityMismatch {
                    context: format!("{}", self.inner_ref().fmt()),
                    expected: 1,
                    found: app.args.len(),
                }))?,
            },
            TCtor::Fun => match &app.args.as_slice() {
                &[arg1, arg2] => {
                    self.span()
                        .make(arg1.syn(ctx.clone())?)
                        .ensure(&Kind::VType, "function argument")?;
                    self.span()
                        .make(arg2.syn(ctx.clone())?)
                        .ensure(&Kind::CType, "function argument")?;
                    Step::Done(Kind::CType)
                }
                _ => Err(self.span().make(ArityMismatch {
                    context: format!("{}", self.inner_ref().fmt()),
                    expected: 2,
                    found: app.args.len(),
                }))?,
            },
        })
    }
}

impl Type {
    #[must_use]
    pub(super) fn head_reduction(
        &self,
    ) -> Result<TypeApp<TCtor, RcType>, Span<TypeCheckError>> {
        let Type { app, kd: _, env } = self;
        // Note: the type is either a type constructor applied with types or a type variable
        if app.args.is_empty() {
            // type variable or data type with no parameters
            let mut tctor = app.tctor.clone();
            if let TCtor::Var(tvar) = &mut tctor {
                if let Some(ty) = env.get(tvar) {
                    return ty.head_reduction();
                }
            }
            // base case: stuck
            Ok(app.clone())
        } else {
            // base case: type constructor
            let args: Vec<_> = (app.args.iter())
                .map(|ty| {
                    let ty_subst = ty.inner_ref().clone().subst(env.clone());
                    rc!(ty.span().make(ty_subst))
                })
                .collect();
            let app = TypeApp { tctor: app.tctor.clone(), args };
            Ok(app)
        }
    }

    pub(super) fn subst(self, diff: Env<TypeV, Type>) -> Self {
        let Type { app, kd, env } = self;
        let env = diff.append(env);
        Type { app, kd, env }
    }
}
