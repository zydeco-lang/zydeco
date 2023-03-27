use super::*;

impl TypeCheck for Span<TermComputation> {
    type Ctx = Ctx;
    type Out = Type;
    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        Ok(match self.inner_ref() {
            TermComputation::TermAnn(TermAnn { term, ty }) => {
                ty.span()
                    .make(ty.syn(ctx.clone())?)
                    .ensure(&Kind::CType, "computation term annotation")?;
                Step::AnaMode((ctx, term), ty.inner_ref().clone())
            }
            TermComputation::Ret(Ret(v)) => {
                // Err(self
                // .span()
                // .make(NeedAnnotation { content: format!("ret") }))?
                let app_body = v.syn(ctx.clone())?.head_reduction()?;
                let ty_body: Type = app_body.into();
                let kd = self.span().make(ty_body.clone()).syn(ctx)?;
                self.span().make(kd).ensure(&Kind::VType, "force")?;
                Step::Done(
                    TypeApp {
                        tctor: TCtor::Ret,
                        args: vec![rc!(self.span().make(ty_body))],
                    }
                    .into(),
                )
            }
            TermComputation::Force(Force(v)) => {
                let app_body = v.syn(ctx.clone())?.head_reduction()?;
                bool_test(app_body.tctor == TCtor::Thunk, || {
                    self.span().make(TypeExpected {
                        context: format!("force"),
                        expected: format!("{{a}}"),
                        found: app_body.clone().into(),
                    })
                })?;
                let ty_body = app_body.args[0].inner_ref().to_owned();
                let kd = self.span().make(ty_body.to_owned()).syn(ctx)?;
                self.span().make(kd).ensure(&Kind::CType, "force")?;
                Step::Done(ty_body)
            }
            TermComputation::Let(Let { var, def, body }) => {
                let ty_def = def.syn(ctx.clone())?;
                let kd = self.span().make(ty_def.clone()).syn(ctx.clone())?;
                self.span().make(kd).ensure(&Kind::VType, "let")?;
                ctx.term_ctx.insert(var.to_owned(), ty_def.clone());
                Step::SynMode((ctx, body))
            }
            TermComputation::Do(Do { var, comp, body }) => {
                let ty_comp = comp.syn(ctx.clone())?;
                let kd = self.span().make(ty_comp.clone()).syn(ctx.clone())?;
                self.span().make(kd).ensure(&Kind::CType, "do")?;
                let ty_app = ty_comp.head_reduction()?;
                bool_test(ty_app.tctor == TCtor::Ret, || {
                    self.span().make(TypeExpected {
                        context: format!("do"),
                        expected: format!("{{a}}"),
                        found: ty_comp.clone(),
                    })
                })?;
                let ty_val = ty_app.args[0].inner_ref().to_owned();
                ctx.term_ctx.insert(var.to_owned(), ty_val);
                Step::SynMode((ctx, body))
            }
            TermComputation::Rec(Rec { var: _, body: _ }) => Err(self
                .span()
                .make(NeedAnnotation { content: format!("rec") }))?,
            TermComputation::Match(_) => todo!(),
            TermComputation::CoMatch(_) => todo!(),
            TermComputation::Dtor(_) => todo!(),
        })
    }
    fn ana_step(
        &self, typ: Self::Out, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, Span<TypeCheckError>> {
        Ok(match self.inner_ref() {
            TermComputation::Ret(Ret(v)) => {
                let app = typ.head_reduction()?;
                let kd = self.span().make(typ.clone()).syn(ctx.clone())?;
                self.span().make(kd).ensure(&Kind::CType, "ret")?;
                bool_test(app.tctor == TCtor::Ret, || {
                    self.span().make(TypeExpected {
                        context: format!("ret"),
                        expected: format!("{{a}}"),
                        found: typ.clone(),
                    })
                })?;
                bool_test(app.args.len() == 1, || {
                    self.span().make(ArityMismatch {
                        context: format!("ret"),
                        expected: 1,
                        found: app.args.len(),
                    })
                })?;
                v.ana(app.args[0].inner_ref().to_owned(), ctx)?;
                Step::Done(typ)
            }
            TermComputation::Force(Force(v)) => {
                let kd = self.span().make(typ.clone()).syn(ctx.clone())?;
                self.span().make(kd).ensure(&Kind::CType, "force")?;
                v.ana(
                    TypeApp {
                        tctor: TCtor::Thunk,
                        args: vec![rc!(self.span().make(typ.clone()))],
                    }
                    .into(),
                    ctx,
                )?;
                Step::Done(typ)
            }
            TermComputation::Let(Let { var, def, body }) => {
                let ty_def = def.syn(ctx.clone())?;
                let kd = self.span().make(ty_def.clone()).syn(ctx.clone())?;
                self.span().make(kd).ensure(&Kind::VType, "let")?;
                ctx.term_ctx.insert(var.to_owned(), ty_def.clone());
                Step::AnaMode((ctx, body), typ)
            }
            TermComputation::Do(Do { var, comp, body }) => {
                let ty_comp = comp.syn(ctx.clone())?;
                let kd = self.span().make(ty_comp.clone()).syn(ctx.clone())?;
                self.span().make(kd).ensure(&Kind::CType, "do")?;
                let ty_app = ty_comp.head_reduction()?;
                bool_test(ty_app.tctor == TCtor::Ret, || {
                    self.span().make(TypeExpected {
                        context: format!("do"),
                        expected: format!("{{a}}"),
                        found: ty_comp.clone(),
                    })
                })?;
                let ty_val = ty_app.args[0].inner_ref().to_owned();
                ctx.term_ctx.insert(var.to_owned(), ty_val);
                Step::AnaMode((ctx, body), typ)
            }
            TermComputation::Rec(Rec { var, body }) => {
                ctx.term_ctx.insert(
                    var.to_owned(),
                    TypeApp {
                        tctor: TCtor::Thunk,
                        args: vec![rc!(self.span().make(typ.clone()))],
                    }
                    .into(),
                );
                Step::AnaMode((ctx, body), typ)
            }
            TermComputation::Match(_) => todo!(),
            TermComputation::CoMatch(_) => todo!(),
            TermComputation::Dtor(_) => todo!(),
            _ => {
                let typ_syn = self.syn(ctx)?;
                typ.eqv(&typ_syn, || self.span().make(Subsumption))?;
                Step::Done(typ)
            }
        })
    }
}
