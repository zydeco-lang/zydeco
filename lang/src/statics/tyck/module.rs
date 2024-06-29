use super::*;

impl prelude::Data {
    fn type_arity(&self) -> Kind {
        TypeArity {
            params: (self.params.iter()).map(|(_, kd)| kd.clone()).collect(),
            kd: Box::new(self.name.span().make(KindBase::VType.into())),
        }
        .into()
    }
}

impl prelude::Codata {
    fn type_arity(&self) -> Kind {
        TypeArity {
            params: (self.params.iter()).map(|(_, kd)| kd.clone()).collect(),
            kd: Box::new(self.name.span().make(KindBase::CType.into())),
        }
        .into()
    }
}

impl prelude::Alias {
    fn type_arity(&self, kd: Kind) -> Kind {
        TypeArity {
            params: (self.params.iter()).map(|(_, kd)| kd.clone()).collect(),
            kd: Box::new(self.name.span().make(kd)),
        }
        .into()
    }
}

impl TypeCheck for Sp<&prelude::Data> {
    type Ctx = Ctx;
    type Out = ();

    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        let data = self.inner_ref();
        for (tvar, kd) in data.params.iter() {
            ctx.type_ctx.insert(tvar.clone(), kd.inner_clone());
        }
        let mut ctorvs = HashSet::new();
        for DataBr { ctorv, tys } in data.ctors.iter() {
            let span = ctorv.span();
            if ctorvs.contains(ctorv) {
                Err(ctx.err(
                    span,
                    NameResolveError::DuplicateCtorDeclaration { name: ctorv.clone() }.into(),
                ))?;
            }
            ctorvs.insert(ctorv.clone());
            for ty in tys {
                ty.ana(KindBase::VType.into(), ctx.clone())?;
            }
        }
        Ok(Step::Done(()))
    }
}

impl TypeCheck for Sp<&prelude::Codata> {
    type Ctx = Ctx;
    type Out = ();

    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        let data = self.inner_ref();
        for (tvar, kd) in data.params.iter() {
            ctx.type_ctx.insert(tvar.clone(), kd.inner_clone());
        }
        let mut dtorvs = HashSet::new();
        for CodataBr { dtorv, ty } in data.dtors.iter() {
            let span = dtorv.span();
            if dtorvs.contains(dtorv) {
                Err(ctx.err(
                    span,
                    NameResolveError::DuplicateDtorDeclaration { name: dtorv.clone() }.into(),
                ))?;
            }
            dtorvs.insert(dtorv.clone());
            ty.ana(KindBase::CType.into(), ctx.clone())?;
        }
        Ok(Step::Done(()))
    }
}

impl TypeCheck for Sp<&prelude::Alias> {
    type Ctx = Ctx;
    type Out = Kind;

    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        let data = self.inner_ref();
        for (tvar, kd) in data.params.iter() {
            ctx.type_ctx.insert(tvar.clone(), kd.inner_clone());
        }
        let kd = data.ty.syn(ctx.clone())?;
        Ok(Step::Done(kd))
    }
}

impl TypeCheck for Sp<Module> {
    type Ctx = Ctx;
    type Out = Seal<Ctx>;
    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        let Module { name: _, data, codata, alias, define, define_ext } = self.inner_ref();
        // register data type, codata type and type alias declarations in the type context
        for DeclSymbol { inner: data, .. } in data {
            let res = ctx.type_ctx.insert(data.name.clone(), data.type_arity());
            if let Some(_) = res {
                Err(ctx.err(
                    data.name.span(),
                    NameResolveError::DuplicateTypeDeclaration { name: data.name.clone() }.into(),
                ))?;
            }
        }
        for DeclSymbol { inner: coda, .. } in codata {
            let res = ctx.type_ctx.insert(coda.name.clone(), coda.type_arity());
            if let Some(_) = res {
                Err(ctx.err(
                    coda.name.span(),
                    NameResolveError::DuplicateTypeDeclaration { name: coda.name.clone() }.into(),
                ))?;
            }
        }
        for DeclSymbol { inner: alias, .. } in alias {
            // type check alias declarations right away
            let kd = alias.name.span().make(alias).syn(ctx.clone())?;
            // register alias declarations in the type context
            let res = ctx.type_ctx.insert(alias.name.clone(), alias.type_arity(kd));
            if let Some(_) = res {
                Err(ctx.err(
                    alias.name.span(),
                    NameResolveError::DuplicateTypeDeclaration { name: alias.name.clone() }.into(),
                ))?;
            }
            ctx.alias_env.insert(alias.name.clone(), alias.clone());
        }
        // type check data and codata type declarations
        for DeclSymbol { inner: data, .. } in data {
            data.name.span().make(data).syn(ctx.clone())?;
            ctx.data_env.insert(data.name.clone(), data.clone());
        }
        for DeclSymbol { inner: coda, .. } in codata {
            coda.name.span().make(coda).syn(ctx.clone())?;
            ctx.codata_env.insert(coda.name.clone(), coda.clone());
        }
        for DeclSymbol { inner: Define { name: (var, ty), def: () }, .. } in define_ext {
            ctx.term_ctx.insert(var.clone(), ty.inner_clone());
        }
        // register term declarations in the term context
        for DeclSymbol { inner: Define { name, def }, external, .. } in define {
            bool_test(!external, || {
                ctx.err(
                    name.span(),
                    NameResolveError::ExternalDeclaration { name: name.name().to_string() }.into(),
                )
            })?;
            let ty_def = def.syn(ctx.clone())?;
            let span = name.span();
            span.make(ty_def.clone()).ana(KindBase::VType.into(), ctx.clone())?;
            ctx.term_ctx.insert(name.clone(), ty_def);
        }
        Ok(Step::Done(Seal(ctx)))
    }
}
