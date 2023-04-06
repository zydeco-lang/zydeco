use super::*;

impl Data<TypeV, Kind, CtorV, RcType> {
    fn type_arity(&self) -> TypeArity<Kind> {
        TypeArity {
            params: (self.params.iter()).map(|(_, kd)| kd.clone()).collect(),
            kd: Kind::VType,
        }
    }
}

impl Codata<TypeV, Kind, DtorV, RcType> {
    fn type_arity(&self) -> TypeArity<Kind> {
        TypeArity {
            params: (self.params.iter()).map(|(_, kd)| kd.clone()).collect(),
            kd: Kind::CType,
        }
    }
}

impl From<Kind> for TypeArity<Kind> {
    fn from(kd: Kind) -> Self {
        TypeArity { params: vec![], kd }
    }
}

impl TypeCheck for Span<&Data<TypeV, Kind, CtorV, RcType>> {
    type Ctx = Ctx;
    type Out = ();

    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        let data = self.inner_ref();
        for (tvar, kd) in data.params.iter() {
            ctx.type_ctx.insert(tvar.clone(), kd.clone().into());
        }
        let mut ctorvs = HashSet::new();
        for DataBr(ctorv, tys) in data.ctors.iter() {
            let span = ctorv.span();
            if ctorvs.contains(ctorv) {
                Err(ctx.err(
                    span,
                    NameResolveError::DuplicateCtorDeclaration { name: ctorv.clone() }.into(),
                ))?;
            }
            ctorvs.insert(ctorv.clone());
            for ty in tys {
                ty.ana(Kind::VType, ctx.clone())?;
            }
        }
        Ok(Step::Done(()))
    }
}

impl TypeCheck for Span<&Codata<TypeV, Kind, DtorV, RcType>> {
    type Ctx = Ctx;
    type Out = ();

    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        let data = self.inner_ref();
        for (tvar, kd) in data.params.iter() {
            ctx.type_ctx.insert(tvar.clone(), kd.clone().into());
        }
        let mut dtorvs = HashSet::new();
        for CodataBr(dtorv, tys, ty) in data.dtors.iter() {
            let span = dtorv.span();
            if dtorvs.contains(dtorv) {
                Err(ctx.err(
                    span,
                    NameResolveError::DuplicateDtorDeclaration { name: dtorv.clone() }.into(),
                ))?;
            }
            dtorvs.insert(dtorv.clone());
            for ty in tys {
                ty.ana(Kind::VType, ctx.clone())?;
            }
            ty.ana(Kind::CType, ctx.clone())?;
        }
        Ok(Step::Done(()))
    }
}

impl TypeCheck for Span<Module> {
    type Ctx = Ctx;
    type Out = Seal<Ctx>;
    fn syn_step(
        &self, mut ctx: Self::Ctx,
    ) -> Result<Step<(Self::Ctx, &Self), Self::Out>, TyckError> {
        let Module { name: _, data, codata: coda, alias: _, define, define_ext } = self.inner_ref();
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
        for DeclSymbol { inner: coda, .. } in coda {
            let res = ctx.type_ctx.insert(coda.name.clone(), coda.type_arity());
            if let Some(_) = res {
                Err(ctx.err(
                    coda.name.span(),
                    NameResolveError::DuplicateTypeDeclaration { name: coda.name.clone() }.into(),
                ))?;
            }
        }
        // TODO: register type aliases in the type context
        // type check data and codata type declarations
        for DeclSymbol { inner: data, .. } in data {
            data.name.span().make(data).syn(ctx.clone())?;
            ctx.data_env.insert(data.name.clone(), data.clone());
        }
        for DeclSymbol { inner: coda, .. } in coda {
            coda.name.span().make(coda).syn(ctx.clone())?;
            ctx.coda_env.insert(coda.name.clone(), coda.clone());
        }
        for DeclSymbol { inner: Define { name: (var, ty), def: () }, .. } in define_ext {
            ctx.term_ctx.insert(var.clone(), ty.inner_ref().clone());
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
            span.make(ty_def.clone()).ana(Kind::VType, ctx.clone())?;
            ctx.term_ctx.insert(name.clone(), ty_def);
        }
        Ok(Step::Done(Seal(ctx)))
    }
}
