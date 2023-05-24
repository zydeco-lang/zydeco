use super::*;
use crate::utils::span::*;

impl<In, Out> SpanHolder for Arrow<In, Out>
where
    In: SpanHolder,
    Out: SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Arrow(r#in, out) = self;
        r#in.span_map_mut(f.clone());
        out.span_map_mut(f);
    }
}

impl<In, Out> SpanHolder for TypeArity<In, Out>
where
    In: KindT + SpanHolder,
    Out: KindT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let TypeArity { params, kd } = self;
        params.span_map_mut(f.clone());
        kd.span_map_mut(f);
    }
}

impl<TyV, T> SpanHolder for TypeApp<TyV, T>
where
    TyV: TyVarT + SpanHolder,
    T: TypeT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let TypeApp { tvar, args } = self;
        tvar.span_map_mut(f.clone());
        args.span_map_mut(f);
    }
}

impl<TyV, Ty> SpanHolder for Forall<TyV, Ty>
where
    TyV: TyVarT + SpanHolder,
    Ty: TypeT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Forall { param, ty } = self;
        param.span_map_mut(f.clone());
        ty.span_map_mut(f);
    }
}

impl<TyV, Ty> SpanHolder for Exists<TyV, Ty>
where
    TyV: TyVarT + SpanHolder,
    Ty: TypeT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Exists { param, ty } = self;
        param.span_map_mut(f.clone());
        ty.span_map_mut(f);
    }
}

impl<B> SpanHolder for Thunk<B>
where
    B: ComputationT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Thunk(body) = self;
        body.span_map_mut(f);
    }
}

impl SpanHolder for Literal {
    fn span_map_mut<F>(&mut self, _f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
    }
}

impl<C, A> SpanHolder for Ctor<C, A>
where
    C: CtorT + SpanHolder,
    A: ValueT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Ctor { ctorv: ctor, args } = self;
        ctor.span_map_mut(f.clone());
        args.span_map_mut(f);
    }
}

impl<Ty, A> SpanHolder for Pack<Ty, A>
where
    Ty: TypeT + SpanHolder,
    A: ValueT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Pack { ty, body: val } = self;
        ty.span_map_mut(f.clone());
        val.span_map_mut(f);
    }
}

impl<A> SpanHolder for Ret<A>
where
    A: ValueT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Ret(arg) = self;
        arg.span_map_mut(f);
    }
}

impl<A> SpanHolder for Force<A>
where
    A: ValueT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Force(arg) = self;
        arg.span_map_mut(f);
    }
}

impl<TeV, A, B> SpanHolder for Let<TeV, A, B>
where
    TeV: VarT + SpanHolder,
    A: ValueT + SpanHolder,
    B: ComputationT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Let { var, def, body } = self;
        var.span_map_mut(f.clone());
        def.span_map_mut(f.clone());
        body.span_map_mut(f);
    }
}

impl<TeV, B1, B2> SpanHolder for Do<TeV, B1, B2>
where
    TeV: VarT + SpanHolder,
    B1: ComputationT + SpanHolder,
    B2: ComputationT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Do { var, comp, body } = self;
        var.span_map_mut(f.clone());
        comp.span_map_mut(f.clone());
        body.span_map_mut(f);
    }
}

impl<TeV, B> SpanHolder for Rec<TeV, B>
where
    TeV: VarT + SpanHolder,
    B: ComputationT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Rec { var, body } = self;
        var.span_map_mut(f.clone());
        body.span_map_mut(f);
    }
}

impl<C, TeV, A, B> SpanHolder for Match<C, TeV, A, B>
where
    C: CtorT + SpanHolder,
    TeV: VarT + SpanHolder,
    A: ValueT + SpanHolder,
    B: ComputationT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Match { scrut, arms } = self;
        scrut.span_map_mut(f.clone());
        for Matcher { ctorv: ctor, vars, body } in arms {
            ctor.span_map_mut(f.clone());
            vars.span_map_mut(f.clone());
            body.span_map_mut(f.clone());
        }
    }
}

impl<D, TeV, B> SpanHolder for Comatch<D, TeV, B>
where
    D: DtorT + SpanHolder,
    TeV: VarT + SpanHolder,
    B: ComputationT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Comatch { arms } = self;
        for Comatcher { dtorv: dtor, vars, body } in arms {
            dtor.span_map_mut(f.clone());
            vars.span_map_mut(f.clone());
            body.span_map_mut(f.clone());
        }
    }
}

impl<B, D, A> SpanHolder for Dtor<B, D, A>
where
    B: ComputationT + SpanHolder,
    D: DtorT + SpanHolder,
    A: ValueT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Dtor { body, dtorv: dtor, args } = self;
        body.span_map_mut(f.clone());
        dtor.span_map_mut(f.clone());
        args.span_map_mut(f);
    }
}

impl<A, TyV, TeV, B> SpanHolder for MatchPack<A, TyV, TeV, B>
where
    A: ValueT + SpanHolder,
    TyV: TyVarT + SpanHolder,
    TeV: VarT + SpanHolder,
    B: ComputationT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let MatchPack { scrut, tvar, var, body } = self;
        scrut.span_map_mut(f.clone());
        tvar.span_map_mut(f.clone());
        var.span_map_mut(f.clone());
        body.span_map_mut(f);
    }
}

impl<TyV, Kd, C, T> SpanHolder for Data<TyV, Kd, C, T>
where
    TyV: TyVarT + SpanHolder,
    Kd: KindT + SpanHolder,
    C: CtorT + SpanHolder,
    T: TypeT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Data { name, params, ctors } = self;
        name.span_map_mut(f.clone());
        params.span_map_mut(f.clone());
        for DataBr { ctorv, tys } in ctors {
            ctorv.span_map_mut(f.clone());
            tys.span_map_mut(f.clone());
        }
    }
}

impl<TyV, Kd, D, T> SpanHolder for Codata<TyV, Kd, D, T>
where
    TyV: TyVarT + SpanHolder,
    Kd: KindT + SpanHolder,
    D: DtorT + SpanHolder,
    T: TypeT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Codata { name, params, dtors } = self;
        name.span_map_mut(f.clone());
        params.span_map_mut(f.clone());
        for CodataBr { dtorv, tys, ty } in dtors {
            dtorv.span_map_mut(f.clone());
            tys.span_map_mut(f.clone());
            ty.span_map_mut(f.clone());
        }
    }
}

impl<TyV, Kd, Ty> SpanHolder for Alias<TyV, Kd, Ty>
where
    TyV: TyVarT + SpanHolder,
    Kd: KindT + SpanHolder,
    Ty: TypeT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Alias { name, params, ty } = self;
        name.span_map_mut(f.clone());
        params.span_map_mut(f.clone());
        ty.span_map_mut(f);
    }
}

impl<TeV, A> SpanHolder for Define<TeV, A>
where
    TeV: VarT + SpanHolder,
    A: ValueT + SpanHolder,
{
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        let Define { name, def } = self;
        name.span_map_mut(f.clone());
        def.span_map_mut(f);
    }
}
