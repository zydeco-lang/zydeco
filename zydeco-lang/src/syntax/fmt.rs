use super::*;
use crate::utils::fmt::*;

mod binder {
    use super::*;
    use crate::syntax::binder::*;
    macro_rules! var_fmt {
        ($Var:ident) => {
            impl FmtArgs for $Var {
                fn fmt_args(&self, _args: Args) -> String {
                    format!("{}", self.name())
                }
            }
            impl std::fmt::Display for $Var {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    write!(f, "{}", <Self as FmtArgs>::fmt(self))
                }
            }
        };
    }

    var_fmt!(CtorV);
    var_fmt!(DtorV);
    var_fmt!(TypeV);
    var_fmt!(TermV);
}

impl FmtArgs for (TypeV, Kind) {
    fn fmt_args(&self, fargs: Args) -> String {
        let (tvar, kd) = self;
        format!("({} : {})", tvar.fmt_args(fargs), kd.fmt_args(fargs))
    }
}

impl FmtArgs for Kind {
    fn fmt_args(&self, _args: Args) -> String {
        match self {
            Kind::VType => "VType".to_owned(),
            Kind::CType => "CType".to_owned(),
        }
    }
}

impl<K> FmtArgs for TypeArity<K>
where
    K: KindT + FmtArgs,
{
    fn fmt_args(&self, args: Args) -> String {
        let TypeArity { params, kd } = self;
        let mut s = String::new();
        s += "(";
        s += &params
            .into_iter()
            .map(|kd| kd.fmt_args(args))
            .collect::<Vec<_>>()
            .join(", ");
        s += ") -> ";
        s += &kd.fmt_args(args);
        s
    }
}

impl<TyV, T> FmtArgs for TypeApp<TyV, T>
where
    TyV: TyVarT + FmtArgs,
    T: TypeT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let TypeApp { tvar: tctor, args } = self;
        let mut s = String::new();
        s += &tctor.fmt_args(fargs);
        s += "(";
        s += &args
            .into_iter()
            .map(|arg| arg.fmt_args(fargs))
            .collect::<Vec<_>>()
            .join(", ");
        s += ")";
        s
    }
}

impl<TyV, Ty> FmtArgs for Forall<TyV, Ty>
where
    TyV: TyVarT + FmtArgs,
    Ty: TypeT + FmtArgs,
{
    fn fmt_args(&self, args: Args) -> String {
        let Forall { param, ty } = self;
        format!("forall {} . {}", param.fmt_args(args), ty.fmt_args(args))
    }
}

impl<TyV, Ty> FmtArgs for Exists<TyV, Ty>
where
    TyV: TyVarT + FmtArgs,
    Ty: TypeT + FmtArgs,
{
    fn fmt_args(&self, args: Args) -> String {
        let Exists { param, ty } = self;
        format!("exists {} . {}", param.fmt_args(args), ty.fmt_args(args))
    }
}

impl<Term, Type> FmtArgs for Annotation<Term, Type>
where
    Term: FmtArgs,
    Type: FmtArgs,
{
    fn fmt_args(&self, args: Args) -> String {
        let Annotation { term: body, ty } = self;
        format!("({} :: {})", body.fmt_args(args), ty.fmt_args(args))
    }
}

impl<B> FmtArgs for Thunk<B>
where
    B: ComputationT + FmtArgs,
{
    fn fmt_args(&self, args: Args) -> String {
        let Thunk(b) = self;
        format!("{{ {} }}", b.fmt_args(args))
    }
}

impl FmtArgs for Literal {
    fn fmt_args(&self, _args: Args) -> String {
        match self {
            Literal::Int(i) => format!("{}", i),
            Literal::String(s) => format!("\"{}\"", s),
            Literal::Char(c) => format!("'{}'", c),
        }
    }
}

impl<C, A> FmtArgs for Ctor<C, A>
where
    C: CtorT + FmtArgs,
    A: ValueT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Ctor { ctor, args } = self;
        let mut s = String::new();
        s += &ctor.fmt_args(fargs);
        s += "(";
        s += &args
            .into_iter()
            .map(|arg| arg.fmt_args(fargs))
            .collect::<Vec<_>>()
            .join(", ");
        s += ")";
        s
    }
}

impl<Ty, A> FmtArgs for Pack<Ty, A>
where
    Ty: TypeT + FmtArgs,
    A: ValueT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Pack { ty, body } = self;
        format!("exists ({}, {})", ty.fmt_args(fargs), body.fmt_args(fargs))
    }
}

impl<A> FmtArgs for Ret<A>
where
    A: ValueT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Ret(a) = self;
        format!("ret {}", a.fmt_args(fargs))
    }
}

impl<A> FmtArgs for Force<A>
where
    A: ValueT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Force(a) = self;
        format!("! {}", a.fmt_args(fargs))
    }
}

impl<TeV, A, B> FmtArgs for Let<TeV, A, B>
where
    TeV: VarT + FmtArgs,
    A: ValueT + FmtArgs,
    B: ComputationT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Let { var, def, body } = self;
        let mut s = String::new();
        s += &format!(
            "let {} = {} in",
            var.fmt_args(fargs),
            def.fmt_args(fargs),
        );
        s += &fargs.br_indent();
        s += &body.fmt_args(fargs);
        s
    }
}

impl<TeV, B1, B2> FmtArgs for Do<TeV, B1, B2>
where
    TeV: VarT + FmtArgs,
    B1: ComputationT + FmtArgs,
    B2: ComputationT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Do { var, comp, body } = self;
        let mut s = String::new();
        s += &format!(
            "do {} <- {} ;",
            var.fmt_args(fargs),
            comp.fmt_args(fargs),
        );
        s += &fargs.br_indent();
        s += &body.fmt_args(fargs);
        s
    }
}

impl<TeV, B> FmtArgs for Rec<TeV, B>
where
    TeV: VarT + FmtArgs,
    B: ComputationT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Rec { var, body } = self;
        let mut s = String::new();
        s += "rec ";
        s += &var.fmt_args(fargs);
        s += " -> (";
        {
            let fargs = fargs.indent();
            s += &fargs.br_indent();
            s += &body.fmt_args(fargs);
        }
        s += &fargs.br_indent();
        s += ")";
        s
    }
}

impl<C, TeV, A, B> FmtArgs for Match<C, TeV, A, B>
where
    C: CtorT + FmtArgs,
    TeV: VarT + FmtArgs,
    A: ValueT + FmtArgs,
    B: ComputationT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Match { scrut, arms } = self;
        let mut s = String::new();
        s += "match ";
        s += &scrut.fmt_args(fargs);
        for Matcher { ctor, vars, body } in arms {
            s += &fargs.br_indent();
            s += "| ";
            s += &ctor.fmt_args(fargs);
            s += "(";
            s += &vars
                .into_iter()
                .map(|var| var.fmt_args(fargs))
                .collect::<Vec<_>>()
                .join(", ");
            s += ") -> ";
            {
                let fargs = fargs.indent();
                s += &fargs.br_indent();
                s += &body.fmt_args(fargs);
            }
        }
        s += &fargs.br_indent();
        s += "end";
        s
    }
}

impl<D, TeV, B> FmtArgs for CoMatch<D, TeV, B>
where
    D: DtorT + FmtArgs,
    TeV: VarT + FmtArgs,
    B: ComputationT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let CoMatch { arms } = self;
        let mut s = String::new();
        s += "comatch ";
        for CoMatcher { dtor, vars, body } in arms {
            s += &fargs.br_indent();
            s += "| .";
            s += &dtor.fmt_args(fargs);
            s += "(";
            s += &vars
                .into_iter()
                .map(|var| var.fmt_args(fargs))
                .collect::<Vec<_>>()
                .join(", ");
            s += ") -> ";
            {
                let fargs = fargs.indent();
                s += &fargs.br_indent();
                s += &body.fmt_args(fargs);
            }
        }
        s += &fargs.br_indent();
        s += "end";
        s
    }
}

impl<B, D, A> FmtArgs for Dtor<B, D, A>
where
    B: ComputationT + FmtArgs,
    D: DtorT + FmtArgs,
    A: ValueT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Dtor { body, dtor, args } = self;
        let mut s = String::new();
        s += &body.fmt_args(fargs);
        s += " .";
        s += &dtor.fmt_args(fargs);
        s += "(";
        s += &args
            .into_iter()
            .map(|arg| arg.fmt_args(fargs))
            .collect::<Vec<_>>()
            .join(", ");
        s += ")";
        s
    }
}

impl<TyV, Kd, B> FmtArgs for TypAbs<TyV, Kd, B>
where
    TyV: TyVarT + FmtArgs,
    Kd: KindT + FmtArgs,
    B: ComputationT + FmtArgs,
{
    fn fmt_args(&self, args: Args) -> String {
        let TypAbs { tvar, kd, body } = self;
        format!(
            "fn typ {} : {} -> {}",
            tvar.fmt_args(args),
            kd.fmt_args(args),
            body.fmt_args(args)
        )
    }
}

impl<B, Ty> FmtArgs for TypApp<B, Ty>
where
    B: ComputationT + FmtArgs,
    Ty: TypeT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let TypApp { body, arg } = self;
        format!("{} [{}]", body.fmt_args(fargs), arg.fmt_args(fargs))
    }
}

impl<A, TyV, TeV, B> FmtArgs for MatchPack<A, TyV, TeV, B>
where
    A: ValueT + FmtArgs,
    TyV: TyVarT + FmtArgs,
    TeV: VarT + FmtArgs,
    B: ComputationT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let MatchPack { scrut, tvar, var, body } = self;
        format!(
            "match {} exists {} : {} -> {}",
            scrut.fmt_args(fargs),
            tvar.fmt_args(fargs),
            var.fmt_args(fargs),
            body.fmt_args(fargs)
        )
    }
}

impl<T> FmtArgs for DeclSymbol<T>
where
    T: FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let DeclSymbol { public, external, inner } = self;
        let mut s = String::new();
        if *public {
            s += "pub ";
        }
        if *external {
            s += "extern ";
        }
        s += &inner.fmt_args(fargs);
        s
    }
}

impl<TyV, C, Ty> FmtArgs for Data<TyV, C, Ty>
where
    TyV: TyVarT + FmtArgs,
    C: CtorT + FmtArgs,
    Ty: TypeT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Data { name, params, ctors } = self;
        let mut s = String::new();
        s += "data ";
        s += &name.fmt_args(fargs);
        for (tyvar, kd) in params {
            s += &format!(
                " ({} : {})",
                tyvar.fmt_args(fargs),
                kd.fmt_args(fargs)
            );
        }
        s += " where ";
        s += &fargs.br_indent();
        for databr in ctors {
            s += &databr.fmt_args(fargs);
            s += &fargs.br_indent();
        }
        s += "end";
        s
    }
}

impl<C, Ty> FmtArgs for DataBr<C, Ty>
where
    C: CtorT + FmtArgs,
    Ty: TypeT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let DataBr(ctorv, tys) = self;
        let mut s = String::new();
        s += "| ";
        s += &ctorv.fmt_args(fargs);
        s += "(";
        s += &tys
            .into_iter()
            .map(|ty| ty.fmt_args(fargs))
            .collect::<Vec<_>>()
            .join(", ");
        s += ")";
        s
    }
}

impl<TyV, D, Ty> FmtArgs for Codata<TyV, D, Ty>
where
    TyV: TyVarT + FmtArgs,
    D: DtorT + FmtArgs,
    Ty: TypeT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Codata { name, params, dtors } = self;
        let mut s = String::new();
        s += "codata ";
        s += &name.fmt_args(fargs);
        for (tyvar, kd) in params {
            s += &format!(
                " ({} : {})",
                tyvar.fmt_args(fargs),
                kd.fmt_args(fargs)
            );
        }
        s += " where ";
        s += &fargs.br_indent();
        for codatabr in dtors {
            s += &codatabr.fmt_args(fargs);
            s += &fargs.br_indent();
        }
        s += "end";
        s
    }
}

impl<D, Ty> FmtArgs for CodataBr<D, Ty>
where
    D: DtorT + FmtArgs,
    Ty: TypeT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let CodataBr(dtorv, tys, ty) = self;
        let mut s = String::new();
        s += "| .";
        s += &dtorv.fmt_args(fargs);
        s += "(";
        s += &tys
            .into_iter()
            .map(|ty| format!("{}", ty.fmt_args(fargs)))
            .collect::<Vec<_>>()
            .join(", ");
        s += ")";
        s += ": ";
        s += &ty.fmt_args(fargs);
        s
    }
}

impl<TeV, A> FmtArgs for Define<TeV, A>
where
    TeV: VarT + FmtArgs,
    A: ValueT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Define { name, def } = self;
        format!("def {} = {} end", name.fmt_args(fargs), def.fmt_args(fargs))
    }
}
