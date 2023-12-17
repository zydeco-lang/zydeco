use super::*;
use crate::utils::fmt::*;

impl FmtArgs for () {
    fn fmt_args(&self, _fargs: Args) -> String {
        String::new()
    }
}

impl<Kd> FmtArgs for (TypeV, Kd)
where
    Kd: KindT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let (tvar, kd) = self;
        format!("({} : {})", tvar.fmt_args(fargs), kd.fmt_args(fargs))
    }
}
impl<Kd> FmtArgs for (TypeV, Option<Kd>)
where
    Kd: KindT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let (tvar, kd) = self;
        match kd {
            Some(kd) => format!("({} : {})", tvar.fmt_args(fargs), kd.fmt_args(fargs)),
            None => format!("{}", tvar.fmt_args(fargs)),
        }
    }
}

impl<Term, Type> FmtArgs for Annotation<Term, Type>
where
    Term: FmtArgs,
    Type: FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Annotation { term: body, ty } = self;
        format!("({} : {})", body.fmt_args(fargs), ty.fmt_args(fargs))
    }
}

impl FmtArgs for Hole {
    fn fmt_args(&self, _fargs: Args) -> String {
        "_?".into()
    }
}

impl<In, Out> FmtArgs for Arrow<In, Out>
where
    In: FmtArgs,
    Out: FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Arrow(in_ty, out_ty) = self;
        format!("{} -> {}", in_ty.fmt_args(fargs), out_ty.fmt_args(fargs))
    }
}

impl FmtArgs for KindBase {
    fn fmt_args(&self, _fargs: Args) -> String {
        match self {
            KindBase::VType => "VType".to_owned(),
            KindBase::CType => "CType".to_owned(),
        }
    }
}

impl<In, Out> FmtArgs for TypeArity<In, Out>
where
    In: KindT + FmtArgs,
    Out: KindT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let TypeArity { params, kd } = self;
        let mut s = String::new();
        s += "(";
        s += &params.into_iter().map(|kd| kd.fmt_args(fargs)).collect::<Vec<_>>().join(", ");
        s += ") -> ";
        s += &kd.fmt_args(fargs);
        s
    }
}

impl<TyV, Ty> FmtArgs for TypeAbs<TyV, Ty>
where
    TyV: TyVarT + FmtArgs,
    Ty: TypeT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let TypeAbs { params, body } = self;
        let mut s = String::new();
        s += "fn ";
        for param in params {
            s += &param.fmt_args(fargs);
            s += " ";
        }
        s += " . ";
        s += &body.fmt_args(fargs);
        s
    }
}

impl<TyV, T> FmtArgs for TypeApp<TyV, T>
where
    TyV: TyVarT + FmtArgs,
    T: TypeT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let TypeApp { tvar, args } = self;
        let mut s = String::new();
        // HACK: pretty printing special types syntactically
        let tvar_name = tvar.fmt_args(fargs);
        if tvar_name == "Fn" {
            s += &args.into_iter().map(|arg| arg.fmt_args(fargs)).collect::<Vec<_>>().join(" -> ");
        } else {
            // normal type application
            s += &tvar_name;
            // omit parentheses for empty type application
            if !args.is_empty() {
                s += "(";
                s +=
                    &args.into_iter().map(|arg| arg.fmt_args(fargs)).collect::<Vec<_>>().join(", ");
                s += ")";
            }
        }
        s
    }
}

impl<TyV, Ty> FmtArgs for Forall<TyV, Ty>
where
    TyV: TyVarT + FmtArgs,
    Ty: TypeT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Forall { param, ty } = self;
        format!("forall {} . {}", param.fmt_args(fargs), ty.fmt_args(fargs))
    }
}

impl<TyV, Ty> FmtArgs for Exists<TyV, Ty>
where
    TyV: TyVarT + FmtArgs,
    Ty: TypeT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Exists { param, ty } = self;
        format!("exists {} . {}", param.fmt_args(fargs), ty.fmt_args(fargs))
    }
}

impl<B> FmtArgs for Thunk<B>
where
    B: ComputationT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Thunk(b) = self;
        let mut s = String::new();
        let s_thunk = &b.fmt_args(fargs.indent());
        s += "{";
        if s_thunk.len() > 40 {
            // s += &fargs.indent().br_indent();
            s += " ";
            s += &s_thunk;
            s += &fargs.br_indent();
        } else {
            s += " ";
            s += &s_thunk;
            s += " ";
        }
        s += "}";
        s
    }
}

impl FmtArgs for Literal {
    fn fmt_args(&self, _fargs: Args) -> String {
        match self {
            Literal::Int(i) => format!("{}", i),
            Literal::String(s) => {
                format!("\"{}\"", s.into_iter().collect::<String>().escape_debug())
            }
            Literal::Char(c) => format!("'{}'", c.escape_debug()),
        }
    }
}

impl<C, A> FmtArgs for Ctor<C, A>
where
    C: CtorT + FmtArgs,
    A: ValueT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Ctor { ctorv: ctor, args } = self;
        let mut s = String::new();
        s += &ctor.fmt_args(fargs);
        s += "(";
        s += &args.into_iter().map(|arg| arg.fmt_args(fargs)).collect::<Vec<_>>().join(", ");
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
        format!("pack ({}, {})", ty.fmt_args(fargs), body.fmt_args(fargs))
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
        s += &format!("let {} = {} in", var.fmt_args(fargs), def.fmt_args(fargs),);
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
        s += &format!("do {} <- {} ;", var.fmt_args(fargs), comp.fmt_args(fargs),);
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
        for Matcher { ctorv: ctor, vars, body } in arms {
            s += &fargs.br_indent();
            s += "| ";
            s += &ctor.fmt_args(fargs);
            s += "(";
            s += &vars.into_iter().map(|var| var.fmt_args(fargs)).collect::<Vec<_>>().join(", ");
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

impl<D, TeV, B> FmtArgs for Comatch<D, TeV, B>
where
    D: DtorT + FmtArgs,
    TeV: VarT + FmtArgs,
    B: ComputationT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Comatch { arms } = self;
        let mut s = String::new();
        s += "comatch";
        for Comatcher { dtorv: dtor, vars, body } in arms {
            s += &fargs.br_indent();
            s += "| .";
            s += &dtor.fmt_args(fargs);
            s += "(";
            s += &vars.into_iter().map(|var| var.fmt_args(fargs)).collect::<Vec<_>>().join(", ");
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
        let Dtor { body, dtorv: dtor, args } = self;
        let mut s = String::new();
        s += &body.fmt_args(fargs);
        s += " .";
        s += &dtor.fmt_args(fargs);
        s += "(";
        s += &args.into_iter().map(|arg| arg.fmt_args(fargs)).collect::<Vec<_>>().join(", ");
        s += ")";
        s
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
            "match {} pack ({}, {}) -> {}",
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

impl<TyV, Kd, C, Ty> FmtArgs for Data<TyV, Kd, C, Ty>
where
    TyV: TyVarT + FmtArgs,
    Kd: KindT + FmtArgs,
    C: CtorT + FmtArgs,
    Ty: TypeT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Data { name, params, ctors } = self;
        let mut s = String::new();
        s += "data ";
        s += &name.fmt_args(fargs);
        for (tyvar, kd) in params {
            s += &format!(" ({} : {})", tyvar.fmt_args(fargs), kd.fmt_args(fargs));
        }
        s += " where ";
        {
            let fargs = fargs.indent();
            for databr in ctors {
                s += &fargs.br_indent();
                s += &databr.fmt_args(fargs);
            }
        }
        s += &fargs.br_indent();
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
        let DataBr { ctorv, tys } = self;
        let mut s = String::new();
        s += "| ";
        s += &ctorv.fmt_args(fargs);
        s += "(";
        s += &tys.into_iter().map(|ty| ty.fmt_args(fargs)).collect::<Vec<_>>().join(", ");
        s += ")";
        s
    }
}

impl<TyV, Kd, D, Ty> FmtArgs for Codata<TyV, Kd, D, Ty>
where
    TyV: TyVarT + FmtArgs,
    Kd: KindT + FmtArgs,
    D: DtorT + FmtArgs,
    Ty: TypeT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Codata { name, params, dtors } = self;
        let mut s = String::new();
        s += "codata ";
        s += &name.fmt_args(fargs);
        for (tyvar, kd) in params {
            s += &format!(" ({} : {})", tyvar.fmt_args(fargs), kd.fmt_args(fargs));
        }
        s += " where ";
        {
            let fargs = fargs.indent();
            for codatabr in dtors {
                s += &fargs.br_indent();
                s += &codatabr.fmt_args(fargs);
            }
        }
        s += &fargs.br_indent();
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
        let CodataBr { dtorv, tys, ty } = self;
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

impl<TyV, Kd, Ty> FmtArgs for Alias<TyV, Kd, Ty>
where
    TyV: TyVarT + FmtArgs,
    Kd: KindT + FmtArgs,
    Ty: TypeT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Alias { name, params, ty } = self;
        let mut s = String::new();
        s += "alias ";
        s += &name.fmt_args(fargs);
        for (tyvar, kd) in params {
            s += &format!(" ({} : {})", tyvar.fmt_args(fargs), kd.fmt_args(fargs));
        }
        s += " = ";
        {
            let fargs = fargs.indent();
            s += &fargs.br_indent();
            s += &ty.fmt_args(fargs);
        }
        s += &fargs.br_indent();
        s += "end";
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
