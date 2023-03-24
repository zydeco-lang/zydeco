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
                    write!(f, "{}", self.fmt_args(Args::new(2)))
                }
            }
        };
    }

    var_fmt!(CtorV);
    var_fmt!(DtorV);
    var_fmt!(TypeV);
    var_fmt!(TermV);
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

impl<Type, Kind> FmtArgs for TypeAnn<Type, Kind>
where
    Type: FmtArgs,
    Kind: FmtArgs,
{
    fn fmt_args(&self, args: Args) -> String {
        let TypeAnn { ty, kd } = self;
        format!("{} : {}", ty.fmt_args(args), kd.fmt_args(args))
    }
}

impl FmtArgs for TCtor {
    fn fmt_args(&self, args: Args) -> String {
        match self {
            TCtor::Var(x) => x.fmt_args(args),
            TCtor::Thunk => format!("Thunk"),
            TCtor::Ret => format!("Ret"),
            TCtor::OS => "OS".to_owned(),
            TCtor::Fun => "Fun".to_owned(),
        }
    }
}

impl<TyV, T> FmtArgs for TypeApp<TyV, T>
where
    TyV: FmtArgs,
    T: TypeT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let TypeApp { tctor, args } = self;
        let mut s = String::new();
        s += &tctor.fmt_args(fargs);
        for arg in args {
            s += " ";
            s += &arg.fmt_args(fargs);
        }
        s
    }
}

impl<Term, Type> FmtArgs for TermAnn<Term, Type>
where
    Term: FmtArgs,
    Type: FmtArgs,
{
    fn fmt_args(&self, args: Args) -> String {
        let TermAnn { term: body, ty } = self;
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
    C: FmtArgs,
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
        format!(
            "let {} = {} in {}",
            var.fmt_args(fargs),
            def.fmt_args(fargs),
            body.fmt_args(fargs)
        )
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
        format!(
            "do {} <- {} ; {}",
            var.fmt_args(fargs),
            comp.fmt_args(fargs),
            body.fmt_args(fargs)
        )
    }
}

impl<TeV, B> FmtArgs for Rec<TeV, B>
where
    TeV: VarT + FmtArgs,
    B: ComputationT + FmtArgs,
{
    fn fmt_args(&self, args: Args) -> String {
        let Rec { var, body } = self;
        let mut s = String::new();
        s += "rec ";
        s += &var.fmt_args(args);
        s += " -> ";
        s += &body.fmt_args(args);
        s
    }
}

impl<C, TeV, A, B> FmtArgs for Match<C, TeV, A, B>
where
    C: FmtArgs,
    TeV: VarT + FmtArgs,
    A: ValueT + FmtArgs,
    B: ComputationT + FmtArgs,
{
    fn fmt_args(&self, args: Args) -> String {
        let Match { scrut, arms } = self;
        let mut s = String::new();
        s += "match ";
        s += &scrut.fmt_args(args);
        s += " ";
        for Matcher { ctor, vars, body } in arms {
            s += "| ";
            s += &ctor.fmt_args(args);
            s += "(";
            s += &vars
                .into_iter()
                .map(|var| var.fmt_args(args))
                .collect::<Vec<_>>()
                .join(", ");
            s += ") -> ";
            s += &body.fmt_args(args);
        }
        s
    }
}

impl<D, TeV, B> FmtArgs for CoMatch<D, TeV, B>
where
    D: FmtArgs,
    TeV: VarT + FmtArgs,
    B: ComputationT + FmtArgs,
{
    fn fmt_args(&self, args: Args) -> String {
        let CoMatch { arms } = self;
        let mut s = String::new();
        s += "comatch ";
        for CoMatcher { dtor, vars, body } in arms {
            s += "| ";
            s += &dtor.fmt_args(args);
            s += "(";
            s += &vars
                .into_iter()
                .map(|var| var.fmt_args(args))
                .collect::<Vec<_>>()
                .join(", ");
            s += ") -> ";
            s += &body.fmt_args(args);
        }
        s
    }
}

impl<B, D, A> FmtArgs for Dtor<B, D, A>
where
    B: ComputationT + FmtArgs,
    D: FmtArgs,
    A: ValueT + FmtArgs,
{
    fn fmt_args(&self, fargs: Args) -> String {
        let Dtor { body, dtor, args } = self;
        let mut s = String::new();
        s += &body.fmt_args(fargs);
        s += " ";
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
