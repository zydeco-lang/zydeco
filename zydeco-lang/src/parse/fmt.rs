use super::syntax::*;
use crate::{
    syntax::binders::*,
    utils::fmt::{Args, FmtArgs},
};

impl FmtArgs for Program {
    fn fmt_args(&self, args: Args) -> String {
        self.comp.fmt_args(args)
    }
}

impl FmtArgs for Value {
    fn fmt_args(&self, args: Args) -> String {
        match self {
            Value::Var(x, _) => format!("{}", x.fmt_args(args)),
            Value::Thunk(e, _) => format!("{{ {} }}", e.fmt_args(args)),
            Value::Ctor(ctor, vs, _) => format!(
                "{}({})",
                ctor.fmt_args(args),
                vs.into_iter()
                    .map(|v| v.fmt_args(args))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            Value::Int(n, _) => format!("{}", n),
            Value::String(s, _) => format!("\"{}\"", s),
            Value::Char(c, _) => format!("'{}'", c),
        }
    }
}

impl FmtArgs for Compute {
    fn fmt_args(&self, fmta: Args) -> String {
        match self {
            Compute::Let { binding, body, .. } => {
                let (x, _, v) = binding;
                format!(
                    "let {} = {};{}{}",
                    x.fmt_args(fmta),
                    v.fmt_args(fmta),
                    fmta.force_space(),
                    body.fmt_args(fmta)
                )
            }
            Compute::Do { binding, body, .. } => {
                let (x, _, v) = binding;
                format!(
                    "do {} <- {};{}{}",
                    x.fmt_args(fmta),
                    v.fmt_args(fmta),
                    fmta.force_space(),
                    body.fmt_args(fmta)
                )
            }
            Compute::Force(v, _) => {
                format!("! {}", v.fmt_args(fmta))
            }
            Compute::Return(v, _) => {
                format!("ret {}", v.fmt_args(fmta))
            }
            Compute::Lam { arg: (x, None), body, .. } => {
                format!("fn ({}) -> {}", x.fmt_args(fmta), body.fmt_args(fmta))
            }
            Compute::Lam { arg: (x, Some(t)), body, .. } => {
                format!(
                    "fn ({}: {}) -> {}",
                    x.fmt_args(fmta),
                    t.fmt_args(fmta),
                    body.fmt_args(fmta)
                )
            }
            Compute::Rec { arg, body, .. } => {
                let (x, _) = arg;
                format!("rec ({}) -> {}", x.fmt_args(fmta), body.fmt_args(fmta))
            }
            Compute::App(e, v, _) => {
                format!("{} {}", e.fmt_args(fmta), v.fmt_args(fmta),)
            }
            Compute::Match { scrut, cases, .. } => {
                format!("match {} {}", scrut.fmt_args(fmta), {
                    let args = fmta.indent();
                    cases
                        .into_iter()
                        .map(|(ctor, vs, e)| {
                            format!(
                                "{}| {}({}) -> {}",
                                args.force_space(),
                                ctor.fmt_args(args),
                                vs.into_iter()
                                    .map(|v| v.fmt_args(args))
                                    .collect::<Vec<_>>()
                                    .join(", "),
                                e.fmt_args(args),
                            )
                        })
                        .collect::<Vec<_>>()
                        .join("")
                })
            }
            Compute::CoMatch { cases, .. } => {
                format!("comatch {}", {
                    let args = fmta.indent();
                    cases
                        .into_iter()
                        .map(|(dtor, vs, e)| {
                            format!(
                                "{}.{}({}) -> {}",
                                args.force_space(),
                                dtor.fmt_args(args),
                                vs.into_iter()
                                    .map(|v| v.fmt_args(args))
                                    .collect::<Vec<_>>()
                                    .join(", "),
                                e.fmt_args(args),
                            )
                        })
                        .collect::<Vec<_>>()
                        .join("")
                })
            }
            Compute::CoApp { body: scrut, dtor, args, .. } => {
                format!(
                    "{}.{}({})",
                    scrut.fmt_args(fmta),
                    dtor.fmt_args(fmta),
                    args.into_iter()
                        .map(|v| v.fmt_args(fmta))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

impl FmtArgs for Kind {
    fn fmt_args(&self, _args: Args) -> String {
        match self {
            Kind::CompType => "CompType".to_owned(),
            Kind::ValType => "ValType".to_owned(),
        }
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

impl FmtArgs for Type {
    fn fmt_args(&self, args: Args) -> String {
        let mut s = self.ctor.fmt_args(args);
        for arg in &self.args {
            if arg.args.len() != 0 {
                s.push_str(&format!(" ({})", arg.fmt_args(args)))
            } else {
                s.push_str(&format!(" {}", arg.fmt_args(args)))
            }
        }
        s
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fmt_args(Args::new(2)))
    }
}
impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fmt_args(Args::new(2)))
    }
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fmt_args(Args::new(2)))
    }
}
impl std::fmt::Display for TCtor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fmt_args(Args::new(2)))
    }
}
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fmt_args(Args::new(2)))
    }
}
impl std::fmt::Display for Compute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fmt_args(Args::new(2)))
    }
}

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

var_fmt!(TypeV);
var_fmt!(CtorV);
var_fmt!(DtorV);
var_fmt!(TermV);
