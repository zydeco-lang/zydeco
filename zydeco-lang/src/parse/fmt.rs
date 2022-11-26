use super::syntax::*;
use crate::utils::fmt::{Args, FmtArgs};

impl<Ann> FmtArgs for Program<Ann> {
    fn fmt_args(&self, args: Args) -> String {
        self.comp.fmt_args(args)
    }
}

impl<Ann> FmtArgs for Value<Ann> {
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
            Value::Bool(b, _) => format!("{}", b),
            Value::Int(n, _) => format!("{}", n),
            Value::String(s, _) => format!("\"{}\"", s),
            Value::Char(c, _) => format!("'{}'", c),
        }
    }
}

impl<Ann> FmtArgs for Compute<Ann> {
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
                format!("!{}", v.fmt_args(fmta))
            }
            Compute::Return(v, _) => {
                format!("ret {}", v.fmt_args(fmta))
            }
            Compute::Lam { arg, body, .. } => {
                let (x, _) = arg;
                format!(
                    "fn ({}) -> {}",
                    // "fn ({}: {}) -> {}",
                    x.fmt_args(fmta),
                    // t.fmt_args(fmta),
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
            Compute::If { cond, thn, els, .. } => {
                format!(
                    "if {}: {} else: {}",
                    cond.fmt_args(fmta),
                    thn.fmt_args(fmta),
                    els.fmt_args(fmta)
                )
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

impl<Ann> FmtArgs for TValue<Ann> {
    fn fmt_args(&self, args: Args) -> String {
        match self {
            TValue::Var(x, _) => format!("{}", x.fmt_args(args)),
            TValue::Comp(c, _) => format!("Thunk({})", c.fmt_args(args)),
        }
    }
}

impl<Ann> FmtArgs for TCompute<Ann> {
    fn fmt_args(&self, args: Args) -> String {
        match self {
            TCompute::Var(x, _) => format!("{}", x.fmt_args(args)),
            TCompute::Ret(v, _) => format!("Ret({})", v.fmt_args(args)),
            TCompute::Lam(t, c, _) => {
                format!("{} -> {}", t.fmt_args(args), c.fmt_args(args))
            }
            TCompute::Os => format!("OS"),
        }
    }
}

impl<Ann> std::fmt::Display for Program<Ann> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fmt_args(Args::new(2)))
    }
}
impl<Ann> std::fmt::Display for TValue<Ann> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fmt_args(Args::new(2)))
    }
}
impl<Ann> std::fmt::Display for TCompute<Ann> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fmt_args(Args::new(2)))
    }
}
impl<Ann> std::fmt::Display for Value<Ann> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fmt_args(Args::new(2)))
    }
}
impl<Ann> std::fmt::Display for Compute<Ann> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fmt_args(Args::new(2)))
    }
}

macro_rules! var_fmt {
    ($Var:ident) => {
        impl<Ann> FmtArgs for $Var<Ann> {
            fn fmt_args(&self, _args: Args) -> String {
                format!("{}", self.name())
            }
        }
        impl<Ann> std::fmt::Display for $Var<Ann> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{}", self.fmt_args(Args::new(2)))
            }
        }
    };
}

var_fmt!(TVar);
var_fmt!(Ctor);
var_fmt!(Dtor);
var_fmt!(VVar);
