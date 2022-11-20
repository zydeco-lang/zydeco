use super::syntax::*;
use crate::utils::fmt::{Args, FmtArgs};

impl FmtArgs for ZValue {
    fn fmt_args(&self, args: Args) -> String {
        match self {
            ZValue::Var(var) => var.to_string(),
            ZValue::Thunk(_, _) => format!("<thunk>"),
            ZValue::Ctor(ctor, cargs) => {
                let cargs: Vec<_> =
                    cargs.iter().map(|arg| arg.fmt_args(args)).collect();
                format!("{}({})", ctor, cargs.join(", "))
            }
            ZValue::Bool(b) => format!("{}", b),
            ZValue::Int(i) => format!("{}", i),
            ZValue::String(s) => format!("\"{}\"", s),
            ZValue::Char(c) => format!("\'{}\'", c),
            ZValue::Triv() => format!("()"),
        }
    }
}

impl FmtArgs for ZCompute {
    fn fmt_args(&self, args: Args) -> String {
        match self {
            ZCompute::Let { binding, body, .. } => format!(
                "let {} = {}; {}",
                binding.0,
                binding.1.fmt_args(args),
                body.fmt_args(args)
            ),
            ZCompute::Do { binding, body, .. } => format!(
                "do {} <- {}; {}",
                binding.0,
                binding.1.fmt_args(args),
                body.fmt_args(args)
            ),
            ZCompute::Force(e) => format!("!{}", e.fmt_args(args)),
            ZCompute::Return(e) => format!("ret {}", e.fmt_args(args)),
            ZCompute::Lam { arg, body, .. } => {
                format!("fn ({}) -> {}", arg, body.fmt_args(args))
            }
            ZCompute::Prim { .. } => format!("prim"),
            ZCompute::Rec { arg, body, .. } => {
                format!("rec ({}) -> {}", arg, body.fmt_args(args))
            }
            ZCompute::App(e, v) => {
                format!("{} {}", e.fmt_args(args), v.fmt_args(args))
            }
            ZCompute::If { cond, thn, els, .. } => format!(
                "if {}: {} else: {}",
                cond.fmt_args(args),
                thn.fmt_args(args),
                els.fmt_args(args)
            ),
            ZCompute::Match { scrut, cases, .. } => format!(
                "match {}{}",
                scrut.fmt_args(args),
                cases
                    .iter()
                    .map(|(ctor, vars, body)| format!(
                        " | {}({}) -> {}",
                        ctor,
                        vars.iter()
                            .map(|v| v.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                        body.fmt_args(args)
                    ))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            ZCompute::CoMatch { cases, .. } => format!(
                "comatch {}",
                cases
                    .iter()
                    .map(|(dtor, vars, body)| format!(
                        " | .{}({}) -> {}",
                        dtor,
                        vars.iter()
                            .map(|v| v.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                        body.fmt_args(args)
                    ))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            ZCompute::CoApp { scrut, dtor, args: dargs, .. } => format!(
                "{} .{}({})",
                scrut.fmt_args(args),
                dtor,
                dargs
                    .iter()
                    .map(|arg| arg.fmt_args(args))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

// impl FmtDefault for ZProgram<()> {}
impl std::fmt::Display for ZValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.fmt_args(Args::new(2)))
    }
}
impl std::fmt::Display for ZCompute {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.fmt_args(Args::new(2)))
    }
}
