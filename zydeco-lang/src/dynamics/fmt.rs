use super::syntax::*;
use crate::utils::{
    ann::AnnT,
    fmt::{Args, FmtArgs, FmtDefault},
};

// impl<Ann: AnnT> FmtArgs for ZProgram<Ann> {
//     fn fmt_args(&self, _args: Args) -> String {
//         format!("{:?}", self)
//     }
// }

impl<Ann: AnnT> FmtArgs for ZValue<Ann> {
    fn fmt_args(&self, args: Args) -> String {
        match self {
            ZValue::Var(var, _) => var.fmt_args(args),
            ZValue::Thunk(compute, _, _) => {
                format!("{{ {} }}", compute.fmt_args(args))
            }
            ZValue::Ctor(ctor, cargs, _) => {
                let cargs: Vec<_> =
                    cargs.iter().map(|arg| arg.fmt_args(args)).collect();
                format!("{}({})", ctor, cargs.join(", "))
            }
            ZValue::Bool(b, _) => format!("{}", b),
            ZValue::Int(i, _) => format!("{}", i),
            ZValue::String(s, _) => format!("{:?}", s),
            ZValue::Char(c, _) => format!("{:?}", c),
            ZValue::Triv(_) => format!("()"),
        }
    }
}

impl<Ann: AnnT> FmtArgs for ZCompute<Ann> {
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
            ZCompute::Force(e, _) => format!("!{}", e.fmt_args(args)),
            ZCompute::Return(e, _) => format!("ret {}", e.fmt_args(args)),
            ZCompute::Lam { arg, body, .. } => {
                format!("fn ({}) -> {}", arg, body.fmt_args(args))
            }
            ZCompute::Prim { .. } => format!("prim"),
            ZCompute::Rec { arg, body, .. } => {
                format!("rec ({}) -> {}", arg, body.fmt_args(args))
            }
            ZCompute::App(e, v, _) => {
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
impl FmtDefault for ZValue<()> {}
impl FmtDefault for ZCompute<()> {}
