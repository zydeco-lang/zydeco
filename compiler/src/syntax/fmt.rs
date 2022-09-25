use super::*;

pub trait FmtWithArgs {
    fn fmt_with_args(&self, args: Args) -> String;
}

#[derive(Clone)]
pub struct Args {
    indent_unit: usize,
    pub indent: usize,
}
impl Args {
    pub fn new(indent_unit: usize) -> Self {
        Self {
            indent_unit,
            indent: 0,
        }
    }
    pub fn indent(&self) -> Self {
        Self {
            indent: self.indent + self.indent_unit,
            ..self.clone()
        }
    }
    pub fn gen_space(&self) -> String {
        format!("\n{}", " ".repeat(self.indent))
    }
}

impl<Ann> FmtWithArgs for TValue<Ann> {
    fn fmt_with_args(&self, args: Args) -> String {
        match self {
            TValue::Comp(c, _) => format!("Comp({})", c.fmt_with_args(args)),
            TValue::Bool(_) => format!("Bool"),
        }
    }
}

impl<Ann> FmtWithArgs for TCompute<Ann> {
    fn fmt_with_args(&self, args: Args) -> String {
        match self {
            TCompute::Ret(v, _) => format!("Ret({})", v.fmt_with_args(args)),
            TCompute::Lam(t, c, _) => {
                format!(
                    "{} -> {}",
                    t.fmt_with_args(args.clone()),
                    c.fmt_with_args(args)
                )
            }
        }
    }
}

impl<Ann> FmtWithArgs for Value<Ann> {
    fn fmt_with_args(&self, args: Args) -> String {
        match self {
            Value::Var(x, _) => format!("{}", x),
            Value::Thunk(e, _) => format!("{{ {} }}", e.fmt_with_args(args)),
            Value::Bool(b, _) => format!("{}", b),
        }
    }
}

impl<Ann> FmtWithArgs for Compute<Ann> {
    fn fmt_with_args(&self, args: Args) -> String {
        match self {
            Compute::Let { binding, body, .. } => {
                let (x, v) = binding;
                format!(
                    "let {} = {};{}{}",
                    x,
                    v.fmt_with_args(args.clone()),
                    args.gen_space(),
                    body.fmt_with_args(args)
                )
            }
            Compute::Do { binding, body, .. } => {
                let (x, v) = binding;
                format!(
                    "do {} <- {};{}{}",
                    x,
                    v.fmt_with_args(args.clone()),
                    args.gen_space(),
                    body.fmt_with_args(args)
                )
            }
            Compute::Force(v, _) => {
                format!("!{}", v.fmt_with_args(args))
            }
            Compute::Return(v, _) => {
                format!("ret {}", v.fmt_with_args(args))
            }
            Compute::Lam { arg, body, .. } => {
                let (x, t) = arg;
                format!(
                    "fn ({}: {}) {{ {} }}",
                    x,
                    t.fmt_with_args(args.clone()),
                    body.fmt_with_args(args)
                )
            }
            Compute::App(e, v, _) => {
                format!(
                    "{} ' {}",
                    v.fmt_with_args(args.clone()),
                    e.fmt_with_args(args)
                )
            }
            Compute::If { cond, thn, els, .. } => {
                format!(
                    "if {} {{ {} }} else {{ {} }}",
                    cond.fmt_with_args(args.clone()),
                    thn.fmt_with_args(args.clone()),
                    els.fmt_with_args(args)
                )
            }
        }
    }
}

pub trait FmtDefault: FmtWithArgs {
    fn fmt(&self) -> String {
        self.fmt_with_args(Args::new(2))
    }
}

impl FmtDefault for TValue<()> {}
impl FmtDefault for TCompute<()> {}
impl FmtDefault for Value<()> {}
impl FmtDefault for Compute<()> {}