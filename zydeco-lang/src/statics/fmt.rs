use super::syntax::*;
use crate::utils::fmt::*;

impl FmtArgs for Type {
    fn fmt_args(&self, fargs: Args) -> String {
        let app = self.app.fmt_args(fargs);
        if let Some(kd) = &self.kd {
            format!("({} :: {})", app, kd.fmt_args(fargs))
        } else {
            app
        }
    }
}

impl FmtArgs for TermValue {
    fn fmt_args(&self, args: Args) -> String {
        match self {
            TermValue::TermAnn(t) => t.fmt_args(args),
            TermValue::Var(t) => t.fmt_args(args),
            TermValue::Thunk(t) => t.fmt_args(args),
            TermValue::Ctor(t) => t.fmt_args(args),
            TermValue::Literal(t) => t.fmt_args(args),
        }
    }
}

impl FmtArgs for TermComputation {
    fn fmt_args(&self, args: Args) -> String {
        match self {
            TermComputation::TermAnn(t) => t.fmt_args(args),
            TermComputation::Ret(t) => t.fmt_args(args),
            TermComputation::Force(t) => t.fmt_args(args),
            TermComputation::Let(t) => t.fmt_args(args),
            TermComputation::Do(t) => t.fmt_args(args),
            TermComputation::Rec(t) => t.fmt_args(args),
            TermComputation::Match(t) => t.fmt_args(args),
            TermComputation::CoMatch(t) => t.fmt_args(args),
            TermComputation::Dtor(t) => t.fmt_args(args),
        }
    }
}

// impl FmtArgs for Module {
//     fn fmt_args(&self, args: Args) -> String {
//         let mut s = String::new();

// }
