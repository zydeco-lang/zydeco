use crate::{
    parse::syntax::{Compute, Program, Value},
    syntax::ann::{AnnHolder, AnnInfo},
};

impl AnnHolder for Program {
    fn ann(&self) -> &AnnInfo {
        &self.ann
    }
}

impl AnnHolder for Value {
    fn ann(&self) -> &AnnInfo {
        match self {
            Value::TermAnn(_, _, a) => a,
            Value::Var(_, a) => a,
            Value::Thunk(_, a) => a,
            Value::Ctor(_, _, a) => a,
            Value::Int(_, a) => a,
            Value::String(_, a) => a,
            Value::Char(_, a) => a,
        }
    }
}

impl AnnHolder for Compute {
    fn ann(&self) -> &AnnInfo {
        match self {
            Compute::TermAnn(_, _, a) => a,
            Compute::Let { ann, .. } => ann,
            Compute::Do { ann, .. } => ann,
            Compute::Force(_, a) => a,
            Compute::App(_, _, a) => a,
            Compute::Lam { ann, .. } => ann,
            Compute::Return(_, a) => a,
            Compute::Rec { ann, .. } => ann,
            Compute::Match { ann, .. } => ann,
            Compute::CoMatch { ann, .. } => ann,
            Compute::CoApp { ann, .. } => ann,
        }
    }
}
