use crate::utils::ann::{AnnHolder, AnnT};

use super::syntax::*;

impl<Ann: AnnT> AnnHolder<Ann> for Value<Ann> {
    fn ann(&self) -> &Ann {
        match self {
            Value::Var(_, ann) => ann,
            Value::Thunk(_, ann) => ann,
            Value::Ctor(_, _, ann) => ann,
            Value::Bool(_, ann) => ann,
            Value::Int(_, ann) => ann,
            Value::String(_, ann) => ann,
        }
    }
}

impl<Ann: AnnT> AnnHolder<Ann> for Compute<Ann> {
    fn ann(&self) -> &Ann {
        match self {
            Compute::Let { ann, .. } => ann,
            Compute::Rec { ann, .. } => ann,
            Compute::Do { ann, .. } => ann,
            Compute::Force(_, ann) => ann,
            Compute::Return(_, ann) => ann,
            Compute::Lam { ann, .. } => ann,
            Compute::App(_, _, ann) => ann,
            Compute::If { ann, .. } => ann,
            Compute::Match { ann, .. } => ann,
            Compute::CoMatch { ann, .. } => ann,
            Compute::CoApp { ann, .. } => ann,
        }
    }
}
