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
            Value::Char(_, ann) => ann,
        }
    }
    fn set_ann(&mut self, ann2: Ann) {
        match self {
            Value::Var(_, ann) => *ann = ann2,
            Value::Thunk(_, ann) => *ann = ann2,
            Value::Ctor(_, _, ann) => *ann = ann2,
            Value::Bool(_, ann) => *ann = ann2,
            Value::Int(_, ann) => *ann = ann2,
            Value::String(_, ann) => *ann = ann2,
            Value::Char(_, ann) => *ann = ann2,
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

    fn set_ann(&mut self, ann2: Ann) {
        match self {
            Compute::Let { ann, .. } => *ann = ann2,
            Compute::Rec { ann, .. } => *ann = ann2,
            Compute::Do { ann, .. } => *ann = ann2,
            Compute::Force(_, ann) => *ann = ann2,
            Compute::Return(_, ann) => *ann = ann2,
            Compute::Lam { ann, .. } => *ann = ann2,
            Compute::App(_, _, ann) => *ann = ann2,
            Compute::If { ann, .. } => *ann = ann2,
            Compute::Match { ann, .. } => *ann = ann2,
            Compute::CoMatch { ann, .. } => *ann = ann2,
            Compute::CoApp { ann, .. } => *ann = ann2,
        }
    }
}
