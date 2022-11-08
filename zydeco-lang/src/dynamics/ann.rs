use super::syntax::*;
use crate::utils::ann::{AnnHolder, AnnT};

impl<Ann: AnnT> AnnHolder<Ann> for ZValue<Ann> {
    fn ann(&self) -> &Ann {
        match self {
            ZValue::Var(_, ann) => ann,
            ZValue::Thunk(_, _, ann) => ann,
            ZValue::Ctor(_, _, ann) => ann,
            ZValue::Bool(_, ann) => ann,
            ZValue::Int(_, ann) => ann,
            ZValue::String(_, ann) => ann,
            ZValue::Triv(_) => todo!(),
        }
    }
    fn set_ann(&mut self, ann2: Ann) {
        match self {
            ZValue::Var(_, ann) => *ann = ann2,
            ZValue::Thunk(_, _, ann) => *ann = ann2,
            ZValue::Ctor(_, _, ann) => *ann = ann2,
            ZValue::Bool(_, ann) => *ann = ann2,
            ZValue::Int(_, ann) => *ann = ann2,
            ZValue::String(_, ann) => *ann = ann2,
            ZValue::Triv(_) => todo!(),
        }
    }
}

impl<Ann: AnnT> AnnHolder<Ann> for ZCompute<Ann> {
    fn ann(&self) -> &Ann {
        match self {
            ZCompute::Let { ann, .. } => ann,
            ZCompute::Rec { ann, .. } => ann,
            ZCompute::Do { ann, .. } => ann,
            ZCompute::Force(_, ann) => ann,
            ZCompute::Return(_, ann) => ann,
            ZCompute::Lam { ann, .. } => ann,
            ZCompute::App(_, _, ann) => ann,
            ZCompute::If { ann, .. } => ann,
            ZCompute::Match { ann, .. } => ann,
            ZCompute::CoMatch { ann, .. } => ann,
            ZCompute::CoApp { ann, .. } => ann,
            ZCompute::Prim { ann, .. } => ann,
        }
    }
    fn set_ann(&mut self, ann2: Ann) {
        match self {
            ZCompute::Let { ann, .. } => *ann = ann2,
            ZCompute::Rec { ann, .. } => *ann = ann2,
            ZCompute::Do { ann, .. } => *ann = ann2,
            ZCompute::Force(_, ann) => *ann = ann2,
            ZCompute::Return(_, ann) => *ann = ann2,
            ZCompute::Lam { ann, .. } => *ann = ann2,
            ZCompute::App(_, _, ann) => *ann = ann2,
            ZCompute::If { ann, .. } => *ann = ann2,
            ZCompute::Match { ann, .. } => *ann = ann2,
            ZCompute::CoMatch { ann, .. } => *ann = ann2,
            ZCompute::CoApp { ann, .. } => *ann = ann2,
            ZCompute::Prim { ann, .. } => *ann = ann2,
        }
    }
}
