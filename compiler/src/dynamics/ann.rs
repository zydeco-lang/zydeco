use super::syntax::*;

impl<Ann> ZValue<Ann> {
    pub fn ann(&self) -> &Ann {
        match self {
            ZValue::Var(_, ann) => ann,
            ZValue::Thunk(_, _, ann) => ann,
            ZValue::Ctor(_, _, ann) => ann,
            ZValue::Bool(_, ann) => ann,
            ZValue::Int(_, ann) => ann,
            ZValue::String(_, ann) => ann,
        }
    }
}

impl<Ann> ZCompute<Ann> {
    pub fn ann(&self) -> &Ann {
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
        }
    }
}
