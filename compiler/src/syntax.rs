pub mod fmt;

#[derive(Clone, Debug)]
pub struct Var<Ann>(String, Ann);
impl<Ann> Var<Ann> {
    pub fn new(s: String, ann: Ann) -> Self {
        Self(s, ann)
    }
}
impl<Ann> std::cmp::PartialEq for Var<Ann> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<Ann> std::cmp::Eq for Var<Ann> {}
impl<Ann> std::hash::Hash for Var<Ann> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TValue<Ann> {
    Comp(Box<TCompute<Ann>>, Ann),
    Bool(Ann),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TCompute<Ann> {
    Ret(Box<TValue<Ann>>, Ann),
    Lam(Box<TValue<Ann>>, Box<TCompute<Ann>>, Ann),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value<Ann> {
    Var(Var<Ann>, Ann),
    Thunk(Box<Compute<Ann>>, Ann),
    Bool(bool, Ann),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Compute<Ann> {
    Let {
        binding: (Var<Ann>, Box<Value<Ann>>),
        body: Box<Compute<Ann>>,
        ann: Ann,
    },
    Do {
        binding: (Var<Ann>, Box<Compute<Ann>>),
        body: Box<Compute<Ann>>,
        ann: Ann,
    },
    Force(Box<Value<Ann>>, Ann),
    Return(Box<Value<Ann>>, Ann),
    Lam {
        arg: (Var<Ann>, Box<TValue<Ann>>),
        body: Box<Compute<Ann>>,
        ann: Ann,
    },
    App(Box<Compute<Ann>>, Box<Value<Ann>>, Ann),
    If {
        cond: Box<Value<Ann>>,
        thn: Box<Compute<Ann>>,
        els: Box<Compute<Ann>>,
        ann: Ann,
    },
}
