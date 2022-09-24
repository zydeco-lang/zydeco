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
    Var(String, Ann),
    Thunk(Box<Compute<Ann>>, Ann),
    Bool(bool, Ann),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Compute<Ann> {
    Let {
        binding: (String, Box<Value<Ann>>),
        body: Box<Compute<Ann>>,
        ann: Ann,
    },
    Do {
        binding: (String, Box<Compute<Ann>>),
        body: Box<Compute<Ann>>,
        ann: Ann,
    },
    Force(Box<Value<Ann>>, Ann),
    Return(Box<Value<Ann>>, Ann),
    Lam {
        arg: (String, Box<TValue<Ann>>),
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
