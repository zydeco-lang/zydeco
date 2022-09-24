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
    Let(String, Box<Value<Ann>>, Box<Compute<Ann>>, Ann),
    Do(String, Box<Compute<Ann>>, Box<Compute<Ann>>, Ann),
    Force(Box<Value<Ann>>, Ann),
    Return(Box<Value<Ann>>, Ann),
    Lam(String, Box<TValue<Ann>>, Box<Compute<Ann>>, Ann),
    App(Box<Compute<Ann>>, Box<Value<Ann>>, Ann),
    If(Box<Value<Ann>>, Box<Compute<Ann>>, Box<Compute<Ann>>, Ann),
}
