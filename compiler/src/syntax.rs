pub mod fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program<Ann> {
    pub decls: Vec<Declare<Ann>>,
    pub comp: Box<Compute<Ann>>,
    pub ann: Ann,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Declare<Ann> {
    Data {
        name: TVar<Ann>,
        ctors: Vec<(Ctor<Ann>, Vec<TValue<Ann>>)>,
        ann: Ann,
    },
    Codata {
        name: TVar<Ann>,
        dtors: Vec<(Dtor<Ann>, Vec<TValue<Ann>>, TCompute<Ann>)>,
        ann: Ann,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value<Ann> {
    Var(VVar<Ann>, Ann),
    Thunk(Box<Compute<Ann>>, Ann),
    Ctor(Ctor<Ann>, Vec<Value<Ann>>, Ann),
    Bool(bool, Ann),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Compute<Ann> {
    Let {
        binding: (VVar<Ann>, Box<Value<Ann>>),
        body: Box<Compute<Ann>>,
        ann: Ann,
    },
    Do {
        binding: (VVar<Ann>, Box<Compute<Ann>>),
        body: Box<Compute<Ann>>,
        ann: Ann,
    },
    Force(Box<Value<Ann>>, Ann),
    Return(Box<Value<Ann>>, Ann),
    Lam {
        arg: (VVar<Ann>, Box<TValue<Ann>>),
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
    Match {
        scrut: Box<Value<Ann>>,
        cases: Vec<(Ctor<Ann>, Vec<VVar<Ann>>, Box<Compute<Ann>>)>,
        ann: Ann,
    },
    CoMatch {
        cases: Vec<(Dtor<Ann>, Vec<VVar<Ann>>, Box<Compute<Ann>>)>,
        ann: Ann,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TValue<Ann> {
    Var(TVar<Ann>, Ann),
    Comp(Box<TCompute<Ann>>, Ann),
    Bool(Ann),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TCompute<Ann> {
    Var(TVar<Ann>, Ann),
    Ret(Box<TValue<Ann>>, Ann),
    Lam(Box<TValue<Ann>>, Box<TCompute<Ann>>, Ann),
}

macro_rules! var {
    ( $Var:ident ) => {
        #[derive(Clone, Debug)]
        pub struct $Var<Ann>(String, Ann);
        impl<Ann> $Var<Ann> {
            pub fn new(s: String, ann: Ann) -> Self {
                Self(s, ann)
            }
        }
        impl<Ann> std::cmp::PartialEq for $Var<Ann> {
            fn eq(&self, other: &Self) -> bool {
                self.0 == other.0
            }
        }
        impl<Ann> std::cmp::Eq for $Var<Ann> {}
        impl<Ann> std::hash::Hash for $Var<Ann> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.0.hash(state);
            }
        }
        impl<Ann> std::fmt::Display for $Var<Ann> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }
    };
}

var!(Ctor);
var!(Dtor);
var!(TVar);
var!(VVar);
