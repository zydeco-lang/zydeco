use crate::utils::ann::Ann;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub decls: Vec<Declare>,
    pub comp: Box<Compute>,
    pub ann: Ann,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValOrComp {
    Val(Value),
    Comp(Compute),
}

pub type Binding<Ty, Def> = (VVar, Option<Box<Ty>>, Box<Def>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Declare {
    Data(Data),
    Codata(Codata),
    Define {
        public: bool,
        name: VVar,
        ty: Option<Box<Type>>,
        def: Option<Box<Value>>,
        ann: Ann,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Data {
    pub name: TVar,
    pub args: Vec<TVar>,
    pub ctors: Vec<DataBranch>,
    pub ann: Ann,
}

pub type DataBranch = (Ctor, Vec<Type>);

impl Into<Type> for &Data {
    fn into(self) -> Type {
        Type {
            ctor: TCtor::Var(self.name),
            args: self
                .args
                .into_iter()
                .map(|name| Type {
                    ctor: TCtor::Var(name),
                    args: Vec::new(),
                    ann: self.ann.clone(),
                })
                .collect(),
            ann: self.ann,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Codata {
    pub name: TVar,
    pub args: Vec<TVar>,
    pub dtors: Vec<CodataBranch>,
    pub ann: Ann,
}

pub type CodataBranch = (Dtor, Vec<Type>, Type);

impl Into<Type> for &Codata {
    fn into(self) -> Type {
        Type {
            ctor: TCtor::Var(self.name),
            args: self
                .args
                .into_iter()
                .map(|name| Type {
                    ctor: TCtor::Var(name),
                    args: Vec::new(),
                    ann: self.ann.clone(),
                })
                .collect(),
            ann: self.ann,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Var(VVar, Ann),
    Thunk(Box<Compute>, Ann),
    Ctor(Ctor, Vec<Value>, Ann),
    Int(i64, Ann),
    String(String, Ann),
    Char(char, Ann),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Compute {
    Let {
        binding: Binding<Type, Value>,
        body: Box<Compute>,
        ann: Ann,
    },
    Do {
        binding: Binding<Type, Compute>,
        body: Box<Compute>,
        ann: Ann,
    },
    Force(Box<Value>, Ann),
    Return(Box<Value>, Ann),
    Lam {
        arg: (VVar, Option<Box<Type>>),
        body: Box<Compute>,
        ann: Ann,
    },
    Rec {
        arg: (VVar, Option<Box<Type>>),
        body: Box<Compute>,
        ann: Ann,
    },
    App(Box<Compute>, Box<Value>, Ann),
    Match {
        scrut: Box<Value>,
        cases: Vec<(Ctor, Vec<VVar>, Box<Compute>)>,
        ann: Ann,
    },
    CoMatch {
        cases: Vec<(Dtor, Vec<VVar>, Box<Compute>)>,
        ann: Ann,
    },
    CoApp {
        body: Box<Compute>,
        dtor: Dtor,
        args: Vec<Value>,
        ann: Ann,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SynType {
    Basic(TCtor, Ann),
    App(Box<SynType>, Box<SynType>, Ann),
    // Thunk(Box<Type>, Ann),
    // Ret(Box<Type>, Ann),
    Arr(Box<SynType>, Box<SynType>, Ann),
    // OS,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TCtor {
    Var(TVar),
    Thunk,
    Ret,
    OS,
    Fun,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    pub ctor: TCtor,
    pub args: Vec<Type>,
    pub ann: Ann,
}

impl SynType {
    pub fn lower(self) -> Box<Type> {
        match self {
            SynType::Basic(ctor, a) => {
                Box::new(Type { ctor, ann: a, args: vec![] })
            }
            SynType::Arr(dom, cod, ann) => Box::new(Type {
                ctor: TCtor::Fun,
                args: vec![*dom.lower(), *cod.lower()],
                ann,
            }),
            SynType::App(f, a, ann) => {
                let mut f = f.lower();
                let a = a.lower();
                f.args.push(*a);
                f
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    ValType,
    CompType,
}

macro_rules! var {
    ( $Var:ident ) => {
        #[derive(Clone, Debug)]
        pub struct $Var(String, Ann);
        impl $Var {
            pub fn new(s: String, ann: Ann) -> Self {
                Self(s, ann)
            }
            pub fn name(&self) -> &str {
                &self.0
            }
            pub fn ann(&self) -> &Ann {
                &self.1
            }
        }
        impl std::cmp::PartialEq for $Var {
            fn eq(&self, other: &Self) -> bool {
                self.0 == other.0
            }
        }
        impl std::cmp::Eq for $Var {}
        impl std::hash::Hash for $Var {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.0.hash(state);
            }
        }
    };
}

var!(Ctor);
var!(Dtor);
var!(TVar);
var!(VVar);
