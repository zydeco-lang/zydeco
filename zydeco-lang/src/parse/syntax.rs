use crate::utils::ann::AnnInfo;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub decls: Vec<Declare>,
    pub comp: Box<Compute>,
    pub ann: AnnInfo,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValOrComp {
    Val(Value),
    Comp(Compute),
}

pub type Binding<Ty, Def> = (TermV, Option<Box<Ty>>, Box<Def>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Declare {
    Data(Data),
    Codata(Codata),
    Define {
        public: bool,
        name: TermV,
        ty: Option<Box<Type>>,
        def: Option<Box<Value>>,
        ann: AnnInfo,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Data {
    pub name: TypeV,
    pub args: Vec<TypeV>,
    pub ctors: Vec<DataBranch>,
    pub ann: AnnInfo,
}

pub type DataBranch = (CtorV, Vec<Type>);

impl Into<Type> for &Data {
    fn into(self) -> Type {
        Type {
            ctor: TCtor::Var(self.name.clone()),
            args: self
                .args
                .iter()
                .map(|name| Type {
                    ctor: TCtor::Var(name.clone()),
                    args: Vec::new(),
                    ann: self.ann.clone(),
                })
                .collect(),
            ann: self.ann.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Codata {
    pub name: TypeV,
    pub args: Vec<TypeV>,
    pub dtors: Vec<CodataBranch>,
    pub ann: AnnInfo,
}

pub type CodataBranch = (DtorV, Vec<Type>, Type);

impl Into<Type> for &Codata {
    fn into(self) -> Type {
        Type {
            ctor: TCtor::Var(self.name.clone()),
            args: self
                .args
                .iter()
                .map(|name| Type {
                    ctor: TCtor::Var(name.clone()),
                    args: Vec::new(),
                    ann: self.ann.clone(),
                })
                .collect(),
            ann: self.ann.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Var(TermV, AnnInfo),
    Thunk(Box<Compute>, AnnInfo),
    Ctor(CtorV, Vec<Value>, AnnInfo),
    Int(i64, AnnInfo),
    String(String, AnnInfo),
    Char(char, AnnInfo),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Compute {
    Let {
        binding: Binding<Type, Value>,
        body: Box<Compute>,
        ann: AnnInfo,
    },
    Do {
        binding: Binding<Type, Compute>,
        body: Box<Compute>,
        ann: AnnInfo,
    },
    Force(Box<Value>, AnnInfo),
    Return(Box<Value>, AnnInfo),
    Lam {
        arg: (TermV, Option<Box<Type>>),
        body: Box<Compute>,
        ann: AnnInfo,
    },
    Rec {
        arg: (TermV, Option<Box<Type>>),
        body: Box<Compute>,
        ann: AnnInfo,
    },
    App(Box<Compute>, Box<Value>, AnnInfo),
    Match {
        scrut: Box<Value>,
        cases: Vec<(CtorV, Vec<TermV>, Box<Compute>)>,
        ann: AnnInfo,
    },
    CoMatch {
        cases: Vec<(DtorV, Vec<TermV>, Box<Compute>)>,
        ann: AnnInfo,
    },
    CoApp {
        body: Box<Compute>,
        dtor: DtorV,
        args: Vec<Value>,
        ann: AnnInfo,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SynType {
    Basic(TCtor, AnnInfo),
    App(Box<SynType>, Box<SynType>, AnnInfo),
    // Thunk(Box<Type>, Ann),
    // Ret(Box<Type>, Ann),
    Arr(Box<SynType>, Box<SynType>, AnnInfo),
    // OS,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TCtor {
    Var(TypeV),
    Thunk,
    Ret,
    OS,
    Fun,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    pub ctor: TCtor,
    pub args: Vec<Type>,
    pub ann: AnnInfo,
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
            SynType::App(f, a, _) => {
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
        pub struct $Var(String, AnnInfo);
        impl $Var {
            pub fn new(s: String, ann: AnnInfo) -> Self {
                Self(s, ann)
            }
            pub fn name(&self) -> &str {
                &self.0
            }
            pub fn ann(&self) -> &AnnInfo {
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

var!(CtorV);
var!(DtorV);
var!(TypeV);
var!(TermV);
