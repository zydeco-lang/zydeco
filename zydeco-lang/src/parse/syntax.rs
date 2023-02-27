use crate::syntax::{binders::*, AnnInfo};

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
    pub params: Vec<(TypeV, Kind)>,
    pub ctors: Vec<DataBranch>,
    pub ann: AnnInfo,
}

pub type DataBranch = (CtorV, Vec<Type>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Codata {
    pub name: TypeV,
    pub params: Vec<(TypeV, Kind)>,
    pub dtors: Vec<CodataBranch>,
    pub ann: AnnInfo,
}

pub type CodataBranch = (DtorV, Vec<Type>, Type);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    TermAnn(Box<Value>, Type, AnnInfo),
    Var(TermV, AnnInfo),
    Thunk(Box<Compute>, AnnInfo),
    Ctor(CtorV, Vec<Value>, AnnInfo),
    Int(i64, AnnInfo),
    String(String, AnnInfo),
    Char(char, AnnInfo),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Compute {
    TermAnn(Box<Compute>, Type, AnnInfo),
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

pub use crate::syntax::TCtor;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SynType {
    Basic(TCtor, AnnInfo),
    App(Box<SynType>, Box<SynType>, AnnInfo),
    // Thunk(Box<Type>, Ann),
    // Ret(Box<Type>, Ann),
    Arr(Box<SynType>, Box<SynType>, AnnInfo),
    // OS,
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

pub use crate::syntax::Kind;
