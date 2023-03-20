use crate::syntax::{binder::*, SpanInfo};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub decls: Vec<Declare>,
    pub comp: Box<Compute>,
    pub ann: SpanInfo,
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
        ann: SpanInfo,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Data {
    pub name: TypeV,
    pub params: Vec<(TypeV, Kind)>,
    pub ctors: Vec<DataBranch>,
    pub ann: SpanInfo,
}

pub type DataBranch = (CtorV, Vec<Type>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Codata {
    pub name: TypeV,
    pub params: Vec<(TypeV, Kind)>,
    pub dtors: Vec<CodataBranch>,
    pub ann: SpanInfo,
}

pub type CodataBranch = (DtorV, (Vec<Type>, Type));

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    TermAnn(Box<Value>, Type, SpanInfo),
    Var(TermV, SpanInfo),
    Thunk(Box<Compute>, SpanInfo),
    Ctor(CtorV, Vec<Value>, SpanInfo),
    Int(i64, SpanInfo),
    String(String, SpanInfo),
    Char(char, SpanInfo),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Compute {
    TermAnn(Box<Compute>, Type, SpanInfo),
    Let {
        binding: Binding<Type, Value>,
        body: Box<Compute>,
        ann: SpanInfo,
    },
    Do {
        binding: Binding<Type, Compute>,
        body: Box<Compute>,
        ann: SpanInfo,
    },
    Force(Box<Value>, SpanInfo),
    Return(Box<Value>, SpanInfo),
    Lam {
        arg: (TermV, Option<Box<Type>>),
        body: Box<Compute>,
        ann: SpanInfo,
    },
    Rec {
        arg: (TermV, Option<Box<Type>>),
        body: Box<Compute>,
        ann: SpanInfo,
    },
    App(Box<Compute>, Box<Value>, SpanInfo),
    Match {
        scrut: Box<Value>,
        arms: Vec<(CtorV, Vec<TermV>, Box<Compute>)>,
        ann: SpanInfo,
    },
    CoMatch {
        arms: Vec<(DtorV, Vec<TermV>, Box<Compute>)>,
        ann: SpanInfo,
    },
    CoApp {
        body: Box<Compute>,
        dtor: DtorV,
        args: Vec<Value>,
        ann: SpanInfo,
    },
}

pub use crate::syntax::TCtor;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SynType {
    Basic(TCtor, SpanInfo),
    App(Box<SynType>, Box<SynType>, SpanInfo),
    // Thunk(Box<Type>, Span),
    // Ret(Box<Type>, Span),
    Arr(Box<SynType>, Box<SynType>, SpanInfo),
    // OS,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    pub ctor: TCtor,
    pub args: Vec<Type>,
    pub ann: SpanInfo,
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
