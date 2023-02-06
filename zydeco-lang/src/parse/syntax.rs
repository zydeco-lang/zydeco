use crate::{syntax::binders::*, utils::ann::AnnInfo};

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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    ValType,
    CompType,
}

pub mod new_syntax {
    use crate::{syntax::*, utils::ann::Ann};
    use enum_dispatch::enum_dispatch;
    use std::rc::Rc;

    /* ---------------------------------- Kind ---------------------------------- */

    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    pub enum Kind {
        VType,
        CType,
    }

    /* ---------------------------------- Type ---------------------------------- */

    #[enum_dispatch(TypeT)]
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum Type {
        TypeAnn(Ann<TypeAnn<T, Ann<Kind>>>),
        TCtor(Ann<TypeApp<TCtor, T>>),
    }
    type T = Rc<Type>;
    impl TypeT for Type {}

    /* ---------------------------------- Term ---------------------------------- */

    #[enum_dispatch(ValueT)]
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum TermValue {
        TermAnn(Ann<TermAnn<TV, T>>),
        Var(Ann<TermV>),
        Thunk(Ann<Thunk<TC>>),
        Ctor(Ann<Ctor<Ann<CtorV>, TV>>),
        Literal(Ann<Literal>),
    }
    type TV = Rc<TermValue>;
    impl ValueT for TermValue {}

    #[enum_dispatch(ComputationT)]
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum TermComputation {
        TermAnn(Ann<TermAnn<TC, T>>),
        Ret(Ann<Ret<TV>>),
        Force(Ann<Force<TV>>),
        Let(Ann<Let<Ann<TermV>, TV, TC>>),
        Do(Ann<Do<Ann<TermV>, TC>>),
        Lam(Ann<Lam<Ann<TermV>, TC>>),
        App(Ann<App<TC, TV>>),
        Rec(Ann<Rec<Ann<TermV>, TC>>),
        Match(Ann<Match<Ann<TermV>, TV, TC>>),
        CoMatch(Ann<CoMatch<Ann<TermV>, TC>>),
        Dtor(Ann<Dtor<Ann<DtorV>, TC, TV>>),
    }
    type TC = Rc<TermComputation>;
    impl ComputationT for TermComputation {}

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum Term {
        Val(TermValue),
        Comp(TermComputation),
    }

    /* --------------------------------- Module --------------------------------- */

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct Module {
        pub name: Option<String>,
        pub decls: Vec<Declaration>,
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct Declaration {
        pub public: bool,
        pub decl: Declare,
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum Declare {
        Data(Ann<Data<Ann<TermV>, Ann<CtorV>, T>>),
        Codata(Ann<Codata<Ann<TermV>, Ann<DtorV>, T>>),
        Define(Ann<Define<Ann<TermV>, T, TV>>),
        Entry(Ann<TermComputation>),
    }
}
