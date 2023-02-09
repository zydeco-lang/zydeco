use crate::{syntax::*, utils::ann::Ann};
use enum_dispatch::enum_dispatch;
use std::rc::Rc;

/* ---------------------------------- Kind ---------------------------------- */

pub use crate::syntax::Kind;

/* ---------------------------------- Type ---------------------------------- */

#[enum_dispatch(TypeT)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    TypeAnn(Ann<TypeAnn<T, Ann<Kind>>>),
    TypeApp(Ann<TypeApp<TCtor, T>>),
}
type T = Rc<Type>;
impl TypeT for Type {}

mod inject_type {
    use super::*;

    impl Into<T> for Ann<TypeAnn<T, Ann<Kind>>> {
        fn into(self) -> T {
            Rc::new(Type::TypeAnn(self))
        }
    }
    impl Into<T> for Ann<TypeApp<TCtor, T>> {
        fn into(self) -> T {
            Rc::new(Type::TypeApp(self))
        }
    }
}

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

mod inject_value {
    use super::*;

    impl Into<TV> for Ann<TermAnn<TV, T>> {
        fn into(self) -> TV {
            Rc::new(TermValue::TermAnn(self))
        }
    }
    impl Into<TV> for Ann<TermV> {
        fn into(self) -> TV {
            Rc::new(TermValue::Var(self))
        }
    }
    impl Into<TV> for Ann<Thunk<TC>> {
        fn into(self) -> TV {
            Rc::new(TermValue::Thunk(self))
        }
    }
    impl Into<TV> for Ann<Ctor<Ann<CtorV>, TV>> {
        fn into(self) -> TV {
            Rc::new(TermValue::Ctor(self))
        }
    }
    impl Into<TV> for Ann<Literal> {
        fn into(self) -> TV {
            Rc::new(TermValue::Literal(self))
        }
    }
}

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
    Dtor(Ann<Dtor<TC, Ann<DtorV>, TV>>),
}
type TC = Rc<TermComputation>;
impl ComputationT for TermComputation {}

mod inject_computation {
    use super::*;

    impl Into<TC> for Ann<TermAnn<TC, T>> {
        fn into(self) -> TC {
            Rc::new(TermComputation::TermAnn(self))
        }
    }
    impl Into<TC> for Ann<Ret<TV>> {
        fn into(self) -> TC {
            Rc::new(TermComputation::Ret(self))
        }
    }
    impl Into<TC> for Ann<Force<TV>> {
        fn into(self) -> TC {
            Rc::new(TermComputation::Force(self))
        }
    }
    impl Into<TC> for Ann<Let<Ann<TermV>, TV, TC>> {
        fn into(self) -> TC {
            Rc::new(TermComputation::Let(self))
        }
    }
    impl Into<TC> for Ann<Do<Ann<TermV>, TC>> {
        fn into(self) -> TC {
            Rc::new(TermComputation::Do(self))
        }
    }
    impl Into<TC> for Ann<Lam<Ann<TermV>, TC>> {
        fn into(self) -> TC {
            Rc::new(TermComputation::Lam(self))
        }
    }
    impl Into<TC> for Ann<App<TC, TV>> {
        fn into(self) -> TC {
            Rc::new(TermComputation::App(self))
        }
    }
    impl Into<TC> for Ann<Rec<Ann<TermV>, TC>> {
        fn into(self) -> TC {
            Rc::new(TermComputation::Rec(self))
        }
    }
    impl Into<TC> for Ann<Match<Ann<TermV>, TV, TC>> {
        fn into(self) -> TC {
            Rc::new(TermComputation::Match(self))
        }
    }
    impl Into<TC> for Ann<CoMatch<Ann<TermV>, TC>> {
        fn into(self) -> TC {
            Rc::new(TermComputation::CoMatch(self))
        }
    }
    impl Into<TC> for Ann<Dtor<TC, Ann<DtorV>, TV>> {
        fn into(self) -> TC {
            Rc::new(TermComputation::Dtor(self))
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Val(TermValue),
    Comp(TermComputation),
}

mod inject_term {
    use super::*;

    impl Into<Term> for TermValue {
        fn into(self) -> Term {
            Term::Val(self)
        }
    }
    impl Into<Term> for TermComputation {
        fn into(self) -> Term {
            Term::Comp(self)
        }
    }
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
    pub external: bool,
    pub decl: Declare,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Declare {
    Data(Ann<Data<Ann<TermV>, Ann<CtorV>, T>>),
    Codata(Ann<Codata<Ann<TermV>, Ann<DtorV>, T>>),
    Define(Ann<Define<Ann<TermV>, T, TV>>),
    Entry(Ann<TermComputation>),
}
