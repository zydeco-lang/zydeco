use crate::utils::ann::Ann;
use std::rc::Rc;

/* ---------------------------------- Meta ---------------------------------- */

pub trait VarT {}
impl<T: VarT> VarT for Box<T> {}
impl<T: VarT> VarT for Rc<T> {}
impl<T: VarT> VarT for Ann<T> {}

pub trait TypeT {}
impl<T: TypeT> TypeT for Box<T> {}
impl<T: TypeT> TypeT for Rc<T> {}
impl<T: TypeT> TypeT for Ann<T> {}

pub trait ValueT {}
impl<T: ValueT> ValueT for Box<T> {}
impl<T: ValueT> ValueT for Rc<T> {}
impl<T: ValueT> ValueT for Ann<T> {}

pub trait ComputationT {}
impl<T: ComputationT> ComputationT for Box<T> {}
impl<T: ComputationT> ComputationT for Rc<T> {}
impl<T: ComputationT> ComputationT for Ann<T> {}

/* --------------------------------- Binders -------------------------------- */

pub mod binders {
    use crate::utils::ann::AnnInfo;

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
    impl super::VarT for TermV {}
}
pub use binders::*;

/* ---------------------------------- Types --------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ThunkT<T: TypeT>(T);
impl<T: TypeT> TypeT for ThunkT<T> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RetT<T: TypeT>(T);
impl<T: TypeT> TypeT for RetT<T> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TCtor<T: TypeT> {
    pub name: TypeV,
    pub args: Vec<T>,
}
impl<T: TypeT> TypeT for TCtor<T> {}

/* ------------------------------ Common Terms ------------------------------ */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Annotation<Term, Type>(Term, Type);
impl<Term: ValueT, Type> ValueT for Annotation<Term, Type> {}
impl<Term: ComputationT, Type> ComputationT for Annotation<Term, Type> {}

/* --------------------------------- Values --------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Thunk<B: ComputationT>(B);
impl<B: ComputationT> ValueT for Thunk<B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Int(i64),
    String(String),
    Char(char),
}
impl ValueT for Literal {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ctor<C, A: ValueT>(C, Vec<A>);
impl<C, A: ValueT> ValueT for Ctor<C, A> {}

/* ------------------------------ Computations ------------------------------ */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ret<A: ValueT>(A);
impl<A: ValueT> ComputationT for Ret<A> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Let<TeV: VarT, A: ValueT, B: ComputationT>(TeV, A, B);
impl<TeV: VarT, A: ValueT, B: ComputationT> ComputationT for Let<TeV, A, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Do<TeV: VarT, B: ComputationT>(TeV, B, B);
impl<TeV: VarT, B: ComputationT> ComputationT for Do<TeV, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Match<TeV: VarT, A: ValueT, B: ComputationT>(
    A,
    Vec<Matcher<TeV, B>>,
);
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Matcher<TeV: VarT, B: ComputationT>(CtorV, Vec<TeV>, B);
impl<TeV: VarT, A: ValueT, B: ComputationT> ComputationT for Match<TeV, A, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoMatch<TeV: VarT, B: ComputationT>(DtorV, Vec<CoMatcher<TeV, B>>);
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoMatcher<TeV: VarT, B: ComputationT>(Vec<TeV>, B);
impl<TeV: VarT, B: ComputationT> ComputationT for CoMatch<TeV, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Dtor<D, B: ComputationT, A: ValueT>(D, B, Vec<A>);
impl<D, B: ComputationT, A: ValueT> ComputationT for Dtor<D, B, A> {}

/* ------------------------------ Declarations ------------------------------ */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Define<T: TypeT, A: ValueT> {
    pub public: bool,
    pub name: TermV,
    pub ty: T,
    pub def: Option<A>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Data<T: TypeT> {
    pub name: TypeV,
    pub args: Vec<TypeV>,
    pub ctors: Vec<DataBr<T>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DataBr<T: TypeT>(CtorV, Vec<T>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Codata<T: TypeT> {
    pub name: TypeV,
    pub args: Vec<TypeV>,
    pub dtors: Vec<CodataBr<T>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CodataBr<T: TypeT>(DtorV, Vec<T>, T);
