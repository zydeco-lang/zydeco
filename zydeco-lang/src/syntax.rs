use crate::utils::ann::Ann;
use std::rc::Rc;

/* ---------------------------------- Meta ---------------------------------- */

macro_rules! sort {
    ( $Sort:ident ) => {
        pub trait $Sort {}
        impl<T: $Sort> $Sort for Box<T> {}
        impl<T: $Sort> $Sort for Rc<T> {}
        impl<T: $Sort> $Sort for Ann<T> {}
    };
}

sort!(VarT);
sort!(KindT);
sort!(TypeT);
sort!(ValueT);
sort!(ComputationT);

/* --------------------------------- Binders -------------------------------- */

pub mod binders {
    use super::VarT;
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
                    self.0.eq(&other.0)
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
    impl VarT for TypeV {}
    var!(TermV);
    impl VarT for TermV {}
}
pub use binders::*;

/* ---------------------------------- Kind ---------------------------------- */

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    VType,
    CType,
}
impl KindT for Kind {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeArity<K: KindT>(Vec<K>, K);
impl<K: KindT> KindT for TypeArity<K> {}

/* ---------------------------------- Types --------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeAnn<Type, Kind>(Type, Kind);
impl<Type: TypeT, Kind> TypeT for TypeAnn<Type, Kind> {}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TCtor {
    Var(TypeV),
    Thunk,
    Ret,
    OS,
    Fun,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeApp<TyV, T: TypeT>(pub TyV, pub Vec<T>);
impl<TyV, T: TypeT> TypeT for TypeApp<TyV, T> {}

/* ---------------------------------- Terms --------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TermAnn<Term, Type>(pub Term, pub Type);
impl<Term: ValueT, Type> ValueT for TermAnn<Term, Type> {}
impl<Term: ComputationT, Type> ComputationT for TermAnn<Term, Type> {}

/* --------------------------------- Values --------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Thunk<B: ComputationT>(pub B);
impl<B: ComputationT> ValueT for Thunk<B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Int(i64),
    String(String),
    Char(char),
}
impl ValueT for Literal {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ctor<C, A: ValueT>(pub C, pub Vec<A>);
impl<C, A: ValueT> ValueT for Ctor<C, A> {}

/* ------------------------------ Computations ------------------------------ */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ret<A: ValueT>(pub A);
impl<A: ValueT> ComputationT for Ret<A> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Force<A: ValueT>(pub A);
impl<A: ValueT> ComputationT for Force<A> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Let<TeV: VarT, A: ValueT, B: ComputationT>(pub TeV, pub A, pub B);
impl<TeV: VarT, A: ValueT, B: ComputationT> ComputationT for Let<TeV, A, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Do<TeV: VarT, B: ComputationT>(pub TeV, pub B, pub B);
impl<TeV: VarT, B: ComputationT> ComputationT for Do<TeV, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Lam<TeV: VarT, B: ComputationT>(pub TeV, pub B);
impl<TeV: VarT, B: ComputationT> ComputationT for Lam<TeV, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct App<B: ComputationT, A: ValueT>(pub B, pub A);
impl<B: ComputationT, A: ValueT> ComputationT for App<B, A> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rec<TeV: VarT, B: ComputationT>(pub TeV, pub B);
impl<TeV: VarT, B: ComputationT> ComputationT for Rec<TeV, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Match<TeV: VarT, A: ValueT, B: ComputationT>(
    pub A,
    pub Vec<Matcher<TeV, B>>,
);
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Matcher<TeV: VarT, B: ComputationT>(pub CtorV, pub Vec<TeV>, B);
impl<TeV: VarT, A: ValueT, B: ComputationT> ComputationT for Match<TeV, A, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoMatch<TeV: VarT, B: ComputationT>(
    pub DtorV,
    pub Vec<CoMatcher<TeV, B>>,
);
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoMatcher<TeV: VarT, B: ComputationT>(pub Vec<TeV>, pub B);
impl<TeV: VarT, B: ComputationT> ComputationT for CoMatch<TeV, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Dtor<B: ComputationT, D, A: ValueT>(pub B, pub D, pub Vec<A>);
impl<B: ComputationT, D, A: ValueT> ComputationT for Dtor<B, D, A> {}

/* ------------------------------ Declarations ------------------------------ */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Define<TeV: VarT, T: TypeT, A: ValueT> {
    pub name: TeV,
    pub ty: T,
    pub def: Option<A>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Data<TyV: VarT, C, T: TypeT> {
    pub name: TyV,
    pub args: Vec<TyV>,
    pub ctors: Vec<DataBr<C, T>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DataBr<C, T: TypeT>(pub C, pub Vec<T>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Codata<TyV: VarT, D, T: TypeT> {
    pub name: TyV,
    pub args: Vec<TyV>,
    pub dtors: Vec<CodataBr<D, T>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CodataBr<D, T: TypeT>(pub D, pub Vec<T>, pub T);
