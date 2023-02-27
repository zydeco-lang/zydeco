pub mod ann;

pub use ann::{Ann, AnnInfo};
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

pub mod binder {
    use super::VarT;
    use crate::syntax::AnnInfo;

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
            impl crate::syntax::ann::AnnHolder for $Var {
                fn ann(&self) -> &AnnInfo {
                    &self.1
                }
                fn ann_map_mut<F>(&mut self, f: F)
                where
                    F: Fn(&mut AnnInfo) + Clone,
                {
                    f(&mut self.1);
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
pub use binder::*;

/* ---------------------------------- Kind ---------------------------------- */

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    VType,
    CType,
}
impl KindT for Kind {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeArity<K: KindT> {
    pub args: Vec<K>,
    pub kd: K,
}
impl<K: KindT> KindT for TypeArity<K> {}

/* ---------------------------------- Types --------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeAnn<Type, Kind> {
    pub ty: Type,
    pub kd: Kind,
}
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
pub struct TypeApp<TyV, T: TypeT> {
    pub tctor: TyV,
    pub args: Vec<T>,
}
impl<TyV, T: TypeT> TypeT for TypeApp<TyV, T> {}

/* ---------------------------------- Terms --------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TermAnn<Term, Type> {
    pub body: Term,
    pub ty: Type,
}
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
pub struct Ctor<C, A: ValueT> {
    pub ctor: C,
    pub args: Vec<A>,
}
impl<C, A: ValueT> ValueT for Ctor<C, A> {}

/* ------------------------------ Computations ------------------------------ */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ret<A: ValueT>(pub A);
impl<A: ValueT> ComputationT for Ret<A> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Force<A: ValueT>(pub A);
impl<A: ValueT> ComputationT for Force<A> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Let<TeV: VarT, A: ValueT, B: ComputationT> {
    pub var: TeV,
    pub def: A,
    pub body: B,
}
impl<TeV: VarT, A: ValueT, B: ComputationT> ComputationT for Let<TeV, A, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Do<TeV: VarT, B: ComputationT> {
    pub var: TeV,
    pub comp: B,
    pub body: B,
}
impl<TeV: VarT, B: ComputationT> ComputationT for Do<TeV, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Lam<TeV: VarT, B: ComputationT> {
    pub var: TeV,
    pub body: B,
}
impl<TeV: VarT, B: ComputationT> ComputationT for Lam<TeV, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct App<B: ComputationT, A: ValueT> {
    pub body: B,
    pub arg: A,
}
impl<B: ComputationT, A: ValueT> ComputationT for App<B, A> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rec<TeV: VarT, B: ComputationT> {
    pub var: TeV,
    pub body: B,
}
impl<TeV: VarT, B: ComputationT> ComputationT for Rec<TeV, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Match<C, TeV: VarT, A: ValueT, B: ComputationT> {
    pub scrut: A,
    pub branches: Vec<Matcher<C, TeV, B>>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Matcher<C, TeV: VarT, B: ComputationT> {
    pub ctor: C,
    pub vars: Vec<TeV>,
    pub body: B,
}
impl<C, TeV: VarT, A: ValueT, B: ComputationT> ComputationT for Match<C, TeV, A, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoMatch<D, TeV: VarT, B: ComputationT> {
    pub branches: Vec<CoMatcher<D, TeV, B>>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoMatcher<D, TeV: VarT, B: ComputationT> {
    pub dtor: D,
    pub vars: Vec<TeV>,
    pub body: B,
}
impl<D, TeV: VarT, B: ComputationT> ComputationT for CoMatch<D, TeV, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Dtor<B: ComputationT, D, A: ValueT> {
    pub body: B,
    pub dtor: D,
    pub args: Vec<A>,
}
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
