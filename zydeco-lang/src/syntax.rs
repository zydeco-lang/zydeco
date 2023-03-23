pub mod span;
pub mod env;
mod fmt;

pub use span::{Span, SpanInfo};
use std::rc::Rc;
use zydeco_derive::EnumGenerator;

/* ---------------------------------- Meta ---------------------------------- */

macro_rules! sort {
    ( $Sort:ident ) => {
        pub trait $Sort {}
        impl<T: $Sort> $Sort for Box<T> {}
        impl<T: $Sort> $Sort for Rc<T> {}
        impl<T: $Sort> $Sort for Span<T> {}
        impl<T: $Sort> $Sort for Option<T> {}
        impl $Sort for () {}
    };
}

sort!(VarT);
sort!(KindT);
sort!(TypeT);
sort!(ValueT);
sort!(ComputationT);

/* --------------------------------- Binders -------------------------------- */

pub mod binder {
    use super::{Span, TypeT, VarT};
    use crate::syntax::SpanInfo;

    macro_rules! var {
        ( $Var:ident ) => {
            #[derive(Clone, Debug)]
            pub struct $Var(String, SpanInfo);
            impl $Var {
                pub fn new(s: String, span: SpanInfo) -> Self {
                    Self(s, span)
                }
                pub fn name(&self) -> &str {
                    &self.0
                }
            }
            impl From<Span<String>> for $Var {
                fn from(span: Span<String>) -> Self {
                    Self(span.inner, span.info)
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
            impl crate::syntax::span::SpanView for $Var {
                fn span(&self) -> &SpanInfo {
                    &self.1
                }
            }
            impl crate::syntax::span::SpanHolder for $Var {
                fn span_map_mut<F>(&mut self, f: F)
                where
                    F: Fn(&mut SpanInfo) + Clone,
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
    impl<T: TypeT> VarT for (TermV, T) {}
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
    pub params: Vec<K>,
    pub kd: K,
}
impl<K: KindT> KindT for TypeArity<K> {}

/* ---------------------------------- Types --------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeAnn<Type, Kind> {
    pub ty: Type,
    pub kd: Kind,
}
impl<Type: TypeT, Kind: KindT> TypeT for TypeAnn<Type, Kind> {}

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

#[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
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
pub struct Do<TeV: VarT, B1: ComputationT, B2: ComputationT> {
    pub var: TeV,
    pub comp: B1,
    pub body: B2,
}
impl<TeV: VarT, B1: ComputationT, B2: ComputationT> ComputationT
    for Do<TeV, B1, B2>
{
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rec<TeV: VarT, B: ComputationT> {
    pub var: TeV,
    pub body: B,
}
impl<TeV: VarT, B: ComputationT> ComputationT for Rec<TeV, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Match<C, TeV: VarT, A: ValueT, B: ComputationT> {
    pub scrut: A,
    pub arms: Vec<Matcher<C, TeV, B>>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Matcher<C, TeV: VarT, B: ComputationT> {
    pub ctor: C,
    pub vars: Vec<TeV>,
    pub body: B,
}
impl<C, TeV: VarT, A: ValueT, B: ComputationT> ComputationT
    for Match<C, TeV, A, B>
{
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoMatch<D, TeV: VarT, B: ComputationT> {
    pub arms: Vec<CoMatcher<D, TeV, B>>,
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
pub struct DeclSymbol<T> {
    pub public: bool,
    pub external: bool,
    pub inner: T,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Data<TyV: VarT, C, T: TypeT> {
    pub name: TyV,
    pub params: Vec<(TyV, Kind)>,
    pub ctors: Vec<DataBr<C, T>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DataBr<C, T: TypeT>(pub C, pub Vec<T>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Codata<TyV: VarT, D, T: TypeT> {
    pub name: TyV,
    pub params: Vec<(TyV, Kind)>,
    pub dtors: Vec<CodataBr<D, T>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CodataBr<D, T: TypeT>(pub D, pub Vec<T>, pub T);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Define<TeV: VarT, A: ValueT> {
    pub name: TeV,
    pub def: A,
}
