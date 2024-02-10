mod term;
pub use term::*;

mod env;
pub use env::*;

mod fmt;
mod span;

use zydeco_derive::SpanHolder;

/* ---------------------------------- Sort ---------------------------------- */

pub mod sort {
    use crate::utils::span::Sp;
    use std::rc::Rc;

    macro_rules! sort {
        ( $Sort:ident ) => {
            pub trait $Sort {}
            impl<T: $Sort> $Sort for Vec<T> {}
            impl<T: $Sort> $Sort for Box<T> {}
            impl<T: $Sort> $Sort for Rc<T> {}
            impl<T: $Sort> $Sort for Sp<T> {}
            impl<T: $Sort> $Sort for Option<T> {}
            impl $Sort for () {}
        };
    }

    sort!(VarT);
    sort!(TyVarT);
    sort!(CtorT);
    sort!(DtorT);
    sort!(KindT);
    sort!(TypeT);
    sort!(ValueT);
    sort!(ComputationT);
}
pub use sort::*;

/* --------------------------------- Binders -------------------------------- */

pub mod binder;
pub use binder::*;

/* ------------------------------ Bi-Diretional ----------------------------- */

#[derive(SpanHolder, Clone, Debug, PartialEq, Eq)]
pub struct Annotation<Term, Type> {
    pub term: Term,
    pub ty: Type,
}

#[derive(SpanHolder, Copy, Clone, Debug, PartialEq, Eq)]
pub struct Hole;

/* ---------------------------------- Meta ---------------------------------- */

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Arrow<In, Out = In>(pub In, pub Out);

#[derive(SpanHolder, Copy, Clone, Debug, PartialEq, Eq)]
pub struct Abs<Param, Body> {
    pub param: Param,
    pub body: Body,
}

#[derive(SpanHolder, Copy, Clone, Debug, PartialEq, Eq)]
pub struct App<Body, Arg> {
    pub body: Body,
    pub arg: Arg,
}

/* ------------------------------ Declarations ------------------------------ */

#[derive(SpanHolder, Clone, Debug, PartialEq, Eq)]
pub struct DeclSymbol<T> {
    pub public: bool,
    pub external: bool,
    pub inner: T,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Data<TyV: TyVarT, Kd: KindT, C: CtorT, Ty: TypeT> {
    pub name: TyV,
    pub params: Vec<(TyV, Kd)>,
    pub ctors: Vec<DataBr<C, Ty>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DataBr<C: CtorT, Ty: TypeT> {
    pub ctorv: C,
    pub tys: Vec<Ty>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Codata<TyV: TyVarT, Kd: KindT, D: DtorT, Ty: TypeT> {
    pub name: TyV,
    pub params: Vec<(TyV, Kd)>,
    pub dtors: Vec<CodataBr<D, Ty>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CodataBr<D: DtorT, Ty: TypeT> {
    pub dtorv: D,
    pub ty: Ty,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Alias<TyV: TyVarT, Kd: KindT, Ty: TypeT> {
    pub name: TyV,
    pub params: Vec<(TyV, Kd)>,
    pub ty: Ty,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Define<TeV: VarT, A: ValueT> {
    pub name: TeV,
    pub def: A,
}
