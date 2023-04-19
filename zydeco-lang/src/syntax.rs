pub mod env;
mod fmt;
mod span;

use zydeco_derive::{IntoEnum, SpanHolder};

/* ---------------------------------- Sort ---------------------------------- */

pub mod sort {
    use crate::utils::span::Span;
    use std::rc::Rc;

    macro_rules! sort {
        ( $Sort:ident ) => {
            pub trait $Sort {}
            impl<T: $Sort> $Sort for Vec<T> {}
            impl<T: $Sort> $Sort for Box<T> {}
            impl<T: $Sort> $Sort for Rc<T> {}
            impl<T: $Sort> $Sort for Span<T> {}
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

/* ---------------------------------- Kind ---------------------------------- */

#[derive(SpanHolder, Copy, Clone, Debug, PartialEq, Eq)]
pub enum KindBase {
    VType,
    CType,
}

/// A kind that represents the arity, a.k.a. parameters of a type constructor.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeArity<In: KindT, Out: KindT> {
    pub params: Vec<In>,
    pub kd: Out,
}

/* ---------------------------------- Types --------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeApp<TyV: TyVarT, Ty: TypeT> {
    pub tvar: TyV,
    pub args: Vec<Ty>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Forall<TyV: TyVarT, Ty: TypeT> {
    pub param: TyV,
    pub ty: Ty,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Exists<TyV: TyVarT, Ty: TypeT> {
    pub param: TyV,
    pub ty: Ty,
}

/* --------------------------------- Values --------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Thunk<B: ComputationT>(pub B);

#[derive(IntoEnum, Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Int(i64),
    String(String),
    Char(char),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ctor<C: CtorT, A: ValueT> {
    pub ctorv: C,
    pub args: Vec<A>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pack<Ty: TypeT, A: ValueT> {
    pub ty: Ty,
    pub body: A,
}

/* ------------------------------ Computations ------------------------------ */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ret<A: ValueT>(pub A);
impl<A: ValueT> ComputationT for Ret<A> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Force<A: ValueT>(pub A);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Let<TeV: VarT, A: ValueT, B: ComputationT> {
    pub var: TeV,
    pub def: A,
    pub body: B,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Do<TeV: VarT, B1: ComputationT, B2: ComputationT> {
    pub var: TeV,
    pub comp: B1,
    pub body: B2,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rec<TeV: VarT, B: ComputationT> {
    pub var: TeV,
    pub body: B,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Match<C: CtorT, TeV: VarT, A: ValueT, B: ComputationT> {
    pub scrut: A,
    pub arms: Vec<Matcher<C, TeV, B>>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Matcher<C: CtorT, TeV: VarT, B: ComputationT> {
    pub ctorv: C,
    pub vars: Vec<TeV>,
    pub body: B,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Comatch<D: DtorT, TeV: VarT, B: ComputationT> {
    pub arms: Vec<Comatcher<D, TeV, B>>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Comatcher<D: DtorT, TeV: VarT, B: ComputationT> {
    pub dtorv: D,
    pub vars: Vec<TeV>,
    pub body: B,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Dtor<B: ComputationT, D: DtorT, A: ValueT> {
    pub body: B,
    pub dtorv: D,
    pub args: Vec<A>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MatchPack<A: ValueT, TyV: TyVarT, TeV: VarT, B: ComputationT> {
    pub scrut: A,
    pub tvar: TyV,
    pub var: TeV,
    pub body: B,
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
    pub tys: Vec<Ty>,
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
