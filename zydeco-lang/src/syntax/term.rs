use super::*;
use derive_more::From;

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
pub struct TypeAbs<TyV: TyVarT, Ty: TypeT> {
    pub params: Vec<TyV>,
    pub body: Ty,
}

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

#[derive(From, Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Int(i64),
    String(Vec<char>),
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
pub struct Comatch<D: DtorT, B: ComputationT> {
    pub arms: Vec<Comatcher<D, B>>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Comatcher<D: DtorT, B: ComputationT> {
    pub dtorv: D,
    pub body: B,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Dtor<B: ComputationT, D: DtorT> {
    pub body: B,
    pub dtorv: D,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BeginBlock<B: ComputationT> {
    pub body: B,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MatchPack<A: ValueT, TyV: TyVarT, TeV: VarT, B: ComputationT> {
    pub scrut: A,
    pub tvar: TyV,
    pub var: TeV,
    pub body: B,
}
