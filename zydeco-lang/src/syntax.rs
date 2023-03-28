pub mod env;
mod fmt;
mod span;

use zydeco_derive::EnumGenerator;

/* ---------------------------------- Meta ---------------------------------- */

pub mod sort {
    use crate::utils::span::Span;
    use std::rc::Rc;

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

pub mod binder {
    use super::sort::*;
    use crate::utils::span::{Span, SpanHolder, SpanInfo, SpanView};
    use std::{
        cmp::{Eq, PartialEq},
        hash::{Hash, Hasher},
    };

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
            impl PartialEq for $Var {
                fn eq(&self, other: &Self) -> bool {
                    self.0.eq(&other.0)
                }
            }
            impl Eq for $Var {}
            impl Hash for $Var {
                fn hash<H: Hasher>(&self, state: &mut H) {
                    self.0.hash(state);
                }
            }
            impl SpanView for $Var {
                fn span(&self) -> &SpanInfo {
                    &self.1
                }
            }
            impl SpanHolder for $Var {
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
    impl CtorT for CtorV {}
    var!(DtorV);
    impl DtorT for DtorV {}
    var!(TypeV);
    impl TyVarT for TypeV {}
    var!(TermV);
    impl VarT for TermV {}
    impl<Ty: TypeT> VarT for (TermV, Ty) {}
}
pub use binder::*;

/* ---------------------------------- Kind ---------------------------------- */

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    VType,
    CType,
}
impl KindT for Kind {}

/// A kind that represents the arity, a.k.a. parameters of a type constructor.
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
    Fun,
}
impl TyVarT for TCtor {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeApp<TyV: TyVarT, Ty: TypeT> {
    pub tctor: TyV,
    pub args: Vec<Ty>,
}
impl<TyV: TyVarT, Ty: TypeT> TypeT for TypeApp<TyV, Ty> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Forall<TyV: TyVarT, Kd: KindT, Ty: TypeT> {
    pub param: TyV,
    pub kd: Kd,
    pub ty: Ty,
}
impl<TyV: TyVarT, Kd: KindT, Ty: TypeT> TypeT for Forall<TyV, Kd, Ty> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Exists<TyV: TyVarT, Kd: KindT, Ty: TypeT> {
    pub param: TyV,
    pub kd: Kd,
    pub ty: Ty,
}
impl<TyV: TyVarT, Kd: KindT, Ty: TypeT> TypeT for Exists<TyV, Kd, Ty> {}

/* ---------------------------------- Terms --------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TermAnn<Term, Type> {
    pub term: Term,
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
pub struct Ctor<C: CtorT, A: ValueT> {
    pub ctor: C,
    pub args: Vec<A>,
}
impl<C: CtorT, A: ValueT> ValueT for Ctor<C, A> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExistsVal<Ty: TypeT, A: ValueT> {
    pub ty: Ty,
    pub body: A,
}
impl<Ty: TypeT, A: ValueT> ValueT for ExistsVal<Ty, A> {}

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
pub struct Match<C: CtorT, TeV: VarT, A: ValueT, B: ComputationT> {
    pub scrut: A,
    pub arms: Vec<Matcher<C, TeV, B>>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Matcher<C: CtorT, TeV: VarT, B: ComputationT> {
    pub ctor: C,
    pub vars: Vec<TeV>,
    pub body: B,
}
impl<C: CtorT, TeV: VarT, A: ValueT, B: ComputationT> ComputationT
    for Match<C, TeV, A, B>
{
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoMatch<D: DtorT, TeV: VarT, B: ComputationT> {
    pub arms: Vec<CoMatcher<D, TeV, B>>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoMatcher<D: DtorT, TeV: VarT, B: ComputationT> {
    pub dtor: D,
    pub vars: Vec<TeV>,
    pub body: B,
}
impl<D: DtorT, TeV: VarT, B: ComputationT> ComputationT for CoMatch<D, TeV, B> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Dtor<B: ComputationT, D: DtorT, A: ValueT> {
    pub body: B,
    pub dtor: D,
    pub args: Vec<A>,
}
impl<B: ComputationT, D: DtorT, A: ValueT> ComputationT for Dtor<B, D, A> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypAbs<TyV: TyVarT, Kd: KindT, B: ComputationT> {
    pub tvar: TyV,
    pub kd: Kd,
    pub body: B,
}
impl<TyV: TyVarT, Kd: KindT, B: ComputationT> ComputationT
    for TypAbs<TyV, Kd, B>
{
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypApp<B: ComputationT, Ty: TypeT> {
    pub body: B,
    pub arg: Ty,
}
impl<B: ComputationT, Ty: TypeT> ComputationT for TypApp<B, Ty> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MatchExists<A: ValueT, TyV: TyVarT, TeV: VarT, B: ComputationT> {
    pub scrut: A,
    pub tvar: TyV,
    pub var: TeV,
    pub body: B,
}
impl<A: ValueT, TyV: TyVarT, TeV: VarT, B: ComputationT> ComputationT
    for MatchExists<A, TyV, TeV, B>
{
}

/* ------------------------------ Declarations ------------------------------ */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DeclSymbol<T> {
    pub public: bool,
    pub external: bool,
    pub inner: T,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Data<TyV: TyVarT, C: CtorT, Ty: TypeT> {
    pub name: TyV,
    pub params: Vec<(TyV, Kind)>,
    pub ctors: Vec<DataBr<C, Ty>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DataBr<C: CtorT, Ty: TypeT>(pub C, pub Vec<Ty>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Codata<TyV: TyVarT, D: DtorT, Ty: TypeT> {
    pub name: TyV,
    pub params: Vec<(TyV, Kind)>,
    pub dtors: Vec<CodataBr<D, Ty>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CodataBr<D: DtorT, Ty: TypeT>(pub D, pub Vec<Ty>, pub Ty);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Define<TeV: VarT, A: ValueT> {
    pub name: TeV,
    pub def: A,
}
