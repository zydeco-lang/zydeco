use crate::prelude::*;
use derive_more::From;
use im::Vector;
use std::rc::Rc;
use zydeco_derive::FmtArgs;

pub use crate::syntax::*;

/* ---------------------------------- Kind ---------------------------------- */

pub use crate::syntax::{KindBase, TypeArity};

#[derive(From, FmtArgs, Clone, Debug)]
pub enum Kind {
    Base(KindBase),
    TypeArity(TypeArity<Sp<Kind>, BoxKind>),
}
pub type BoxKind = Box<Sp<Kind>>;
impl KindT for Kind {}

/* ---------------------------------- Type ---------------------------------- */

#[derive(Clone, Debug, PartialEq)]
pub struct AbstVar(pub usize);
#[derive(From, FmtArgs, Clone, Debug, PartialEq)]
pub enum NeutralVar {
    Var(TypeV),
    Abst(AbstVar),
}
impl TyVarT for NeutralVar {}
#[derive(From, FmtArgs, Clone, Debug)]
pub enum SynType {
    TypeAbs(TypeAbs<(TypeV, Sp<Kind>), RcType>),
    TypeApp(TypeApp<NeutralVar, RcType>),
    Arrow(Arrow<RcType, RcType>),
    Forall(Forall<(TypeV, Sp<Kind>), RcType>),
    Exists(Exists<(TypeV, Sp<Kind>), RcType>),
    AbstVar(AbstVar),
    Hole(Hole),
}

#[derive(Clone, Debug)]
pub struct Type {
    pub synty: SynType,
}
pub type RcType = Rc<Sp<Type>>;
impl TypeT for Type {}

macro_rules! impl_from {
    ($T:ty) => {
        impl From<$T> for Type {
            fn from(synty: $T) -> Self {
                Self { synty: synty.into() }
            }
        }
    };
}
impl_from!(TypeAbs<(TypeV, Sp<Kind>), RcType>);
impl_from!(TypeApp<NeutralVar, RcType>);
impl_from!(Arrow<RcType, RcType>);
impl_from!(Forall<(TypeV, Sp<Kind>), RcType>);
impl_from!(Exists<(TypeV, Sp<Kind>), RcType>);
impl_from!(AbstVar);
impl_from!(Hole);
impl From<TypeV> for Type {
    fn from(tvar: TypeV) -> Self {
        TypeApp { tvar: tvar.into(), args: vec![] }.into()
    }
}

/* ---------------------------------- Term ---------------------------------- */

#[derive(From, FmtArgs, Clone, Debug)]
pub enum TermValue {
    Annotation(Annotation<RcValue, RcType>),
    Var(TermV),
    Thunk(Thunk<RcComp>),
    Ctor(Ctor<CtorV, RcValue>),
    Literal(Literal),
    Pack(Pack<RcType, RcValue>),
}
pub type RcValue = Rc<Sp<TermValue>>;
impl ValueT for TermValue {}

#[derive(From, FmtArgs, Clone, Debug)]
pub enum TailTerm {
    Let(Let<TermV, RcValue, ()>),
    Do(Do<TermV, RcComp, ()>),
}

#[derive(Clone, Debug)]
pub struct TailGroup {
    pub group: Vector<TailTerm>,
    pub body: RcComp,
}

#[derive(From, FmtArgs, Clone, Debug)]
pub enum TermComputation {
    Annotation(Annotation<RcComp, RcType>),
    Abs(Abs<TermV, RcComp>),
    App(App<RcComp, RcValue>),
    Ret(Ret<RcValue>),
    Force(Force<RcValue>),
    TailGroup(TailGroup),
    Rec(Rec<TermV, RcComp>),
    Match(Match<CtorV, TermV, RcValue, RcComp>),
    Comatch(Comatch<DtorV, RcComp>),
    Dtor(Dtor<RcComp, DtorV>),
    BeginBlock(BeginBlock<RcComp>),
    TyAbsTerm(Abs<(TypeV, Option<Sp<Kind>>), RcComp>),
    TyAppTerm(App<RcComp, RcType>),
    MatchPack(MatchPack<RcValue, TypeV, TermV, RcComp>),
}
pub type RcComp = Rc<Sp<TermComputation>>;
impl ComputationT for TermComputation {}

#[derive(From, FmtArgs, Clone, Debug)]
pub enum Term {
    Value(TermValue),
    Computation(TermComputation),
}

/* --------------------------------- Module --------------------------------- */

#[derive(Clone, Debug)]
pub struct Module {
    pub name: Option<String>,
    pub data: Vec<DeclSymbol<prelude::Data>>,
    pub codata: Vec<DeclSymbol<prelude::Codata>>,
    pub alias: Vec<DeclSymbol<prelude::Alias>>,
    pub define: Vec<DeclSymbol<Define<TermV, RcValue>>>,
    pub define_ext: Vec<DeclSymbol<Define<(TermV, RcType), ()>>>,
}

#[derive(Clone, Debug)]
pub struct Program {
    pub module: Sp<Module>,
    pub entry: Sp<TermComputation>,
}

pub mod prelude {
    use super::*;
    pub type Data = super::Data<TypeV, Sp<Kind>, CtorV, RcType>;
    pub type Codata = super::Codata<TypeV, Sp<Kind>, DtorV, RcType>;
    pub type Alias = super::Alias<TypeV, Sp<Kind>, RcType>;
}
