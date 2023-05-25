use crate::prelude::*;
use zydeco_derive::{IntoEnum, SpanHolder};

pub use crate::syntax::*;

/* --------------------------------- Binder --------------------------------- */

type TypeDef = NameDef;
type TypeRef = NameRef;

type TermDef = NameDef;
type TermRef = NameRef;

/* --------------------------------- Pattern -------------------------------- */

pub type TypePattern = (TypeDef, Option<Sp<Kind>>);
pub type TermPattern = (TermDef, Option<Sp<Type>>);

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum Pattern {
    TypePattern(TypePattern),
    TermPattern(TermPattern),
}

/* ---------------------------------- Kind ---------------------------------- */

pub use crate::syntax::{KindBase, TypeArity};

#[derive(IntoEnum, SpanHolder, Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    Base(KindBase),
    Arrow(Arrow<BoxKind>),
}
pub type BoxKind = Box<Sp<Kind>>;
impl KindT for Kind {}

/* ---------------------------------- Type ---------------------------------- */

#[derive(SpanHolder, Clone, Debug)]
pub struct TypeApp(pub BoxType, pub BoxType);

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum Type {
    Basic(TypeRef),
    App(TypeApp),
    Arrow(Arrow<BoxType>),
    Forall(Forall<Vec<TypePattern>, BoxType>),
    Exists(Exists<Vec<TypePattern>, BoxType>),
    Hole(Hole),
}
pub type BoxType = Box<Sp<Type>>;
impl TypeT for Type {}

/* ---------------------------------- Term ---------------------------------- */

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum TermValue {
    TermAnn(Annotation<BoxValue, Sp<Type>>),
    Var(TermRef),
    Thunk(Thunk<BoxComp>),
    Ctor(Ctor<CtorV, Sp<TermValue>>),
    Literal(Literal),
    Pack(Pack<BoxType, BoxValue>),
}
pub type BoxValue = Box<Sp<TermValue>>;
impl ValueT for TermValue {}

#[derive(SpanHolder, Clone, Debug)]
pub struct GenLet {
    pub rec: bool,
    pub fun: bool,
    pub name: TermPattern,
    pub params: Vec<Pattern>,
    pub def: Option<Box<Sp<Term>>>,
}

#[derive(SpanHolder, Clone, Debug)]
pub struct Let {
    pub gen: GenLet,
    pub body: BoxComp,
}

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum TermComputation {
    TermAnn(Annotation<BoxComp, Sp<Type>>),
    Ret(Ret<BoxValue>),
    Force(Force<BoxValue>),
    Let(Let),
    Do(Do<TermPattern, BoxComp, BoxComp>),
    Rec(Rec<TermPattern, BoxComp>),
    Match(Match<CtorV, TermDef, BoxValue, Sp<TermComputation>>),
    Abs(Abs<Vec<Pattern>, BoxComp>),
    App(App<BoxComp, BoxValue>),
    Comatch(Comatch<DtorV, TermDef, Sp<TermComputation>>),
    Dtor(Dtor<BoxComp, DtorV, Sp<TermValue>>),
    TyAppTerm(App<BoxComp, BoxType>),
    MatchPack(MatchPack<BoxValue, TypeDef, TermDef, BoxComp>),
}
pub type BoxComp = Box<Sp<TermComputation>>;
impl ComputationT for TermComputation {}

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum Term {
    Value(TermValue),
    Computation(TermComputation),
}

/* -------------------------------- TopLevel -------------------------------- */

#[derive(SpanHolder, Clone, Debug)]
pub struct Module {
    pub name: Option<NameRef>,
    pub declarations: Vec<DeclSymbol<Declaration>>,
}

#[derive(SpanHolder, Clone, Debug)]
pub struct UseAll;

#[derive(SpanHolder, Clone, Debug)]
pub struct UseCluster {
    pub path: NameRef,
    pub cluster: Vec<UseDef>,
}

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum UseDef {
    Name(NameRef),
    UseAll(UseAll),
    Cluster(UseCluster),
}

#[derive(SpanHolder, Clone, Debug)]
pub struct Define(pub GenLet);

#[derive(SpanHolder, Clone, Debug)]
pub struct Main {
    pub entry: Sp<TermComputation>,
}

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum Declaration {
    Module(Module),
    UseDef(UseDef),
    Data(Data<TypeDef, Option<Sp<Kind>>, CtorV, Sp<Type>>),
    Codata(Codata<TypeDef, Option<Sp<Kind>>, DtorV, Sp<Type>>),
    Alias(Alias<TypeDef, Option<Sp<Kind>>, BoxType>),
    Define(Define),
    Main(Main),
}

#[derive(SpanHolder, Clone, Debug)]
pub struct TopLevel {
    pub declarations: Vec<DeclSymbol<Declaration>>,
}

impl Monoid for TopLevel {
    fn empty() -> Self {
        Self { declarations: Vec::new() }
    }
    fn append(self, other: Self) -> Self {
        Self { declarations: self.declarations.into_iter().chain(other.declarations).collect() }
    }
}
