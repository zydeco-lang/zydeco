use crate::prelude::*;
use zydeco_derive::{IntoEnum, SpanHolder};

pub use crate::syntax::*;

/* --------------------------------- Pattern -------------------------------- */

pub type TypePattern = (NameDef, Option<Span<Kind>>);
pub type TypeKindPattern = (NameDef, Span<Kind>);

pub type TermPattern = (NameDef, Option<Span<Type>>);

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum Pattern {
    TypePattern(TypePattern),
    TermPattern(TermPattern),
}

/* ---------------------------------- Kind ---------------------------------- */

pub use crate::syntax::{KindBase, TypeArity};

#[derive(SpanHolder, Clone, Debug, PartialEq, Eq)]
pub struct KindArrow(pub BoxKind, pub BoxKind);

#[derive(IntoEnum, SpanHolder, Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    Base(KindBase),
    Arrow(KindArrow),
}
pub type BoxKind = Box<Span<Kind>>;
impl KindT for Kind {}

/* ---------------------------------- Type ---------------------------------- */

#[derive(SpanHolder, Clone, Debug)]
pub struct TypeApp(pub BoxType, pub BoxType);

#[derive(SpanHolder, Clone, Debug)]
pub struct TypeArrow(pub BoxType, pub BoxType);

#[derive(SpanHolder, Clone, Debug)]
pub struct Forall(pub Vec<TypeKindPattern>, pub BoxType);

#[derive(SpanHolder, Clone, Debug)]
pub struct Exists(pub Vec<TypeKindPattern>, pub BoxType);

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum Type {
    Basic(NameRef),
    App(TypeApp),
    Arrow(TypeArrow),
    Forall(Forall),
    Exists(Exists),
    Hole(Hole),
}
pub type BoxType = Box<Span<Type>>;
impl TypeT for Type {}

/* ---------------------------------- Term ---------------------------------- */

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum TermValue {
    TermAnn(Annotation<BoxValue, Span<Type>>),
    Var(NameRef),
    Thunk(Thunk<BoxComp>),
    Ctor(Ctor<CtorV, Span<TermValue>>),
    Literal(Literal),
    Pack(Pack<BoxType, BoxValue>),
}
pub type BoxValue = Box<Span<TermValue>>;
impl ValueT for TermValue {}

#[derive(SpanHolder, Clone, Debug)]
pub struct GenAbs {
    pub params: Vec<Pattern>,
    pub body: BoxComp,
}

#[derive(SpanHolder, Clone, Debug)]
pub struct TermApp {
    pub body: BoxComp,
    pub arg: BoxValue,
}

#[derive(SpanHolder, Clone, Debug)]
pub struct GenLet {
    pub rec: bool,
    pub fun: bool,
    pub name: TermPattern,
    pub params: Vec<Pattern>,
    pub def: Option<Box<Span<Term>>>,
}

#[derive(SpanHolder, Clone, Debug)]
pub struct Let {
    pub gen: GenLet,
    pub body: BoxComp,
}

#[derive(SpanHolder, Clone, Debug)]
pub struct TyAppTerm {
    pub body: BoxComp,
    pub arg: Box<Span<Type>>,
}

#[derive(SpanHolder, Clone, Debug)]
pub struct MatchPack {
    pub scrut: BoxValue,
    pub tvar: NameDef,
    pub var: NameDef,
    pub body: BoxComp,
}

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum TermComputation {
    TermAnn(Annotation<BoxComp, Span<Type>>),
    Ret(Ret<BoxValue>),
    Force(Force<BoxValue>),
    Let(Let),
    Do(Do<TermPattern, BoxComp, BoxComp>),
    Rec(Rec<TermPattern, BoxComp>),
    Match(Match<CtorV, NameDef, BoxValue, Span<TermComputation>>),
    Abs(GenAbs),
    App(TermApp),
    Comatch(Comatch<DtorV, NameDef, Span<TermComputation>>),
    Dtor(Dtor<BoxComp, DtorV, Span<TermValue>>),
    TyAppTerm(TyAppTerm),
    MatchPack(MatchPack),
}
pub type BoxComp = Box<Span<TermComputation>>;
impl ComputationT for TermComputation {}

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum Term {
    Value(TermValue),
    Computation(TermComputation),
}

/* -------------------------------- TopLevel -------------------------------- */

pub type Define = GenLet;

#[derive(SpanHolder, Clone, Debug)]
pub struct UseAll;

#[derive(SpanHolder, Clone, Debug)]
pub struct UseCluster {
    pub modv: ModV,
    pub cluster: Vec<UseDef>,
}

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum UseDef {
    Name(NameRef),
    UseAll(UseAll),
    Cluster(UseCluster),
}

#[derive(SpanHolder, Clone, Debug)]
pub struct Main {
    pub entry: Span<TermComputation>,
}

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum Declaration {
    Module(Module),
    UseDef(UseDef),
    Data(Data<NameDef, Span<Kind>, CtorV, Span<Type>>),
    Codata(Codata<NameDef, Span<Kind>, DtorV, Span<Type>>),
    Alias(Alias<NameDef, Span<Kind>, BoxType>),
    Define(Define),
    Main(Main),
}

#[derive(SpanHolder, Clone, Debug)]
pub struct Module {
    pub name: Option<String>,
    pub declarations: Vec<DeclSymbol<Declaration>>,
}

impl Monoid for Module {
    fn empty() -> Self {
        Self { name: None, declarations: Vec::new() }
    }
    fn append(self, other: Self) -> Self {
        Self {
            name: other.name.or(self.name),
            declarations: self.declarations.into_iter().chain(other.declarations).collect(),
        }
    }
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
