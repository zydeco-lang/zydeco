use crate::utils::{monoid::Monoid, span::Span};
use zydeco_derive::{IntoEnum, SpanHolder};

pub use crate::syntax::*;

/* ---------------------------------- Kind ---------------------------------- */

pub use crate::syntax::{KindBase, TypeArity};

/* ---------------------------------- Type ---------------------------------- */

pub type TypePattern = (TypeV, Option<Span<KindBase>>);
pub type TypeKindPattern = (TypeV, Span<KindBase>);

#[derive(SpanHolder, Clone, Debug)]
pub struct TypeApp(pub BoxType, pub BoxType);

#[derive(SpanHolder, Clone, Debug)]
pub struct Arrow(pub BoxType, pub BoxType);

#[derive(SpanHolder, Clone, Debug)]
pub struct Forall(pub Vec<TypeKindPattern>, pub BoxType);

#[derive(SpanHolder, Clone, Debug)]
pub struct Exists(pub Vec<TypeKindPattern>, pub BoxType);

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum Type {
    Basic(TypeV),
    App(TypeApp),
    Arrow(Arrow),
    Forall(Forall),
    Exists(Exists),
    Hole(Hole),
}
pub type BoxType = Box<Span<Type>>;
impl TypeT for Type {}

/* ---------------------------------- Term ---------------------------------- */

pub type TermPattern = (TermV, Option<Span<Type>>);

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum TermValue {
    TermAnn(Annotation<BoxValue, Span<Type>>),
    Var(TermV),
    Thunk(Thunk<BoxComp>),
    Ctor(Ctor<CtorV, Span<TermValue>>),
    Literal(Literal),
    Pack(Pack<BoxType, BoxValue>),
}
pub type BoxValue = Box<Span<TermValue>>;
impl ValueT for TermValue {}

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum Pattern {
    TypePattern(TypePattern),
    TermPattern(TermPattern),
}

#[derive(SpanHolder, Clone, Debug)]
pub struct Abstraction {
    pub params: Vec<Pattern>,
    pub body: BoxComp,
}

#[derive(SpanHolder, Clone, Debug)]
pub struct Application {
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
    pub tvar: TypeV,
    pub var: TermV,
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
    Match(Match<CtorV, TermV, BoxValue, Span<TermComputation>>),
    Abs(Abstraction),
    App(Application),
    Comatch(Comatch<DtorV, TermV, Span<TermComputation>>),
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

/* --------------------------------- Module --------------------------------- */

pub type Define = GenLet;

#[derive(IntoEnum, SpanHolder, Clone, Debug)]
pub enum Declaration {
    Module(Module),
    Data(Data<TypeV, Span<KindBase>, CtorV, Span<Type>>),
    Codata(Codata<TypeV, Span<KindBase>, DtorV, Span<Type>>),
    Alias(Alias<TypeV, Span<KindBase>, BoxType>),
    Define(Define),
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
pub struct Program {
    pub module: Span<Module>,
    pub entry: Span<TermComputation>,
}
