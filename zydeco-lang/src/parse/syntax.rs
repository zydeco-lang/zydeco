use crate::utils::monoid::Monoid;
use crate::utils::span::Span;
use zydeco_derive::{EnumGenerator, SpanHolder};

pub use crate::syntax::*;

/* ---------------------------------- Kind ---------------------------------- */

pub use crate::syntax::Kind;

/* ---------------------------------- Type ---------------------------------- */

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TCtor {
    Var(TypeV),
    Thunk,
    Ret,
}
impl TyVarT for TCtor {}

#[derive(Clone, Debug)]
pub struct TypeApp(pub BoxType, pub BoxType);

#[derive(Clone, Debug)]
pub struct Arrow(pub BoxType, pub BoxType);

#[derive(Clone, Debug)]
pub struct Forall(pub Vec<(TypeV, Kind)>, pub BoxType);

#[derive(Clone, Debug)]
pub struct Exists(pub Vec<(TypeV, Kind)>, pub BoxType);

#[derive(EnumGenerator, SpanHolder, Clone, Debug)]
pub enum Type {
    Basic(TCtor),
    App(TypeApp),
    Arrow(Arrow),
    Forall(Forall),
    Exists(Exists),
    Hole(Hole),
}
pub type BoxType = Box<Span<Type>>;
impl TypeT for Type {}

/* ---------------------------------- Term ---------------------------------- */

#[derive(EnumGenerator, SpanHolder, Clone, Debug)]
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

pub type TermPattern = (TermV, Option<Span<Type>>);

#[derive(Clone, Debug)]
pub struct Abstraction {
    pub params: Vec<TermPattern>,
    pub body: BoxComp,
}

#[derive(Clone, Debug)]
pub struct Application {
    pub body: BoxComp,
    pub arg: BoxValue,
}

#[derive(Clone, Debug)]
pub struct GenLet {
    pub rec: bool,
    pub fun: bool,
    pub name: TermPattern,
    pub params: Vec<TermPattern>,
    pub def: Option<Box<Span<Term>>>,
}

#[derive(Clone, Debug)]
pub struct Let {
    pub gen: GenLet,
    pub body: BoxComp,
}

#[derive(Clone, Debug)]
pub struct TypAbs {
    pub params: Vec<(TypeV, Kind)>,
    pub body: BoxComp,
}

#[derive(Clone, Debug)]
pub struct TypApp {
    pub body: BoxComp,
    pub arg: Box<Span<Type>>,
}

#[derive(Clone, Debug)]
pub struct MatchPack {
    pub scrut: BoxValue,
    pub tvar: TypeV,
    pub var: TermV,
    pub body: BoxComp,
}

#[derive(EnumGenerator, SpanHolder, Clone, Debug)]
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
    CoMatch(CoMatch<DtorV, TermV, Span<TermComputation>>),
    Dtor(Dtor<BoxComp, DtorV, Span<TermValue>>),
    TypAbs(TypAbs),
    TypApp(TypApp),
    MatchPack(MatchPack),
}
pub type BoxComp = Box<Span<TermComputation>>;
impl ComputationT for TermComputation {}

#[derive(EnumGenerator, SpanHolder, Clone, Debug)]
pub enum Term {
    Value(TermValue),
    Computation(TermComputation),
}

/* --------------------------------- Module --------------------------------- */

pub type Define = GenLet;

#[derive(EnumGenerator, SpanHolder, Clone, Debug)]
pub enum Declaration {
    Data(Data<TypeV, CtorV, Span<Type>>),
    Codata(Codata<TypeV, DtorV, Span<Type>>),
    Define(Define),
}

#[derive(Clone, Debug)]
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
            declarations: self
                .declarations
                .into_iter()
                .chain(other.declarations)
                .collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Program {
    pub module: Span<Module>,
    pub entry: Span<TermComputation>,
}
