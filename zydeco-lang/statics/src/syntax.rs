pub use crate::surface_syntax::*;

use crate::surface_syntax as sc;
use derive_more::From;
use indexmap::IndexMap;
use zydeco_utils::{
    arena::{ArenaAssoc, ArenaSparse},
    new_key_type,
};

pub type DeclId = sc::DeclId;
new_key_type! {
    pub struct PatId;
    pub struct CoPatId;
    pub struct KindId;
    pub struct TypeId;
    pub struct TermId;
}

/* --------------------------------- Pattern -------------------------------- */

#[derive(From, Clone, Debug)]
pub enum Pattern {
    Ann(Ann<PatId, TermId>),
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<PatId>),
    Paren(Paren<PatId>),
}

#[derive(From, Clone, Debug)]
pub enum CoPattern {
    Pat(PatId),
    Dtor(DtorName),
    App(App<CoPatId>),
}

/* ---------------------------------- Type ---------------------------------- */

#[derive(From, Clone, Debug)]
pub enum CoPatternType {
    Var(DefId),
    Ann(Ann<CoPatId, KindId>),
}

/// `pi (x: A) -> B`
#[derive(Clone, Debug)]
pub struct Pi(pub CoPatId, pub TypeId);

/// `sigma (x: A) . A'`
#[derive(Clone, Debug)]
pub struct Sigma(pub CoPatId, pub TypeId);

/// data | C_1 ty | ... end
#[derive(Clone, Debug)]
pub struct Data {
    pub arms: IndexMap<CtorName, TypeId>,
}

/// `codata | .d_1 cp : ty | ... end`
#[derive(Clone, Debug)]
pub struct CoData {
    pub arms: IndexMap<DtorName, TypeId>,
}

#[derive(From, Clone, Debug)]
pub enum Type {
    Ann(Ann<TypeId, KindId>),
    Hole(Hole),
    Var(DefId),
    Abs(Abs<CoPatId, TypeId>),
    // Arrow(Arrow<TypeId, TypeId>),
    Pi(Pi),
    Sigma(Sigma),
    Data(Data),
    CoData(CoData),
}

/* ---------------------------------- Term ---------------------------------- */

/// `rec (x: A) -> b`
#[derive(Clone, Debug)]
pub struct Rec(pub PatId, pub TermId);

/// `ret a` has type `Ret A`
#[derive(Clone, Debug)]
pub struct Return(pub TermId);
/// `do x <- b; ...`
#[derive(Clone, Debug)]
pub struct Bind {
    pub binder: PatId,
    pub bindee: TermId,
    pub tail: TermId,
}
/// `let x = a in ...`
#[derive(Clone, Debug)]
pub struct PureBind {
    pub binder: PatId,
    pub bindee: TermId,
    pub tail: TermId,
}

// /// `use let x = a in ...`
// #[derive(Clone, Debug)]
// pub struct UseBind {
//     pub uses: UsePath,
//     pub tail: TermId,
// }

/// `match a | C_1 p -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct Match {
    pub scrut: TermId,
    pub arms: Vec<Matcher>,
}
#[derive(Clone, Debug)]
pub struct Matcher {
    pub binder: PatId,
    pub tail: TermId,
}

/// `comatch | .d_1 -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct CoMatch {
    pub arms: Vec<CoMatcher>,
}
#[derive(Clone, Debug)]
pub struct CoMatcher {
    pub params: CoPatId,
    pub tail: TermId,
}

#[derive(From, Clone, Debug)]
pub enum Term {
    Ann(Ann<TermId, TypeId>),
    Hole(Hole),
    Var(DefId),
    Paren(Paren<TermId>),
    Abs(Abs<CoPatId, TermId>),
    App(App<TermId>),
    Rec(Rec),
    Thunk(Thunk<TermId>),
    Force(Force<TermId>),
    Ret(Return),
    Do(Bind),
    Let(PureBind),
    // UseLet(UseBind),
    Ctor(Ctor<TermId>),
    Match(Match),
    CoMatch(CoMatch),
    Dtor(Dtor<TermId>),
    Lit(Literal),
}

/* -------------------------------- TopLevel -------------------------------- */

#[derive(Clone, Debug)]
pub struct Alias {
    pub binder: PatId,
    pub bindee: TermId,
}

#[derive(Clone, Debug)]
pub struct Extern {
    pub comp: bool,
    pub binder: PatId,
    pub params: Option<CoPatId>,
    pub ty: Option<TermId>,
}

// #[derive(Clone, Debug)]
// pub struct Layer {
//     pub name: Option<NameRef<VarName>>,
//     pub uses: Vec<Modifiers<UsePath>>,
//     pub top: TopLevel,
// }

// #[derive(From, Clone, Debug)]
// pub struct UseDef(pub UsePath);

// #[derive(Clone, Debug)]
// pub struct UseBlock {
//     pub uses: UsePath,
//     pub top: TopLevel,
// }

#[derive(Clone, Debug)]
pub struct Main(pub TermId);

#[derive(Clone, From, Debug)]
pub enum Declaration {
    Alias(Alias),
    Extern(Extern),
    // Layer(Layer),
    // UseDef(UseDef),
    // UseBlock(UseBlock),
    Main(Main),
}

/* ---------------------------------- Kind ---------------------------------- */

#[derive(Clone, Debug)]
pub struct VType;
#[derive(Clone, Debug)]
pub struct CType;

#[derive(Debug, Clone)]
pub enum Kind {
    VType(VType),
    CType(CType),
    Arrow(Arrow<KindId, KindId>),
}

/* ---------------------------------- Arena --------------------------------- */

#[derive(Debug)]
pub struct _SortedArena {
    /// arena for kinds
    pub kinds: ArenaSparse<KindId, Kind>,
    /// arena for types
    pub types: ArenaSparse<TypeId, Type>,
    /// marks if a type is sealed
    pub sealed: ArenaAssoc<TypeId, ()>,
    /// copattern types
    pub copatys: ArenaSparse<CoPatId, CoPatternType>,
}
