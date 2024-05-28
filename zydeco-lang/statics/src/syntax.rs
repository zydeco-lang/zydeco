pub use crate::surface_syntax::*;

use crate::surface_syntax as sc;
use derive_more::From;
use indexmap::IndexMap;
use zydeco_utils::{
    arena::{ArenaAssoc, ArenaSparse},
    new_key_type,
};

pub type DeclId = sc::DeclId;
pub type DefId = sc::DefId;
pub type PatId = sc::PatId;
pub type CoPatId = sc::CoPatId;
// TermId is unsorted, while we've got the following:
new_key_type! {
    pub struct KindId;
    pub struct TypeId;
    pub struct ValueId;
    pub struct CompuId;
}
/// the sort of a term which is unsorted
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From)]
pub enum TermSort {
    Kind(KindId),
    Type(TypeId),
    Value(ValueId),
    Compu(CompuId),
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

/* ---------------------------------- Type ---------------------------------- */

#[derive(From, Clone, Debug)]
pub enum TypePattern {
    Ann(Ann<PatId, KindId>),
    Var(DefId),
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
    Sealed(Sealed<TermId>),
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

/* ---------------------------------- Value --------------------------------- */

#[derive(From, Clone, Debug)]
pub enum Value {
    Ann(Ann<ValueId, TypeId>),
    Hole(Hole),
    Var(DefId),
    Paren(Paren<ValueId>),
    Thunk(Thunk<CompuId>),
    Ctor(Ctor<ValueId>),
    Lit(Literal),
}

#[derive(From, Clone, Debug)]
pub enum ValuePattern {
    Ann(Ann<PatId, TermId>),
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<PatId>),
    Paren(Paren<PatId>),
}

/* ------------------------------- Computation ------------------------------ */

/// `rec (x: A) -> b`
#[derive(Clone, Debug)]
pub struct Rec(pub PatId, pub CompuId);

/// `ret a` has type `Ret A`
#[derive(Clone, Debug)]
pub struct Return(pub ValueId);
/// `do x <- b; ...`
#[derive(Clone, Debug)]
pub struct Bind {
    pub binder: PatId,
    pub bindee: CompuId,
    pub tail: CompuId,
}
/// `let x = a in ...`
#[derive(Clone, Debug)]
pub struct PureBind {
    pub binder: PatId,
    pub bindee: ValueId,
    pub tail: CompuId,
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
    pub scrut: ValueId,
    pub arms: Vec<Matcher>,
}
#[derive(Clone, Debug)]
pub struct Matcher {
    pub binder: PatId,
    pub tail: CompuId,
}

/// `comatch | .d_1 -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct CoMatch {
    pub arms: Vec<CoMatcher>,
}
#[derive(Clone, Debug)]
pub struct CoMatcher {
    pub params: CoPatId,
    pub tail: CompuId,
}

#[derive(From, Clone, Debug)]
pub enum Computation {
    Ann(Ann<CompuId, TypeId>),
    Hole(Hole),
    Abs(Abs<CoPatId, CompuId>),
    App(App<CompuId>),
    Rec(Rec),
    Force(Force<ValueId>),
    Ret(Return),
    Do(Bind),
    Let(PureBind),
    // UseLet(UseBind),
    Match(Match),
    CoMatch(CoMatch),
    Dtor(Dtor<ValueId>),
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
pub struct Main(pub CompuId);

#[derive(Clone, From, Debug)]
pub enum Declaration {
    Alias(Alias),
    Extern(Extern),
    // Layer(Layer),
    // UseDef(UseDef),
    // UseBlock(UseBlock),
    Main(Main),
}

/* ---------------------------------- Arena --------------------------------- */

#[derive(Debug)]
pub struct StaticArena {
    /// sorted terms
    pub sorts: ArenaAssoc<TermId, TermSort>,
    /// ... and back
    pub unsorts: ArenaAssoc<TermSort, TermId>,
    /// arena for kinds
    pub kinds: ArenaSparse<KindId, Kind>,
    /// arena for types
    pub types: ArenaSparse<TypeId, Type>,
    // /// copattern types
    // pub copatys: ArenaSparse<CoPatId, CoPatternType>,
    /// arena for values
    pub values: ArenaSparse<ValueId, Value>,
    /// arena for computations
    pub compus: ArenaSparse<CompuId, Computation>,
}
