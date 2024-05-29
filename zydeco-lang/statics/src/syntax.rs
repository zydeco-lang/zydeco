pub use zydeco_syntax::*;

use crate::surface_syntax as sc;
use derive_more::From;
use indexmap::IndexMap;
use zydeco_utils::{
    arena::{ArenaAssoc, ArenaSparse},
    new_key_type,
};

pub type DeclId = sc::DeclId;
pub type DefId = sc::DefId;
// TermId is unsorted, while we've got the following:
new_key_type! {
    pub struct KindId;
    pub struct TPatId;
    pub struct TypeId;
    pub struct VPatId;
    pub struct ValueId;
    pub struct CompuId;
}
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, From)]
pub enum SortedId {
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
    Arrow(Arrow<KindId>),
}

/* ---------------------------------- Type ---------------------------------- */

#[derive(From, Clone, Debug)]
pub enum TypePattern {
    Ann(Ann<TPatId, KindId>),
    Hole(Hole),
    Var(DefId),
}

/// `pi (x: A) -> B`
#[derive(Clone, Debug)]
pub struct Forall(pub TPatId, pub TypeId);

/// `sigma (x: A) . A'`
#[derive(Clone, Debug)]
pub struct Exists(pub TPatId, pub TypeId);

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
    Sealed(Sealed<sc::TermId>),
    Ann(Ann<TypeId, KindId>),
    Hole(Hole),
    Var(DefId),
    Abs(Abs<TPatId, TypeId>),
    Arrow(Arrow<TypeId>),
    Forall(Forall),
    Prod(Prod<TypeId>),
    Exists(Exists),
    Data(Data),
    CoData(CoData),
}

/* ---------------------------------- Value --------------------------------- */

#[derive(From, Clone, Debug)]
pub enum ValuePattern {
    Ann(Ann<VPatId, sc::TermId>),
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<VPatId>),
    Paren(Paren<VPatId>),
}

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

/* ------------------------------- Computation ------------------------------ */

/// `rec (x: A) -> b`
#[derive(Clone, Debug)]
pub struct Rec(pub VPatId, pub CompuId);

/// `ret a` has type `Ret A`
#[derive(Clone, Debug)]
pub struct Return(pub ValueId);
/// `do x <- b; ...`
#[derive(Clone, Debug)]
pub struct Bind {
    pub binder: VPatId,
    pub bindee: CompuId,
    pub tail: CompuId,
}
/// `let x = a in ...`
#[derive(Clone, Debug)]
pub struct PureBind {
    pub binder: VPatId,
    pub bindee: ValueId,
    pub tail: CompuId,
}

// /// `use let x = a in ...`
// #[derive(Clone, Debug)]
// pub struct UseBind {
//     pub uses: UsePath,
//     pub tail: sc::TermId,
// }

/// `match a | C_1 p -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct Match {
    pub scrut: ValueId,
    pub arms: Vec<Matcher>,
}
#[derive(Clone, Debug)]
pub struct Matcher {
    pub binder: VPatId,
    pub tail: CompuId,
}

/// `comatch | .d_1 -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct CoMatch {
    pub arms: Vec<CoMatcher>,
}
#[derive(Clone, Debug)]
pub struct CoMatcher {
    pub params: VPatId,
    pub tail: CompuId,
}

#[derive(From, Clone, Debug)]
pub enum Computation {
    Ann(Ann<CompuId, TypeId>),
    Hole(Hole),
    Abs(Abs<VPatId, CompuId>),
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
pub struct TAlias {
    pub binder: TPatId,
    pub bindee: TypeId,
}

#[derive(Clone, Debug)]
pub struct VAlias {
    pub binder: VPatId,
    pub bindee: ValueId,
}

#[derive(Clone, Debug)]
pub struct Extern {
    pub comp: bool,
    pub binder: VPatId,
    pub params: Option<VPatId>,
    pub ty: Option<sc::TermId>,
}

#[derive(Clone, Debug)]
pub struct Main(pub CompuId);

#[derive(Clone, From, Debug)]
pub enum Declaration {
    TAlias(TAlias),
    VAlias(VAlias),
    Extern(Extern),
    Main(Main),
}

/* ---------------------------------- Arena --------------------------------- */

#[derive(Debug)]
pub struct StaticArena {
    /// sorted terms
    pub sorts: ArenaAssoc<sc::TermId, SortedId>,
    /// ... and back
    pub unsorts: ArenaAssoc<SortedId, sc::TermId>,
    /// arena for kinds
    pub kinds: ArenaSparse<KindId, Kind>,
    /// arena for types
    pub types: ArenaSparse<TypeId, Type>,
    /// arena for values
    pub values: ArenaSparse<ValueId, Value>,
    /// arena for computations
    pub compus: ArenaSparse<CompuId, Computation>,
}
