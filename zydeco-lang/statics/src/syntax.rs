pub use crate::surface_syntax::*;

use derive_more::From;
use indexmap::IndexMap;
use zydeco_utils::{
    arena::{ArenaAssoc, ArenaSparse},
    new_key_type,
};

new_key_type! {
    pub struct DefId;
    pub struct PatId;
    pub struct CoPatId;
    pub struct KindId;
    pub struct TypeId;
    pub struct TermId;
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
    Pi(Pi),
    Sigma(Sigma),
    Data(Data),
    CoData(CoData),
}

/* ---------------------------------- Arena --------------------------------- */

#[derive(Debug)]
pub struct SortedArena {
    /// arena for kinds
    pub kinds: ArenaSparse<KindId, Kind>,
    /// arena for types
    pub types: ArenaSparse<TypeId, Type>,
    /// marks if a type is sealed
    pub sealed: ArenaAssoc<TypeId, ()>,
    /// copattern types
    pub copatys: ArenaSparse<CoPatId, CoPatternType>,
}
