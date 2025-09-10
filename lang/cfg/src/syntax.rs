use derive_more::{Deref, DerefMut, From};
use indexmap::IndexSet;

pub use zydeco_syntax::*;

pub type DefId = zydeco_statics::syntax::DefId;
/// Indirect Value.
#[derive(Deref, DerefMut, From, Clone, Debug)]
pub struct IndValue(Box<Value>);
// pub struct IndValue(Box<MemValue>);
impl IndValue {
    pub fn new(value: Value) -> Self {
        Self(Box::new(value))
    }
}
/// Indirect Computation.
#[derive(Deref, DerefMut, From, Clone, Debug)]
pub struct IndCompu(Box<Computation>);
impl IndCompu {
    pub fn new(computation: Computation) -> Self {
        Self(Box::new(computation))
    }
}

/// Shallow value pattern. Only the first level can be a pattern.
#[derive(From, Clone, Debug)]
pub enum ValuePattern {
    Hole(Hole),
    Var(DefId),
    Ctor(Ctor<DefId, usize>),
    Triv(Triv),
    VCons(Cons<DefId, DefId>),
    // TCons(Cons<DefId, DefId>),
}

// #[derive(Deref, DerefMut)]
// pub struct MemValue {
//     /// The size of the value in bytes.
//     pub size: usize,
//     /// The value itself.
//     #[deref]
//     #[deref_mut]
//     pub value: Value,
// }

/// A block contains a closed computation.
#[derive(Clone, Debug)]
pub struct Block<T>(pub T);

#[derive(From, Clone, Debug)]
pub struct Closure<T> {
    pub capture: Vec<DefId>,
    pub inner: T,
}

#[derive(From, Clone, Debug)]
pub enum Value {
    Hole(Hole),
    Var(DefId),
    Block(Block<IndCompu>),
    Closure(Closure<Block<IndCompu>>),
    Ctor(Ctor<IndValue, usize>),
    Triv(Triv),
    VCons(Cons<IndValue, IndValue>),
    // TCons(Cons<TypeId, IndValue>),
    Lit(Literal),
}

#[derive(From, Clone, Debug)]
pub enum Computation {
    Hole(Hole),
    #[from(ignore)]
    Save(IndCompu),
    #[from(ignore)]
    Restore(IndCompu),
    VAbs(Abs<ValuePattern, IndCompu>),
    VApp(App<IndCompu, IndValue>),
    // TAbs(Abs<TPatId, CompuId>),
    // TApp(App<CompuId, TypeId>),
    Fix(Fix<ValuePattern, IndCompu>),
    #[from(ignore)]
    ForceClosure(Force<IndValue>),
    #[from(ignore)]
    ForceLabel(Force<IndValue>),
    Ret(Return<IndValue>),
    Do(Bind<ValuePattern, IndCompu, IndCompu>),
    Let(Let<ValuePattern, IndValue, IndCompu>),
    Match(Match<IndValue, ValuePattern, IndCompu>),
    CoMatch(CoMatch<IndCompu, usize>),
    Dtor(Dtor<IndCompu, usize>),
}

pub struct TopLevel {
    pub externs: IndexSet<DefId>,
    pub program: Computation,
}
