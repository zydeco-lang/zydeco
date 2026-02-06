use super::syntax::*;
use derive_more::{AsMut, AsRef};
use zydeco_derive::{AsMutSelf, AsRefSelf};

#[derive(Debug, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct SNormArena {
    // arenas
    pub svpats: ArenaAssoc<VPatId, ValuePattern>,
    pub svalues: ArenaAssoc<ValueId, Value>,
    pub sstacks: ArenaAssoc<StackId, Stack>,
    pub scompus: ArenaAssoc<CompuId, SComputation>,

    // users
    pub users: ArenaAssoc<DefId, usize>,

    // entry points (each compu may start with a let chain binding former globals)
    pub entry: ArenaAssoc<CompuId, ()>,
}

impl SNormArena {
    pub fn new() -> Self {
        Self {
            svpats: ArenaAssoc::new(),
            svalues: ArenaAssoc::new(),
            sstacks: ArenaAssoc::new(),
            scompus: ArenaAssoc::new(),
            users: ArenaAssoc::new(),
            entry: ArenaAssoc::new(),
        }
    }
}

pub trait SConstruct<S, T, Arena>: Sized + Into<S> {
    /// The previous id of the node.
    type Id;
    /// The associated structure of the node.
    type Structure;
    /// Allocate the node in the arena.
    fn sbuild(self, arena: &mut Arena, id: Self::Id, structure: Self::Structure) -> T;
}

impl<U, Arena> SConstruct<ValuePattern, VPatId, Arena> for U
where
    Arena: AsMut<SNormArena>,
    U: Into<ValuePattern>,
{
    type Id = VPatId;
    type Structure = ();
    fn sbuild(self, arena: &mut Arena, id: Self::Id, (): Self::Structure) -> VPatId {
        let this = &mut *arena.as_mut();
        this.svpats.insert(id, self.into());
        id
    }
}

impl<U, Arena> SConstruct<Value, ValueId, Arena> for U
where
    Arena: AsMut<SNormArena>,
    U: Into<Value>,
{
    type Id = ValueId;
    type Structure = ();
    fn sbuild(self, arena: &mut Arena, id: Self::Id, (): Self::Structure) -> ValueId {
        let this = &mut *arena.as_mut();
        this.svalues.insert(id, self.into());
        id
    }
}

impl<U, Arena> SConstruct<Stack, StackId, Arena> for U
where
    Arena: AsMut<SNormArena>,
    U: Into<Stack>,
{
    type Id = StackId;
    type Structure = ();
    fn sbuild(self, arena: &mut Arena, id: Self::Id, (): Self::Structure) -> StackId {
        let this = &mut *arena.as_mut();
        this.sstacks.insert(id, self.into());
        id
    }
}

impl<U, Arena> SConstruct<Computation<NonJoin>, CompuId, Arena> for U
where
    Arena: AsMut<SNormArena>,
    U: Into<Computation<NonJoin>>,
{
    type Id = CompuId;
    type Structure = SubstPatMap;
    fn sbuild(self, arena: &mut Arena, id: Self::Id, map: Self::Structure) -> CompuId {
        let this = &mut *arena.as_mut();
        this.scompus.insert(id, SComputation { compu: self.into(), map });
        id
    }
}
