use super::syntax::*;
use derive_more::{AsMut, AsRef};
use zydeco_derive::{AsMutSelf, AsRefSelf};

/// Arena for substitution normal form of stack IR.
#[derive(Debug, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct SNormArena {
    /// administrative arena
    #[as_ref]
    #[as_mut]
    pub admin: AdminArena,

    /// inner arena that stores the nodes and associated properties
    #[as_ref]
    #[as_mut]
    pub inner: SNormInnerArena,
}

#[derive(Debug, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct SNormInnerArena {
    /// value pattern arena
    pub svpats: ArenaAssoc<VPatId, ValuePattern>,
    /// value arena
    pub svalues: ArenaAssoc<ValueId, Value>,
    /// stack arena
    pub sstacks: ArenaAssoc<StackId, Stack>,
    /// computation arena
    pub scompus: ArenaAssoc<CompuId, SComputation>,

    /// users of variables
    pub users: ArenaAssoc<DefId, Vec<ValueId>>,
    /// hole (bullet) in stacks. LHS is the stack, RHS is the bullet stack id.
    pub holes: ArenaAssoc<StackId, StackId>,

    // entry points (each compu may start with a let chain binding former globals)
    pub entry: ArenaAssoc<CompuId, ()>,
}

impl SNormArena {
    pub fn new(admin: AdminArena) -> Self {
        Self { admin, inner: SNormInnerArena::new() }
    }
}

impl SNormInnerArena {
    pub fn new() -> Self {
        Self {
            svpats: ArenaAssoc::new(),
            svalues: ArenaAssoc::new(),
            sstacks: ArenaAssoc::new(),
            scompus: ArenaAssoc::new(),
            users: ArenaAssoc::new(),
            holes: ArenaAssoc::new(),
            entry: ArenaAssoc::new(),
        }
    }
}

#[derive(AsRef, AsMut)]
pub struct SNormArenaMut<'a> {
    #[as_ref]
    #[as_mut]
    pub admin: &'a mut AdminArena,
    #[as_ref]
    #[as_mut]
    pub inner: &'a mut SNormInnerArena,
}

/// Construct a normalized stack IR node without allocating a new id.
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
    Arena: AsMut<SNormInnerArena>,
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
    Arena: AsMut<SNormInnerArena>,
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
    Arena: AsMut<SNormInnerArena>,
    U: Into<Stack>,
{
    type Id = StackId;
    /// The stack id of the hole in the stack.
    type Structure = StackId;
    fn sbuild(self, arena: &mut Arena, id: Self::Id, hole: Self::Structure) -> StackId {
        let this = &mut *arena.as_mut();
        this.sstacks.insert(id, self.into());
        this.holes.insert(id, hole);
        id
    }
}

impl<U, Arena> SConstruct<Computation<NonJoin>, CompuId, Arena> for U
where
    Arena: AsMut<SNormInnerArena>,
    U: Into<Computation<NonJoin>>,
{
    type Id = CompuId;
    type Structure = SubstAssignments;
    fn sbuild(self, arena: &mut Arena, id: Self::Id, assignments: Self::Structure) -> CompuId {
        let this = &mut *arena.as_mut();
        this.scompus.insert(id, SComputation { compu: self.into(), assignments });
        id
    }
}
