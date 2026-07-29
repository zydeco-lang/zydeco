use super::syntax::*;
use derive_more::{AsMut, AsRef};
use zydeco_derive::{AsMutSelf, AsRefSelf};

/// Owning storage scope for substitution-normal stack IR.
#[derive(Debug)]
pub enum SNormScope {}

impl ArenaSchema<VPatId> for SNormScope {
    type Item = ValuePattern;
}
impl ArenaSchema<ValueId> for SNormScope {
    type Item = Value;
}
impl ArenaSchema<StackId> for SNormScope {
    type Item = Stack;
}
impl ArenaSchema<CompuId> for SNormScope {
    type Item = SComputation;
}

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

#[derive(Debug, Default, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct SNormInnerArena {
    /// value pattern arena
    pub svpats: ArenaSparse<SNormScope, VPatId>,
    /// value arena
    pub svalues: ArenaSparse<SNormScope, ValueId>,
    /// stack arena
    pub sstacks: ArenaSparse<SNormScope, StackId>,
    /// computation arena
    pub scompus: ArenaSparse<SNormScope, CompuId>,

    /// users of variables
    pub users: ArenaAssoc<DefId, Vec<ValueId>>,
    /// hole (bullet) in stacks. LHS is the stack, RHS is the bullet stack id.
    pub holes: ArenaAssoc<StackId, StackId>,

    // entry points (each compu may start with a let chain binding former globals)
    pub entry: ArenaAssoc<CompuId, ()>,
}

impl SNormArena {
    pub fn new(admin: AdminArena) -> Self {
        Self { admin, inner: SNormInnerArena::default() }
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
        this.svpats.insert_new(id, self.into());
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
        this.svalues.insert_new(id, self.into());
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
        this.sstacks.insert_new(id, self.into());
        this.holes.insert_new(id, hole);
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
        this.scompus.insert_new(id, SComputation { compu: self.into(), assignments });
        id
    }
}
