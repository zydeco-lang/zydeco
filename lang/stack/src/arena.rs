use super::{syntax::*, *};

/// All arenas for the stack-passing style ZIR.
/// The definitions and patterns are equivalent to the ones in
/// [`zydeco_statics::tyck::syntax::StaticsArena`].
#[derive(Debug)]
pub struct StackArena {
    /// value arena
    pub values: ArenaSparse<ValueId, Value>,
    /// stack arena
    pub stacks: ArenaSparse<StackId, Stack>,
    /// computation arena
    pub compus: ArenaSparse<CompuId, Computation>,

    /// external or defined values
    pub globals: ArenaAssoc<DefId, Global>,
    /// entry point(s), i.e. declarations that are marked as entry points;
    /// typically the main function, which normally should only be unique
    pub entry: ArenaAssoc<CompuId, ()>,

    /// untyped to typed bijective maps for terms
    pub terms: ArenaBijective<ss::TermId, TermId>,
}

impl StackArena {
    pub fn new_arc(alloc: ArcGlobalAlloc) -> Self {
        Self {
            values: ArenaSparse::new(alloc.alloc()),
            stacks: ArenaSparse::new(alloc.alloc()),
            compus: ArenaSparse::new(alloc.alloc()),
            globals: ArenaAssoc::new(),
            entry: ArenaAssoc::new(),
            terms: ArenaBijective::new(),
        }
    }
}
impl AsMut<StackArena> for StackArena {
    fn as_mut(&mut self) -> &mut StackArena {
        self
    }
}

pub trait StackArenaLike {
    /// Allocate a value.
    fn value(&mut self, value: impl Into<Value>) -> ValueId;
    /// Allocate a stack.
    fn stack(&mut self, stack: impl Into<Stack>) -> StackId;
    /// Allocate a computation.
    fn compu(&mut self, compu: impl Into<Computation>) -> CompuId;
}

impl<T> StackArenaLike for T
where
    T: AsMut<StackArena>,
{
    fn value(&mut self, value: impl Into<Value>) -> ValueId {
        let this = &mut *self.as_mut();
        this.values.alloc(value.into())
    }
    fn stack(&mut self, stack: impl Into<Stack>) -> StackId {
        let this = &mut *self.as_mut();
        this.stacks.alloc(stack.into())
    }
    fn compu(&mut self, compu: impl Into<Computation>) -> CompuId {
        let this = &mut *self.as_mut();
        this.compus.alloc(compu.into())
    }
}
