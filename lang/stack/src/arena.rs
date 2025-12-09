use super::{syntax::*, *};

/// All arenas for the stack-passing style ZIR.
/// The definitions and patterns are equivalent to the ones in
/// [`zydeco_statics::tyck::syntax::StaticsArena`].
#[derive(Debug)]
pub struct StackArena {
    /// value pattern arena
    pub vpats: ArenaSparse<VPatId, ValuePattern>,
    /// value arena
    pub values: ArenaSparse<ValueId, Value>,
    /// stack arena
    pub stacks: ArenaSparse<StackId, Stack>,
    /// computation arena
    pub compus: ArenaSparse<CompuId, Computation>,

    /// external or defined values
    pub globals: ArenaAssoc<DefId, Global>,
    /// the initialization sequence of globals
    pub global_seq: Vec<DefId>,
    /// entry point(s), i.e. declarations that are marked as entry points;
    /// typically the main function, which normally should only be unique
    pub entry: ArenaAssoc<CompuId, ()>,

    /// untyped to typed bijective maps for patterns
    pub pats: ArenaBijective<ss::PatId, VPatId>,
    /// untyped to typed bijective maps for terms
    pub terms: ArenaBijective<ss::TermId, TermId>,
}

impl StackArena {
    pub fn new_arc(alloc: ArcGlobalAlloc) -> Self {
        Self {
            vpats: ArenaSparse::new(alloc.alloc()),
            values: ArenaSparse::new(alloc.alloc()),
            stacks: ArenaSparse::new(alloc.alloc()),
            compus: ArenaSparse::new(alloc.alloc()),
            globals: ArenaAssoc::new(),
            global_seq: Vec::new(),
            entry: ArenaAssoc::new(),
            pats: ArenaBijective::new(),
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
    /// Allocate a value pattern.
    fn vpat(&mut self, site: Option<ss::PatId>, vpat: impl Into<ValuePattern>) -> VPatId;
    /// Allocate a value.
    fn value(&mut self, site: Option<ss::TermId>, value: impl Into<Value>) -> ValueId;
    /// Allocate a stack.
    fn stack(&mut self, site: Option<ss::TermId>, stack: impl Into<Stack>) -> StackId;
    /// Allocate a computation.
    fn compu(&mut self, site: Option<ss::TermId>, compu: impl Into<Computation>) -> CompuId;
}

impl<T> StackArenaLike for T
where
    T: AsMut<StackArena>,
{
    fn vpat(&mut self, site: Option<ss::PatId>, vpat: impl Into<ValuePattern>) -> VPatId {
        let this = &mut *self.as_mut();
        let vpat_id = this.vpats.alloc(vpat.into());
        if let Some(site) = site {
            this.pats.insert(site, vpat_id);
        }
        vpat_id
    }
    fn value(&mut self, site: Option<ss::TermId>, value: impl Into<Value>) -> ValueId {
        let this = &mut *self.as_mut();
        let value_id = this.values.alloc(value.into());
        if let Some(site) = site {
            this.terms.insert(site, TermId::Value(value_id));
        }
        value_id
    }
    fn stack(&mut self, site: Option<ss::TermId>, stack: impl Into<Stack>) -> StackId {
        let this = &mut *self.as_mut();
        let stack_id = this.stacks.alloc(stack.into());
        if let Some(site) = site {
            this.terms.insert(site, TermId::Stack(stack_id));
        }
        stack_id
    }
    fn compu(&mut self, site: Option<ss::TermId>, compu: impl Into<Computation>) -> CompuId {
        let this = &mut *self.as_mut();
        let compu_id = this.compus.alloc(compu.into());
        if let Some(site) = site {
            this.terms.insert(site, TermId::Compu(compu_id));
        }
        compu_id
    }
}
