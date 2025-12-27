//! Arenas and [`Construct`] trait for the stack-passing style ZIR.

use super::{syntax::*, *};
use derive_more::{AsMut, AsRef};

/// All arenas for the stack-passing style ZIR.
/// The definitions and patterns are equivalent to the ones in
/// [`zydeco_statics::tyck::syntax::StaticsArena`].
#[derive(Debug, AsRef, AsMut)]
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
    pub sequence: Vec<DefId>,
    /// entry point(s), i.e. declarations that are marked as entry points;
    /// typically the main function, which normally should only be unique
    pub entry: ArenaAssoc<CompuId, ()>,

    /// Zydeco to ZIR bijective maps for patterns
    pub pats: ArenaBijective<ss::PatId, VPatId>,
    /// Zydeco to ZIR bijective maps for terms
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
            sequence: Vec::new(),
            entry: ArenaAssoc::new(),
            pats: ArenaBijective::new(),
            terms: ArenaBijective::new(),
        }
    }
}
impl AsRef<StackArena> for StackArena {
    fn as_ref(&self) -> &StackArena {
        self
    }
}
impl AsMut<StackArena> for StackArena {
    fn as_mut(&mut self) -> &mut StackArena {
        self
    }
}

pub trait Construct<S, T, Arena>: Sized + Into<S> {
    type Site;
    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> T;
}

impl<S, Arena> Construct<ValuePattern, VPatId, Arena> for S
where
    Arena: AsMut<StackArena>,
    S: Into<ValuePattern>,
{
    type Site = ss::PatId;
    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> VPatId {
        let this = &mut *arena.as_mut();
        let vpat_id = this.vpats.alloc(self.into());
        if let Some(site) = site {
            this.pats.insert(site, vpat_id);
        }
        vpat_id
    }
}

impl<S, Arena> Construct<Value, ValueId, Arena> for S
where
    Arena: AsMut<StackArena>,
    S: Into<Value>,
{
    type Site = ss::TermId;
    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> ValueId {
        let this = &mut *arena.as_mut();
        let value_id = this.values.alloc(self.into());
        if let Some(site) = site {
            this.terms.insert(site, TermId::Value(value_id));
        }
        value_id
    }
}

impl<S, Arena> Construct<Stack, StackId, Arena> for S
where
    Arena: AsMut<StackArena>,
    S: Into<Stack>,
{
    type Site = ss::TermId;
    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> StackId {
        let this = &mut *arena.as_mut();
        let stack_id = this.stacks.alloc(self.into());
        if let Some(site) = site {
            this.terms.insert(site, TermId::Stack(stack_id));
        }
        stack_id
    }
}

impl<S, Arena> Construct<Computation, CompuId, Arena> for S
where
    Arena: AsMut<StackArena>,
    S: Into<Computation>,
{
    type Site = ss::TermId;
    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> CompuId {
        let this = &mut *arena.as_mut();
        let compu_id = this.compus.alloc(self.into());
        if let Some(site) = site {
            this.terms.insert(site, TermId::Compu(compu_id));
        }
        compu_id
    }
}
