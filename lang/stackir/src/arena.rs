//! Arenas and [`Construct`] trait for the stack-passing style ZIR.

use super::{syntax::*, *};
use derive_more::{AsMut, AsRef};
use zydeco_derive::{AsMutSelf, AsRefSelf};

/// All arenas for the stack-passing style ZIR.
/// The definitions and patterns are equivalent to the ones in
/// [`zydeco_statics::tyck::syntax::StaticsArena`].
#[derive(Debug, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct StackirArena {
    /// value pattern arena
    pub vpats: ArenaSparse<VPatId, ValuePattern>,
    /// value arena
    pub values: ArenaSparse<ValueId, Value>,
    /// stack arena
    pub stacks: ArenaSparse<StackId, Stack>,
    /// computation arena
    pub compus: ArenaSparse<CompuId, Computation>,

    /// builtin operators and functions
    pub builtins: BuiltinMap,
    /// globally defined values
    pub globals: ArenaAssoc<DefId, ValueId>,
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

impl StackirArena {
    pub fn new_arc(alloc: ArcGlobalAlloc) -> Self {
        Self {
            vpats: ArenaSparse::new(alloc.alloc()),
            values: ArenaSparse::new(alloc.alloc()),
            stacks: ArenaSparse::new(alloc.alloc()),
            compus: ArenaSparse::new(alloc.alloc()),
            builtins: Builtin::all(),
            globals: ArenaAssoc::new(),
            sequence: Vec::new(),
            entry: ArenaAssoc::new(),
            pats: ArenaBijective::new(),
            terms: ArenaBijective::new(),
        }
    }
}

/// Build a stack IR node and optionally record its source site mapping.
pub trait Construct<S, T, Arena>: Sized + Into<S> {
    type Site;
    /// Allocate the node in the arena, recording a typed-site mapping if provided.
    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> T;
}

impl<U, Arena> Construct<ValuePattern, VPatId, Arena> for U
where
    Arena: AsMut<StackirArena>,
    U: Into<ValuePattern>,
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

impl<U, Arena> Construct<Value, ValueId, Arena> for U
where
    Arena: AsMut<StackirArena>,
    U: Into<Value>,
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

impl<U, Arena> Construct<Stack, StackId, Arena> for U
where
    Arena: AsMut<StackirArena>,
    U: Into<Stack>,
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

impl<U, Arena> Construct<Computation, CompuId, Arena> for U
where
    Arena: AsMut<StackirArena>,
    U: Into<Computation>,
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
