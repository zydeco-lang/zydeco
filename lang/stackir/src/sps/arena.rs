//! Arenas and [`Construct`] trait for the stack-passing style ZIR.

use super::syntax::*;
use crate::static_syntax as ss;
use derive_more::{AsMut, AsRef};
use zydeco_derive::{AsMutSelf, AsRefSelf};

/// All arenas for the stack-passing style ZIR.
/// The definitions and patterns are equivalent to the ones in
/// [`zydeco_statics::tyck::syntax::StaticsArena`].
#[derive(Debug, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct StackirArena {
    /// arena allocator
    pub allocator: IndexAlloc<usize>,

    /// value pattern arena
    pub vpats: ArenaAssoc<VPatId, ValuePattern>,
    /// value arena
    pub values: ArenaAssoc<ValueId, Value>,
    /// stack arena
    pub stacks: ArenaAssoc<StackId, Stack>,
    /// computation arena
    pub compus: ArenaAssoc<CompuId, Computation<LetJoin>>,

    /// builtin operators and functions
    pub builtins: BuiltinMap,
    /// entry point(s), i.e. declarations that are marked as entry points;
    /// each entry compu is wrapped in a let chain binding globals (in order) then the body
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
            allocator: alloc.alloc(),
            vpats: ArenaAssoc::new(),
            values: ArenaAssoc::new(),
            stacks: ArenaAssoc::new(),
            compus: ArenaAssoc::new(),
            builtins: Builtin::all(),
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
        let vpat_id = this.allocator.alloc();
        this.vpats.insert(vpat_id, self.into());
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
        let value_id = this.allocator.alloc();
        this.values.insert(value_id, self.into());
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
        let stack_id = this.allocator.alloc();
        this.stacks.insert(stack_id, self.into());
        if let Some(site) = site {
            this.terms.insert(site, TermId::Stack(stack_id));
        }
        stack_id
    }
}

impl<U, Arena> Construct<Computation<LetJoin>, CompuId, Arena> for U
where
    Arena: AsMut<StackirArena>,
    U: Into<Computation<LetJoin>>,
{
    type Site = ss::TermId;
    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> CompuId {
        let this = &mut *arena.as_mut();
        let compu_id = this.allocator.alloc();
        this.compus.insert(compu_id, self.into());
        if let Some(site) = site {
            this.terms.insert(site, TermId::Compu(compu_id));
        }
        compu_id
    }
}
