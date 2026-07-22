//! Arenas and [`Construct`] trait for the stack-passing style ZIR.

use super::syntax::*;
use crate::static_syntax as ss;
use derive_more::{AsMut, AsRef};
use zydeco_derive::{AsMutSelf, AsRefSelf};

impl ArenaSchema<VPatId> for StackirScope {
    type Item = ValuePattern;
}
impl ArenaSchema<ValueId> for StackirScope {
    type Item = Value;
}
impl ArenaSchema<StackId> for StackirScope {
    type Item = Stack;
}
impl ArenaSchema<CompuId> for StackirScope {
    type Item = Computation<LetJoin>;
}

/// All arenas for the stack-passing style ZIR.
/// The definitions and patterns are equivalent to the ones in
/// [`zydeco_statics::tyck::syntax::StaticsArena`].
#[derive(Debug, Default, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct StackirArena {
    /// administrative arena
    #[as_ref]
    #[as_mut]
    pub admin: AdminArena,

    /// inner arena that stores the nodes and associated properties
    #[as_ref]
    #[as_mut]
    pub inner: StackirInnerArena,
}

#[derive(Debug, Default, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct StackirInnerArena {
    /// value pattern arena
    pub vpats: ArenaSparse<StackirScope, VPatId>,
    /// value arena
    pub values: ArenaSparse<StackirScope, ValueId>,
    /// stack arena
    pub stacks: ArenaSparse<StackirScope, StackId>,
    /// computation arena
    pub compus: ArenaSparse<StackirScope, CompuId>,

    /// entry point(s), i.e. declarations that are marked as entry points;
    /// each entry compu is wrapped in a let chain binding globals (in order) then the body
    /// typically the main function, which normally should only be unique
    pub entry: ArenaAssoc<CompuId, ()>,
}

impl StackirArena {
    pub fn new() -> Self {
        Self::default()
    }
}

impl StackirInnerArena {
    pub fn new() -> Self {
        Self::default()
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
        let vpat_id = this.admin.fresh();
        this.inner.vpats.insert_new(vpat_id, self.into());
        if let Some(site) = site {
            this.admin.pats.insert_new(site, vpat_id);
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
        let value_id = this.admin.fresh();
        this.inner.values.insert_new(value_id, self.into());
        if let Some(site) = site {
            this.admin.terms.insert_new(site, TermId::Value(value_id));
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
        let stack_id = this.admin.fresh();
        this.inner.stacks.insert_new(stack_id, self.into());
        if let Some(site) = site {
            this.admin.terms.insert_new(site, TermId::Stack(stack_id));
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
        let compu_id = this.admin.fresh();
        this.inner.compus.insert_new(compu_id, self.into());
        if let Some(site) = site {
            this.admin.terms.insert_new(site, TermId::Compu(compu_id));
        }
        compu_id
    }
}
