//! Owning arenas and constructors for first-order SPS.

use super::syntax::*;
use crate::{
    arena::{AdminArena as HighAdminArena, StackirScope},
    static_syntax as ss,
};
use derive_more::{AsMut, AsRef};
use zydeco_derive::{AsMutSelf, AsRefSelf};

#[derive(Debug)]
pub enum SpsLowScope {}

impl Allocates<VPatId> for SpsLowScope {}
impl Allocates<ValueId> for SpsLowScope {}
impl Allocates<StackId> for SpsLowScope {}
impl Allocates<CompuId> for SpsLowScope {}

impl ArenaSchema<VPatId> for SpsLowScope {
    type Item = ValuePattern;
}
impl ArenaSchema<ValueId> for SpsLowScope {
    type Item = Value;
}
impl ArenaSchema<StackId> for SpsLowScope {
    type Item = Stack;
}
impl ArenaSchema<CompuId> for SpsLowScope {
    type Item = Computation;
}

#[derive(Debug, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct SpsLowAdminArena {
    node_allocator: IdAllocator<SpsLowScope>,
    def_allocator: IdAllocator<StackirScope>,
    pub builtins: BuiltinMap,
    pub pats: ArenaForth<ss::PatId, VPatId>,
    pub terms: ArenaForth<ss::TermId, TermId>,
}

impl SpsLowAdminArena {
    pub fn new() -> Self {
        Self {
            node_allocator: IdAllocator::new(),
            def_allocator: IdAllocator::new(),
            builtins: Builtin::all(),
            pats: ArenaForth::new(),
            terms: ArenaForth::new(),
        }
    }

    pub(crate) fn from_high(admin: HighAdminArena) -> Self {
        let HighAdminArena { allocator, builtins, pats: _, terms: _ } = admin;
        Self {
            node_allocator: IdAllocator::new(),
            def_allocator: allocator,
            builtins,
            pats: ArenaForth::new(),
            terms: ArenaForth::new(),
        }
    }

    pub(crate) fn fresh_node<Id>(&mut self) -> Id
    where
        Id: ArenaId,
        SpsLowScope: Allocates<Id>,
    {
        self.node_allocator.alloc()
    }

    pub(crate) fn fresh_def(&mut self) -> DefId {
        self.def_allocator.alloc()
    }
}

impl Default for SpsLowAdminArena {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Default, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct SpsLowInnerArena {
    pub vpats: ArenaSparse<SpsLowScope, VPatId>,
    pub values: ArenaSparse<SpsLowScope, ValueId>,
    pub stacks: ArenaSparse<SpsLowScope, StackId>,
    pub compus: ArenaSparse<SpsLowScope, CompuId>,
}

#[derive(Debug, Default, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct SpsLowArena {
    #[as_ref]
    #[as_mut]
    pub admin: SpsLowAdminArena,
    #[as_ref]
    #[as_mut]
    pub inner: SpsLowInnerArena,
}

pub trait Construct<S, T, Arena>: Sized + Into<S> {
    type Site;
    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> T;
}

impl<U, Arena> Construct<ValuePattern, VPatId, Arena> for U
where
    Arena: AsMut<SpsLowArena>,
    U: Into<ValuePattern>,
{
    type Site = ss::PatId;

    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> VPatId {
        let this = &mut *arena.as_mut();
        let id = this.admin.fresh_node();
        this.inner.vpats.insert_new(id, self.into());
        if let Some(site) = site {
            this.admin.pats.insert_new(site, id);
        }
        id
    }
}

impl<U, Arena> Construct<Value, ValueId, Arena> for U
where
    Arena: AsMut<SpsLowArena>,
    U: Into<Value>,
{
    type Site = ss::TermId;

    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> ValueId {
        let this = &mut *arena.as_mut();
        let id = this.admin.fresh_node();
        this.inner.values.insert_new(id, self.into());
        if let Some(site) = site {
            this.admin.terms.insert_new(site, TermId::Value(id));
        }
        id
    }
}

impl<U, Arena> Construct<Stack, StackId, Arena> for U
where
    Arena: AsMut<SpsLowArena>,
    U: Into<Stack>,
{
    type Site = ss::TermId;

    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> StackId {
        let this = &mut *arena.as_mut();
        let id = this.admin.fresh_node();
        this.inner.stacks.insert_new(id, self.into());
        if let Some(site) = site {
            this.admin.terms.insert_new(site, TermId::Stack(id));
        }
        id
    }
}

impl<U, Arena> Construct<Computation, CompuId, Arena> for U
where
    Arena: AsMut<SpsLowArena>,
    U: Into<Computation>,
{
    type Site = ss::TermId;

    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> CompuId {
        let this = &mut *arena.as_mut();
        let id = this.admin.fresh_node();
        this.inner.compus.insert_new(id, self.into());
        if let Some(site) = site {
            this.admin.terms.insert_new(site, TermId::Compu(id));
        }
        id
    }
}
