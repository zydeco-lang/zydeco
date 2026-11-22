//! Shared builders and layered definition names for Stack IR.

use super::syntax::*;
use crate::{high::arena::SpsScope, ir};

/// Read-only definition names for an IR phase.
pub trait DefinitionNames {
    fn defs(&self) -> &ArenaSparse<SpsScope, DefId>;

    fn def_name(&self, id: &DefId) -> VarName {
        self.defs().get(id).copied().unwrap_or_else(|| VarName::new("_"))
    }
}

/// Build a stack IR node and optionally record its core site mapping.
pub trait Construct<S, T, Arena>: Sized + Into<S> {
    type Site;
    /// Allocate the node in the arena, recording a core-site mapping if provided.
    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> T;
}

/// Definition names and core provenance of one phase.
#[derive(Debug)]
pub struct AdminArena<P: Phase> {
    /// Allocator of this phase's nodes.
    pub(crate) node_allocator: IdAllocator<P::Scope>,
    /// Allocator of the definitions carried across stack IR phases.
    pub(crate) def_allocator: IdAllocator<SpsScope>,
    /// Names for definitions introduced or lowered into stack IR.
    pub defs: ArenaSparse<SpsScope, DefId>,
    /// One core pattern may originate several generated patterns; every generated pattern
    /// has at most one core pattern.
    pub pats: ArenaForth<ir::VPatId, P::VPatId>,
    /// One core term may originate several generated nodes; every generated node has
    /// at most one core term.
    pub terms: ArenaForth<ir::TermId, TermId<P>>,
}

impl<P: Phase> AdminArena<P> {
    pub fn new() -> Self {
        Self {
            node_allocator: IdAllocator::new(),
            def_allocator: IdAllocator::new(),
            defs: ArenaSparse::default(),
            pats: ArenaForth::new(),
            terms: ArenaForth::new(),
        }
    }

    /// Continue the definitions of an earlier phase with fresh node allocation and provenance.
    pub(crate) fn continuing<Q: Phase>(earlier: AdminArena<Q>) -> Self {
        let AdminArena { def_allocator, defs, .. } = earlier;
        Self { def_allocator, defs, ..Self::new() }
    }

    pub(crate) fn fresh_node<Id>(&mut self) -> Id
    where
        Id: ArenaId,
        P::Scope: Allocates<Id>,
    {
        self.node_allocator.alloc()
    }

    pub(crate) fn fresh_def(&mut self) -> DefId {
        self.def_allocator.alloc()
    }

    /// The identity domain of the definitions this arena introduces.
    pub fn definition_key_space(&self) -> KeySpaceId {
        self.def_allocator.key_space()
    }

    pub(crate) fn insert_def(&mut self, id: DefId, name: VarName) {
        self.defs.insert_new(id, name);
    }

    /// The core term that originated a generated node, if any.
    pub fn term_site(&self, id: impl Into<TermId<P>>) -> Option<ir::TermId> {
        self.terms.back(&id.into()).copied()
    }

    /// The core pattern that originated a generated pattern, if any.
    pub fn pattern_site(&self, id: P::VPatId) -> Option<ir::VPatId> {
        self.pats.back(&id).copied()
    }
}

impl<P: Phase> Default for AdminArena<P> {
    fn default() -> Self {
        Self::new()
    }
}

impl<P: Phase> DefinitionNames for AdminArena<P> {
    fn defs(&self) -> &ArenaSparse<SpsScope, DefId> {
        &self.defs
    }
}

/// The node tables of one phase.
pub trait Nodes: Sized {
    type Phase: Phase<Nodes = Self>;
    fn vpats(&self) -> &ArenaSparse<<Self::Phase as Phase>::Scope, <Self::Phase as Phase>::VPatId>;
    fn values(
        &self,
    ) -> &ArenaSparse<<Self::Phase as Phase>::Scope, <Self::Phase as Phase>::ValueId>;
    fn stacks(
        &self,
    ) -> &ArenaSparse<<Self::Phase as Phase>::Scope, <Self::Phase as Phase>::StackId>;
    fn compus(
        &self,
    ) -> &ArenaSparse<<Self::Phase as Phase>::Scope, <Self::Phase as Phase>::CompuId>;
    fn vpats_mut(
        &mut self,
    ) -> &mut ArenaSparse<<Self::Phase as Phase>::Scope, <Self::Phase as Phase>::VPatId>;
    fn values_mut(
        &mut self,
    ) -> &mut ArenaSparse<<Self::Phase as Phase>::Scope, <Self::Phase as Phase>::ValueId>;
    fn stacks_mut(
        &mut self,
    ) -> &mut ArenaSparse<<Self::Phase as Phase>::Scope, <Self::Phase as Phase>::StackId>;
    fn compus_mut(
        &mut self,
    ) -> &mut ArenaSparse<<Self::Phase as Phase>::Scope, <Self::Phase as Phase>::CompuId>;
}

/// Allocate one node of a phase and record its core source site.
pub trait Builder<P: Phase> {
    fn build_pattern(
        &mut self, node: ValuePattern<P::VPatId>, site: Option<ir::VPatId>,
    ) -> P::VPatId;
    fn build_value(&mut self, node: Value<P>, site: Option<ir::TermId>) -> P::ValueId;
    fn build_stack(&mut self, node: Stack<P>, site: Option<ir::TermId>) -> P::StackId;
    fn build_computation(&mut self, node: Computation<P>, site: Option<ir::TermId>) -> P::CompuId;
}

impl<P: Phase, A: AsMut<AdminArena<P>> + AsMut<P::Nodes>> Builder<P> for A {
    fn build_pattern(
        &mut self, node: ValuePattern<P::VPatId>, site: Option<ir::VPatId>,
    ) -> P::VPatId {
        let id = AsMut::<AdminArena<P>>::as_mut(self).fresh_node();
        AsMut::<P::Nodes>::as_mut(self).vpats_mut().insert_new(id, node);
        if let Some(site) = site {
            AsMut::<AdminArena<P>>::as_mut(self).pats.insert_new(site, id);
        }
        id
    }

    fn build_value(&mut self, node: Value<P>, site: Option<ir::TermId>) -> P::ValueId {
        let id = AsMut::<AdminArena<P>>::as_mut(self).fresh_node();
        AsMut::<P::Nodes>::as_mut(self).values_mut().insert_new(id, node);
        if let Some(site) = site {
            AsMut::<AdminArena<P>>::as_mut(self).terms.insert_new(site, TermId::Value(id));
        }
        id
    }

    fn build_stack(&mut self, node: Stack<P>, site: Option<ir::TermId>) -> P::StackId {
        let id = AsMut::<AdminArena<P>>::as_mut(self).fresh_node();
        AsMut::<P::Nodes>::as_mut(self).stacks_mut().insert_new(id, node);
        if let Some(site) = site {
            AsMut::<AdminArena<P>>::as_mut(self).terms.insert_new(site, TermId::Stack(id));
        }
        id
    }

    fn build_computation(&mut self, node: Computation<P>, site: Option<ir::TermId>) -> P::CompuId {
        let id = AsMut::<AdminArena<P>>::as_mut(self).fresh_node();
        AsMut::<P::Nodes>::as_mut(self).compus_mut().insert_new(id, node);
        if let Some(site) = site {
            AsMut::<AdminArena<P>>::as_mut(self).terms.insert_new(site, TermId::Compu(id));
        }
        id
    }
}

/// The [`Construct`] instances of one phase, which allocate through [`Builder`]. Invoke it where
/// the phase's `VPatId`, `ValueId`, `StackId`, and `CompuId` are in scope.
macro_rules! phase_constructors {
    ($phase:ty, $arena:ty) => {
        impl<U, Arena> $crate::arena::Construct<$crate::syntax::ValuePattern<VPatId>, VPatId, Arena>
            for U
        where
            Arena: AsMut<$arena>,
            U: Into<$crate::syntax::ValuePattern<VPatId>>,
        {
            type Site = $crate::ir::VPatId;
            fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> VPatId {
                $crate::arena::Builder::<$phase>::build_pattern(arena.as_mut(), self.into(), site)
            }
        }

        impl<U, Arena> $crate::arena::Construct<$crate::syntax::Value<$phase>, ValueId, Arena> for U
        where
            Arena: AsMut<$arena>,
            U: Into<$crate::syntax::Value<$phase>>,
        {
            type Site = $crate::ir::TermId;
            fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> ValueId {
                $crate::arena::Builder::<$phase>::build_value(arena.as_mut(), self.into(), site)
            }
        }

        impl<U, Arena> $crate::arena::Construct<$crate::syntax::Stack<$phase>, StackId, Arena> for U
        where
            Arena: AsMut<$arena>,
            U: Into<$crate::syntax::Stack<$phase>>,
        {
            type Site = $crate::ir::TermId;
            fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> StackId {
                $crate::arena::Builder::<$phase>::build_stack(arena.as_mut(), self.into(), site)
            }
        }

        impl<U, Arena> $crate::arena::Construct<$crate::syntax::Computation<$phase>, CompuId, Arena>
            for U
        where
            Arena: AsMut<$arena>,
            U: Into<$crate::syntax::Computation<$phase>>,
        {
            type Site = $crate::ir::TermId;
            fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> CompuId {
                $crate::arena::Builder::<$phase>::build_computation(
                    arena.as_mut(),
                    self.into(),
                    site,
                )
            }
        }
    };
}
pub(crate) use phase_constructors;
