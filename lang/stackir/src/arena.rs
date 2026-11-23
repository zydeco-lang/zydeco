//! Shared builders and layered definition names for Stack IR.

use super::syntax::*;
use crate::ir;

/// Read-only definition names for an IR phase.
pub trait DefinitionNames {
    fn defs(&self) -> &DenseArena<DefId, VarName>;

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
    /// Names for definitions introduced or lowered into stack IR.
    pub defs: DenseArena<DefId, VarName>,
    /// Core pattern provenance per generated pattern, when recorded.
    pub pat_sites: SecondaryMap<P::VPatId, ir::VPatId>,
    /// Core term provenance per generated value, when recorded.
    pub value_sites: SecondaryMap<P::ValueId, ir::TermId>,
    /// Core term provenance per generated stack, when recorded.
    pub stack_sites: SecondaryMap<P::StackId, ir::TermId>,
    /// Core term provenance per generated computation, when recorded.
    pub compu_sites: SecondaryMap<P::CompuId, ir::TermId>,
}

impl<P: Phase> AdminArena<P> {
    pub fn new() -> Self {
        Self {
            defs: DenseArena::new(),
            pat_sites: SecondaryMap::new(),
            value_sites: SecondaryMap::new(),
            stack_sites: SecondaryMap::new(),
            compu_sites: SecondaryMap::new(),
        }
    }

    /// Continue the definitions of an earlier phase with fresh node provenance.
    pub(crate) fn continuing<Q: Phase>(earlier: AdminArena<Q>) -> Self {
        let AdminArena { defs, .. } = earlier;
        Self { defs, ..Self::new() }
    }

    pub fn alloc_def(&mut self, name: VarName) -> DefId {
        self.defs.alloc(name)
    }

    pub fn fresh_def(&mut self) -> DefId {
        self.alloc_def(VarName::new("_"))
    }

    /// The core term that originated a generated node, if any.
    pub fn term_site(&self, id: impl Into<TermId<P>>) -> Option<ir::TermId> {
        match id.into() {
            | TermId::Value(id) => self.value_sites.get(id).copied(),
            | TermId::Stack(id) => self.stack_sites.get(id).copied(),
            | TermId::Compu(id) => self.compu_sites.get(id).copied(),
        }
    }

    /// The core pattern that originated a generated pattern, if any.
    pub fn pattern_site(&self, id: P::VPatId) -> Option<ir::VPatId> {
        self.pat_sites.get(id).copied()
    }
}

impl<P: Phase> Default for AdminArena<P> {
    fn default() -> Self {
        Self::new()
    }
}

impl<P: Phase> DefinitionNames for AdminArena<P> {
    fn defs(&self) -> &DenseArena<DefId, VarName> {
        &self.defs
    }
}

/// The node tables of one phase.
pub trait Nodes: Sized {
    type Phase: Phase<Nodes = Self>;
    fn vpats(
        &self,
    ) -> &DenseArena<<Self::Phase as Phase>::VPatId, ValuePattern<<Self::Phase as Phase>::VPatId>>;
    fn values(&self) -> &DenseArena<<Self::Phase as Phase>::ValueId, Value<Self::Phase>>;
    fn stacks(&self) -> &DenseArena<<Self::Phase as Phase>::StackId, Stack<Self::Phase>>;
    fn compus(&self) -> &DenseArena<<Self::Phase as Phase>::CompuId, Computation<Self::Phase>>;
    fn vpats_mut(
        &mut self,
    ) -> &mut DenseArena<<Self::Phase as Phase>::VPatId, ValuePattern<<Self::Phase as Phase>::VPatId>>;
    fn values_mut(
        &mut self,
    ) -> &mut DenseArena<<Self::Phase as Phase>::ValueId, Value<Self::Phase>>;
    fn stacks_mut(
        &mut self,
    ) -> &mut DenseArena<<Self::Phase as Phase>::StackId, Stack<Self::Phase>>;
    fn compus_mut(
        &mut self,
    ) -> &mut DenseArena<<Self::Phase as Phase>::CompuId, Computation<Self::Phase>>;
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
        let id = AsMut::<P::Nodes>::as_mut(self).vpats_mut().alloc(node);
        if let Some(site) = site {
            AsMut::<AdminArena<P>>::as_mut(self).pat_sites.insert_new(id, site);
        }
        id
    }

    fn build_value(&mut self, node: Value<P>, site: Option<ir::TermId>) -> P::ValueId {
        let id = AsMut::<P::Nodes>::as_mut(self).values_mut().alloc(node);
        if let Some(site) = site {
            AsMut::<AdminArena<P>>::as_mut(self).value_sites.insert_new(id, site);
        }
        id
    }

    fn build_stack(&mut self, node: Stack<P>, site: Option<ir::TermId>) -> P::StackId {
        let id = AsMut::<P::Nodes>::as_mut(self).stacks_mut().alloc(node);
        if let Some(site) = site {
            AsMut::<AdminArena<P>>::as_mut(self).stack_sites.insert_new(id, site);
        }
        id
    }

    fn build_computation(&mut self, node: Computation<P>, site: Option<ir::TermId>) -> P::CompuId {
        let id = AsMut::<P::Nodes>::as_mut(self).compus_mut().alloc(node);
        if let Some(site) = site {
            AsMut::<AdminArena<P>>::as_mut(self).compu_sites.insert_new(id, site);
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
