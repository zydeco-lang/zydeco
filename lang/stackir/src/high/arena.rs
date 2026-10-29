//! Owning arenas and constructors for lexical SPS.

use super::syntax::*;
use crate::static_syntax as ss;
use derive_more::{AsMut, AsRef};
use zydeco_derive::{AsMutSelf, AsRefSelf};

/// Allocation scope for lexical SPS nodes and the definitions that stack IR lowering introduces.
#[derive(Debug)]
pub enum SpsScope {}

impl Allocates<VPatId> for SpsScope {}
impl Allocates<ValueId> for SpsScope {}
impl Allocates<StackId> for SpsScope {}
impl Allocates<CompuId> for SpsScope {}
impl Allocates<DefId> for SpsScope {}

impl ArenaSchema<VPatId> for SpsScope {
    type Item = ValuePattern;
}
impl ArenaSchema<ValueId> for SpsScope {
    type Item = Value;
}
impl ArenaSchema<StackId> for SpsScope {
    type Item = Stack;
}
impl ArenaSchema<CompuId> for SpsScope {
    type Item = Computation;
}

/// Generated definition names and typed-source provenance for lexical SPS.
pub type AdminArena = crate::arena::AdminArena<Sps>;

/// All arenas for the stack-passing style ZIR.
/// The definitions and patterns are equivalent to the ones in
/// [`zydeco_statics::arena::StaticsArena`].
#[derive(Debug, Default, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct SpsArena {
    /// administrative arena
    #[as_ref]
    #[as_mut]
    pub admin: AdminArena,

    /// inner arena that stores the nodes and associated properties
    #[as_ref]
    #[as_mut]
    pub inner: SpsInnerArena,
}

#[derive(Debug, Default, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct SpsInnerArena {
    pub protocols: std::sync::Arc<crate::protocol::ProtocolGraph>,
    /// Closed compiler-defined builtin bodies may be exposed at every known call.
    /// This is compiler provenance, not an annotation on arbitrary source closures.
    pub(crate) builtin_functions: ArenaAssoc<ValueId, BuiltinValueRole>,
    pub value_protocols: ArenaAssoc<ValueId, crate::protocol::ValueProtocol>,
    pub pattern_protocols: ArenaAssoc<VPatId, crate::protocol::ValueProtocol>,
    /// Intrinsic protocols of recursive entries, procedure bodies, and codata eliminations.
    pub compu_protocols: ArenaAssoc<CompuId, crate::protocol::StackProtocol>,
    /// value pattern arena
    pub vpats: ArenaSparse<SpsScope, VPatId>,
    /// value arena
    pub values: ArenaSparse<SpsScope, ValueId>,
    /// stack arena
    pub stacks: ArenaSparse<SpsScope, StackId>,
    /// computation arena
    pub compus: ArenaSparse<SpsScope, CompuId>,
}

impl Nodes for SpsInnerArena {
    type Phase = Sps;
    fn vpats(&self) -> &ArenaSparse<SpsScope, VPatId> {
        &self.vpats
    }
    fn values(&self) -> &ArenaSparse<SpsScope, ValueId> {
        &self.values
    }
    fn stacks(&self) -> &ArenaSparse<SpsScope, StackId> {
        &self.stacks
    }
    fn compus(&self) -> &ArenaSparse<SpsScope, CompuId> {
        &self.compus
    }
    fn vpats_mut(&mut self) -> &mut ArenaSparse<SpsScope, VPatId> {
        &mut self.vpats
    }
    fn values_mut(&mut self) -> &mut ArenaSparse<SpsScope, ValueId> {
        &mut self.values
    }
    fn stacks_mut(&mut self) -> &mut ArenaSparse<SpsScope, StackId> {
        &mut self.stacks
    }
    fn compus_mut(&mut self) -> &mut ArenaSparse<SpsScope, CompuId> {
        &mut self.compus
    }
}

/// A complete Stack IR program with one computation at its top level.
#[derive(Debug)]
pub struct SpsProgram {
    arena: FrozenArena<SpsArena>,
    root: CompuId,
}

/// Read-only source storage and empty target storage for a structural rebuild.
pub(crate) struct SpsRebuild {
    pub source: SpsArena,
    pub target: SpsArena,
    pub root: CompuId,
}

impl SpsProgram {
    pub fn new(arena: SpsArena, root: CompuId) -> Self {
        Self { arena: FrozenArena::new(arena), root }
    }

    pub fn arena(&self) -> &SpsArena {
        &self.arena
    }

    pub fn root(&self) -> CompuId {
        self.root
    }

    /// Reopen the arena for a pass that rewrites nodes in place.
    pub(crate) fn into_parts(self) -> (SpsArena, CompuId) {
        (self.arena.into_inner(), self.root)
    }

    pub(crate) fn into_rebuild(self) -> SpsRebuild {
        let Self { arena, root } = self;
        let SpsArena { mut admin, inner } = arena.into_inner();
        let source_admin = AdminArena {
            pats: std::mem::take(&mut admin.pats),
            terms: std::mem::take(&mut admin.terms),
            ..AdminArena::default()
        };
        let protocols = inner.protocols.clone();
        SpsRebuild {
            source: SpsArena { admin: source_admin, inner },
            target: SpsArena { admin, inner: SpsInnerArena { protocols, ..Default::default() } },
            root,
        }
    }
}

impl SpsArena {
    /// Wrap `tail` in stack let-bindings naming `frames`, the first outermost.
    pub(crate) fn name_frames(
        &mut self, frames: Vec<StackId>, tail: CompuId, site: Option<ss::TermId>,
    ) -> CompuId {
        frames.into_iter().rev().fold(tail, |tail, frame| {
            Let { binder: StackVar, bindee: frame, tail }.build(self, site)
        })
    }
}

crate::arena::phase_constructors!(Sps, SpsArena);
