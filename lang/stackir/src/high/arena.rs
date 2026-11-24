//! Owning arenas and constructors for lexical SPS.

use super::syntax::*;
use crate::ir;
use derive_more::{AsMut, AsRef};
use zydeco_derive::{AsMutSelf, AsRefSelf};

/// Generated definition names and core provenance for lexical SPS.
pub type AdminArena = crate::arena::AdminArena<Sps>;

/// All arenas for the stack-passing style ZIR.
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
    /// Closed compiler-defined host-operation bodies may be exposed at every known call.
    /// This is compiler provenance, not an annotation on arbitrary source closures.
    pub(crate) builtin_functions: SecondaryMap<ValueId, HostOp>,
    pub value_protocols: SecondaryMap<ValueId, crate::protocol::ValueProtocol>,
    pub pattern_protocols: SecondaryMap<VPatId, crate::protocol::ValueProtocol>,
    /// Intrinsic protocols of recursive entries, procedure bodies, and codata eliminations.
    pub compu_protocols: SecondaryMap<CompuId, crate::protocol::StackProtocol>,
    /// value pattern arena
    pub vpats: DenseArena<VPatId, ValuePattern>,
    /// value arena
    pub values: DenseArena<ValueId, Value>,
    /// stack arena
    pub stacks: DenseArena<StackId, Stack>,
    /// computation arena
    pub compus: DenseArena<CompuId, Computation>,
}

impl SpsInnerArena {
    /// Physical representation summary of a value node, refining unknown protocols from syntax.
    pub fn value_rep(&self, id: ValueId) -> RepType {
        if let Some(protocol) = self.value_protocols.get(id) {
            let rep = RepType::from_protocol(protocol);
            if rep != RepType::Word {
                return rep;
            }
        }
        match &self.values[id] {
            | Value::Triv(_) => RepType::Unit,
            | Value::Literal(literal) => RepType::from_literal(literal),
            | Value::Primitive(Primitive { operation, .. }) => {
                RepType::Scalar(operation.scalar_type())
            }
            | Value::AddrOffset(_) => RepType::Address,
            | Value::VCons(VCons { layout, .. }) => u16::try_from(layout.arity)
                .map_or(RepType::Word, |arity| RepType::Product { arity }),
            | Value::Ext(ValueExt::Closure(_)) => RepType::Closure,
            | Value::Hole(_) | Value::Var(_) | Value::Ctor(_) => RepType::Word,
        }
    }

    /// Physical representation summary of a value pattern, refining unknown protocols from syntax.
    pub fn pattern_rep(&self, id: VPatId) -> RepType {
        if let Some(protocol) = self.pattern_protocols.get(id) {
            let rep = RepType::from_protocol(protocol);
            if rep != RepType::Word {
                return rep;
            }
        }
        match &self.vpats[id] {
            | ValuePattern::Triv(_) => RepType::Unit,
            | ValuePattern::VCons(VCons { layout, .. }) => u16::try_from(layout.arity)
                .map_or(RepType::Word, |arity| RepType::Product { arity }),
            | ValuePattern::Hole(_)
            | ValuePattern::Var(_)
            | ValuePattern::Ctor(_)
            | ValuePattern::Alias(_) => RepType::Word,
        }
    }
}

impl Nodes for SpsInnerArena {
    type Phase = Sps;
    fn vpats(&self) -> &DenseArena<VPatId, ValuePattern> {
        &self.vpats
    }
    fn values(&self) -> &DenseArena<ValueId, Value> {
        &self.values
    }
    fn stacks(&self) -> &DenseArena<StackId, Stack> {
        &self.stacks
    }
    fn compus(&self) -> &DenseArena<CompuId, Computation> {
        &self.compus
    }
    fn vpats_mut(&mut self) -> &mut DenseArena<VPatId, ValuePattern> {
        &mut self.vpats
    }
    fn values_mut(&mut self) -> &mut DenseArena<ValueId, Value> {
        &mut self.values
    }
    fn stacks_mut(&mut self) -> &mut DenseArena<StackId, Stack> {
        &mut self.stacks
    }
    fn compus_mut(&mut self) -> &mut DenseArena<CompuId, Computation> {
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
            pat_sites: std::mem::take(&mut admin.pat_sites),
            value_sites: std::mem::take(&mut admin.value_sites),
            stack_sites: std::mem::take(&mut admin.stack_sites),
            compu_sites: std::mem::take(&mut admin.compu_sites),
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
        &mut self, frames: Vec<StackId>, tail: CompuId, site: Option<ir::TermId>,
    ) -> CompuId {
        frames.into_iter().rev().fold(tail, |tail, frame| {
            Let { binder: StackVar, bindee: frame, tail }.build(self, site)
        })
    }
}

crate::arena::phase_constructors!(Sps, SpsArena);
