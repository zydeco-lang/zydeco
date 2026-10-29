//! Owning arenas and constructors for first-order SPS.

use super::syntax::*;
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

/// Generated definition names and typed-source provenance for first-order SPS.
pub type SpsLowAdminArena = crate::arena::AdminArena<SpsLow>;

#[derive(Debug, Default, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct SpsLowInnerArena {
    pub protocols: std::sync::Arc<crate::protocol::ProtocolGraph>,
    pub value_protocols: ArenaAssoc<ValueId, crate::protocol::ValueProtocol>,
    pub pattern_protocols: ArenaAssoc<VPatId, crate::protocol::ValueProtocol>,
    pub entry_protocols: ArenaAssoc<ValueId, super::entry::EntryProtocol>,
    pub case_protocols: ArenaAssoc<CompuId, crate::protocol::StackProtocol>,
    /// The stack that each label's body expects, which every jump to the label supplies.
    pub label_protocols: ArenaAssoc<DefId, crate::protocol::StackProtocol>,
    /// Capture ownership before conversion erased it into products and code.
    /// These references describe existing nodes; they do not add executable occurrences.
    pub continuations: ArenaAssoc<StackId, ContinuationEntry>,
    pub vpats: ArenaSparse<SpsLowScope, VPatId>,
    pub values: ArenaSparse<SpsLowScope, ValueId>,
    pub stacks: ArenaSparse<SpsLowScope, StackId>,
    pub compus: ArenaSparse<SpsLowScope, CompuId>,
}

impl Nodes for SpsLowInnerArena {
    type Phase = SpsLow;
    fn vpats(&self) -> &ArenaSparse<SpsLowScope, VPatId> {
        &self.vpats
    }
    fn values(&self) -> &ArenaSparse<SpsLowScope, ValueId> {
        &self.values
    }
    fn stacks(&self) -> &ArenaSparse<SpsLowScope, StackId> {
        &self.stacks
    }
    fn compus(&self) -> &ArenaSparse<SpsLowScope, CompuId> {
        &self.compus
    }
    fn vpats_mut(&mut self) -> &mut ArenaSparse<SpsLowScope, VPatId> {
        &mut self.vpats
    }
    fn values_mut(&mut self) -> &mut ArenaSparse<SpsLowScope, ValueId> {
        &mut self.values
    }
    fn stacks_mut(&mut self) -> &mut ArenaSparse<SpsLowScope, StackId> {
        &mut self.stacks
    }
    fn compus_mut(&mut self) -> &mut ArenaSparse<SpsLowScope, CompuId> {
        &mut self.compus
    }
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

crate::arena::phase_constructors!(SpsLow, SpsLowArena);
