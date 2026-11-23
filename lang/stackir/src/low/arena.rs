//! Owning arenas and constructors for first-order SPS.

use super::syntax::*;
use derive_more::{AsMut, AsRef};
use zydeco_derive::{AsMutSelf, AsRefSelf};

/// Generated definition names and typed-source provenance for first-order SPS.
pub type SpsLowAdminArena = crate::arena::AdminArena<SpsLow>;

#[derive(Debug, Default, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct SpsLowInnerArena {
    pub protocols: std::sync::Arc<crate::protocol::ProtocolGraph>,
    pub value_protocols: SecondaryMap<ValueId, crate::protocol::ValueProtocol>,
    pub pattern_protocols: SecondaryMap<VPatId, crate::protocol::ValueProtocol>,
    pub entry_protocols: SecondaryMap<ValueId, super::entry::EntryProtocol>,
    pub case_protocols: SecondaryMap<CompuId, crate::protocol::StackProtocol>,
    /// The stack that each label's body expects, which every jump to the label supplies.
    pub label_protocols: SecondaryMap<DefId, crate::protocol::StackProtocol>,
    /// Capture ownership before conversion erased it into products and code.
    /// These references describe existing nodes; they do not add executable occurrences.
    pub continuations: SecondaryMap<StackId, ContinuationEntry>,
    pub vpats: DenseArena<VPatId, ValuePattern>,
    pub values: DenseArena<ValueId, Value>,
    pub stacks: DenseArena<StackId, Stack>,
    pub compus: DenseArena<CompuId, Computation>,
}

impl SpsLowInnerArena {
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
            | Value::Ext(ValueExt::Block(_) | ValueExt::ClosurePackage(_)) => RepType::Closure,
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

    /// Return the constructor tag index when `id` constructs a nullary value `Ctor(tag, Triv)`.
    pub fn nullary_ctor_tag(&self, id: ValueId) -> Option<usize> {
        match &self.values[id] {
            | Value::Ctor(Ctor(tag, body)) if matches!(self.values[*body], Value::Triv(_)) => {
                Some(tag.idx)
            }
            | _ => None,
        }
    }
}

impl Nodes for SpsLowInnerArena {
    type Phase = SpsLow;
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
