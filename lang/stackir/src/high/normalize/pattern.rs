//! Rebuild pattern occurrences while retaining definitions and source evidence.

use super::*;
use zydeco_utils::fold::{Folder, Step};

pub(super) struct PatternFolder<'a> {
    pub norm: &'a mut Normalization,
    pub renamings: HashMap<DefId, DefId>,
}

pub(super) struct PatternFrame {
    source: VPatId,
    pattern: ValuePattern,
    position: usize,
}

impl PatternFrame {
    /// The current child slot belongs to this owned layer of the pattern.
    fn child(&mut self) -> Option<&mut VPatId> {
        match &mut self.pattern {
            | ValuePattern::Ctor(Ctor(_, child)) => (self.position == 0).then_some(child),
            | ValuePattern::Alias(Alias(ConsN(head, tail))) => {
                if self.position == head.len() {
                    Some(tail)
                } else {
                    head.get_mut(self.position)
                }
            }
            | ValuePattern::VCons(VCons { items, .. }) => items.get_mut(self.position),
            | ValuePattern::Hole(_) | ValuePattern::Var(_) | ValuePattern::Triv(_) => None,
        }
    }
}

impl Folder for PatternFolder<'_> {
    type Input = VPatId;
    type Output = VPatId;
    type Frame = PatternFrame;

    fn enter(&mut self, source: VPatId) -> Step<Self> {
        let pattern = self.norm.source.inner.vpats[&source].clone();
        self.advance(PatternFrame { source, pattern, position: 0 })
    }

    fn resume(&mut self, mut frame: PatternFrame, child: VPatId) -> Step<Self> {
        *frame.child().expect("a suspended pattern has a child slot") = child;
        frame.position += 1;
        self.advance(frame)
    }
}

impl PatternFolder<'_> {
    fn advance(&mut self, mut frame: PatternFrame) -> Step<Self> {
        if let Some(&mut input) = frame.child() {
            return Step::Call { input, frame };
        }
        let site = self.norm.source.admin.pats.back(&frame.source).copied();
        let pattern = match frame.pattern {
            | ValuePattern::VCons(VCons { items, layout }) => VCons::new(items, layout).into(),
            | ValuePattern::Var(def) => {
                ValuePattern::Var(self.renamings.get(&def).copied().unwrap_or(def))
            }
            | pattern => pattern,
        };
        let node = pattern.build(self.norm, site);
        if let Some(protocol) = self.norm.source.inner.pattern_protocols.get(&frame.source) {
            self.norm.arena.inner.pattern_protocols.insert_new(node, protocol.clone());
        }
        Step::Return(node)
    }
}

#[cfg(test)]
mod tests;
