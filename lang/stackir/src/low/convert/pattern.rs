//! Rename pattern binders while reconstructing their first-order syntax.

use super::*;
use zydeco_utils::fold::{Folder, Step};

pub(super) struct PatternFolder<'a, 'source> {
    pub conversion: &'a mut ClosureConversion<'source>,
}

pub(super) enum PatternFrame {
    Constructor {
        source: high::VPatId,
        tag: high::CtorIdx,
    },
    Fields {
        source: high::VPatId,
        position: usize,
        patterns: Vec<low::VPatId>,
        bindings: Vec<(high::DefId, high::DefId)>,
    },
}

impl Folder for PatternFolder<'_, '_> {
    type Input = high::VPatId;
    type Output = PatternTranslation;
    type Frame = PatternFrame;

    fn enter(&mut self, source: high::VPatId) -> Step<Self> {
        let site = self.conversion.pattern_site(source);
        let translated = match &self.conversion.source.inner.vpats[&source] {
            | high::ValuePattern::Hole(_) => PatternTranslation {
                pattern: low::Hole.build(self.conversion, site),
                bindings: Vec::new(),
            },
            | high::ValuePattern::Triv(_) => PatternTranslation {
                pattern: low::Triv.build(self.conversion, site),
                bindings: Vec::new(),
            },
            | high::ValuePattern::Var(def) => {
                let def = *def;
                let translated = self.conversion.alloc_like(def);
                PatternTranslation {
                    pattern: translated.build(self.conversion, site),
                    bindings: vec![(def, translated)],
                }
            }
            | high::ValuePattern::Ctor(high::Ctor(tag, child)) => {
                return Step::Call {
                    input: *child,
                    frame: PatternFrame::Constructor { source, tag: tag.clone() },
                };
            }
            | high::ValuePattern::Alias(_) | high::ValuePattern::VCons(_) => {
                return self.advance(source, 0, Vec::new(), Vec::new());
            }
        };
        self.finish(source, translated)
    }

    fn resume(&mut self, frame: PatternFrame, child: PatternTranslation) -> Step<Self> {
        match frame {
            | PatternFrame::Constructor { source, tag } => {
                let site = self.conversion.pattern_site(source);
                let pattern = low::Ctor(tag, child.pattern).build(self.conversion, site);
                self.finish(source, PatternTranslation { pattern, bindings: child.bindings })
            }
            | PatternFrame::Fields { source, position, mut patterns, mut bindings } => {
                patterns.push(child.pattern);
                bindings.extend(child.bindings);
                self.advance(source, position + 1, patterns, bindings)
            }
        }
    }
}

impl PatternFolder<'_, '_> {
    fn advance(
        &mut self, source: high::VPatId, position: usize, patterns: Vec<low::VPatId>,
        bindings: Vec<(high::DefId, high::DefId)>,
    ) -> Step<Self> {
        let pattern = &self.conversion.source.inner.vpats[&source];
        let child = match pattern {
            | high::ValuePattern::Alias(high::Alias(high::ConsN(head, tail))) => {
                if position == head.len() { Some(*tail) } else { head.get(position).copied() }
            }
            | high::ValuePattern::VCons(high::VCons { items, .. }) => items.get(position).copied(),
            | _ => unreachable!("pattern fields"),
        };
        if let Some(input) = child {
            return Step::Call {
                input,
                frame: PatternFrame::Fields { source, position, patterns, bindings },
            };
        }
        let translated: low::ValuePattern = match pattern {
            | high::ValuePattern::Alias(_) => {
                low::Alias(low::ConsN::from_vec(patterns).expect("an alias pattern is non-empty"))
                    .into()
            }
            | high::ValuePattern::VCons(high::VCons { layout, .. }) => {
                low::VCons::new(patterns, *layout).into()
            }
            | _ => unreachable!("pattern fields"),
        };
        let site = self.conversion.pattern_site(source);
        let pattern = translated.build(self.conversion, site);
        self.finish(source, PatternTranslation { pattern, bindings })
    }

    fn finish(&mut self, source: high::VPatId, translated: PatternTranslation) -> Step<Self> {
        if let Some(protocol) = self.conversion.source.inner.pattern_protocols.get(&source) {
            self.conversion
                .arena
                .inner
                .pattern_protocols
                .insert_new(translated.pattern, protocol.clone());
        }
        Step::Return(translated)
    }
}

#[cfg(test)]
mod tests;
