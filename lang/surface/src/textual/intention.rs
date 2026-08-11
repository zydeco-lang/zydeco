//! Author-selected layout retained alongside canonical syntax.

use super::syntax::{EntityId, PatId};
use std::{
    collections::BTreeSet,
    ops::{Bound::Excluded, Range},
};
use zydeco_utils::arena::{ArenaAccess, ArenaAssoc};

/// A zero-based source line used only for relative layout comparisons.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct SourceLine(pub usize);

/// The first and last source lines occupied by one parsed entity.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct LineExtent {
    pub first: SourceLine,
    pub last: SourceLine,
}

impl LineExtent {
    pub fn new(first: usize, last: usize) -> Self {
        Self { first: SourceLine(first), last: SourceLine(last) }
    }
}

/// The vertical separation originally observed between two source anchors.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BreakIntent {
    /// Both anchors originally occupied the same line.
    Joined,
    /// The anchors were separated by a line break but no empty line.
    Broken,
    /// The anchors were separated by at least one empty line.
    BlankLine,
}

/// A grammatical source gap whose vertical layout may be retained.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LayoutBoundary {
    Between { before: EntityId, after: EntityId },
    AfterStart { enclosing: EntityId, first: EntityId },
    AfterArmPrefix { payload: EntityId },
    BeforeExistentialParameter { enclosing: EntityId, parameter: PatId },
    BeforeEnd { last: EntityId, enclosing: EntityId },
}

impl LayoutBoundary {
    pub fn between(before: impl Into<EntityId>, after: impl Into<EntityId>) -> Self {
        Self::Between { before: before.into(), after: after.into() }
    }

    pub fn after_start(enclosing: impl Into<EntityId>, first: impl Into<EntityId>) -> Self {
        Self::AfterStart { enclosing: enclosing.into(), first: first.into() }
    }

    pub fn after_arm_prefix(payload: impl Into<EntityId>) -> Self {
        Self::AfterArmPrefix { payload: payload.into() }
    }

    pub fn before_existential_parameter(enclosing: impl Into<EntityId>, parameter: PatId) -> Self {
        Self::BeforeExistentialParameter { enclosing: enclosing.into(), parameter }
    }

    pub fn before_end(last: impl Into<EntityId>, enclosing: impl Into<EntityId>) -> Self {
        Self::BeforeEnd { last: last.into(), enclosing: enclosing.into() }
    }
}

impl BreakIntent {
    fn between(before: SourceLine, after: SourceLine, contains_blank_line: bool) -> Self {
        if before == after {
            Self::Joined
        } else if contains_blank_line {
            Self::BlankLine
        } else {
            Self::Broken
        }
    }

    /// Whether preserving this intention requires a physical line break.
    pub fn requires_line_break(self) -> bool {
        self != Self::Joined
    }
}

#[derive(Clone, Debug)]
struct SourceLayout {
    blank_lines: BTreeSet<SourceLine>,
}

impl SourceLayout {
    fn new(source: &str, exclusions: &[Range<usize>]) -> Self {
        let blank_lines = source
            .split('\n')
            .enumerate()
            .scan(0usize, |start, (line, contents)| {
                let line_start = *start;
                *start = start.saturating_add(contents.len()).saturating_add(1);
                Some((SourceLine(line), line_start, contents))
            })
            .filter_map(|(line, start, contents)| {
                (contents.trim().is_empty()
                    && !exclusions.iter().any(|range| range.contains(&start)))
                .then_some(line)
            })
            .collect();
        Self { blank_lines }
    }

    fn contains_blank_line_between(&self, before: SourceLine, after: SourceLine) -> bool {
        before < after
            && self.blank_lines.range((Excluded(before), Excluded(after))).next().is_some()
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct SourceLayoutId(usize);

/// Optional layout choices keyed by canonical textual syntax IDs.
///
/// Printers may preserve or ignore these choices. Concision rules such as
/// named-term punning remain canonical printer behavior rather than intent.
#[derive(Clone, Default, Debug)]
pub struct SurfaceIntentions {
    line_extents: ArenaAssoc<EntityId, LineExtent>,
    presentation_start_overrides: ArenaAssoc<EntityId, SourceLine>,
    arm_prefix_breaks: ArenaAssoc<EntityId, BreakIntent>,
    existential_parameter_starts: ArenaAssoc<PatId, SourceLine>,
    entity_sources: ArenaAssoc<EntityId, SourceLayoutId>,
    source_layouts: Vec<SourceLayout>,
}

impl SurfaceIntentions {
    /// Return the source lines occupied by one parsed entity.
    pub fn line_extent(&self, entity: EntityId) -> Option<LineExtent> {
        self.line_extents.get(&entity).copied()
    }

    /// Record the source lines occupied by one parsed entity.
    pub fn record_line_extent(&mut self, entity: EntityId, extent: LineExtent) {
        self.line_extents.insert_new(entity, extent);
    }

    pub(crate) fn record_source_layout(
        &mut self, source: &str, trivia_owned_ranges: &[Range<usize>],
        layouts: impl IntoIterator<Item = (EntityId, LineExtent, SourceLine)>,
        arm_layouts: impl IntoIterator<Item = (EntityId, SourceLine, SourceLine)>,
        existential_layouts: impl IntoIterator<Item = (PatId, SourceLine)>,
    ) {
        let source_id = SourceLayoutId(self.source_layouts.len());
        self.source_layouts.push(SourceLayout::new(source, trivia_owned_ranges));
        layouts.into_iter().for_each(|(entity, extent, presentation_start)| {
            self.line_extents.insert_new(entity, extent);
            if presentation_start != extent.first {
                self.presentation_start_overrides.insert_new(entity, presentation_start);
            }
            self.entity_sources.insert_new(entity, source_id);
        });
        arm_layouts.into_iter().for_each(|(payload, prefix, presentation_start)| {
            let contains_blank_line = self.source_layouts[source_id.0]
                .contains_blank_line_between(prefix, presentation_start);
            let intent = BreakIntent::between(prefix, presentation_start, contains_blank_line);
            self.arm_prefix_breaks.insert_new(payload, intent);
        });
        existential_layouts.into_iter().for_each(|(parameter, start)| {
            self.existential_parameter_starts.insert_new(parameter, start);
        });
    }

    fn presentation_start(&self, entity: EntityId) -> Option<SourceLine> {
        self.presentation_start_overrides
            .get(&entity)
            .copied()
            .or_else(|| self.line_extent(entity).map(|extent| extent.first))
    }

    fn break_intent(
        &self, before_entity: EntityId, before: SourceLine, after_entity: EntityId,
        after: SourceLine,
    ) -> BreakIntent {
        let contains_blank_line = self
            .entity_sources
            .get(&before_entity)
            .zip(self.entity_sources.get(&after_entity))
            .filter(|(before, after)| before == after)
            .and_then(|(source, _)| self.source_layouts.get(source.0))
            .is_some_and(|source| source.contains_blank_line_between(before, after));
        BreakIntent::between(before, after, contains_blank_line)
    }

    /// Return the observed vertical separation at one grammatical boundary.
    pub fn at(&self, boundary: LayoutBoundary) -> Option<BreakIntent> {
        match boundary {
            | LayoutBoundary::Between { before, after } => {
                let before_extent = self.line_extent(before)?;
                let after_start = self.presentation_start(after)?;
                Some(self.break_intent(before, before_extent.last, after, after_start))
            }
            | LayoutBoundary::AfterStart { enclosing, first } => {
                let enclosing_extent = self.line_extent(enclosing)?;
                let first_start = self.presentation_start(first)?;
                Some(self.break_intent(enclosing, enclosing_extent.first, first, first_start))
            }
            | LayoutBoundary::AfterArmPrefix { payload } => {
                self.arm_prefix_breaks.get(&payload).copied()
            }
            | LayoutBoundary::BeforeExistentialParameter { enclosing, parameter } => {
                let enclosing_extent = self.line_extent(enclosing)?;
                let parameter_start = self.existential_parameter_starts.get(&parameter).copied()?;
                Some(self.break_intent(
                    enclosing,
                    enclosing_extent.first,
                    parameter.into(),
                    parameter_start,
                ))
            }
            | LayoutBoundary::BeforeEnd { last, enclosing } => {
                let last_extent = self.line_extent(last)?;
                let enclosing_extent = self.line_extent(enclosing)?;
                Some(self.break_intent(last, last_extent.last, enclosing, enclosing_extent.last))
            }
        }
    }
}
