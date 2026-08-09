//! Author-selected layout retained alongside canonical syntax.

use super::syntax::EntityId;
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

/// Whether two source anchors originally shared a line boundary.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BreakIntent {
    Joined,
    Broken,
}

impl BreakIntent {
    fn between(before: SourceLine, after: SourceLine) -> Self {
        if before == after { Self::Joined } else { Self::Broken }
    }
}

/// Optional layout choices keyed by canonical textual syntax IDs.
///
/// Printers may preserve or ignore these choices. Concision rules such as
/// named-term punning remain canonical printer behavior rather than intent.
#[derive(Clone, Default, Debug)]
pub struct SurfaceIntentions {
    line_extents: ArenaAssoc<EntityId, LineExtent>,
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

    /// Return the observed boundary between two consecutive entities.
    pub fn between(&self, before: EntityId, after: EntityId) -> Option<BreakIntent> {
        let before = self.line_extent(before)?;
        let after = self.line_extent(after)?;
        Some(BreakIntent::between(before.last, after.first))
    }

    /// Return the observed boundary between an enclosing entity's start and
    /// the first entity rendered inside it.
    pub fn after_start(&self, enclosing: EntityId, first: EntityId) -> Option<BreakIntent> {
        let enclosing = self.line_extent(enclosing)?;
        let first = self.line_extent(first)?;
        Some(BreakIntent::between(enclosing.first, first.first))
    }

    /// Return the observed boundary between the final contained entity and an
    /// enclosing entity's end.
    pub fn before_end(&self, last: EntityId, enclosing: EntityId) -> Option<BreakIntent> {
        let last = self.line_extent(last)?;
        let enclosing = self.line_extent(enclosing)?;
        Some(BreakIntent::between(last.last, enclosing.last))
    }
}
