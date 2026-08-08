//! Author-selected layout retained alongside canonical syntax.

use super::syntax::EntityId;
use zydeco_utils::arena::{ArenaAccess, ArenaAssoc};

/// Whether one parsed entity originally occupied one line or several.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LineLayout {
    Inline,
    Multiline,
}

/// Optional layout choices keyed by canonical textual syntax IDs.
///
/// Printers may preserve or ignore these choices. Concision rules such as
/// named-term punning remain canonical printer behavior rather than intent.
#[derive(Clone, Default, Debug)]
pub struct SurfaceIntentions {
    line_layouts: ArenaAssoc<EntityId, LineLayout>,
}

impl SurfaceIntentions {
    /// Return the original line layout recorded for one parsed entity.
    pub fn line_layout(&self, entity: EntityId) -> Option<LineLayout> {
        self.line_layouts.get(&entity).copied()
    }

    /// Record one layout that was observed while parsing source text.
    pub fn record_line_layout(&mut self, entity: EntityId, layout: LineLayout) {
        self.line_layouts.insert_new(entity, layout);
    }
}
