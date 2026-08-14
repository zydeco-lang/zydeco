//! Source content retained outside canonical textual syntax.

mod comment;

use super::syntax::EntityId;
pub use comment::{
    BlockComment, LeadingComment, LineComment, LineSeparation, SurfaceComment, TextBlock,
    TrailingComment,
};
pub(crate) use comment::{CommentCapture, SpannedEntity};
use zydeco_utils::arena::{ArenaAccess, ArenaAssoc};

/// Comments and other source material anchored to canonical textual IDs.
///
/// Trivia is separate from [`super::SurfaceIntentions`]: comments carry source
/// content and are therefore always printed, while intentions are optional
/// author choices that a printer may ignore.
#[derive(Clone, Default, Debug)]
pub struct SurfaceTrivia {
    leading_comments: ArenaAssoc<EntityId, Vec<LeadingComment>>,
    before_arm_comments: ArenaAssoc<EntityId, Vec<LeadingComment>>,
    trailing_comments: ArenaAssoc<EntityId, Vec<TrailingComment>>,
}

impl SurfaceTrivia {
    /// Comments printed immediately before one textual entity.
    pub fn leading_comments(&self, entity: EntityId) -> &[LeadingComment] {
        self.leading_comments.get(&entity).map(Vec::as_slice).unwrap_or_default()
    }

    /// Comments printed before an arm whose first syntax entity has this ID.
    pub fn before_arm_comments(&self, entity: EntityId) -> &[LeadingComment] {
        self.before_arm_comments.get(&entity).map(Vec::as_slice).unwrap_or_default()
    }

    /// Comments printed after a complete root entity.
    pub fn trailing_comments(&self, entity: EntityId) -> &[TrailingComment] {
        self.trailing_comments.get(&entity).map(Vec::as_slice).unwrap_or_default()
    }

    /// The text block semantically attached to an immediately
    /// following `@[doc]` or `@[literal]` annotation, when present.
    pub fn attached_text(&self, entity: EntityId) -> Option<&TextBlock> {
        self.leading_comments(entity)
            .last()
            .filter(|comment| comment.separation_after() == LineSeparation::NextLine)
            .and_then(|comment| comment.comment().as_text())
    }

    /// Every text block retained from this source arena.
    pub fn text_blocks(&self) -> impl Iterator<Item = &TextBlock> {
        self.leading_comments
            .iter()
            .flat_map(|(_, comments)| comments)
            .filter_map(|comment| comment.comment().as_text())
            .chain(
                self.before_arm_comments
                    .iter()
                    .flat_map(|(_, comments)| comments)
                    .filter_map(|comment| comment.comment().as_text()),
            )
            .chain(
                self.trailing_comments
                    .iter()
                    .flat_map(|(_, comments)| comments)
                    .filter_map(|comment| comment.comment().as_text()),
            )
    }

    pub(crate) fn record_comments(&mut self, capture: CommentCapture) {
        let CommentCapture { leading, before_arms, trailing, .. } = capture;
        leading.into_iter().for_each(|(entity, comment)| {
            self.leading_comments.entry(entity).or_default().push(comment);
        });
        before_arms.into_iter().for_each(|(entity, comment)| {
            self.before_arm_comments.entry(entity).or_default().push(comment);
        });
        trailing.into_iter().for_each(|(entity, comment)| {
            self.trailing_comments.entry(entity).or_default().push(comment);
        });
    }
}
