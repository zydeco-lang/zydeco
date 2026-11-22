//! Contiguous `--|` documentation and literal text blocks retained for semantic analysis.

use super::{
    lexer::{LexicalRole, LexicalTokens},
    syntax::{EntityId, Span},
};
use std::{cmp::Reverse, ops::Range, sync::Arc};
use zydeco_foundation::arena::{ArenaAccess, ArenaAssoc};

/// Text recovered from one contiguous `--|` source block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextBlock {
    pub text: Arc<str>,
    pub range: Range<usize>,
}

impl TextBlock {
    /// Map offsets in stripped prose back to the original comment lines.
    /// A changed source block or an invalid UTF-8 boundary has no mapping.
    pub fn source_range(&self, source: &str, range: Range<usize>) -> Option<Range<usize>> {
        if range.start > range.end
            || !self.text.is_char_boundary(range.start)
            || !self.text.is_char_boundary(range.end)
        {
            return None;
        }
        let block = source.get(self.range.clone())?;
        let lines = LexicalTokens::new(block)
            .filter(|token| token.role == LexicalRole::TextBlock)
            .map(|token| line_content_range(block, &token.range))
            .scan(0usize, |offset, source| {
                let start = *offset;
                *offset += source.len() + 1;
                Some((start, source))
            })
            .collect::<Vec<_>>();
        let reconstructed =
            lines.iter().map(|(_, range)| &block[range.clone()]).collect::<Vec<_>>().join("\n");
        if reconstructed != self.text.as_ref() {
            return None;
        }
        let locate = |offset| {
            lines.iter().find_map(|(start, source)| {
                (*start <= offset && offset <= *start + source.len())
                    .then(|| self.range.start + source.start + offset - start)
            })
        };
        Some(locate(range.start)?..locate(range.end)?)
    }
}

/// Contiguous `--|` text blocks retained by parsing and their immediate attachments.
#[derive(Clone, Default, Debug)]
pub struct TextBlocks {
    blocks: Vec<TextBlock>,
    attached: ArenaAssoc<EntityId, usize>,
}

impl TextBlocks {
    /// The text block semantically attached to an immediately following
    /// `@[doc]` or `@[literal]` annotation, when present.
    pub fn attached_text(&self, entity: EntityId) -> Option<&TextBlock> {
        self.attached.get(&entity).map(|&index| &self.blocks[index])
    }

    /// Every text block retained from this source arena.
    pub fn iter(&self) -> impl Iterator<Item = &TextBlock> {
        self.blocks.iter()
    }

    /// Build contiguous `--|` blocks from lexer-captured line ranges and attach
    /// each unblocked next-line block to its outermost immediately following entity.
    pub(crate) fn record<'a>(
        &mut self, source: &str, text_lines: Vec<Range<usize>>,
        entities: impl Iterator<Item = (EntityId, &'a Span)>,
    ) {
        if text_lines.is_empty() {
            return;
        }

        let mut spanned = entities
            .filter_map(|(entity, span)| {
                let span = span.range();
                source.get(span.start..span.end)?;
                Some(SpannedAnchor { entity, start: span.start, end: span.end })
            })
            .collect::<Vec<_>>();
        spanned.sort_unstable_by_key(|anchor| anchor.leading_key());

        for group in text_lines.chunk_by(|left, right| {
            source
                .get(left.end..right.start)
                .is_some_and(|gap| gap.chars().all(is_horizontal_whitespace))
        }) {
            let range = group.first().expect("chunks are nonempty").start
                ..group.last().expect("chunks are nonempty").end;
            let text: Arc<str> = group
                .iter()
                .map(|line| &source[line_content_range(source, line)])
                .collect::<Vec<_>>()
                .join("\n")
                .into();
            let index = self.blocks.len();
            self.blocks.push(TextBlock { text, range: range.clone() });

            let pos = spanned.partition_point(|anchor| anchor.start < range.end);
            let Some(anchor) = spanned.get(pos) else {
                continue;
            };
            let Some(gap) = source.get(range.end..anchor.start) else {
                continue;
            };
            if !gap.chars().all(is_gap_whitespace) {
                continue;
            }
            let breaks = line_breaks(gap) + usize::from(consumes_line_ending(source, &range));
            if breaks == 1 {
                self.attached.insert_new(anchor.entity, index);
            }
        }
    }
}

#[derive(Copy, Clone)]
struct SpannedAnchor {
    entity: EntityId,
    start: usize,
    end: usize,
}

impl SpannedAnchor {
    fn nesting_rank(self) -> u8 {
        match self.entity {
            | EntityId::Def(_) => 0,
            | EntityId::Pat(_) => 1,
            | EntityId::CoPat(_) => 2,
            | EntityId::Term(_) => 3,
            | EntityId::Meta(_) => 4,
        }
    }

    fn leading_key(self) -> (usize, Reverse<usize>, Reverse<u8>, Reverse<EntityId>) {
        (self.start, Reverse(self.end), Reverse(self.nesting_rank()), Reverse(self.entity))
    }
}

fn line_content_range(source: &str, range: &Range<usize>) -> Range<usize> {
    let line = &source[range.clone()];
    let line = line.strip_suffix('\n').unwrap_or(line);
    let line = line.strip_suffix('\r').unwrap_or(line);
    let end = range.start + line.len();
    let line = line.strip_prefix("--|").expect("text line tokens begin with `--|`");
    let line = line.strip_prefix(' ').unwrap_or(line);
    (end - line.len())..end
}

fn consumes_line_ending(source: &str, range: &Range<usize>) -> bool {
    source.get(range.clone()).is_some_and(|comment| comment.ends_with(['\r', '\n']))
}

fn is_horizontal_whitespace(character: char) -> bool {
    matches!(character, ' ' | '\t' | '\u{000c}')
}

fn is_gap_whitespace(character: char) -> bool {
    is_horizontal_whitespace(character) || matches!(character, '\r' | '\n')
}

fn line_breaks(source: &str) -> usize {
    source
        .bytes()
        .enumerate()
        .filter(|(index, byte)| {
            *byte == b'\n' || (*byte == b'\r' && source.as_bytes().get(index + 1) != Some(&b'\n'))
        })
        .count()
}
