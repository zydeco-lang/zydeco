use super::super::{
    lexer::{LexicalToken, LexicalTokenKind, LexicalTokens},
    syntax::{EntityId, MetaId},
};
use std::{cmp::Reverse, collections::BTreeMap, ops::Range, sync::Arc};

/// Text recovered from one contiguous `--|` source block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextBlock {
    pub text: Arc<str>,
    pub range: Range<usize>,
}

/// Text recovered from one contiguous block of ordinary `--` lines.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LineComment {
    pub text: Arc<str>,
    pub range: Range<usize>,
}

/// A nested `/-` ... `-/` comment retained as canonical multiline text.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockComment {
    pub text: Arc<str>,
    pub range: Range<usize>,
}

/// One typed comment retained outside canonical textual syntax.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SurfaceComment {
    Text(TextBlock),
    Line(LineComment),
    Block(BlockComment),
}

impl SurfaceComment {
    pub fn range(&self) -> &Range<usize> {
        match self {
            | Self::Text(text) => &text.range,
            | Self::Line(comment) => &comment.range,
            | Self::Block(comment) => &comment.range,
        }
    }

    pub fn as_text(&self) -> Option<&TextBlock> {
        match self {
            | Self::Text(text) => Some(text),
            | Self::Line(_) | Self::Block(_) => None,
        }
    }

    fn consumes_line_ending(&self, source: &str) -> bool {
        source.get(self.range().clone()).is_some_and(|comment| comment.ends_with(['\r', '\n']))
    }
}

/// The canonical line separation between trivia and adjacent syntax.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LineSeparation {
    SameLine,
    NextLine,
    BlankLine,
}

impl LineSeparation {
    fn after_comment(source: &str, comment: &SurfaceComment, end: usize) -> Self {
        source.get(comment.range().end..end).map_or(Self::BlankLine, |gap| {
            let line_breaks = Self::line_breaks(Self::whitespace_prefix(gap))
                + usize::from(comment.consumes_line_ending(source));
            Self::from_line_breaks(line_breaks)
        })
    }

    fn before_comment(source: &str, start: usize, comment: &SurfaceComment) -> Self {
        source.get(start..comment.range().start).map_or(Self::BlankLine, |gap| {
            Self::from_line_breaks(Self::line_breaks(Self::whitespace_suffix(gap)))
        })
    }

    fn from_line_breaks(line_breaks: usize) -> Self {
        match line_breaks {
            | 0 => Self::SameLine,
            | 1 => Self::NextLine,
            | _ => Self::BlankLine,
        }
    }

    fn whitespace_prefix(gap: &str) -> &str {
        let end = gap
            .char_indices()
            .find(|(_, character)| !Self::is_gap_whitespace(*character))
            .map_or(gap.len(), |(index, _)| index);
        &gap[..end]
    }

    fn whitespace_suffix(gap: &str) -> &str {
        let start = gap
            .char_indices()
            .rev()
            .find(|(_, character)| !Self::is_gap_whitespace(*character))
            .map_or(0, |(index, character)| index + character.len_utf8());
        &gap[start..]
    }

    fn line_breaks(source: &str) -> usize {
        source
            .bytes()
            .enumerate()
            .filter(|(index, byte)| {
                *byte == b'\n'
                    || (*byte == b'\r' && source.as_bytes().get(index + 1) != Some(&b'\n'))
            })
            .count()
    }

    fn is_gap_whitespace(character: char) -> bool {
        Self::is_horizontal_whitespace(character) || matches!(character, '\r' | '\n')
    }

    fn is_horizontal_whitespace(character: char) -> bool {
        matches!(character, ' ' | '\t' | '\u{000c}')
    }
}

/// One leading comment and the separation following it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LeadingComment {
    comment: SurfaceComment,
    separation_after: LineSeparation,
}

impl LeadingComment {
    fn new(comment: SurfaceComment, separation_after: LineSeparation) -> Self {
        Self { comment, separation_after }
    }

    pub fn comment(&self) -> &SurfaceComment {
        &self.comment
    }

    pub fn separation_after(&self) -> LineSeparation {
        self.separation_after
    }
}

/// One trailing comment and the separation preceding it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TrailingComment {
    separation_before: LineSeparation,
    comment: SurfaceComment,
}

impl TrailingComment {
    fn new(separation_before: LineSeparation, comment: SurfaceComment) -> Self {
        Self { separation_before, comment }
    }

    pub fn separation_before(&self) -> LineSeparation {
        self.separation_before
    }

    pub fn comment(&self) -> &SurfaceComment {
        &self.comment
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) struct SpannedEntity {
    entity: EntityId,
    start: usize,
    end: usize,
}

impl SpannedEntity {
    pub(crate) fn new(entity: EntityId, start: usize, end: usize) -> Self {
        Self { entity, start, end }
    }

    pub(crate) fn entity(self) -> EntityId {
        self.entity
    }

    pub(crate) fn start(self) -> usize {
        self.start
    }

    pub(crate) fn end(self) -> usize {
        self.end
    }

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

    fn trailing_key(self) -> (usize, Reverse<usize>, u8, EntityId) {
        (self.end, Reverse(self.start), self.nesting_rank(), self.entity)
    }
}

pub(crate) struct CommentCapture {
    pub(super) leading: Vec<(EntityId, LeadingComment)>,
    pub(super) before_arms: Vec<(EntityId, LeadingComment)>,
    pub(super) before_metadata: Vec<(EntityId, LeadingComment)>,
    pub(super) trailing: Vec<(EntityId, TrailingComment)>,
    pub(super) layout_exclusions: Vec<Range<usize>>,
}

impl CommentCapture {
    pub(crate) fn new(source: &str, entities: &[SpannedEntity]) -> Self {
        let comments = CommentBlocks::new(source).collect();
        let anchors = comments
            .iter()
            .map(|comment| Self::leading_anchor(comment, entities))
            .collect::<Vec<_>>();
        let first_trailing = anchors.iter().position(Option::is_none).unwrap_or(comments.len());

        let leading = comments[..first_trailing]
            .iter()
            .enumerate()
            .map(|(index, comment)| {
                let anchor = anchors[index].expect("non-trailing comments have an anchor");
                let next_start = Self::leading_next_start(index, &comments, &anchors, anchor);
                let separation = LineSeparation::after_comment(source, comment, next_start);
                (anchor.entity, LeadingComment::new(comment.clone(), separation))
            })
            .collect();

        let trailing_anchor = Self::trailing_anchor(entities);
        let trailing = trailing_anchor
            .map(|anchor| {
                comments[first_trailing..]
                    .iter()
                    .scan(None, |previous, comment| {
                        let separation = previous.map_or_else(
                            || LineSeparation::before_comment(source, anchor.end, comment),
                            |previous| {
                                LineSeparation::after_comment(
                                    source,
                                    previous,
                                    comment.range().start,
                                )
                            },
                        );
                        *previous = Some(comment);
                        Some((anchor.entity, TrailingComment::new(separation, comment.clone())))
                    })
                    .collect()
            })
            .unwrap_or_default();

        let comment_ranges = comments.iter().map(|comment| comment.range().clone());
        let leading_ranges =
            comments[..first_trailing].iter().enumerate().map(|(index, comment)| {
                let anchor = anchors[index].expect("non-trailing comments have an anchor");
                comment.range().end..Self::leading_next_start(index, &comments, &anchors, anchor)
            });
        let trailing_ranges = trailing_anchor.into_iter().flat_map(|anchor| {
            comments[first_trailing..].iter().scan(anchor.end, |previous_end, comment| {
                let range = *previous_end..comment.range().start;
                *previous_end = comment.range().end;
                Some(range)
            })
        });
        let layout_exclusions =
            comment_ranges.chain(leading_ranges).chain(trailing_ranges).collect();

        Self {
            leading,
            before_arms: Vec::new(),
            before_metadata: Vec::new(),
            trailing,
            layout_exclusions,
        }
    }

    /// Move comments written before an arm marker from the arm's first entity
    /// to the structural boundary. Comments written after the marker remain
    /// attached to that entity.
    pub(crate) fn with_arm_prefixes(
        mut self, prefixes: impl IntoIterator<Item = (EntityId, usize)>,
    ) -> Self {
        let prefixes = prefixes.into_iter().collect::<BTreeMap<_, _>>();
        let (before_arms, leading) = self.leading.into_iter().partition(|(entity, comment)| {
            prefixes.get(entity).is_some_and(|prefix| comment.comment().range().start < *prefix)
        });
        self.leading = leading;
        self.before_arms = before_arms;
        self
    }

    /// Keep comments written before an annotation's `@` outside its brackets.
    /// Comments after the prefix remain leading trivia of the metadata value.
    pub(crate) fn with_metadata_prefixes(
        mut self, prefixes: impl IntoIterator<Item = (MetaId, usize)>,
    ) -> Self {
        let prefixes = prefixes
            .into_iter()
            .map(|(metadata, start)| (EntityId::Meta(metadata), start))
            .collect::<BTreeMap<_, _>>();
        let (before_metadata, leading) = self.leading.into_iter().partition(|(entity, comment)| {
            prefixes.get(entity).is_some_and(|prefix| comment.comment().range().start < *prefix)
        });
        self.leading = leading;
        self.before_metadata = before_metadata;
        self
    }

    /// Byte ranges whose vertical whitespace is already represented by the
    /// captured comment separations.
    pub(crate) fn layout_exclusions(&self) -> &[Range<usize>] {
        &self.layout_exclusions
    }

    /// Include an entity's leading trivia in the start used for boundary
    /// layout. The comment content remains owned by `SurfaceTrivia`.
    pub(crate) fn presentation_start(&self, entity: EntityId, syntax_start: usize) -> usize {
        self.leading
            .iter()
            .chain(self.before_arms.iter())
            .chain(self.before_metadata.iter())
            .filter(|(anchor, _)| *anchor == entity)
            .map(|(_, comment)| comment.comment().range().start)
            .min()
            .unwrap_or(syntax_start)
    }

    /// Start of the entity payload including comments written after an arm
    /// marker, but excluding comments that precede the whole arm.
    pub(crate) fn arm_payload_start(&self, entity: EntityId, syntax_start: usize) -> usize {
        self.leading
            .iter()
            .filter(|(anchor, _)| *anchor == entity)
            .map(|(_, comment)| comment.comment().range().start)
            .min()
            .unwrap_or(syntax_start)
    }

    fn leading_next_start(
        index: usize, comments: &[SurfaceComment], anchors: &[Option<SpannedEntity>],
        anchor: SpannedEntity,
    ) -> usize {
        comments
            .get(index + 1)
            .zip(anchors.get(index + 1))
            .filter(|(_, next_anchor)| **next_anchor == Some(anchor))
            .map_or(anchor.start, |(next, _)| next.range().start)
    }

    fn leading_anchor(
        comment: &SurfaceComment, entities: &[SpannedEntity],
    ) -> Option<SpannedEntity> {
        entities
            .iter()
            .copied()
            .filter(|entity| entity.start >= comment.range().end)
            .min_by_key(|entity| entity.leading_key())
    }

    fn trailing_anchor(entities: &[SpannedEntity]) -> Option<SpannedEntity> {
        entities.iter().copied().max_by_key(|entity| entity.trailing_key())
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum CommentTokenKind {
    TextLine,
    Line,
    Block,
}

impl CommentTokenKind {
    fn is_line(self) -> bool {
        matches!(self, Self::TextLine | Self::Line)
    }
}

#[derive(Clone, Debug)]
struct CommentToken {
    lexical: LexicalToken,
    kind: CommentTokenKind,
}

struct CommentBlocks<'source> {
    source: &'source str,
}

impl<'source> CommentBlocks<'source> {
    fn new(source: &'source str) -> Self {
        Self { source }
    }

    fn collect(&self) -> Vec<SurfaceComment> {
        let comments = LexicalTokens::new(self.source)
            .filter_map(|token| self.comment_token(token))
            .collect::<Vec<_>>();
        comments
            .chunk_by(|left, right| self.same_block(left, right))
            .map(|block| self.comment(block))
            .collect()
    }

    fn comment_token(&self, lexical: LexicalToken) -> Option<CommentToken> {
        let kind = match lexical.kind {
            | LexicalTokenKind::TextBlock => CommentTokenKind::TextLine,
            | LexicalTokenKind::Comment
                if self
                    .source
                    .get(lexical.range.clone())
                    .is_some_and(|comment| comment.starts_with("--")) =>
            {
                CommentTokenKind::Line
            }
            | LexicalTokenKind::Comment => CommentTokenKind::Block,
            | _ => return None,
        };
        Some(CommentToken { lexical, kind })
    }

    fn same_block(&self, left: &CommentToken, right: &CommentToken) -> bool {
        left.kind == right.kind
            && left.kind.is_line()
            && self
                .source
                .get(left.lexical.range.end..right.lexical.range.start)
                .is_some_and(|gap| gap.chars().all(LineSeparation::is_horizontal_whitespace))
    }

    fn comment(&self, block: &[CommentToken]) -> SurfaceComment {
        let first = block.first().expect("comment blocks are non-empty");
        let last = block.last().expect("comment blocks are non-empty");
        let range = first.lexical.range.start..last.lexical.range.end;
        match first.kind {
            | CommentTokenKind::TextLine => {
                let text = self.line_block(block, "--|");
                SurfaceComment::Text(TextBlock { text, range })
            }
            | CommentTokenKind::Line => {
                let text = self.line_block(block, "--");
                SurfaceComment::Line(LineComment { text, range })
            }
            | CommentTokenKind::Block => {
                debug_assert_eq!(block.len(), 1);
                let text = self.block_text(&first.lexical);
                SurfaceComment::Block(BlockComment { text, range })
            }
        }
    }

    fn line_block(&self, block: &[CommentToken], marker: &str) -> Arc<str> {
        block
            .iter()
            .map(|comment| self.line_text(&comment.lexical, marker))
            .collect::<Vec<_>>()
            .join("\n")
            .into()
    }

    fn line_text<'comment>(&'comment self, comment: &LexicalToken, marker: &str) -> &'comment str {
        let line = &self.source[comment.range.clone()];
        let line = line.strip_suffix('\n').unwrap_or(line);
        let line = line.strip_suffix('\r').unwrap_or(line);
        let line = line
            .strip_prefix(marker)
            .expect("line comment tokens begin with their classified marker");
        line.strip_prefix(' ').unwrap_or(line)
    }

    fn block_text(&self, comment: &LexicalToken) -> Arc<str> {
        let source = &self.source[comment.range.clone()];
        let source = source.strip_suffix('\n').unwrap_or(source);
        let source = source.strip_suffix('\r').unwrap_or(source);
        let lines = source
            .split('\n')
            .map(|line| line.strip_suffix('\r').unwrap_or(line))
            .collect::<Vec<_>>();
        let indentation = self.opening_indentation(comment.range.start);
        lines
            .iter()
            .enumerate()
            .map(
                |(index, line)| {
                    if index == 0 {
                        *line
                    } else {
                        &line[Self::indentation(line).min(indentation)..]
                    }
                },
            )
            .collect::<Vec<_>>()
            .join("\n")
            .into()
    }

    fn opening_indentation(&self, start: usize) -> usize {
        let line_start = self.source[..start].rfind('\n').map_or(0, |newline| newline + 1);
        let prefix = &self.source[line_start..start];
        if prefix.chars().all(LineSeparation::is_horizontal_whitespace) { prefix.len() } else { 0 }
    }

    fn indentation(line: &str) -> usize {
        line.len() - line.trim_start_matches(LineSeparation::is_horizontal_whitespace).len()
    }
}
