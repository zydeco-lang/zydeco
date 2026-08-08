use super::super::{
    lexer::{LexicalToken, LexicalTokenKind, LexicalTokens},
    syntax::EntityId,
};
use std::{cmp::Reverse, ops::Range, sync::Arc};

/// Markdown recovered from one contiguous `--|` source block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DocumentationComment {
    pub markdown: Arc<str>,
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
    Documentation(DocumentationComment),
    Line(LineComment),
    Block(BlockComment),
}

impl SurfaceComment {
    pub fn range(&self) -> &Range<usize> {
        match self {
            | Self::Documentation(comment) => &comment.range,
            | Self::Line(comment) => &comment.range,
            | Self::Block(comment) => &comment.range,
        }
    }

    pub fn as_documentation(&self) -> Option<&DocumentationComment> {
        match self {
            | Self::Documentation(comment) => Some(comment),
            | Self::Line(_) | Self::Block(_) => None,
        }
    }

    fn consumes_line_ending(&self, source: &str) -> bool {
        source.get(self.range().clone()).is_some_and(|comment| comment.ends_with('\n'))
    }
}

/// The canonical vertical separation between trivia and adjacent syntax.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LineSeparation {
    NextLine,
    BlankLine,
}

impl LineSeparation {
    fn after_comment(source: &str, comment: &SurfaceComment, end: usize) -> Self {
        let allowed_line_breaks = usize::from(!comment.consumes_line_ending(source));
        let adjacent = source.get(comment.range().end..end).is_some_and(|gap| {
            gap.chars().all(Self::is_gap_whitespace)
                && Self::line_breaks(gap) <= allowed_line_breaks
        });
        if adjacent { Self::NextLine } else { Self::BlankLine }
    }

    fn before_comment(source: &str, start: usize, comment: &SurfaceComment) -> Self {
        let adjacent = source.get(start..comment.range().start).is_some_and(|gap| {
            gap.chars().all(Self::is_gap_whitespace) && Self::line_breaks(gap) <= 1
        });
        if adjacent { Self::NextLine } else { Self::BlankLine }
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
    pub(super) trailing: Vec<(EntityId, TrailingComment)>,
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
                let next_start = comments
                    .get(index + 1)
                    .zip(anchors.get(index + 1))
                    .filter(|(_, next_anchor)| **next_anchor == Some(anchor))
                    .map(|(next, _)| next.range().start)
                    .unwrap_or(anchor.start);
                let separation = LineSeparation::after_comment(source, comment, next_start);
                (anchor.entity, LeadingComment::new(comment.clone(), separation))
            })
            .collect();

        let trailing = Self::trailing_anchor(entities)
            .map(|anchor| {
                comments[first_trailing..]
                    .iter()
                    .scan(anchor.end, |previous_end, comment| {
                        let separation =
                            LineSeparation::before_comment(source, *previous_end, comment);
                        *previous_end = comment.range().end;
                        Some((anchor.entity, TrailingComment::new(separation, comment.clone())))
                    })
                    .collect()
            })
            .unwrap_or_default();

        Self { leading, trailing }
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
    DocumentationLine,
    Line,
    Block,
}

impl CommentTokenKind {
    fn is_line(self) -> bool {
        matches!(self, Self::DocumentationLine | Self::Line)
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
            | LexicalTokenKind::DocumentationComment => CommentTokenKind::DocumentationLine,
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
            | CommentTokenKind::DocumentationLine => {
                let markdown = self.line_block(block, "--|");
                SurfaceComment::Documentation(DocumentationComment { markdown, range })
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
