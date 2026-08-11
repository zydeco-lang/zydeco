//! Configurable pretty printing for canonical textual syntax.

mod config;
mod context;
mod punning;
#[cfg(test)]
mod corpus;

use super::syntax::*;
pub use config::{IndentWidth, LayoutIntentions, Parentheses, PrettyOptions};
use context::{GrammarContext, PatternRequirement, TermPrecedence, TermRequirement};
use pretty::{DocAllocator, RcAllocator, RcDoc};
pub use punning::NamedTermPunningAudit;
use punning::{PunnedPatternPayload, PunnedTermPayload, Punning};
use zydeco_syntax::Pretty;

static DOC_ALLOCATOR: RcAllocator = RcAllocator;

/// A pretty printer over one parsed textual arena.
pub struct PrettyFormatter<'arena> {
    arena: &'arena TextArena,
    grammar: GrammarContext<'arena>,
    punning: Punning<'arena>,
    options: PrettyOptions,
}

struct ManifestParameterView<'arena> {
    fields: Vec<(PatId, &'arena FieldName)>,
    binder: PatId,
    definition: TermId,
    classifier: Option<TermId>,
}

#[derive(Copy, Clone)]
struct LayoutAnchors {
    first: EntityId,
    last: EntityId,
}

#[derive(Clone)]
struct LayoutFragment<'arena> {
    document: RcDoc<'arena>,
    anchors: LayoutAnchors,
}

impl<'arena> LayoutFragment<'arena> {
    fn entity(entity: EntityId, document: RcDoc<'arena>) -> Self {
        Self { document, anchors: LayoutAnchors { first: entity, last: entity } }
    }

    fn map_document(self, transform: impl FnOnce(RcDoc<'arena>) -> RcDoc<'arena>) -> Self {
        let Self { document, anchors } = self;
        Self { document: transform(document), anchors }
    }
}

/// The three relevant projections of one grammatical layout layer.
///
/// `compact` joins every boundary owned by the layer. `retained` keeps the
/// source rows, provided each row still fits. `expanded` breaks every boundary
/// in the layer while leaving child layouts independent.
struct GroupLayout<'arena> {
    compact: RcDoc<'arena>,
    retained: RcDoc<'arena>,
    expanded: RcDoc<'arena>,
    retains_break: bool,
}

#[derive(Copy, Clone)]
enum BoundaryIntent {
    Canonical,
    Preserve(LayoutBoundary),
    PreserveBlankLine(LayoutBoundary),
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum BoundaryPlacement {
    Joined,
    Broken,
    BlankLine,
}

impl BoundaryIntent {
    fn between(before: impl Into<EntityId>, after: impl Into<EntityId>) -> Self {
        Self::Preserve(LayoutBoundary::between(before, after))
    }

    fn after_start(enclosing: impl Into<EntityId>, first: impl Into<EntityId>) -> Self {
        Self::Preserve(LayoutBoundary::after_start(enclosing, first))
    }

    fn after_arm_prefix(payload: impl Into<EntityId>) -> Self {
        Self::Preserve(LayoutBoundary::after_arm_prefix(payload))
    }

    fn before_existential_parameter(enclosing: impl Into<EntityId>, parameter: PatId) -> Self {
        Self::Preserve(LayoutBoundary::before_existential_parameter(enclosing, parameter))
    }

    fn before_end(last: impl Into<EntityId>, enclosing: impl Into<EntityId>) -> Self {
        Self::Preserve(LayoutBoundary::before_end(last, enclosing))
    }

    fn blank_line_only(self) -> Self {
        match self {
            | Self::Canonical => Self::Canonical,
            | Self::Preserve(boundary) | Self::PreserveBlankLine(boundary) => {
                Self::PreserveBlankLine(boundary)
            }
        }
    }

    fn resolve(self, arena: &TextArena) -> Option<BreakIntent> {
        match self {
            | Self::Canonical => None,
            | Self::Preserve(boundary) => arena.intentions.at(boundary),
            | Self::PreserveBlankLine(boundary) => {
                arena.intentions.at(boundary).filter(|intent| *intent == BreakIntent::BlankLine)
            }
        }
    }
}

#[derive(Copy, Clone)]
enum DelimiterSpacing {
    Tight,
    Spaced,
}

#[derive(Clone)]
struct BoundaryLayout<'arena> {
    prefix: RcDoc<'arena>,
    joined: RcDoc<'arena>,
    broken: RcDoc<'arena>,
    blank: RcDoc<'arena>,
    joined_indent: isize,
    broken_indent: isize,
}

#[derive(Copy, Clone)]
enum StagedBoundary {
    Annotation,
    BindingType,
    Definition,
}

impl StagedBoundary {
    fn marker(self) -> &'static str {
        match self {
            | Self::Annotation | Self::BindingType => ":",
            | Self::Definition => "=",
        }
    }
}

#[derive(Copy, Clone)]
enum InfixOperator {
    Product,
    Arrow,
}

impl InfixOperator {
    fn symbol(self) -> &'static str {
        match self {
            | Self::Product => "*",
            | Self::Arrow => "->",
        }
    }

    fn left_precedence(self) -> TermPrecedence {
        match self {
            | Self::Product => TermPrecedence::Application,
            | Self::Arrow => TermPrecedence::Product,
        }
    }

    fn right_precedence(self) -> TermPrecedence {
        match self {
            | Self::Product => TermPrecedence::Product,
            | Self::Arrow => TermPrecedence::Arrow,
        }
    }

    fn split(self, term: &Term) -> Option<(TermId, TermId)> {
        match (self, term) {
            | (Self::Product, Term::Prod(Prod(left, right)))
            | (Self::Arrow, Term::Arrow(Arrow(left, right))) => Some((*left, *right)),
            | _ => None,
        }
    }
}

struct InfixOperand<'arena> {
    document: RcDoc<'arena>,
    starts_with_comment: bool,
}

impl<'arena> BoundaryLayout<'arena> {
    fn after(separator: &'static str, joined_indent: isize, broken_indent: isize) -> Self {
        Self {
            prefix: if separator.is_empty() { RcDoc::nil() } else { RcDoc::text(separator) },
            joined: RcDoc::space(),
            broken: RcDoc::hardline(),
            blank: Self::blank_line(),
            joined_indent,
            broken_indent,
        }
    }

    fn tight(joined_indent: isize, broken_indent: isize) -> Self {
        Self {
            prefix: RcDoc::nil(),
            joined: RcDoc::nil(),
            broken: RcDoc::hardline(),
            blank: Self::blank_line(),
            joined_indent,
            broken_indent,
        }
    }

    fn aligned(separator: &'static str) -> Self {
        Self::after(separator, 0, 0)
    }

    fn hanging(separator: &'static str, indent: isize) -> Self {
        Self::after(separator, 0, indent)
    }

    fn nested(separator: &'static str, indent: isize) -> Self {
        Self::after(separator, indent, indent)
    }

    fn blank_line() -> RcDoc<'arena> {
        RcDoc::nesting(|nesting| {
            let nesting = isize::try_from(nesting).unwrap_or(isize::MAX);
            RcDoc::hardline().append(RcDoc::hardline()).nest(-nesting)
        })
    }

    fn place(&self, placement: BoundaryPlacement, continuation: RcDoc<'arena>) -> RcDoc<'arena> {
        let (boundary, indent) = match placement {
            | BoundaryPlacement::Joined => (self.joined.clone(), self.joined_indent),
            | BoundaryPlacement::Broken => (self.broken.clone(), self.broken_indent),
            | BoundaryPlacement::BlankLine => (self.blank.clone(), self.broken_indent),
        };
        self.prefix.clone().append(boundary.append(continuation).nest(indent))
    }
}

impl DelimiterSpacing {
    fn boundary<'arena>(
        self, joined_indent: isize, broken_indent: isize,
    ) -> BoundaryLayout<'arena> {
        match self {
            | Self::Tight => BoundaryLayout::tight(joined_indent, broken_indent),
            | Self::Spaced => BoundaryLayout::after("", joined_indent, broken_indent),
        }
    }
}

impl<'arena> PrettyFormatter<'arena> {
    pub fn new(arena: &'arena TextArena) -> Self {
        Self::with_options(arena, PrettyOptions::default())
    }

    pub fn with_options(arena: &'arena TextArena, options: PrettyOptions) -> Self {
        Self { arena, grammar: GrammarContext::new(arena), punning: Punning::new(arena), options }
    }

    fn indent(&self) -> isize {
        self.options.indent.nesting()
    }

    /// Render one complete source unit with a trailing newline.
    pub fn render_unit(&'arena self, unit: SourceUnit) -> String {
        let document = self.with_trailing_comments(unit.root.into(), unit.pretty(self));
        self.render_doc(document.append(RcDoc::hardline()))
    }

    /// Render one term without adding a trailing newline.
    pub fn render_term(&'arena self, term: TermId) -> String {
        let document = self.with_trailing_comments(term.into(), term.pretty(self));
        self.render_doc(document)
    }

    /// Render one pattern without adding a trailing newline.
    pub fn render_pattern(&'arena self, pattern: PatId) -> String {
        let document = self.with_trailing_comments(pattern.into(), pattern.pretty(self));
        self.render_doc(document)
    }

    /// Render one copattern spine without adding a trailing newline.
    pub fn render_copattern(&'arena self, pattern: CoPatId) -> String {
        let document = self.with_trailing_comments(pattern.into(), pattern.pretty(self));
        self.render_doc(document)
    }

    fn render_doc(&self, document: RcDoc<'arena>) -> String {
        let mut output = String::new();
        document.render_fmt(self.options.line_width, &mut output).unwrap();
        output
    }

    fn with_leading_comments(
        &'arena self, entity: EntityId, document: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        self.with_comments(self.arena.trivia.leading_comments(entity), document)
    }

    fn with_before_arm_comments(
        &'arena self, entity: EntityId, document: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        self.with_comments(self.arena.trivia.before_arm_comments(entity), document)
    }

    fn with_comments(
        &'arena self, comments: &'arena [LeadingComment], document: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        comments
            .iter()
            .fold(RcDoc::nil(), |prefix, comment| {
                prefix
                    .append(if comment.comment().as_documentation().is_some() {
                        self.ensure_line_start()
                    } else {
                        RcDoc::nil()
                    })
                    .append(self.comment(comment.comment()))
                    .append(self.line_separation(comment.separation_after()))
            })
            .append(document)
    }

    fn ensure_line_start(&self) -> RcDoc<'arena> {
        RcDoc::column(|column| {
            RcDoc::nesting(
                move |nesting| {
                    if column > nesting { RcDoc::hardline() } else { RcDoc::nil() }
                },
            )
        })
    }

    fn at_line_start(&self, document: RcDoc<'arena>) -> RcDoc<'arena> {
        RcDoc::column(|column| {
            RcDoc::nesting(
                move |nesting| {
                    if column > nesting { RcDoc::fail() } else { RcDoc::nil() }
                },
            )
        })
        .append(document)
    }

    fn after_line_start(&self, document: RcDoc<'arena>) -> RcDoc<'arena> {
        RcDoc::column(|column| {
            RcDoc::nesting(
                move |nesting| {
                    if column == nesting { RcDoc::fail() } else { RcDoc::nil() }
                },
            )
        })
        .append(document)
    }

    fn with_trailing_comments(
        &'arena self, entity: EntityId, document: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        self.arena.trivia.trailing_comments(entity).iter().fold(document, |document, comment| {
            let separation = if comment.comment().as_documentation().is_some()
                && comment.separation_before() == LineSeparation::SameLine
            {
                LineSeparation::NextLine
            } else {
                comment.separation_before()
            };
            document
                .append(self.line_separation(separation))
                .append(self.comment(comment.comment()))
        })
    }

    fn comment(&'arena self, comment: &'arena SurfaceComment) -> RcDoc<'arena> {
        match comment {
            | SurfaceComment::Documentation(comment) => {
                self.marked_comment_lines("--|", &comment.markdown)
            }
            | SurfaceComment::Line(comment) => self.marked_comment_lines("--", &comment.text),
            | SurfaceComment::Block(comment) => self.block_comment(comment),
        }
    }

    fn marked_comment_lines(
        &'arena self, marker: &'static str, text: &'arena str,
    ) -> RcDoc<'arena> {
        RcDoc::intersperse(
            text.split('\n').map(|line| {
                if line.is_empty() {
                    RcDoc::text(marker)
                } else {
                    RcDoc::text(marker).append(RcDoc::space()).append(RcDoc::text(line))
                }
            }),
            RcDoc::hardline(),
        )
    }

    fn block_comment(&'arena self, comment: &'arena BlockComment) -> RcDoc<'arena> {
        RcDoc::intersperse(comment.text.split('\n').map(RcDoc::text), RcDoc::hardline())
    }

    fn line_separation(&self, separation: LineSeparation) -> RcDoc<'arena> {
        match separation {
            | LineSeparation::SameLine => RcDoc::space(),
            | LineSeparation::NextLine => RcDoc::hardline(),
            | LineSeparation::BlankLine => BoundaryLayout::blank_line(),
        }
    }

    fn mandatory_line_break(&self, intention: Option<BreakIntent>) -> RcDoc<'arena> {
        if self.options.layout_intentions == LayoutIntentions::Preserve
            && intention == Some(BreakIntent::BlankLine)
        {
            BoundaryLayout::blank_line()
        } else {
            RcDoc::hardline()
        }
    }

    fn retained_placement(&self, intent: BoundaryIntent) -> BoundaryPlacement {
        match (self.options.layout_intentions, intent.resolve(self.arena)) {
            | (LayoutIntentions::Preserve, Some(BreakIntent::Broken)) => BoundaryPlacement::Broken,
            | (LayoutIntentions::Preserve, Some(BreakIntent::BlankLine)) => {
                BoundaryPlacement::BlankLine
            }
            | _ => BoundaryPlacement::Joined,
        }
    }

    fn expanded_placement(&self, intent: BoundaryIntent) -> BoundaryPlacement {
        match (self.options.layout_intentions, intent.resolve(self.arena)) {
            | (LayoutIntentions::Preserve, Some(BreakIntent::BlankLine)) => {
                BoundaryPlacement::BlankLine
            }
            | _ => BoundaryPlacement::Broken,
        }
    }

    fn layout_boundary(
        &'arena self, intent: BoundaryIntent, layout: BoundaryLayout<'arena>,
        continuation: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        let retained = self.retained_placement(intent);
        match retained {
            | BoundaryPlacement::Joined => self.flexible(
                layout.place(BoundaryPlacement::Joined, continuation.clone()),
                layout.place(BoundaryPlacement::Broken, continuation),
            ),
            | BoundaryPlacement::Broken | BoundaryPlacement::BlankLine => {
                layout.place(retained, continuation)
            }
        }
    }

    fn fragment_boundary(
        &'arena self, intent: BoundaryIntent, layout: BoundaryLayout<'arena>,
        continuation: LayoutFragment<'arena>,
    ) -> RcDoc<'arena> {
        self.layout_boundary(intent, layout, continuation.document)
    }

    fn prefixed(
        &'arena self, enclosing: impl Into<EntityId>, prefix: &'static str,
        layout: BoundaryLayout<'arena>, child: LayoutFragment<'arena>,
    ) -> RcDoc<'arena> {
        RcDoc::text(prefix).append(self.fragment_boundary(
            BoundaryIntent::after_start(enclosing, child.anchors.first),
            layout,
            child,
        ))
    }

    fn flexible(&'arena self, joined: RcDoc<'arena>, broken: RcDoc<'arena>) -> RcDoc<'arena> {
        // Expose the joined projection to enclosing groups while retaining a
        // complete broken alternative for a boundary whose own group overflows.
        joined.clone().union(broken).flat_alt(joined)
    }

    fn join(
        &'arena self, left: LayoutFragment<'arena>, layout: BoundaryLayout<'arena>,
        right: LayoutFragment<'arena>,
    ) -> LayoutFragment<'arena> {
        let intent = BoundaryIntent::between(left.anchors.last, right.anchors.first);
        let anchors = LayoutAnchors { first: left.anchors.first, last: right.anchors.last };
        let document = left.document.append(self.layout_boundary(intent, layout, right.document));
        LayoutFragment { document, anchors }
    }

    fn separated(
        &'arena self, items: Vec<LayoutFragment<'arena>>, layout: BoundaryLayout<'arena>,
    ) -> Option<LayoutFragment<'arena>> {
        items.into_iter().rev().reduce(|right, left| self.join(left, layout.clone(), right))
    }

    fn grouped_join(
        &'arena self, left: LayoutFragment<'arena>, before_boundary: RcDoc<'arena>,
        right: LayoutFragment<'arena>, continuation_indent: isize,
    ) -> LayoutFragment<'arena> {
        let intent = BoundaryIntent::between(left.anchors.last, right.anchors.first);
        let intention = intent.resolve(self.arena);
        let anchors = LayoutAnchors { first: left.anchors.first, last: right.anchors.last };
        let preserve_break = self.options.layout_intentions == LayoutIntentions::Preserve
            && intention.is_some_and(BreakIntent::requires_line_break);
        let separator =
            if preserve_break { self.mandatory_line_break(intention) } else { RcDoc::line() };
        let document = left
            .document
            .append(before_boundary)
            .append(separator.append(right.document).nest(continuation_indent));
        LayoutFragment {
            document: if preserve_break { document } else { document.group() },
            anchors,
        }
    }

    /// Separate a multiline binder head from the token that introduces its
    /// scope, while retaining the compact single-line form when it fits.
    fn scoped_join(
        &'arena self, head: LayoutFragment<'arena>, separator: &'static str,
        body: LayoutFragment<'arena>, continuation_indent: isize,
    ) -> LayoutFragment<'arena> {
        self.grouped_join(
            head.map_document(|document| self.append_aligned_separator(document, separator)),
            RcDoc::nil(),
            body,
            continuation_indent,
        )
    }

    /// Append a separator to a one-line constituent, or align it with the
    /// enclosing construct when that constituent renders across lines.
    fn append_aligned_separator(
        &self, document: RcDoc<'arena>, separator: &'static str,
    ) -> RcDoc<'arena> {
        let joined = document.clone().append(RcDoc::space()).append(RcDoc::text(separator));
        let broken = document.append(RcDoc::hardline()).append(RcDoc::text(separator));
        self.single_line_or(joined, broken)
    }

    /// Put a separator before the right-hand constituent when the left-hand
    /// constituent is multiline, keeping the right-hand constituent on the
    /// separator's line whenever it still fits.
    fn staged_join(
        &'arena self, left: LayoutFragment<'arena>, right: LayoutFragment<'arena>,
        boundary: StagedBoundary,
    ) -> LayoutFragment<'arena> {
        let separator = boundary.marker();
        let (inline_layout, aligned_layout, aligned_separator_indent) = match boundary {
            | StagedBoundary::Annotation => {
                (BoundaryLayout::aligned(""), BoundaryLayout::nested("", self.indent()), 0)
            }
            | StagedBoundary::BindingType => (
                BoundaryLayout::nested("", self.indent()),
                BoundaryLayout::nested("", self.indent()),
                0,
            ),
            | StagedBoundary::Definition => (
                BoundaryLayout::aligned(""),
                BoundaryLayout::hanging("", self.indent()),
                -self.indent(),
            ),
        };
        let intent = BoundaryIntent::between(left.anchors.last, right.anchors.first);
        let anchors = LayoutAnchors { first: left.anchors.first, last: right.anchors.last };
        let inline_head =
            left.document.clone().append(RcDoc::space()).append(RcDoc::text(separator));
        let inline = self.single_line(inline_head).append(self.layout_boundary(
            intent,
            inline_layout,
            right.document.clone(),
        ));
        // An ordinary entity-to-entity break is satisfied by moving the
        // separator. A blank line still belongs between it and the right side.
        let aligned = left.document.append(
            RcDoc::hardline()
                .append(RcDoc::text(separator))
                .append(self.layout_boundary(
                    intent.blank_line_only(),
                    aligned_layout,
                    right.document,
                ))
                .nest(aligned_separator_indent),
        );
        let document = self.flexible(inline, aligned);
        LayoutFragment { document, anchors }
    }

    fn grouped_separated(
        &'arena self, items: Vec<LayoutFragment<'arena>>, separator: &'static str,
    ) -> Option<LayoutFragment<'arena>> {
        items.into_iter().rev().reduce(|right, left| {
            self.grouped_join(
                left,
                if separator.is_empty() { RcDoc::nil() } else { RcDoc::text(separator) },
                right,
                0,
            )
        })
    }

    fn infix_chain(&'arena self, root: TermId, operator: InfixOperator) -> RcDoc<'arena> {
        let mut operands = Vec::new();
        let mut boundaries = Vec::new();
        let mut current = root;

        loop {
            let (left, right) = operator
                .split(&self.arena.terms[&current])
                .expect("infix chains start and continue with their selected operator");
            let mut document = self.term_through(left, operator.left_precedence());
            let mut starts_with_comment =
                !self.arena.trivia.leading_comments(left.into()).is_empty();
            if current != root {
                starts_with_comment |=
                    !self.arena.trivia.leading_comments(current.into()).is_empty();
                document = self.with_leading_comments(current.into(), document);
            }
            operands.push(InfixOperand { document, starts_with_comment });
            boundaries.push(BoundaryIntent::between(left, right).resolve(self.arena));

            if operator.split(&self.arena.terms[&right]).is_some() {
                current = right;
                continue;
            }

            operands.push(InfixOperand {
                document: self.term_through(right, operator.right_precedence()),
                starts_with_comment: !self.arena.trivia.leading_comments(right.into()).is_empty(),
            });
            break;
        }

        let symbol = operator.symbol();
        let mut joined = operands[0].document.clone();
        for operand in &operands[1..] {
            joined = joined
                .append(RcDoc::space())
                .append(RcDoc::text(symbol))
                .append(RcDoc::space())
                .append(operand.document.clone());
        }

        let symbol_width = isize::try_from(symbol.len()).unwrap_or(isize::MAX);
        let hanging_indent = self.indent().max(symbol_width.saturating_add(1));
        let hanging_indent_usize = usize::try_from(hanging_indent).unwrap_or_default();
        let operator_padding =
            usize::try_from(hanging_indent.saturating_sub(symbol_width)).unwrap_or_default();
        let mut table = operands[0].document.clone().nest(hanging_indent);
        for (operand, intention) in operands[1..].iter().zip(boundaries.iter().copied()) {
            let continuation = operand.document.clone().nest(hanging_indent);
            let line_break = self.mandatory_line_break(intention);
            let inline = RcDoc::space()
                .append(RcDoc::text(symbol))
                .append(RcDoc::space())
                .append(continuation.clone());
            let broken = if operand.starts_with_comment {
                // Keep the operator trailing at a commented boundary so the
                // comment remains aligned with the operand it annotates.
                RcDoc::space()
                    .append(RcDoc::text(symbol))
                    .append(line_break)
                    .append(RcDoc::text(" ".repeat(hanging_indent_usize)))
                    .append(continuation)
            } else {
                line_break
                    .append(RcDoc::text(symbol))
                    .append(RcDoc::text(" ".repeat(operator_padding)))
                    .append(continuation)
            };
            let boundary = if self.options.layout_intentions == LayoutIntentions::Preserve
                && intention.is_some_and(BreakIntent::requires_line_break)
            {
                broken
            } else {
                self.flexible(inline, broken)
            };
            table = table.append(boundary);
        }
        let begins_after_same_line_comment = self
            .arena
            .trivia
            .leading_comments(root.into())
            .last()
            .is_some_and(|comment| comment.separation_after() == LineSeparation::SameLine);
        let hanging = if begins_after_same_line_comment {
            Self::hang_before_current_column(table, hanging_indent_usize)
        } else {
            self.at_line_start(RcDoc::text(" ".repeat(hanging_indent_usize)).append(table))
        };

        let preserves_break = self.options.layout_intentions == LayoutIntentions::Preserve
            && boundaries.iter().flatten().any(|intention| intention.requires_line_break());
        if preserves_break {
            return hanging;
        }

        self.flexible(joined, hanging)
    }

    fn hang_before_current_column(document: RcDoc<'arena>, hanging_indent: usize) -> RcDoc<'arena> {
        DOC_ALLOCATOR
            .column(move |column| {
                let document = document.clone();
                DOC_ALLOCATOR
                    .nesting(move |nesting| {
                        let target = column.saturating_sub(hanging_indent);
                        let target = isize::try_from(target).unwrap_or(isize::MAX);
                        let nesting = isize::try_from(nesting).unwrap_or(isize::MAX);
                        document.clone().nest(target.saturating_sub(nesting))
                    })
                    .into_doc()
            })
            .into_doc()
    }

    fn single_line_or(
        &self, single_line: RcDoc<'arena>, multiline: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        let flat = single_line.clone();
        self.single_line(single_line).union(multiline).flat_alt(flat)
    }

    /// Restrict a candidate to the renderer's flat mode. An enclosing union
    /// can then select an expanded alternative without a separate fit pass.
    fn single_line(&self, document: RcDoc<'arena>) -> RcDoc<'arena> {
        document.append(RcDoc::fail().flat_alt(RcDoc::nil())).group()
    }

    fn separated_group_layout(
        &'arena self, items: &[LayoutFragment<'arena>], separator: &'static str,
    ) -> GroupLayout<'arena> {
        let first = items.first().expect("grouped sequences are nonempty");
        let boundary_layout = BoundaryLayout::aligned(separator);

        let compact = items.iter().skip(1).fold(first.document.clone(), |document, item| {
            document.append(boundary_layout.place(BoundaryPlacement::Joined, item.document.clone()))
        });

        let (completed_rows, current_row, retains_break) = items.windows(2).fold(
            (RcDoc::nil(), first.document.clone(), false),
            |(completed, current, retains_break), pair| {
                let [left, right] = pair else { unreachable!("windows contain two items") };
                let intent = BoundaryIntent::between(left.anchors.last, right.anchors.first);
                let placement = self.retained_placement(intent);
                match placement {
                    | BoundaryPlacement::Joined => (
                        completed,
                        current.append(boundary_layout.place(placement, right.document.clone())),
                        retains_break,
                    ),
                    | BoundaryPlacement::Broken | BoundaryPlacement::BlankLine => (
                        completed
                            .append(self.single_line(current))
                            .append(boundary_layout.place(placement, RcDoc::nil())),
                        right.document.clone(),
                        true,
                    ),
                }
            },
        );
        let retained = completed_rows.append(self.single_line(current_row));

        let expanded = items.windows(2).fold(first.document.clone(), |document, pair| {
            let [left, right] = pair else { unreachable!("windows contain two items") };
            let intent = BoundaryIntent::between(left.anchors.last, right.anchors.first);
            document.append(
                boundary_layout.place(self.expanded_placement(intent), right.document.clone()),
            )
        });

        GroupLayout { compact, retained, expanded, retains_break }
    }

    fn select_group_layout(&self, layout: GroupLayout<'arena>) -> RcDoc<'arena> {
        let GroupLayout { compact, retained, expanded, retains_break } = layout;
        let preferred = if retains_break { retained } else { self.single_line(compact.clone()) };
        let selected = preferred.union(expanded);
        if retains_break { selected } else { selected.flat_alt(compact) }
    }

    fn delimited(
        &'arena self, entity: Option<EntityId>, open: &'static str,
        items: Vec<LayoutFragment<'arena>>, separator: &'static str, close: &'static str,
    ) -> RcDoc<'arena> {
        self.delimited_with_spacing(entity, open, items, separator, close, DelimiterSpacing::Tight)
    }

    fn delimited_with_spacing(
        &'arena self, entity: Option<EntityId>, open: &'static str,
        items: Vec<LayoutFragment<'arena>>, separator: &'static str, close: &'static str,
        spacing: DelimiterSpacing,
    ) -> RcDoc<'arena> {
        if items.is_empty() {
            return RcDoc::text(open).append(RcDoc::text(close));
        }
        let anchors = LayoutAnchors {
            first: items.first().expect("delimited items are nonempty").anchors.first,
            last: items.last().expect("delimited items are nonempty").anchors.last,
        };
        let item_layout = self.separated_group_layout(&items, separator);
        let after_open = entity.map_or(BoundaryIntent::Canonical, |entity| {
            BoundaryIntent::after_start(entity, anchors.first)
        });
        let before_close = entity.map_or(BoundaryIntent::Canonical, |entity| {
            BoundaryIntent::before_end(anchors.last, entity)
        });
        let contents_layout = spacing.boundary(self.indent(), self.indent());
        let close_layout = spacing.boundary(0, 0);
        let retained_after_open = self.retained_placement(after_open);
        let retained_before_close = self.retained_placement(before_close);
        let GroupLayout {
            compact: compact_items,
            retained: retained_items,
            expanded: expanded_items,
            retains_break: items_retain_break,
        } = item_layout;

        let compact = RcDoc::text(open)
            .append(contents_layout.place(BoundaryPlacement::Joined, compact_items))
            .append(close_layout.place(BoundaryPlacement::Joined, RcDoc::text(close)));
        let retained = RcDoc::text(open)
            .append(contents_layout.place(retained_after_open, retained_items))
            .append(close_layout.place(retained_before_close, RcDoc::text(close)));
        let expanded = RcDoc::text(open)
            .append(contents_layout.place(self.expanded_placement(after_open), expanded_items))
            .append(close_layout.place(self.expanded_placement(before_close), RcDoc::text(close)));
        self.select_group_layout(GroupLayout {
            compact,
            retained,
            expanded,
            retains_break: items_retain_break
                || retained_after_open != BoundaryPlacement::Joined
                || retained_before_close != BoundaryPlacement::Joined,
        })
    }

    fn annotation(
        &'arena self, entity: EntityId, term: LayoutFragment<'arena>, ty: TermId,
        parenthesized: bool,
    ) -> RcDoc<'arena> {
        let ty = self.term_fragment(ty);
        let annotation = self.staged_join(term, ty, StagedBoundary::Annotation);
        if parenthesized {
            self.delimited(Some(entity), "(", vec![annotation], ",", ")")
        } else {
            annotation.document
        }
    }

    /// Elide a transparent group unless its delimiters carry a preserved
    /// multiline layout boundary.
    fn should_elide_parentheses(&self, entity: EntityId, inner: EntityId) -> bool {
        let carries_break = self
            .arena
            .intentions
            .at(LayoutBoundary::after_start(entity, inner))
            .is_some_and(BreakIntent::requires_line_break)
            || self
                .arena
                .intentions
                .at(LayoutBoundary::before_end(inner, entity))
                .is_some_and(BreakIntent::requires_line_break);
        self.options.parentheses == Parentheses::Minimal
            && !(self.options.layout_intentions == LayoutIntentions::Preserve && carries_break)
    }

    /// Applications already own a compact-or-hanging layout boundary, so a
    /// singleton wrapper does not contribute an additional grouping choice.
    fn term_layout_subsumes_group(&self, term: TermId) -> bool {
        match &self.arena.terms[&term] {
            | Term::SourceBoundary(SourceBoundary(inner)) => {
                self.term_layout_subsumes_group(*inner)
            }
            | Term::Paren(Paren(terms)) => match terms.as_slice() {
                | [inner] => self.term_layout_subsumes_group(*inner),
                | _ => false,
            },
            | Term::App(_) => true,
            | _ => false,
        }
    }

    fn pattern(&'arena self, pattern: PatId) -> RcDoc<'arena> {
        self.pattern_with_requirement(pattern, PatternRequirement::Pattern)
    }

    fn annotated_pattern(&'arena self, pattern: PatId) -> RcDoc<'arena> {
        self.pattern_with_requirement(pattern, PatternRequirement::Annotated)
    }

    fn pattern_with_requirement(
        &'arena self, pattern: PatId, requirement: PatternRequirement,
    ) -> RcDoc<'arena> {
        let document = match &self.arena.pats[&pattern] {
            | Pattern::Ann(Ann { tm, ty }) => self.annotation(
                pattern.into(),
                LayoutFragment::entity((*tm).into(), self.pattern(*tm)),
                *ty,
                requirement == PatternRequirement::Pattern,
            ),
            | Pattern::Manifest(ManifestPattern { binder, definition }) => {
                let manifest = self.join(
                    LayoutFragment::entity((*binder).into(), self.annotated_pattern(*binder)),
                    BoundaryLayout::aligned(" as"),
                    self.term_fragment(*definition),
                );
                self.delimited(Some(pattern.into()), "(", vec![manifest], ",", ")")
            }
            | Pattern::Hole(_) => RcDoc::text("_"),
            | Pattern::Var(definition) => self.definition(*definition),
            | Pattern::Named(Named(field, inner)) => self.named_pattern(pattern, field, *inner),
            | Pattern::Ctor(Ctor(name, inner)) => {
                self.constructor(name).append(self.pattern_constructor_argument(*inner))
            }
            | Pattern::Project(ProjectionPattern(field, inner)) => {
                self.projection_pattern(pattern, field, *inner)
            }
            | Pattern::Alias(Alias(patterns)) => self.delimited(
                Some(pattern.into()),
                "(",
                patterns
                    .iter()
                    .map(|pattern| {
                        LayoutFragment::entity((*pattern).into(), self.annotated_pattern(*pattern))
                    })
                    .collect(),
                ";",
                ")",
            ),
            | Pattern::Paren(Paren(patterns)) => match patterns.as_slice() {
                | [inner]
                    if self.should_elide_parentheses(pattern.into(), (*inner).into())
                        && self.grammar.accepts_pattern(requirement, *inner) =>
                {
                    self.pattern_with_requirement(*inner, requirement)
                }
                | _ => self.delimited(
                    Some(pattern.into()),
                    "(",
                    patterns
                        .iter()
                        .map(|pattern| {
                            LayoutFragment::entity(
                                (*pattern).into(),
                                self.annotated_pattern(*pattern),
                            )
                        })
                        .collect(),
                    ",",
                    ")",
                ),
            },
        };
        let document = if self.grammar.accepts_pattern(requirement, pattern) {
            document
        } else {
            self.delimited(
                None,
                "(",
                vec![LayoutFragment::entity(pattern.into(), document)],
                ",",
                ")",
            )
        };
        self.with_leading_comments(pattern.into(), document)
    }

    fn copattern(&'arena self, pattern: CoPatId) -> RcDoc<'arena> {
        let document = match &self.arena.copats[&pattern] {
            | CoPattern::Pat(pattern) => self.pattern(*pattern),
            | CoPattern::Dtor(name) => self.destructor(name),
            | CoPattern::App(_) => self.copattern_application(pattern).nest(self.indent()),
        };
        self.with_leading_comments(pattern.into(), document)
    }

    fn copattern_application(&'arena self, pattern: CoPatId) -> RcDoc<'arena> {
        let CoPattern::App(Appli(patterns)) = &self.arena.copats[&pattern] else {
            unreachable!("copattern applications are selected before rendering")
        };
        self.grouped_separated(
            patterns
                .iter()
                .map(|pattern| {
                    let document = match self.arena.copats[pattern] {
                        | CoPattern::App(_) => self.with_leading_comments(
                            (*pattern).into(),
                            self.copattern_application(*pattern),
                        ),
                        | _ => self.copattern(*pattern),
                    };
                    LayoutFragment::entity((*pattern).into(), document)
                })
                .collect(),
            "",
        )
        .expect("copattern applications contain at least two patterns")
        .document
    }

    fn term(&'arena self, term: TermId) -> RcDoc<'arena> {
        self.term_with_requirement(term, TermRequirement::Any)
    }

    fn annotated_term(&'arena self, term: TermId) -> RcDoc<'arena> {
        self.term_with_requirement(term, TermRequirement::Annotated)
    }

    fn term_through(&'arena self, term: TermId, precedence: TermPrecedence) -> RcDoc<'arena> {
        self.term_with_requirement(term, TermRequirement::Through(precedence))
    }

    fn term_fragment(&'arena self, term: TermId) -> LayoutFragment<'arena> {
        self.term_fragment_with_requirement(term, TermRequirement::Any)
    }

    fn annotated_term_fragment(&'arena self, term: TermId) -> LayoutFragment<'arena> {
        self.term_fragment_with_requirement(term, TermRequirement::Annotated)
    }

    fn term_through_fragment(
        &'arena self, term: TermId, precedence: TermPrecedence,
    ) -> LayoutFragment<'arena> {
        self.term_fragment_with_requirement(term, TermRequirement::Through(precedence))
    }

    fn term_fragment_with_requirement(
        &'arena self, term: TermId, requirement: TermRequirement,
    ) -> LayoutFragment<'arena> {
        LayoutFragment::entity(term.into(), self.term_with_requirement(term, requirement))
    }

    fn term_with_requirement(
        &'arena self, term: TermId, requirement: TermRequirement,
    ) -> RcDoc<'arena> {
        if let Term::SourceBoundary(SourceBoundary(inner)) = &self.arena.terms[&term] {
            let document = self.term_with_requirement(*inner, requirement);
            return self.with_leading_comments(term.into(), document);
        }
        let document = match &self.arena.terms[&term] {
            | Term::Meta(MetaT(meta, inner)) => RcDoc::text("@[")
                .append(RcDoc::text(meta.to_string()))
                .append(self.fragment_boundary(
                    BoundaryIntent::after_start(term, *inner),
                    BoundaryLayout::aligned("]"),
                    self.term_through_fragment(*inner, TermPrecedence::Binder),
                )),
            | Term::SourceBoundary(_) => unreachable!("source boundaries return before rendering"),
            | Term::Ann(Ann { tm, ty }) => self.annotation(
                term.into(),
                self.term_fragment(*tm),
                *ty,
                requirement != TermRequirement::Annotated,
            ),
            | Term::Hole(_) => RcDoc::text("_"),
            | Term::Var(name) => self.variable(name),
            | Term::Named(Named(field, inner)) => self.named_term(term, field, *inner),
            | Term::Label(Label(field, inner)) => self.field(field).append(self.fragment_boundary(
                BoundaryIntent::after_start(term, *inner),
                BoundaryLayout::aligned(" ::"),
                self.annotated_term_fragment(*inner),
            )),
            | Term::Paren(Paren(terms)) => match terms.as_slice() {
                | [inner]
                    if (self.should_elide_parentheses(term.into(), (*inner).into())
                        || self.term_layout_subsumes_group(*inner))
                        && self.grammar.accepts_term(requirement, *inner) =>
                {
                    self.term_with_requirement(*inner, requirement)
                }
                | _ => self.delimited(
                    Some(term.into()),
                    "(",
                    terms.iter().map(|term| self.annotated_term_fragment(*term)).collect(),
                    ",",
                    ")",
                ),
            },
            | Term::Abs(Abs(pattern, body)) => {
                self.scoped_join(
                    LayoutFragment::entity(
                        (*pattern).into(),
                        self.prefixed(
                            term,
                            "fn",
                            BoundaryLayout::hanging("", self.indent()),
                            LayoutFragment::entity((*pattern).into(), self.copattern(*pattern)),
                        ),
                    ),
                    "=>",
                    self.term_through_fragment(*body, TermPrecedence::Binder),
                    self.indent(),
                )
                .document
            }
            | Term::App(Appli(terms)) => self.application(terms),
            | Term::KontCall(KontCall { body, tail }) => RcDoc::text("do~")
                .append(self.fragment_boundary(
                    BoundaryIntent::after_start(term, *body),
                    BoundaryLayout::hanging("", self.indent()),
                    self.term_through_fragment(*body, TermPrecedence::Binder),
                ))
                .append(RcDoc::text(";"))
                .append(self.sequence_tail((*body).into(), *tail)),
            | Term::Fix(Fix(pattern, body)) => {
                self.scoped_join(
                    LayoutFragment::entity(
                        (*pattern).into(),
                        self.prefixed(
                            term,
                            "fix",
                            BoundaryLayout::hanging("", self.indent()),
                            LayoutFragment::entity((*pattern).into(), self.pattern(*pattern)),
                        ),
                    ),
                    "=>",
                    self.term_through_fragment(*body, TermPrecedence::Binder),
                    self.indent(),
                )
                .document
            }
            | Term::Pi(Pi(pattern, body)) => self.quantifier(term, "pi", *pattern, *body),
            | Term::Forall(Forall(pattern, body)) => {
                self.quantifier(term, "forall", *pattern, *body)
            }
            | Term::Arrow(_) => self.infix_chain(term, InfixOperator::Arrow),
            | Term::Sigma(Sigma(pattern, body)) => self.quantifier(term, "sigma", *pattern, *body),
            | Term::Exists(exists) => self.exists(term, exists),
            | Term::Prod(_) => self.infix_chain(term, InfixOperator::Product),
            | Term::Thunk(Thunk(body)) => self.delimited_with_spacing(
                Some(term.into()),
                "{",
                vec![self.term_fragment(*body)],
                ",",
                "}",
                DelimiterSpacing::Spaced,
            ),
            | Term::Force(Force(body)) => self.prefixed(
                term,
                "!",
                BoundaryLayout::hanging("", self.indent()),
                self.term_through_fragment(*body, TermPrecedence::Atom),
            ),
            | Term::Ret(Return(body)) => self.prefixed(
                term,
                "ret",
                BoundaryLayout::hanging("", self.indent()),
                self.term_through_fragment(*body, TermPrecedence::Atom),
            ),
            | Term::Do(Bind { binder, bindee, tail }) => self
                .prefixed(
                    term,
                    "do",
                    BoundaryLayout::aligned(""),
                    LayoutFragment::entity((*binder).into(), self.pattern(*binder)),
                )
                .append(self.fragment_boundary(
                    BoundaryIntent::between(*binder, *bindee),
                    BoundaryLayout::hanging(" <-", self.indent()),
                    self.term_through_fragment(*bindee, TermPrecedence::Binder),
                ))
                .append(RcDoc::text(";"))
                .append(self.sequence_tail((*bindee).into(), *tail)),
            | Term::Let(GenLet { binding, tail }) => RcDoc::text("let")
                .append(self.placed_binding(term, binding, Placement::In))
                .append(self.sequence_tail(binding.bindee.into(), *tail)),
            | Term::Param(Param { binder, placement, tail }) => self
                .prefixed(
                    term,
                    "param",
                    BoundaryLayout::aligned(""),
                    LayoutFragment::entity((*binder).into(), self.annotated_pattern(*binder)),
                )
                .append(RcDoc::text(" "))
                .append(self.placement(*placement))
                .append(self.sequence_tail((*binder).into(), *tail)),
            | Term::ContextBind(ContextBind { mode, binding, placement, tail }) => {
                let keyword = match mode {
                    | DefinitionMode::Transparent => "let",
                    | DefinitionMode::Nominal => "def",
                };
                RcDoc::text(keyword)
                    .append(self.placed_binding(term, binding, *placement))
                    .append(self.sequence_tail(binding.bindee.into(), *tail))
            }
            | Term::Block(Block(body)) => self.block("begin", *body, "end"),
            | Term::MoBlock(MoBlock(body)) => self.block("monadic", *body, "end"),
            | Term::Data(Data { arms }) => self.data(arms),
            | Term::CoData(CoData { arms }) => self.codata(arms),
            | Term::Ctor(Ctor(name, body)) => {
                self.constructor(name).append(self.term_constructor_argument(*body))
            }
            | Term::Match(Match { scrut, arms }) => self.matcher(term, *scrut, arms),
            | Term::CoMatch(CoMatchParam { arms }) => self.comatcher(arms),
            | Term::Dtor(Dtor(body, name)) => self
                .term_through(*body, TermPrecedence::Application)
                .append(RcDoc::text(" "))
                .append(self.destructor(name)),
            | Term::Proj(Proj(body, field)) => self
                .term_through(*body, TermPrecedence::Projection)
                .append(RcDoc::text("/"))
                .append(self.field(field)),
            | Term::Lit(literal) => self.literal(literal),
        };
        let document = if self.grammar.accepts_term(requirement, term) {
            document
        } else {
            self.delimited(None, "(", vec![LayoutFragment::entity(term.into(), document)], ",", ")")
        };
        self.with_leading_comments(term.into(), document)
    }

    fn application(&'arena self, terms: &[TermId]) -> RcDoc<'arena> {
        let [head, first_argument, arguments @ ..] = terms else {
            unreachable!("application terms always contain a function and an argument")
        };
        let head = LayoutFragment::entity(
            (*head).into(),
            self.term_through(*head, TermPrecedence::Application),
        );
        let arguments = self
            .separated(
                std::iter::once(first_argument)
                    .chain(arguments)
                    .map(|argument| {
                        self.term_through_fragment(*argument, TermPrecedence::Projection)
                    })
                    .collect(),
                BoundaryLayout::hanging("", self.indent()),
            )
            .expect("applications contain at least one argument");
        self.join(head, BoundaryLayout::hanging("", self.indent()), arguments).document
    }

    fn named_term(&'arena self, term: TermId, field: &FieldName, inner: TermId) -> RcDoc<'arena> {
        let payload = self.punning.term_payload(field, inner);
        match payload {
            | Some(PunnedTermPayload::Variable) => RcDoc::text("= ").append(self.field(field)),
            | Some(PunnedTermPayload::Annotated { variable, classifier }) => {
                RcDoc::text("= ").append(self.field(field)).append(self.fragment_boundary(
                    BoundaryIntent::between(variable, classifier),
                    BoundaryLayout::aligned(" :"),
                    self.term_fragment(classifier),
                ))
            }
            | _ => self.field(field).append(self.fragment_boundary(
                BoundaryIntent::after_start(term, inner),
                BoundaryLayout::aligned(" ="),
                self.annotated_term_fragment(inner),
            )),
        }
    }

    fn term_constructor_argument(&'arena self, body: TermId) -> RcDoc<'arena> {
        match &self.arena.terms[&body] {
            | Term::Paren(Paren(terms)) => self.with_leading_comments(
                body.into(),
                self.delimited(
                    Some(body.into()),
                    "(",
                    terms.iter().map(|term| self.annotated_term_fragment(*term)).collect(),
                    ",",
                    ")",
                ),
            ),
            | _ => self.delimited(None, "(", vec![self.annotated_term_fragment(body)], ",", ")"),
        }
    }

    fn pattern_constructor_argument(&'arena self, body: PatId) -> RcDoc<'arena> {
        match &self.arena.pats[&body] {
            | Pattern::Alias(_) | Pattern::Manifest(_) => self.annotated_pattern(body),
            | Pattern::Paren(Paren(patterns)) => match patterns.as_slice() {
                | [inner] if self.should_elide_parentheses(body.into(), (*inner).into()) => self
                    .with_leading_comments(body.into(), self.pattern_constructor_argument(*inner)),
                | _ => self.with_leading_comments(
                    body.into(),
                    self.delimited(
                        Some(body.into()),
                        "(",
                        patterns
                            .iter()
                            .map(|pattern| {
                                LayoutFragment::entity(
                                    (*pattern).into(),
                                    self.annotated_pattern(*pattern),
                                )
                            })
                            .collect(),
                        ",",
                        ")",
                    ),
                ),
            },
            | _ => self.delimited(
                None,
                "(",
                vec![LayoutFragment::entity(body.into(), self.annotated_pattern(body))],
                ",",
                ")",
            ),
        }
    }

    fn named_pattern(
        &'arena self, pattern: PatId, field: &FieldName, inner: PatId,
    ) -> RcDoc<'arena> {
        match self.punning.pattern_payload(field, inner) {
            | Some(PunnedPatternPayload::Variable) => RcDoc::text("= ").append(self.field(field)),
            | Some(PunnedPatternPayload::Annotated { variable, classifier }) => {
                RcDoc::text("= ").append(self.field(field)).append(self.fragment_boundary(
                    BoundaryIntent::between(variable, classifier),
                    BoundaryLayout::aligned(" :"),
                    self.term_fragment(classifier),
                ))
            }
            | None => self.field(field).append(self.fragment_boundary(
                BoundaryIntent::after_start(pattern, inner),
                BoundaryLayout::aligned(" ="),
                LayoutFragment::entity(inner.into(), self.annotated_pattern(inner)),
            )),
        }
    }

    fn projection_pattern(
        &'arena self, pattern: PatId, field: &FieldName, inner: PatId,
    ) -> RcDoc<'arena> {
        match self.punning.pattern_payload(field, inner) {
            | Some(PunnedPatternPayload::Variable) => RcDoc::text("/").append(self.field(field)),
            | Some(PunnedPatternPayload::Annotated { variable, classifier }) => {
                RcDoc::text("/").append(self.field(field)).append(self.fragment_boundary(
                    BoundaryIntent::between(variable, classifier),
                    BoundaryLayout::aligned(" :"),
                    self.term_fragment(classifier),
                ))
            }
            | None => RcDoc::text("/").append(self.field(field)).append(self.fragment_boundary(
                BoundaryIntent::after_start(pattern, inner),
                BoundaryLayout::aligned(" ="),
                LayoutFragment::entity(inner.into(), self.annotated_pattern(inner)),
            )),
        }
    }

    fn quantifier(
        &'arena self, term: TermId, keyword: &'static str, pattern: CoPatId, body: TermId,
    ) -> RcDoc<'arena> {
        self.scoped_join(
            LayoutFragment::entity(
                pattern.into(),
                self.prefixed(
                    term,
                    keyword,
                    BoundaryLayout::hanging("", self.indent()),
                    LayoutFragment::entity(pattern.into(), self.copattern(pattern)),
                ),
            ),
            ".",
            self.term_through_fragment(body, TermPrecedence::Quantifier),
            self.indent(),
        )
        .document
    }

    fn exists(&'arena self, term: TermId, exists: &Exists) -> RcDoc<'arena> {
        let Exists { parameters, body } = exists;
        let Some((first, rest)) = parameters.split_first() else {
            unreachable!("the parser requires at least one existential parameter")
        };
        let parameters = self
            .grouped_separated(
                std::iter::once(self.existential_parameter(first))
                    .chain(rest.iter().map(|parameter| self.existential_parameter(parameter)))
                    .collect(),
                "",
            )
            .expect("existentials contain at least one parameter");
        let head = LayoutFragment {
            document: RcDoc::text("exists").append(self.layout_boundary(
                BoundaryIntent::before_existential_parameter(term, first.binder),
                BoundaryLayout::nested("", self.indent()),
                parameters.document,
            )),
            anchors: parameters.anchors,
        };
        self.scoped_join(head, ".", self.term_fragment(*body), self.indent()).document
    }

    fn existential_parameter(
        &'arena self, parameter: &ExistentialParameter,
    ) -> LayoutFragment<'arena> {
        let annotations = parameter.annotations.iter().map(|annotation| {
            RcDoc::text("@[")
                .append(RcDoc::text(annotation.inner.to_string()))
                .append(RcDoc::text("]"))
        });
        let binder = self
            .manifest_parameter_view(parameter.binder)
            .map(|view| self.manifest_parameter(view, parameter.binder))
            .unwrap_or_else(|| match self.arena.pats[&parameter.binder] {
                | Pattern::Ann(_) | Pattern::Manifest(_) | Pattern::Paren(_) => {
                    self.pattern(parameter.binder)
                }
                | _ => self.delimited(
                    None,
                    "(",
                    vec![LayoutFragment::entity(
                        parameter.binder.into(),
                        self.annotated_pattern(parameter.binder),
                    )],
                    ",",
                    ")",
                ),
            });
        LayoutFragment::entity(
            parameter.binder.into(),
            RcDoc::intersperse(annotations.chain(std::iter::once(binder)), RcDoc::line()).group(),
        )
    }

    fn manifest_parameter_view(
        &'arena self, pattern: PatId,
    ) -> Option<ManifestParameterView<'arena>> {
        let mut fields = Vec::new();
        let mut current = pattern;
        loop {
            current = self.transparent_pattern_group(current);
            if current != pattern && !self.arena.trivia.leading_comments(current.into()).is_empty()
            {
                return None;
            }
            match &self.arena.pats[&current] {
                | Pattern::Named(Named(field, inner)) => {
                    fields.push((current, field));
                    current = *inner;
                }
                | Pattern::Ann(Ann { tm, ty }) => {
                    let manifest = self.transparent_pattern_group(*tm);
                    let Pattern::Manifest(ManifestPattern { binder, definition }) =
                        self.arena.pats[&manifest]
                    else {
                        return None;
                    };
                    if manifest != pattern
                        && !self.arena.trivia.leading_comments(manifest.into()).is_empty()
                    {
                        return None;
                    }
                    return Some(ManifestParameterView {
                        fields,
                        binder,
                        definition,
                        classifier: Some(*ty),
                    });
                }
                | Pattern::Manifest(ManifestPattern { binder, definition }) => {
                    return Some(ManifestParameterView {
                        fields,
                        binder: *binder,
                        definition: *definition,
                        classifier: None,
                    });
                }
                | _ => return None,
            }
        }
    }

    fn transparent_pattern_group(&self, pattern: PatId) -> PatId {
        if let Pattern::Paren(Paren(patterns)) = &self.arena.pats[&pattern]
            && let [inner] = patterns.as_slice()
            && self.should_elide_parentheses(pattern.into(), (*inner).into())
            && self.arena.trivia.leading_comments(pattern.into()).is_empty()
        {
            self.transparent_pattern_group(*inner)
        } else {
            pattern
        }
    }

    fn manifest_parameter(
        &'arena self, view: ManifestParameterView<'arena>, entity: PatId,
    ) -> RcDoc<'arena> {
        let ManifestParameterView { fields, binder, definition, classifier } = view;
        let binder = fields.iter().rev().enumerate().fold(
            LayoutFragment::entity(binder.into(), self.annotated_pattern(binder)),
            |inner, (depth, (named, field))| {
                let inner_last = inner.anchors.last;
                let document = if depth == 0 {
                    match self.punning.pattern_payload(field, binder) {
                        | Some(PunnedPatternPayload::Variable) => {
                            RcDoc::text("= ").append(self.field(field))
                        }
                        | Some(PunnedPatternPayload::Annotated { variable, classifier }) => {
                            RcDoc::text("= ").append(self.field(field)).append(
                                self.fragment_boundary(
                                    BoundaryIntent::between(variable, classifier),
                                    BoundaryLayout::aligned(" :"),
                                    self.term_fragment(classifier),
                                ),
                            )
                        }
                        | None => self.field(field).append(self.fragment_boundary(
                            BoundaryIntent::after_start(*named, inner.anchors.first),
                            BoundaryLayout::aligned(" ="),
                            inner,
                        )),
                    }
                } else {
                    self.field(field).append(self.fragment_boundary(
                        BoundaryIntent::after_start(*named, inner.anchors.first),
                        BoundaryLayout::aligned(" ="),
                        inner,
                    ))
                };
                LayoutFragment {
                    document,
                    anchors: LayoutAnchors { first: (*named).into(), last: inner_last },
                }
            },
        );
        let binder =
            self.join(binder, BoundaryLayout::aligned(" as"), self.term_fragment(definition));
        let binder = match classifier {
            | Some(classifier) => {
                self.join(binder, BoundaryLayout::aligned(" :"), self.term_fragment(classifier))
            }
            | None => binder,
        };
        let document = self.delimited(Some(entity.into()), "(", vec![binder], ",", ")");
        self.with_leading_comments(entity.into(), document)
    }

    fn placed_binding(
        &'arena self, enclosing: TermId, binding: &GenBind<TermId>, placement: Placement,
    ) -> RcDoc<'arena> {
        let GenBind { fix, comp, binder, params, ty, bindee } = binding;
        let modifiers = [(*comp).then_some("!"), (*fix).then_some("fix")]
            .into_iter()
            .flatten()
            .map(RcDoc::text);
        let head_last = params.as_ref().map_or_else(|| (*binder).into(), |params| (*params).into());
        let head = modifiers
            .chain(std::iter::once(self.pattern(*binder)))
            .chain(params.iter().map(|params| self.copattern(*params)));
        let head = LayoutFragment {
            document: RcDoc::intersperse(head, RcDoc::space()),
            anchors: LayoutAnchors { first: (*binder).into(), last: head_last },
        };
        let head_anchors = head.anchors;
        let head = LayoutFragment {
            document: self.fragment_boundary(
                BoundaryIntent::after_start(enclosing, head.anchors.first),
                BoundaryLayout::aligned(""),
                head,
            ),
            anchors: head_anchors,
        };
        let placement = self.placement(placement);
        let bindee = self
            .term_fragment(*bindee)
            .map_document(|document| self.bindee_with_placement(document, placement));
        match ty {
            | Some(ty) => {
                let assignment =
                    self.staged_join(self.term_fragment(*ty), bindee, StagedBoundary::Definition);
                self.staged_join(head, assignment, StagedBoundary::BindingType).document
            }
            | None => {
                self.join(head, BoundaryLayout::hanging(" =", self.indent()), bindee).document
            }
        }
    }

    fn sequence_tail(&'arena self, before: EntityId, tail: TermId) -> RcDoc<'arena> {
        self.mandatory_line_break(BoundaryIntent::between(before, tail).resolve(self.arena))
            .append(self.term_through(tail, TermPrecedence::Binder))
    }

    fn bindee_with_placement(
        &'arena self, bindee: RcDoc<'arena>, placement: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        let joined =
            self.after_line_start(bindee.clone().append(RcDoc::space()).append(placement.clone()));
        let broken = bindee.append(RcDoc::hardline().append(placement).nest(-self.indent()));
        self.single_line_or(joined, broken)
    }

    fn placement(&self, placement: Placement) -> RcDoc<'arena> {
        RcDoc::text(match placement {
            | Placement::In => "in",
            | Placement::That => "that",
        })
    }

    fn block(
        &'arena self, keyword: &'static str, body: TermId, end: &'static str,
    ) -> RcDoc<'arena> {
        RcDoc::text(keyword)
            .append(RcDoc::hardline().append(self.annotated_term(body)).nest(self.indent()))
            .append(RcDoc::hardline())
            .append(RcDoc::text(end))
    }

    fn arm_block(
        &'arena self, head: RcDoc<'arena>, arms: impl IntoIterator<Item = LayoutFragment<'arena>>,
    ) -> RcDoc<'arena> {
        arms.into_iter()
            .fold(head, |document, arm| {
                document
                    .append(RcDoc::hardline())
                    .append(self.with_before_arm_comments(arm.anchors.first, arm.document))
            })
            .append(RcDoc::hardline())
            .append(RcDoc::text("end"))
    }

    fn data(&'arena self, arms: &[DataArm]) -> RcDoc<'arena> {
        let arms = arms.iter().map(|arm| {
            let document = RcDoc::text("| ").append(self.constructor(&arm.name)).append(
                self.fragment_boundary(
                    BoundaryIntent::after_arm_prefix(arm.param),
                    BoundaryLayout::hanging(" :", self.indent()),
                    self.term_fragment(arm.param),
                ),
            );
            LayoutFragment::entity(arm.param.into(), document)
        });
        self.arm_block(RcDoc::text("data"), arms)
    }

    fn codata(&'arena self, arms: &[CoDataArm]) -> RcDoc<'arena> {
        let arms = arms.iter().map(|arm| {
            let document = RcDoc::text("| ").append(self.destructor(&arm.name));
            let document = match arm.params {
                | Some(params) => document.append(RcDoc::text(" ")).append(self.copattern(params)),
                | None => document,
            };
            let first: EntityId = arm.params.map_or_else(|| arm.out.into(), Into::into);
            let document = document.append(self.fragment_boundary(
                BoundaryIntent::after_arm_prefix(arm.out),
                BoundaryLayout::hanging(" :", self.indent()),
                self.term_fragment(arm.out),
            ));
            LayoutFragment { document, anchors: LayoutAnchors { first, last: arm.out.into() } }
        });
        self.arm_block(RcDoc::text("codata"), arms)
    }

    fn matcher(
        &'arena self, term: TermId, scrutinee: TermId, arms: &[Matcher<PatId, TermId>],
    ) -> RcDoc<'arena> {
        let head = RcDoc::text("match").append(self.fragment_boundary(
            BoundaryIntent::after_start(term, scrutinee),
            BoundaryLayout::hanging("", self.indent()),
            self.term_fragment(scrutinee),
        ));
        let arms = arms.iter().map(|arm| {
            let document =
                RcDoc::text("| ").append(self.pattern(arm.binder)).append(self.fragment_boundary(
                    BoundaryIntent::between(arm.binder, arm.tail),
                    BoundaryLayout::hanging(" =>", self.indent()),
                    self.term_fragment(arm.tail),
                ));
            LayoutFragment {
                document,
                anchors: LayoutAnchors { first: arm.binder.into(), last: arm.tail.into() },
            }
        });
        self.arm_block(head, arms)
    }

    fn comatcher(&'arena self, arms: &[CoMatcherParam]) -> RcDoc<'arena> {
        let arms = arms.iter().map(|arm| {
            let document = RcDoc::text("| ").append(self.copattern(arm.params)).append(
                self.fragment_boundary(
                    BoundaryIntent::between(arm.params, arm.tail),
                    BoundaryLayout::hanging(" =>", self.indent()),
                    self.term_fragment(arm.tail),
                ),
            );
            LayoutFragment {
                document,
                anchors: LayoutAnchors { first: arm.params.into(), last: arm.tail.into() },
            }
        });
        self.arm_block(RcDoc::text("comatch"), arms)
    }

    fn definition(&'arena self, definition: DefId) -> RcDoc<'arena> {
        let document = self.variable(&self.arena.defs[&definition]);
        self.with_leading_comments(definition.into(), document)
    }

    fn variable(&self, name: &VarName) -> RcDoc<'arena> {
        RcDoc::text(name.0.clone())
    }

    fn field(&self, name: &FieldName) -> RcDoc<'arena> {
        RcDoc::text(name.0.clone())
    }

    fn constructor(&self, name: &CtorName) -> RcDoc<'arena> {
        RcDoc::text(name.0.clone())
    }

    fn destructor(&self, name: &DtorName) -> RcDoc<'arena> {
        RcDoc::text(name.0.clone())
    }

    fn literal(&self, literal: &Literal) -> RcDoc<'arena> {
        RcDoc::text(match literal {
            | Literal::Int(value) => format!("{value:?}"),
            | Literal::Float(value) => format!("{value:?}"),
            | Literal::String(value) => format!("{value:?}"),
            | Literal::Char(value) => format!("{value:?}"),
        })
    }
}

impl<'a> Pretty<'a, PrettyFormatter<'a>> for SourceUnit {
    fn pretty(&self, formatter: &'a PrettyFormatter<'a>) -> RcDoc<'a> {
        formatter.term(self.root)
    }
}

impl<'a> Pretty<'a, PrettyFormatter<'a>> for DefId {
    fn pretty(&self, formatter: &'a PrettyFormatter<'a>) -> RcDoc<'a> {
        formatter.definition(*self)
    }
}

impl<'a> Pretty<'a, PrettyFormatter<'a>> for PatId {
    fn pretty(&self, formatter: &'a PrettyFormatter<'a>) -> RcDoc<'a> {
        formatter.pattern(*self)
    }
}

impl<'a> Pretty<'a, PrettyFormatter<'a>> for CoPatId {
    fn pretty(&self, formatter: &'a PrettyFormatter<'a>) -> RcDoc<'a> {
        formatter.copattern(*self)
    }
}

impl<'a> Pretty<'a, PrettyFormatter<'a>> for TermId {
    fn pretty(&self, formatter: &'a PrettyFormatter<'a>) -> RcDoc<'a> {
        formatter.term(*self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitter::{
        SourceUnitDesugarer, arena::BitterArena, fmt::Formatter as BitterFormatter,
    };
    use crate::textual::{Lexer, LexicalTokenKind, LexicalTokens, SourceUnitParser};
    use zydeco_syntax::Ugly;
    use zydeco_utils::pass::CompilerPass;
    use zydeco_utils::span::LocationCtx;

    struct ParsedSource {
        unit: SourceUnit,
        parser: Parser,
    }

    struct RetainedComments;

    impl RetainedComments {
        fn collect(source: &str) -> Vec<(LexicalTokenKind, String)> {
            LexicalTokens::new(source)
                .filter(|token| {
                    matches!(
                        token.kind,
                        LexicalTokenKind::Comment | LexicalTokenKind::DocumentationComment
                    )
                })
                .map(|token| {
                    let comment = &source[token.range];
                    let comment = comment.strip_suffix('\n').unwrap_or(comment);
                    let comment = comment.strip_suffix('\r').unwrap_or(comment);
                    (token.kind, comment.to_string())
                })
                .collect()
        }
    }

    impl ParsedSource {
        fn new(source: &str) -> Self {
            Self::named(source, "test source")
        }

        fn named(source: &str, name: &str) -> Self {
            let mut parser = Parser::new();
            let unit = SourceUnitParser::new()
                .parse(source, &LocationCtx::Plain, &mut parser, Lexer::new(source))
                .unwrap_or_else(|error| panic!("failed to parse {name}: {error:?}\n{source}"));
            Self { unit, parser }
        }

        fn render(&self, layout_intentions: LayoutIntentions) -> String {
            self.render_with_options(
                PrettyOptions::default()
                    .with_line_width(120)
                    .with_layout_intentions(layout_intentions),
            )
        }

        fn render_with_options(&self, options: PrettyOptions) -> String {
            PrettyFormatter::with_options(&self.parser.arena, options).render_unit(self.unit)
        }

        fn desugared_shape(&self) -> String {
            let output = SourceUnitDesugarer::new(
                &self.parser.spans,
                &self.parser.arena,
                self.unit,
                BitterArena::default(),
            )
            .run()
            .unwrap();
            output.root.ugly(&BitterFormatter::new(&output.arena))
        }
    }

    #[test]
    fn records_layout_intentions_without_adding_syntax_variants() {
        let source = "(field = field,\n= kept)";
        let parsed = ParsedSource::new(source);
        assert_eq!(
            parsed.parser.arena.intentions.line_extent(parsed.unit.root.into()),
            Some(LineExtent::new(0, 1))
        );
        let Term::Paren(Paren(fields)) = &parsed.parser.arena.terms[&parsed.unit.root] else {
            panic!("expected a tuple")
        };
        assert_eq!(fields.len(), 2);
        assert!(
            fields.iter().all(|field| matches!(parsed.parser.arena.terms[field], Term::Named(_)))
        );
    }

    #[test]
    fn canonicalizes_term_and_pattern_puns_independently() {
        let source = concat!(
            "let (field = field, /projected = projected) = input in ",
            "(field = field, = kept, annotated = annotated : Type, renamed = other)",
        );
        let parsed = ParsedSource::new(source);

        assert_eq!(
            parsed.render(LayoutIntentions::Preserve),
            concat!(
                "let (= field, /projected) = input in\n",
                "(= field, = kept, = annotated : Type, renamed = other)\n",
            )
        );
    }

    #[test]
    fn punned_annotations_use_the_surviving_classifier_boundary() {
        let cases = [
            ("(field =\nfield : Type)", "(= field : Type)\n"),
            ("param (field =\nfield : Type) that _", "param = field : Type that\n_\n"),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        });
    }

    #[test]
    fn preserves_annotation_ownership_around_punned_fields() {
        let cases = [
            ("param (/process) : Type that _", "param (/process) : Type that\n_\n"),
            ("((field = field) : Type)", "((= field) : Type)\n"),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Ignore);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
        });
    }

    #[test]
    fn spaces_inline_thunk_contents_and_indents_broken_contents() {
        let cases = [
            ("{SomeComputation}", "{ SomeComputation }\n"),
            ("{ SomeComputation }", "{ SomeComputation }\n"),
            ("{\nSomeComputation\n}", "{\n  SomeComputation\n}\n"),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");
            assert!(formatted.lines().all(|line| line.trim_end() == line));

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });

        let parsed = ParsedSource::new("{SomeComputation}");
        assert_eq!(
            parsed.render_with_options(
                PrettyOptions::default()
                    .with_line_width(12)
                    .with_layout_intentions(LayoutIntentions::Ignore),
            ),
            "{\n  SomeComputation\n}\n"
        );
    }

    #[test]
    fn removes_only_parentheses_allowed_by_the_grammar_context() {
        let cases = [
            ("((x))", "x\n"),
            ("((A -> B))", "A -> B\n"),
            ("(A -> B) -> C", "(A -> B) -> C\n"),
            ("A -> (B -> C)", "A -> B -> C\n"),
            ("(A * B) * C", "(A * B) * C\n"),
            ("A * (B * C)", "A * B * C\n"),
            ("! (value/field)", "! (value/field)\n"),
            ("f (value/field)", "f value/field\n"),
            ("f (g x)", "f (g x)\n"),
            ("(f x)/field", "(f x)/field\n"),
            ("((field = field))", "(= field)\n"),
            ("comatch x => x end", "fn x => x\n"),
            ("! comatch x => x end", "! (fn x => x)\n"),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            assert_eq!(parsed.render(LayoutIntentions::Ignore), expected, "source: {source}");
            let reparsed = ParsedSource::new(expected);
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
        });

        let parsed = ParsedSource::new("((x))");
        assert_eq!(
            parsed.render_with_options(
                PrettyOptions::default()
                    .with_line_width(120)
                    .with_layout_intentions(LayoutIntentions::Ignore)
                    .with_parentheses(Parentheses::Preserve),
            ),
            "((x))\n"
        );
    }

    #[test]
    fn does_not_duplicate_constructor_pattern_parentheses() {
        let canonical = concat!(
            "match tree\n",
            "| +Leaf() => ret +Leaf()\n",
            "| +Node(/left; /value; /right) => ret +Leaf()\n",
            "end\n",
        );
        let redundant = concat!(
            "match tree\n",
            "| +Leaf() => ret +Leaf()\n",
            "| +Node((/left; /value; /right)) => ret +Leaf()\n",
            "end\n",
        );

        [canonical, redundant].into_iter().for_each(|source| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Ignore);
            assert_eq!(formatted, canonical, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Ignore));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn does_not_add_parentheses_to_multiline_applications() {
        let canonical =
            concat!("Thk\n", "  (\n", "    forall (B : CType) .\n", "      B\n", "  )\n",);
        let parsed = ParsedSource::new(canonical);
        let formatted = parsed.render_with_options(PrettyOptions::default());

        assert_eq!(formatted, canonical);
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render_with_options(PrettyOptions::default()));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());

        let redundant = concat!(
            "(\n",
            "  (\n",
            "    Thk\n",
            "    (\n",
            "      forall (B : CType) .\n",
            "        B\n",
            "    )\n",
            "  )\n",
            ")\n",
        );
        let redundant = ParsedSource::new(redundant);
        assert_eq!(redundant.render_with_options(PrettyOptions::default()), canonical);
        assert_eq!(redundant.desugared_shape(), parsed.desugared_shape());
    }

    #[test]
    fn keeps_a_joined_application_boundary_around_a_multiline_argument() {
        let source = concat!("Thk (\n", "  forall (B : CType) .\n", "    B\n", ")\n",);
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render_with_options(PrettyOptions::default());

        assert_eq!(formatted, source);
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render_with_options(PrettyOptions::default()));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    }

    #[test]
    fn keeps_right_nested_terms_at_the_same_precedence_unparenthesized() {
        let cases = [
            (
                "begin let first = value that let second = first that second end",
                concat!(
                    "begin\n",
                    "  let first = value that\n",
                    "  let second = first that\n",
                    "  second\n",
                    "end\n",
                ),
            ),
            (
                "forall (A : Type) . forall (B : Type) . A -> B",
                "forall (A : Type) . forall (B : Type) . A -> B\n",
            ),
            ("fn x => fn y => x", "fn x => fn y => x\n"),
            (
                "do x <- first; do y <- second; ret y",
                concat!("do x <- first;\n", "do y <- second;\n", "ret y\n"),
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Ignore);
            assert_eq!(formatted, expected, "source: {source}");
            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn aligns_sequential_continuations_at_the_current_indentation() {
        let cases = [
            (
                "do~ first; do~ second; ret value",
                concat!("do~ first;\n", "do~ second;\n", "ret value\n"),
            ),
            (
                "let first = one in let second = two in second",
                concat!("let first = one in\n", "let second = two in\n", "second\n"),
            ),
            (
                "let first = one that let second = two that second",
                concat!("let first = one that\n", "let second = two that\n", "second\n"),
            ),
            (
                "def first = one in def second = two in second",
                concat!("def first = one in\n", "def second = two in\n", "second\n"),
            ),
            (
                "def first = one that def second = two that second",
                concat!("def first = one that\n", "def second = two that\n", "second\n"),
            ),
            (
                "param (A : VType) in param (B : VType) in body",
                concat!("param A : VType in\n", "param B : VType in\n", "body\n"),
            ),
            (
                "param (A : VType) that param (B : VType) that body",
                concat!("param A : VType that\n", "param B : VType that\n", "body\n"),
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Ignore);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Ignore));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn preserves_at_most_one_empty_line_between_sequential_expressions() {
        let cases = [
            ("let a = 1 in\na", "let a = 1 in\na\n"),
            ("let a = 1 in\n\na", "let a = 1 in\n\na\n"),
            ("let a = 1 in\n\n\na", "let a = 1 in\n\na\n"),
            ("let a = 1 in\n \t \na", "let a = 1 in\n\na\n"),
            ("do a <- first;\n\n\nsecond", "do a <- first;\n\nsecond\n"),
            ("def a = first in\n\nsecond", "def a = first in\n\nsecond\n"),
            ("param (A : VType) that\n\nbody", "param A : VType that\n\nbody\n"),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");
            assert!(formatted.lines().all(|line| line.trim_end() == line));

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });

        let parsed = ParsedSource::new("let a = 1 in\n\n\na");
        assert_eq!(parsed.render(LayoutIntentions::Ignore), "let a = 1 in\na\n");
    }

    #[test]
    fn keeps_empty_line_intentions_local_to_each_parsed_source() {
        let first = concat!("begin\n", "  let a = 1 in\n", "\n", "\n", "  a\n", "end");
        let second = concat!("begin\n", "  let b =\n", "    value\n", "  in\n", "  b\n", "end");
        let mut parser = Parser::new();
        SourceUnitParser::new()
            .parse(first, &LocationCtx::Plain, &mut parser, Lexer::new(first))
            .expect("the first source should parse");
        let second_unit = SourceUnitParser::new()
            .parse(second, &LocationCtx::Plain, &mut parser, Lexer::new(second))
            .expect("the second source should parse");

        let formatted = PrettyFormatter::with_options(
            &parser.arena,
            PrettyOptions::default().with_line_width(120),
        )
        .render_unit(second_unit);

        assert_eq!(formatted, format!("{second}\n"));
    }

    #[test]
    fn preserves_the_position_of_an_empty_line_around_a_leading_comment() {
        let cases = [
            ("let a = 1 in\n-- Tail note.\n\na", "let a = 1 in\n-- Tail note.\n\na\n"),
            ("let a = 1 in\n\n-- Tail note.\na", "let a = 1 in\n\n-- Tail note.\na\n"),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");
            assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn preserves_one_empty_line_at_composed_layout_boundaries() {
        let cases = [
            ("(first,\n\n\nsecond)", "(first,\n\n  second)\n"),
            ("fn x =>\n\n\nbody", "fn x =>\n\n  body\n"),
            ("A *\n\n\nB", "  A\n\n* B\n"),
            ("{\n\n\nbody\n\n\n}", "{\n\n  body\n\n}\n"),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");
            assert!(formatted.lines().all(|line| line.trim_end() == line));

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn leads_multiline_infix_operators_without_recursive_indentation() {
        let cases = [
            ("A *\nB *\nC", concat!("  A\n", "* B\n", "* C\n")),
            ("A ->\nB ->\nC", concat!("   A\n", "-> B\n", "-> C\n")),
            ("A * B *\nC", concat!("  A * B\n", "* C\n")),
            ("A -> B *\nC", concat!("   A\n", "->   B\n", "   * C\n")),
            ("A *\nB ->\nC", concat!("     A\n", "   * B\n", "-> C\n")),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn leads_infix_operators_when_the_inline_chain_exceeds_the_line_width() {
        let options = PrettyOptions::default()
            .with_line_width(12)
            .with_layout_intentions(LayoutIntentions::Ignore);
        let cases = [
            ("Alpha * Beta * Gamma", concat!("  Alpha\n", "* Beta\n", "* Gamma\n")),
            ("Alpha -> Beta -> Gamma", concat!("   Alpha\n", "-> Beta\n", "-> Gamma\n")),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render_with_options(options);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render_with_options(options));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn starts_a_hanging_infix_chain_at_its_enclosing_layout_boundary() {
        let cases = [
            (
                "(field :: A *\nB *\nC)",
                concat!("(\n", "  field ::\n", "    A\n", "  * B\n", "  * C\n", ")\n"),
            ),
            (
                "data\n| +Pair : A *\nB\nend",
                concat!("data\n", "| +Pair :\n", "    A\n", "  * B\n", "end\n"),
            ),
            (
                "codata\n| .call : A ->\nB\nend",
                concat!("codata\n", "| .call :\n", "     A\n", "  -> B\n", "end\n"),
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);

            assert_eq!(formatted, expected, "source: {source}");
            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn preserves_comments_around_hanging_infix_chains() {
        let cases = [
            (
                concat!("A *\n", "-- Keep this operand note.\n", "B"),
                concat!("  A *\n", "  -- Keep this operand note.\n", "  B\n"),
            ),
            (
                concat!("A *\n", "-- Keep this operand note.\n", "\n", "B"),
                concat!("  A *\n", "  -- Keep this operand note.\n", "\n", "  B\n"),
            ),
            (
                concat!("A *\n", "\n", "-- Keep this operand note.\n", "B"),
                concat!("  A *\n", "\n", "  -- Keep this operand note.\n", "  B\n"),
            ),
            (
                "/- Keep this prefix. -/ A *\nB",
                "/- Keep this prefix. -/ A\n                      * B\n",
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);

            assert_eq!(formatted, expected, "source: {source}");
            assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn keeps_placement_with_short_context_bindees() {
        let cases = [
            ("let value = item in value", "let value = item in\nvalue\n"),
            ("def value = item that value", "def value = item that\nvalue\n"),
            (
                "let Cmp (A : VType) =\n  Thk (A -> A -> Ret Bool)\nthat\nvalue",
                "let Cmp (A : VType) = Thk (A -> A -> Ret Bool) that\nvalue\n",
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Ignore);
            assert_eq!(formatted, expected, "source: {source}");
            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn breaks_placement_after_multiline_bindings() {
        let cases = [
            (
                "let value = A *\nB in value",
                concat!("let value =\n", "    A\n", "  * B\n", "in\n", "value\n"),
            ),
            (
                "def value = A ->\nB that value",
                concat!("def value =\n", "     A\n", "  -> B\n", "that\n", "value\n"),
            ),
            (
                "let value = begin\nitem\nend in value",
                concat!("let value = begin\n", "  item\n", "end\n", "in\n", "value\n"),
            ),
            (
                "let Cmp (A : VType) =\n  Thk (A -> A -> Ret Bool)\nthat\nvalue",
                concat!(
                    "let Cmp (A : VType) =\n",
                    "  Thk (A -> A -> Ret Bool)\n",
                    "that\n",
                    "value\n",
                ),
            ),
            (
                "let Cmp (A : VType) =\n  Thk (A -> A -> Ret Bool)\nin\nvalue",
                concat!(
                    "let Cmp (A : VType) =\n",
                    "  Thk (A -> A -> Ret Bool)\n",
                    "in\n",
                    "value\n",
                ),
            ),
            (
                "def Cmp (A : VType) =\n  Thk (A -> A -> Ret Bool)\nthat\nvalue",
                concat!(
                    "def Cmp (A : VType) =\n",
                    "  Thk (A -> A -> Ret Bool)\n",
                    "that\n",
                    "value\n",
                ),
            ),
            (
                "def Cmp (A : VType) =\n  Thk (A -> A -> Ret Bool)\nin\nvalue",
                concat!(
                    "def Cmp (A : VType) =\n",
                    "  Thk (A -> A -> Ret Bool)\n",
                    "in\n",
                    "value\n",
                ),
            ),
            (
                concat!(
                    "begin\n",
                    "  let Cmp (A : VType) =\n",
                    "    Thk (A -> A -> Ret Bool)\n",
                    "  that\n",
                    "  value\n",
                    "end",
                ),
                concat!(
                    "begin\n",
                    "  let Cmp (A : VType) =\n",
                    "    Thk (A -> A -> Ret Bool)\n",
                    "  that\n",
                    "  value\n",
                    "end\n",
                ),
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn aligns_binding_separators_after_multiline_parameters_and_types() {
        let cases = [
            (
                concat!(
                    "begin\n",
                    "  let f (first : Type)\n",
                    "    (second : Type) : Result = value in\n",
                    "  f\n",
                    "end",
                ),
                concat!(
                    "begin\n",
                    "  let f (first : Type)\n",
                    "    (second : Type)\n",
                    "  : Result = value in\n",
                    "  f\n",
                    "end\n",
                ),
            ),
            (
                concat!(
                    "begin\n",
                    "  def f : (\n",
                    "    Result\n",
                    "  ) = value that\n",
                    "  f\n",
                    "end",
                ),
                concat!(
                    "begin\n",
                    "  def f : (\n",
                    "      Result\n",
                    "    )\n",
                    "  = value that\n",
                    "  f\n",
                    "end\n",
                ),
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn aligns_binding_separators_after_width_wrapping() {
        let options = PrettyOptions::default()
            .with_line_width(38)
            .with_layout_intentions(LayoutIntentions::Ignore);
        let cases = [
            (
                "let f (first : FirstClassifier) (second : SecondClassifier) : Result = value in f",
                concat!(
                    "let f (first : FirstClassifier)\n",
                    "  (second : SecondClassifier)\n",
                    ": Result = value in\n",
                    "f\n",
                ),
            ),
            (
                "def f : FirstClassifier * SecondClassifier * ThirdClassifier = value that f",
                concat!(
                    "def f :\n",
                    "    FirstClassifier * SecondClassifier\n",
                    "  * ThirdClassifier\n",
                    "= value that\n",
                    "f\n",
                ),
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render_with_options(options);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render_with_options(options));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn breaks_placement_after_width_wrapped_context_bindees() {
        let source = "let value = Alpha * Beta * Gamma in value";
        let expected =
            concat!("let value =\n", "    Alpha * Beta\n", "  * Gamma\n", "in\n", "value\n");
        let options = PrettyOptions::default()
            .with_line_width(20)
            .with_layout_intentions(LayoutIntentions::Ignore);
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render_with_options(options);

        assert_eq!(formatted, expected);
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render_with_options(options));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    }

    #[test]
    fn outdents_placement_after_long_context_bindees() {
        let options = PrettyOptions::default()
            .with_line_width(20)
            .with_layout_intentions(LayoutIntentions::Ignore);
        let cases = [
            (
                "let value = extraordinarily_long_bindee in value",
                concat!("let value =\n", "  extraordinarily_long_bindee\n", "in\n", "value\n"),
            ),
            (
                "def value = extraordinarily_long_bindee that value",
                concat!("def value =\n", "  extraordinarily_long_bindee\n", "that\n", "value\n",),
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render_with_options(options);
            assert_eq!(formatted, expected, "source: {source}");
            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn compacts_manifest_existentials_when_grouping_is_semantically_transparent() {
        let parsed = ParsedSource::new("exists (Counter = ((Counter as Int) : VType)) . Counter");

        assert_eq!(
            parsed.render(LayoutIntentions::Ignore),
            "exists (= Counter as Int : VType) . Counter\n"
        );
        let reparsed = ParsedSource::new(&parsed.render(LayoutIntentions::Ignore));
        assert_eq!(
            reparsed.render(LayoutIntentions::Ignore),
            "exists (= Counter as Int : VType) . Counter\n"
        );
    }

    #[test]
    fn preserves_comments_on_manifest_parameter_wrappers() {
        let source = concat!(
            "exists (outer =\n",
            "  -- Keep the inner field.\n",
            "  inner = ((value as Definition) : Classifier)) . body",
        );
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        let reparsed = ParsedSource::new(&formatted);

        assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
    }

    #[test]
    fn preserves_observed_line_boundaries() {
        let parsed = ParsedSource::new("(first = first,\nsecond = second)");

        assert_eq!(parsed.render(LayoutIntentions::Preserve), "(= first,\n  = second)\n");
        assert_eq!(parsed.render(LayoutIntentions::Ignore), "(= first, = second)\n");
    }

    #[test]
    fn preserves_the_boundary_before_an_existential_parameter() {
        let source = "exists\n  (value : Type) . value";
        let expected = concat!("exists\n", "  (value : Type)\n", ".\n", "  value\n");
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);

        assert_eq!(formatted, expected);
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
    }

    #[test]
    fn preserves_multiple_items_grouped_on_each_source_line() {
        let source =
            concat!("(first = first, second = second,\n", "third = third, fourth = fourth)",);
        let parsed = ParsedSource::new(source);

        assert_eq!(
            parsed.render(LayoutIntentions::Preserve),
            concat!("(= first, = second,\n", "  = third, = fourth)\n")
        );
    }

    #[test]
    fn expands_the_immediate_layer_of_an_overflowing_delimited_group() {
        let joined = concat!(
            "match left\n",
            "| +Node(/left = left_left; /value = left_value; ",
            "/tree_height = left_height; /right = left_right) => ret +Leaf()\n",
            "end",
        );
        let source_grouped = concat!(
            "match left\n",
            "| +Node(\n",
            "  /left = left_left; /value = left_value; ",
            "/tree_height = left_height; /right = left_right\n",
            ") => ret +Leaf()\n",
            "end",
        );
        let expected = concat!(
            "match left\n",
            "| +Node(\n",
            "  /left = left_left;\n",
            "  /value = left_value;\n",
            "  /tree_height = left_height;\n",
            "  /right = left_right\n",
            ") => ret +Leaf()\n",
            "end\n",
        );
        let options = PrettyOptions::default().with_line_width(80);

        [joined, source_grouped].into_iter().for_each(|source| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render_with_options(options);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
            assert_eq!(formatted, reparsed.render_with_options(options));
        });
    }

    #[test]
    fn preserves_fitting_groups_inside_multiline_layout() {
        let source = concat!(
            "begin\n",
            "  param (\n",
            "    (/VType; /CType; /Thk; /Ret; /Unit; /Int; /String; /OS; /int; /string; /stdio; /process) :\n",
            "    @[import(\"../../lib/std/builtin.zy\")] _\n",
            "  ) that\n",
            "  _\n",
            "end\n",
        );
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render_with_options(PrettyOptions::default());

        assert_eq!(formatted, source);
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render_with_options(PrettyOptions::default()));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());

        let narrow = parsed.render_with_options(PrettyOptions::default().with_line_width(90));
        assert_ne!(narrow, source);
        assert!(narrow.lines().all(|line| line.len() <= 90));
        assert!(narrow.contains("/stdio;\n      /process"), "{narrow}");
        let reparsed = ParsedSource::new(&narrow);
        assert_eq!(
            narrow,
            reparsed.render_with_options(PrettyOptions::default().with_line_width(90))
        );
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    }

    #[test]
    fn accounts_for_a_staged_marker_in_the_line_width() {
        let source = concat!(
            "begin\n",
            "  param (\n",
            "    (/VType; /CType; /Thk; /Ret; /Unit; /Int; /Float; /Char; /String; /Bytes; /Reader; /Writer; /OS) :\n",
            "    @[import(\"builtin.zy\")] _\n",
            "  ) in\n",
            "  _\n",
            "end\n",
        );
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render_with_options(PrettyOptions::default());

        assert!(formatted.lines().all(|line| line.len() <= 100), "{formatted}");
        assert!(formatted.contains("/OS)\n    : @[import"), "{formatted}");
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        assert_eq!(formatted, reparsed.render_with_options(PrettyOptions::default()));
    }

    #[test]
    fn places_scope_separators_after_parameters_that_wrap() {
        let options = PrettyOptions::default()
            .with_line_width(38)
            .with_layout_intentions(LayoutIntentions::Ignore);
        let cases = [
            (
                "fn (first : FirstClassifier) (second : SecondClassifier) => result",
                concat!(
                    "fn (first : FirstClassifier)\n",
                    "  (second : SecondClassifier)\n",
                    "=>\n",
                    "  result\n",
                ),
            ),
            (
                "pi (first : FirstClassifier) (second : SecondClassifier) . Result",
                concat!(
                    "pi (first : FirstClassifier)\n",
                    "  (second : SecondClassifier)\n",
                    ".\n",
                    "  Result\n",
                ),
            ),
            (
                "forall (first : FirstClassifier) (second : SecondClassifier) . Result",
                concat!(
                    "forall (first : FirstClassifier)\n",
                    "  (second : SecondClassifier)\n",
                    ".\n",
                    "  Result\n",
                ),
            ),
            (
                "sigma (first : FirstClassifier) (second : SecondClassifier) . Result",
                concat!(
                    "sigma (first : FirstClassifier)\n",
                    "  (second : SecondClassifier)\n",
                    ".\n",
                    "  Result\n",
                ),
            ),
            (
                "exists (first : FirstClassifier) (second : SecondClassifier) . Result",
                concat!(
                    "exists (first : FirstClassifier)\n",
                    "  (second : SecondClassifier)\n",
                    ".\n",
                    "  Result\n",
                ),
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render_with_options(options);
            assert_eq!(formatted, expected, "source: {source}");
            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render_with_options(options));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn places_function_separator_after_a_preserved_multiline_pattern() {
        let source = concat!("fix (\n", "  value\n", ") => result");
        let expected = concat!("fix (\n", "  value\n", ")\n", "=>\n", "  result\n");
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);

        assert_eq!(formatted, expected);
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    }

    #[test]
    fn does_not_pad_comment_driven_line_breaks() {
        let source = concat!(
            "exists\n",
            "  --| Parameter documentation.\n",
            "\n",
            "  (Value : Type) . Value",
        );
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);

        assert_eq!(
            formatted,
            concat!(
                "exists\n",
                "  --| Parameter documentation.\n",
                "\n",
                "  (Value : Type)\n",
                ".\n",
                "  Value\n",
            )
        );
        assert!(formatted.lines().all(|line| line.trim_end() == line));

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    }

    #[test]
    fn preserves_a_single_line_break_after_nested_comments() {
        let source =
            concat!("exists\n", "  --| Parameter documentation.\n", "  (Value : Type) . Value",);
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);

        assert_eq!(
            formatted,
            concat!(
                "exists\n",
                "  --| Parameter documentation.\n",
                "  (Value : Type)\n",
                ".\n",
                "  Value\n",
            )
        );

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    }

    #[test]
    fn preserves_documentation_comments_around_formatted_syntax() {
        let source = concat!(
            "--| Package heading\n",
            "--|\n",
            "--| Package details.\n",
            "@[doc] begin\n",
            "  --| A documented binding.\n",
            "  let value = ((1)) that\n",
            "  --| Use the value.\n",
            "  value\n",
            "end\n",
            "--| Trailing note.\n",
        );
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Ignore);

        assert_eq!(
            formatted,
            concat!(
                "--| Package heading\n",
                "--|\n",
                "--| Package details.\n",
                "@[doc] begin\n",
                "  --| A documented binding.\n",
                "  let value = 1 that\n",
                "  --| Use the value.\n",
                "  value\n",
                "end\n",
                "--| Trailing note.\n",
            )
        );
        assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
    }

    #[test]
    fn starts_documentation_on_the_anchored_syntax_line() {
        let source = "fn parameter => --| Body documentation.\n@[doc] body";
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Ignore);

        assert_eq!(
            formatted,
            concat!("fn parameter =>\n", "  --| Body documentation.\n", "  @[doc] body\n",)
        );
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    }

    #[test]
    fn preserves_ordinary_and_nested_block_comments() {
        let source = concat!(
            "-- Package note.\n",
            "/- Outer note.\n",
            "   /- Nested note. -/\n",
            "-/\n",
            "begin\n",
            "  -- Binding note.\n",
            "  let value = ((1)) that\n",
            "  /- Use the\n",
            "     bound value. -/\n",
            "  value\n",
            "end\n",
            "-- End note.\n",
            "/- Final note. -/\n",
        );
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Ignore);

        assert_eq!(
            formatted,
            concat!(
                "-- Package note.\n",
                "/- Outer note.\n",
                "   /- Nested note. -/\n",
                "-/\n",
                "begin\n",
                "  -- Binding note.\n",
                "  let value = 1 that\n",
                "  /- Use the\n",
                "     bound value. -/\n",
                "  value\n",
                "end\n",
                "-- End note.\n",
                "/- Final note. -/\n",
            )
        );
        assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Ignore));
    }

    #[test]
    fn preserves_comment_ownership_around_arm_prefixes() {
        let cases = [
            concat!(
                "data\n",
                "| +First : Unit\n",
                "-- Before the second arm.\n",
                "| +Second :\n",
                "  -- The second payload.\n",
                "  Unit\n",
                "end",
            ),
            concat!(
                "match value\n",
                "| +Present(result) => ret result\n",
                "-- Before the fallback arm.\n",
                "| _ =>\n",
                "  -- The fallback body.\n",
                "  ret default\n",
                "end",
            ),
        ];

        cases.into_iter().for_each(|source| {
            let expected = format!("{source}\n");
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        });
    }

    #[test]
    fn preserves_line_separation_around_comments() {
        let cases = [
            ("/- Inline. -/value", "/- Inline. -/ value\n"),
            ("/- Above. -/\nvalue", "/- Above. -/\nvalue\n"),
            ("/- Detached. -/\n\nvalue", "/- Detached. -/\n\nvalue\n"),
            ("value/- Trailing. -/", "value /- Trailing. -/\n"),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Ignore);
            assert_eq!(formatted, expected, "source: {source}");
            assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Ignore));
        });
    }

    #[test]
    fn leading_comments_participate_in_layout_boundaries() {
        let source = concat!("begin value end\n", "-- Explain the argument.\n", "argument");
        let expected = concat!(
            "begin\n",
            "  value\n",
            "end\n",
            "  -- Explain the argument.\n",
            "  argument\n",
        );
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);

        assert_eq!(formatted, expected);
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
    }

    #[test]
    fn prefix_forms_respect_leading_comment_boundaries() {
        let cases = [
            ("ret\n-- Explain the value.\nvalue", "ret\n  -- Explain the value.\n  value\n"),
            (
                "param\n-- Explain the binder.\nvalue that tail",
                "param\n-- Explain the binder.\nvalue that\ntail\n",
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
            assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
        });
    }

    #[test]
    fn does_not_pun_away_payload_comments() {
        let cases = [
            "(field =\n-- Keep the term payload.\nfield)",
            "param (field =\n-- Keep the pattern payload.\nfield) that _",
        ];

        cases.into_iter().for_each(|source| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert!(formatted.contains("field =\n"), "source: {source}\n{formatted}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
            assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
        });
    }

    #[test]
    fn preserves_separation_of_detached_documentation() {
        [
            ("--| Detached\n\n@[doc] _", "--| Detached\n\n@[doc] _\n"),
            ("--| Detached\n-- barrier\n@[doc] _", "--| Detached\n-- barrier\n@[doc] _\n"),
            ("@[doc] value --| Detached", "@[doc] value\n--| Detached\n"),
        ]
        .into_iter()
        .for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Ignore);
            assert_eq!(formatted, expected);

            let reparsed = ParsedSource::new(&formatted);
            let documentation =
                reparsed.unit.documentation(&reparsed.parser.arena, &reparsed.parser.spans);
            let [site] = documentation.as_slice() else {
                panic!("expected one documentation annotation")
            };
            assert!(site.directive.comment.is_none());
        });
    }

    #[test]
    fn canonical_pretty_printing_round_trips_idempotently() {
        let parsed = ParsedSource::new("(field = field, = kept, renamed = other)");
        let first = parsed.render(LayoutIntentions::Ignore);
        let reparsed = ParsedSource::new(&first);
        let second = reparsed.render(LayoutIntentions::Ignore);

        assert_eq!(first, second);
    }

    #[test]
    fn records_standard_library_named_term_punning_backlog() {
        let sources = [
            ("bool.zy", include_str!("../../../../lib/std/bool.zy"), 11),
            ("interface.zy", include_str!("../../../../lib/std/interface.zy"), 8),
            ("io-types.zy", include_str!("../../../../lib/std/io-types.zy"), 18),
            ("list.zy", include_str!("../../../../lib/std/list.zy"), 11),
            ("monad.zy", include_str!("../../../../lib/std/monad.zy"), 2),
            ("option.zy", include_str!("../../../../lib/std/option.zy"), 9),
            ("result.zy", include_str!("../../../../lib/std/result.zy"), 8),
            ("std.zy", include_str!("../../../../lib/std/std.zy"), 29),
        ];

        let observed = sources
            .into_iter()
            .map(|(name, source, expected)| {
                let parsed = ParsedSource::new(source);
                let candidates =
                    NamedTermPunningAudit::new(source, &parsed.parser.spans, &parsed.parser.arena)
                        .candidates()
                        .len();
                assert_eq!(candidates, expected, "unexpected punning backlog in {name}");
                candidates
            })
            .sum::<usize>();

        assert_eq!(observed, 96);
    }

    #[test]
    fn standard_library_pretty_printing_reparses_idempotently() {
        let sources = [
            ("bool.zy", include_str!("../../../../lib/std/bool.zy")),
            ("builtin.zy", include_str!("../../../../lib/std/builtin.zy")),
            ("interface.zy", include_str!("../../../../lib/std/interface.zy")),
            ("io-types.zy", include_str!("../../../../lib/std/io-types.zy")),
            ("list.zy", include_str!("../../../../lib/std/list.zy")),
            ("monad.zy", include_str!("../../../../lib/std/monad.zy")),
            ("option.zy", include_str!("../../../../lib/std/option.zy")),
            ("result.zy", include_str!("../../../../lib/std/result.zy")),
            ("std.zy", include_str!("../../../../lib/std/std.zy")),
        ];

        sources.into_iter().for_each(|(name, source)| {
            let original = ParsedSource::new(source);
            let first = original.render(LayoutIntentions::Ignore);
            let reparsed = ParsedSource::named(&first, name);
            let second = reparsed.render(LayoutIntentions::Ignore);
            assert_eq!(
                original.desugared_shape(),
                reparsed.desugared_shape(),
                "formatter changed the desugared structure of {name}"
            );
            assert_eq!(first, second, "formatter is not idempotent for {name}");
            assert_eq!(
                RetainedComments::collect(source),
                RetainedComments::collect(&first),
                "formatter changed comments in {name}"
            );
        });
    }
}
