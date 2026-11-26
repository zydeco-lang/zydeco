//! Comment placement, boundary intents, and shared layout combinators.

use super::*;

#[derive(Copy, Clone)]
pub(super) struct LayoutAnchors {
    pub(super) first: EntityId,
    pub(super) last: EntityId,
}

#[derive(Clone)]
pub(super) struct LayoutFragment<'arena> {
    pub(super) document: RcDoc<'arena>,
    pub(super) anchors: LayoutAnchors,
}

impl<'arena> LayoutFragment<'arena> {
    pub(super) fn entity(entity: EntityId, document: RcDoc<'arena>) -> Self {
        Self { document, anchors: LayoutAnchors { first: entity, last: entity } }
    }

    pub(super) fn map_document(
        self, transform: impl FnOnce(RcDoc<'arena>) -> RcDoc<'arena>,
    ) -> Self {
        let Self { document, anchors } = self;
        Self { document: transform(document), anchors }
    }
}

/// The three relevant projections of one grammatical layout layer.
///
/// `compact` joins every boundary owned by the layer. `retained` keeps the
/// source rows, provided each row still fits. `expanded` breaks every boundary
/// in the layer while leaving child layouts independent.
#[derive(Clone)]
pub(super) struct GroupLayout<'arena> {
    pub(super) compact: RcDoc<'arena>,
    pub(super) retained: RcDoc<'arena>,
    pub(super) expanded: RcDoc<'arena>,
    pub(super) retains_break: bool,
}

#[derive(Copy, Clone)]
pub(super) enum BoundaryIntent {
    Canonical,
    Preserve(LayoutBoundary),
    PreserveBlankLine(LayoutBoundary),
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub(super) enum BoundaryPlacement {
    Joined,
    Broken,
    BlankLine,
}

impl BoundaryIntent {
    pub(super) fn between(before: impl Into<EntityId>, after: impl Into<EntityId>) -> Self {
        Self::Preserve(LayoutBoundary::between(before, after))
    }

    pub(super) fn after_start(enclosing: impl Into<EntityId>, first: impl Into<EntityId>) -> Self {
        Self::Preserve(LayoutBoundary::after_start(enclosing, first))
    }

    pub(super) fn after_arm_prefix(payload: impl Into<EntityId>) -> Self {
        Self::Preserve(LayoutBoundary::after_arm_prefix(payload))
    }

    pub(super) fn after_existential_open(parameter: PatId) -> Self {
        Self::Preserve(LayoutBoundary::after_existential_open(parameter))
    }

    pub(super) fn before_existential_parameter(
        enclosing: impl Into<EntityId>, parameter: PatId,
    ) -> Self {
        Self::Preserve(LayoutBoundary::before_existential_parameter(enclosing, parameter))
    }

    pub(super) fn before_end(last: impl Into<EntityId>, enclosing: impl Into<EntityId>) -> Self {
        Self::Preserve(LayoutBoundary::before_end(last, enclosing))
    }

    pub(super) fn blank_line_only(self) -> Self {
        match self {
            | Self::Canonical => Self::Canonical,
            | Self::Preserve(boundary) | Self::PreserveBlankLine(boundary) => {
                Self::PreserveBlankLine(boundary)
            }
        }
    }

    pub(super) fn resolve(self, intentions: &SurfaceIntentions) -> Option<BreakIntent> {
        match self {
            | Self::Canonical => None,
            | Self::Preserve(boundary) => intentions.at(boundary),
            | Self::PreserveBlankLine(boundary) => {
                intentions.at(boundary).filter(|intent| *intent == BreakIntent::BlankLine)
            }
        }
    }
}

#[derive(Copy, Clone)]
pub(super) enum DelimiterSpacing {
    Tight,
    Spaced,
}

#[derive(Clone)]
pub(super) struct BoundaryLayout<'arena> {
    prefix: RcDoc<'arena>,
    joined: RcDoc<'arena>,
    broken: RcDoc<'arena>,
    blank: RcDoc<'arena>,
    joined_indent: isize,
    broken_indent: isize,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub(super) enum StagedBoundary {
    Annotation,
    BindingType,
}

#[derive(Copy, Clone)]
pub(super) enum ScopedForm {
    Function,
    ValueFunction,
    Pi,
    ValPi,
    Forall,
    Sigma,
}

pub(super) struct ScopeTelescope<Parameter> {
    pub(super) parameters: Vec<Parameter>,
    pub(super) body: TermId,
}

impl StagedBoundary {
    pub(super) fn marker(self) -> &'static str {
        match self {
            | Self::Annotation | Self::BindingType => ":",
        }
    }
}

impl ScopedForm {
    /// Whether a nested scope of this form means the same as a longer telescope.
    ///
    /// Consecutive parameters of a function form one group, which a nested
    /// function does not, so those stay as written.
    pub(super) fn merges_nested_scopes(self) -> bool {
        match self {
            | Self::Function | Self::Pi | Self::Forall => false,
            | Self::ValueFunction | Self::ValPi | Self::Sigma => true,
        }
    }

    pub(super) fn keyword(self) -> &'static str {
        match self {
            | Self::Function => "fn",
            | Self::ValueFunction => "val",
            | Self::Pi => "pi",
            | Self::ValPi => "val pi",
            | Self::Forall => "forall",
            | Self::Sigma => "sigma",
        }
    }

    pub(super) fn marker(self) -> &'static str {
        match self {
            | Self::Function | Self::ValueFunction => "=>",
            | Self::Pi | Self::ValPi | Self::Forall | Self::Sigma => ".",
        }
    }

    pub(super) fn body_precedence(self) -> TermPrecedence {
        match self {
            | Self::Function | Self::ValueFunction => TermPrecedence::Binder,
            | Self::Pi | Self::ValPi | Self::Forall | Self::Sigma => TermPrecedence::Quantifier,
        }
    }

    pub(super) fn split(self, term: &Term) -> Option<(CoPatId, TermId)> {
        match (self, term) {
            | (Self::Function, Term::Abs(Abs(parameter, body)))
            | (Self::ValueFunction, Term::ValAbs(Abs(parameter, body)))
            | (Self::Pi, Term::Pi(Pi(parameter, body)))
            | (Self::ValPi, Term::ValPi(ValPi(parameter, body)))
            | (Self::Forall, Term::Forall(Forall(parameter, body)))
            | (Self::Sigma, Term::Sigma(Sigma(parameter, body))) => Some((*parameter, *body)),
            | _ => None,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub(super) enum InfixOperator {
    Product,
    Arrow,
}

impl InfixOperator {
    pub(super) fn symbol(self) -> &'static str {
        match self {
            | Self::Product => "*",
            | Self::Arrow => "->",
        }
    }

    pub(super) fn left_precedence(self) -> TermPrecedence {
        match self {
            | Self::Product => TermPrecedence::Application,
            | Self::Arrow => TermPrecedence::Product,
        }
    }

    pub(super) fn right_precedence(self) -> TermPrecedence {
        match self {
            | Self::Product => TermPrecedence::Product,
            | Self::Arrow => TermPrecedence::Arrow,
        }
    }

    /// One operand of an infix chain, with the operator node whose leading
    /// comments annotate it. Binary chains carry their operator nodes; a flat
    /// n-ary product has none between its components.
    pub(super) fn operands(
        self, arena: &TextArena, term: &Term,
    ) -> Option<Vec<(TermId, Option<TermId>)>> {
        match self {
            | Self::Product => match term {
                | Term::Prod(Prod(components)) => {
                    Some(components.iter().map(|component| (*component, None)).collect())
                }
                | _ => None,
            },
            | Self::Arrow => {
                let mut operands = Vec::new();
                let mut current = match term {
                    | Term::Arrow(Arrow(left, right)) => (*left, *right),
                    | _ => return None,
                };
                operands.push((current.0, None));
                loop {
                    let (_, right) = current;
                    match &arena.terms[&right] {
                        | Term::Arrow(Arrow(inner_left, inner_right)) => {
                            operands.push((*inner_left, Some(right)));
                            current = (*inner_left, *inner_right);
                        }
                        | _ => {
                            operands.push((right, None));
                            return Some(operands);
                        }
                    }
                }
            }
        }
    }
}

pub(super) struct InfixOperand<'arena> {
    document: RcDoc<'arena>,
    starts_with_comment: bool,
    comments_end_line: bool,
}

impl<'arena> BoundaryLayout<'arena> {
    pub(super) fn after(
        separator: &'static str, joined_indent: isize, broken_indent: isize,
    ) -> Self {
        Self {
            prefix: if separator.is_empty() { RcDoc::nil() } else { RcDoc::text(separator) },
            joined: RcDoc::space(),
            broken: RcDoc::hardline(),
            blank: Self::blank_line(),
            joined_indent,
            broken_indent,
        }
    }

    pub(super) fn tight(joined_indent: isize, broken_indent: isize) -> Self {
        Self {
            prefix: RcDoc::nil(),
            joined: RcDoc::nil(),
            broken: RcDoc::hardline(),
            blank: Self::blank_line(),
            joined_indent,
            broken_indent,
        }
    }

    pub(super) fn aligned(separator: &'static str) -> Self {
        Self::after(separator, 0, 0)
    }

    pub(super) fn hanging(separator: &'static str, indent: isize) -> Self {
        Self::after(separator, 0, indent)
    }

    pub(super) fn nested(separator: &'static str, indent: isize) -> Self {
        Self::after(separator, indent, indent)
    }

    fn blank_line() -> RcDoc<'arena> {
        RcDoc::nesting(|nesting| {
            let nesting = isize::try_from(nesting).unwrap_or(isize::MAX);
            RcDoc::hardline().append(RcDoc::hardline()).nest(-nesting)
        })
    }

    pub(super) fn place(
        &self, placement: BoundaryPlacement, continuation: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        let (boundary, indent) = match placement {
            | BoundaryPlacement::Joined => (self.joined.clone(), self.joined_indent),
            | BoundaryPlacement::Broken => (self.broken.clone(), self.broken_indent),
            | BoundaryPlacement::BlankLine => (self.blank.clone(), self.broken_indent),
        };
        self.prefix.clone().append(boundary.append(continuation).nest(indent))
    }
}

impl DelimiterSpacing {
    pub(super) fn boundary<'arena>(
        self, joined_indent: isize, broken_indent: isize,
    ) -> BoundaryLayout<'arena> {
        match self {
            | Self::Tight => BoundaryLayout::tight(joined_indent, broken_indent),
            | Self::Spaced => BoundaryLayout::after("", joined_indent, broken_indent),
        }
    }
}

impl<'arena> PrettyFormatter<'arena> {
    pub(super) fn with_leading_comments(
        &self, entity: EntityId, document: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        self.with_comments(self.presentation.trivia.leading_comments(entity), document)
    }

    pub(super) fn with_before_arm_comments(
        &self, entity: EntityId, document: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        self.with_comments(self.presentation.trivia.before_arm_comments(entity), document)
    }

    pub(super) fn with_before_metadata_comments(
        &self, entity: EntityId, document: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        self.with_comments(self.presentation.trivia.before_metadata_comments(entity), document)
    }

    pub(super) fn with_before_existential_comments(
        &self, entity: EntityId, document: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        self.with_comments(self.presentation.trivia.before_existential_comments(entity), document)
    }

    pub(super) fn with_comments(
        &self, comments: &'arena [LeadingComment], document: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        self.with_separated_comments(comments, LineSeparation::SameLine, document)
    }

    /// Print comments before a document. Each separation is at least what
    /// the following item requires: another comment its own least
    /// separation, and the document `minimum`.
    pub(super) fn with_separated_comments(
        &self, comments: &'arena [LeadingComment], minimum: LineSeparation, document: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        let required = comments
            .iter()
            .skip(1)
            .map(|comment| Self::least_separation_before(comment.comment()))
            .chain(std::iter::once(minimum));
        comments
            .iter()
            .zip(required)
            .fold(RcDoc::nil(), |prefix, (comment, required)| {
                prefix
                    .append(if comment.comment().as_text().is_some() {
                        self.ensure_line_start()
                    } else {
                        RcDoc::nil()
                    })
                    .append(self.comment(comment.comment()))
                    .append(self.line_separation(comment.separation_after().max(required)))
            })
            .append(document)
    }

    pub(super) fn ensure_line_start(&self) -> RcDoc<'arena> {
        RcDoc::column(|column| {
            RcDoc::nesting(
                move |nesting| {
                    if column > nesting { RcDoc::hardline() } else { RcDoc::nil() }
                },
            )
        })
    }

    pub(super) fn at_line_start(&self, document: RcDoc<'arena>) -> RcDoc<'arena> {
        RcDoc::column(|column| {
            RcDoc::nesting(
                move |nesting| {
                    if column > nesting { RcDoc::fail() } else { RcDoc::nil() }
                },
            )
        })
        .append(document)
    }

    pub(super) fn after_line_start(&self, document: RcDoc<'arena>) -> RcDoc<'arena> {
        RcDoc::column(|column| {
            RcDoc::nesting(
                move |nesting| {
                    if column == nesting { RcDoc::fail() } else { RcDoc::nil() }
                },
            )
        })
        .append(document)
    }

    pub(super) fn with_trailing_comments(
        &self, entity: EntityId, document: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        self.presentation.trivia.trailing_comments(entity).iter().fold(
            document,
            |document, comment| {
                let separation = comment
                    .separation_before()
                    .max(Self::least_separation_before(comment.comment()));
                document
                    .append(self.line_separation(separation))
                    .append(self.comment(comment.comment()))
            },
        )
    }

    pub(super) fn comment(&self, comment: &'arena SurfaceComment) -> RcDoc<'arena> {
        match comment {
            | SurfaceComment::Text(comment) => self.marked_comment_lines("--|", &comment.text),
            | SurfaceComment::Line(comment) => self.marked_comment_lines("--", &comment.text),
            | SurfaceComment::Block(comment) => self.block_comment(comment),
        }
    }

    pub(super) fn marked_comment_lines(
        &self, marker: &'static str, text: &'arena str,
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

    pub(super) fn block_comment(&self, comment: &'arena BlockComment) -> RcDoc<'arena> {
        RcDoc::intersperse(comment.text.split('\n').map(RcDoc::text), RcDoc::hardline())
    }

    pub(super) fn line_separation(&self, separation: LineSeparation) -> RcDoc<'arena> {
        match separation {
            | LineSeparation::SameLine => RcDoc::space(),
            | LineSeparation::NextLine => RcDoc::hardline(),
            | LineSeparation::BlankLine => BoundaryLayout::blank_line(),
        }
    }

    /// Documentation starts on a fresh line; other comments may share the
    /// line of whatever precedes them.
    pub(super) fn least_separation_before(comment: &SurfaceComment) -> LineSeparation {
        if comment.as_text().is_some() {
            LineSeparation::NextLine
        } else {
            LineSeparation::SameLine
        }
    }

    /// Whether an observed blank line must appear at a boundary under the
    /// configured layout-intentions policy.
    pub(super) fn preserves_blank_line(&self, intention: Option<BreakIntent>) -> bool {
        matches!(
            self.options.layout_intentions,
            LayoutIntentions::Preserve | LayoutIntentions::BlankLinesOnly
        ) && intention == Some(BreakIntent::BlankLine)
    }

    /// Whether an observed separation must appear in the output under the
    /// configured layout-intentions policy. `Preserve` keeps every break,
    /// `BlankLinesOnly` keeps only blank lines, and `Ignore` keeps neither.
    pub(super) fn forces_break(&self, intention: Option<BreakIntent>) -> bool {
        match (self.options.layout_intentions, intention) {
            | (LayoutIntentions::Ignore, _) => false,
            | (LayoutIntentions::BlankLinesOnly, Some(BreakIntent::BlankLine)) => true,
            | (LayoutIntentions::BlankLinesOnly, _) => false,
            | (LayoutIntentions::Preserve, Some(intention)) => intention.requires_line_break(),
            | (LayoutIntentions::Preserve, None) => false,
        }
    }

    pub(super) fn mandatory_line_break(&self, intention: Option<BreakIntent>) -> RcDoc<'arena> {
        if self.preserves_blank_line(intention) {
            BoundaryLayout::blank_line()
        } else {
            RcDoc::hardline()
        }
    }

    pub(super) fn retained_placement(&self, intent: BoundaryIntent) -> BoundaryPlacement {
        let intention = intent.resolve(&self.presentation.intentions);
        if self.preserves_blank_line(intention) {
            return BoundaryPlacement::BlankLine;
        }
        if self.forces_break(intention) {
            return BoundaryPlacement::Broken;
        }
        BoundaryPlacement::Joined
    }

    pub(super) fn expanded_placement(&self, intent: BoundaryIntent) -> BoundaryPlacement {
        let intention = intent.resolve(&self.presentation.intentions);
        if self.preserves_blank_line(intention) {
            BoundaryPlacement::BlankLine
        } else {
            BoundaryPlacement::Broken
        }
    }

    /// Place a continuation after a boundary. A continuation whose leading
    /// comments end their line cannot share the running line, so its
    /// boundary breaks before the comments.
    pub(super) fn layout_boundary(
        &self, intent: BoundaryIntent, layout: BoundaryLayout<'arena>,
        continuation: LayoutFragment<'arena>,
    ) -> RcDoc<'arena> {
        let LayoutFragment { document, anchors } = continuation;
        if self.leading_comments_end_line(anchors.first) {
            layout.place(self.expanded_placement(intent), document)
        } else {
            self.retained_boundary(intent, layout, document)
        }
    }

    /// Place a continuation at the boundary's retained placement. A joined
    /// boundary keeps its broken alternative for width selection.
    pub(super) fn retained_boundary(
        &self, intent: BoundaryIntent, layout: BoundaryLayout<'arena>, continuation: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        match self.retained_placement(intent) {
            | BoundaryPlacement::Joined => self.flexible(
                layout.place(BoundaryPlacement::Joined, continuation.clone()),
                layout.place(BoundaryPlacement::Broken, continuation),
            ),
            | retained @ (BoundaryPlacement::Broken | BoundaryPlacement::BlankLine) => {
                layout.place(retained, continuation)
            }
        }
    }

    /// Whether an entity's leading comments end a line before it. Line and
    /// documentation comments always do; a block comment does when a line
    /// break follows it.
    pub(super) fn leading_comments_end_line(&self, entity: EntityId) -> bool {
        self.presentation
            .trivia
            .leading_comments(entity)
            .iter()
            .any(|comment| comment.separation_after() != LineSeparation::SameLine)
    }

    pub(super) fn prefixed(
        &self, enclosing: impl Into<EntityId>, prefix: &'static str,
        layout: BoundaryLayout<'arena>, child: LayoutFragment<'arena>,
    ) -> RcDoc<'arena> {
        RcDoc::text(prefix).append(self.layout_boundary(
            BoundaryIntent::after_start(enclosing, child.anchors.first),
            layout,
            child,
        ))
    }

    pub(super) fn flexible(&self, joined: RcDoc<'arena>, broken: RcDoc<'arena>) -> RcDoc<'arena> {
        // Expose the joined projection to enclosing groups while retaining a
        // complete broken alternative for a boundary whose own group overflows.
        joined.clone().union(broken).flat_alt(joined)
    }

    pub(super) fn join(
        &self, left: LayoutFragment<'arena>, layout: BoundaryLayout<'arena>,
        right: LayoutFragment<'arena>,
    ) -> LayoutFragment<'arena> {
        let intent = BoundaryIntent::between(left.anchors.last, right.anchors.first);
        let anchors = LayoutAnchors { first: left.anchors.first, last: right.anchors.last };
        let document = left.document.append(self.layout_boundary(intent, layout, right));
        LayoutFragment { document, anchors }
    }

    pub(super) fn separated(
        &self, items: Vec<LayoutFragment<'arena>>, layout: BoundaryLayout<'arena>,
    ) -> Option<LayoutFragment<'arena>> {
        items.into_iter().rev().reduce(|right, left| self.join(left, layout.clone(), right))
    }

    pub(super) fn grouped_join(
        &self, left: LayoutFragment<'arena>, before_boundary: RcDoc<'arena>,
        right: LayoutFragment<'arena>, continuation_indent: isize,
    ) -> LayoutFragment<'arena> {
        let intent = BoundaryIntent::between(left.anchors.last, right.anchors.first);
        let intention = intent.resolve(&self.presentation.intentions);
        let anchors = LayoutAnchors { first: left.anchors.first, last: right.anchors.last };
        let preserve_break = self.forces_break(intention);
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

    /// Separate a multiline head from its stage marker, while retaining the
    /// compact single-line form when it fits.
    pub(super) fn separator_join(
        &self, head: LayoutFragment<'arena>, separator: &'static str, body: LayoutFragment<'arena>,
        continuation_indent: isize,
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
    pub(super) fn append_aligned_separator(
        &self, document: RcDoc<'arena>, separator: &'static str,
    ) -> RcDoc<'arena> {
        let joined = document.clone().append(RcDoc::space()).append(RcDoc::text(separator));
        let broken = document.append(RcDoc::hardline()).append(RcDoc::text(separator));
        self.single_line_or(joined, broken)
    }

    /// Join two stages through their marker.
    ///
    /// An annotation marker belongs to the final line of its left-hand
    /// constituent, even when that constituent is multiline. A source break
    /// after the marker does not force the classifier onto another line;
    /// width still may. Binding classifiers retain their aligned separator
    /// tier because the preceding binding head is not one annotated pattern.
    pub(super) fn staged_join(
        &self, left: LayoutFragment<'arena>, right: LayoutFragment<'arena>,
        boundary: StagedBoundary,
    ) -> LayoutFragment<'arena> {
        let separator = boundary.marker();
        let intent = BoundaryIntent::between(left.anchors.last, right.anchors.first);
        let anchors = LayoutAnchors { first: left.anchors.first, last: right.anchors.last };
        if boundary == StagedBoundary::Annotation {
            let document =
                left.document.append(RcDoc::space()).append(RcDoc::text(separator)).append(
                    self.layout_boundary(
                        intent.blank_line_only(),
                        BoundaryLayout::aligned(""),
                        right,
                    ),
                );
            return LayoutFragment { document, anchors };
        }

        let inline_layout = BoundaryLayout::hanging("", self.indent());
        let aligned_layout = BoundaryLayout::nested("", self.indent());
        let inline_head =
            left.document.clone().append(RcDoc::space()).append(RcDoc::text(separator));
        let inline = self.single_line(inline_head).append(self.layout_boundary(
            intent,
            inline_layout,
            right.clone(),
        ));
        // An ordinary entity-to-entity break is satisfied by moving the
        // separator. A blank line still belongs between it and the right side.
        let aligned = left.document.append(
            RcDoc::hardline().append(RcDoc::text(separator)).append(self.layout_boundary(
                intent.blank_line_only(),
                aligned_layout,
                right,
            )),
        );
        let document = self.flexible(inline, aligned);
        LayoutFragment { document, anchors }
    }

    /// Join a type and its bindee through the `=` stage of a binding.
    ///
    /// The separator stays on the head's line when the whole stage fits,
    /// follows the type's final line when that line returns to the binding
    /// indentation (a delimited closer that returned to its opener), and
    /// otherwise breaks onto its own line at the binding indentation.
    /// A broken bindee hangs one level below the separator.
    pub(super) fn definition_join(
        &self, ty: LayoutFragment<'arena>, bindee: LayoutFragment<'arena>, binding_nesting: isize,
    ) -> LayoutFragment<'arena> {
        let intent = BoundaryIntent::between(ty.anchors.last, bindee.anchors.first);
        let anchors = LayoutAnchors { first: ty.anchors.first, last: bindee.anchors.last };
        let inline = self
            .single_line(ty.document.clone().append(RcDoc::space()).append(RcDoc::text("=")))
            .append(self.layout_boundary(
                intent,
                BoundaryLayout::hanging("", self.indent()),
                bindee.clone(),
            ));
        // A blank line between the type and the bindee belongs before the
        // separator, so it excludes the attached form.
        let attachable =
            !matches!(intent.resolve(&self.presentation.intentions), Some(BreakIntent::BlankLine));
        let attached = if attachable {
            ty.document
                .clone()
                .append(Self::at_binding_guard(binding_nesting))
                .append(RcDoc::text(" ="))
                .append(self.layout_boundary(
                    intent,
                    BoundaryLayout::hanging("", self.indent()),
                    bindee.clone(),
                ))
        } else {
            RcDoc::fail()
        };
        let aligned = ty.document.append(Self::separator_at_binding(
            "=",
            binding_nesting,
            self.layout_boundary(
                intent.blank_line_only(),
                BoundaryLayout::hanging("", self.indent()),
                bindee,
            ),
        ));
        let document = self.flexible(inline, self.flexible(attached, aligned));
        LayoutFragment { document, anchors }
    }

    /// Join an unannotated binding head to its definition.
    ///
    /// A one-line head keeps `=` beside it even when the bindee is multiline.
    /// Once the head itself becomes multiline, `=` aligns with the binding and
    /// the bindee starts one level below it.
    pub(super) fn untyped_definition_join(
        &self, head: LayoutFragment<'arena>, bindee: LayoutFragment<'arena>,
    ) -> LayoutFragment<'arena> {
        let intent = BoundaryIntent::between(head.anchors.last, bindee.anchors.first);
        let anchors = LayoutAnchors { first: head.anchors.first, last: bindee.anchors.last };
        let body_layout = BoundaryLayout::hanging("", self.indent());
        let probe = head.document.clone().append(RcDoc::text(" ="));
        let inline = self.single_line(probe.clone()).append(self.layout_boundary(
            intent,
            body_layout.clone(),
            bindee.clone(),
        ));
        let aligned = head
            .document
            .append(RcDoc::hardline())
            .append(RcDoc::text("="))
            .append(body_layout.place(self.expanded_placement(intent), bindee.document));
        let document = self.select_after_multiline_probe(probe, inline, aligned);
        LayoutFragment { document, anchors }
    }

    /// A guard that fails unless the current line starts at the given
    /// indentation, used to attach a separator only to lines that return to
    /// the binding level.
    pub(super) fn at_binding_guard(binding_nesting: isize) -> RcDoc<'arena> {
        DOC_ALLOCATOR
            .nesting(move |nesting| {
                let nesting = isize::try_from(nesting).unwrap_or(isize::MAX);
                if nesting == binding_nesting { RcDoc::nil() } else { RcDoc::fail() }
            })
            .into_doc()
    }

    /// A hardline-separated marker placed at the given indentation.
    pub(super) fn separator_at_binding(
        separator: &'static str, binding_nesting: isize, continuation: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        DOC_ALLOCATOR
            .nesting({
                let continuation = continuation.clone();
                move |nesting| {
                    let nesting = isize::try_from(nesting).unwrap_or(isize::MAX);
                    RcDoc::hardline()
                        .append(RcDoc::text(separator))
                        .append(continuation.clone())
                        .nest(binding_nesting.saturating_sub(nesting))
                }
            })
            .into_doc()
    }

    pub(super) fn infix_chain(&self, root: TermId, operator: InfixOperator) -> RcDoc<'arena> {
        let sites = operator
            .operands(self.arena, &self.arena.terms[&root])
            .expect("infix chains start and continue with their selected operator");
        let last = sites.len().saturating_sub(1);
        let mut operands = Vec::new();
        let mut boundaries = Vec::new();

        for (index, (operand, comment_site)) in sites.iter().copied().enumerate() {
            // Products are flat n-ary nodes, so every operand prints tight
            // enough to parenthesize a nested product; arrows stay
            // right-associative and print their final operand loosely.
            let precedence = if operator == InfixOperator::Product || index < last {
                operator.left_precedence()
            } else {
                operator.right_precedence()
            };
            let mut document = self.term_through(operand, precedence);
            let mut starts_with_comment =
                !self.presentation.trivia.leading_comments(operand.into()).is_empty();
            let mut comments_end_line = self.leading_comments_end_line(operand.into());
            if let Some(site) = comment_site {
                starts_with_comment |=
                    !self.presentation.trivia.leading_comments(site.into()).is_empty();
                comments_end_line |= self.leading_comments_end_line(site.into());
                document = self.with_leading_comments(site.into(), document);
            }
            operands.push(InfixOperand { document, starts_with_comment, comments_end_line });
            if index < last {
                boundaries.push(
                    BoundaryIntent::between(operand, sites[index + 1].0)
                        .resolve(&self.presentation.intentions),
                );
            }
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
        // A boundary breaks where the source broke it or where the following
        // operand's comments end their line.
        let forced_breaks = operands[1..]
            .iter()
            .zip(boundaries.iter().copied())
            .map(|(operand, intention)| operand.comments_end_line || self.forces_break(intention))
            .collect::<Vec<_>>();
        let mut table = operands[0].document.clone().nest(hanging_indent);
        for ((operand, intention), forced) in
            operands[1..].iter().zip(boundaries.iter().copied()).zip(forced_breaks.iter().copied())
        {
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
            let boundary = if forced { broken } else { self.flexible(inline, broken) };
            table = table.append(boundary);
        }
        let begins_after_same_line_comment = self
            .presentation
            .trivia
            .leading_comments(root.into())
            .last()
            .is_some_and(|comment| comment.separation_after() == LineSeparation::SameLine);
        let hanging = if begins_after_same_line_comment {
            Self::hang_before_current_column(table, hanging_indent_usize)
        } else {
            self.at_line_start(RcDoc::text(" ".repeat(hanging_indent_usize)).append(table))
        };

        if forced_breaks.contains(&true) {
            return hanging;
        }

        self.flexible(joined, hanging)
    }

    pub(super) fn hang_before_current_column(
        document: RcDoc<'arena>, hanging_indent: usize,
    ) -> RcDoc<'arena> {
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

    pub(super) fn single_line_or(
        &self, single_line: RcDoc<'arena>, multiline: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        let flat = single_line.clone();
        self.single_line(single_line).union(multiline).flat_alt(flat)
    }

    /// Restrict a candidate to the renderer's flat mode. An enclosing union
    /// can then select an expanded alternative without a separate fit pass.
    pub(super) fn single_line(&self, document: RcDoc<'arena>) -> RcDoc<'arena> {
        document.append(RcDoc::fail().flat_alt(RcDoc::nil())).group()
    }

    /// Choose a continuation from the layout of a prefix alone.
    ///
    /// A regular union measures its first alternative together with every
    /// document that follows it. That is normally desirable, but a stage
    /// marker belongs to the preceding head: an overlong body must not move
    /// the marker. Probe the prefix at its actual column and indentation to
    /// commit that choice before rendering either continuation.
    pub(super) fn select_after_multiline_probe(
        &self, probe: RcDoc<'arena>, single_line: RcDoc<'arena>, multiline: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        let line_width = self.options.line_width;
        DOC_ALLOCATOR
            .column(move |column| {
                let probe = probe.clone();
                let single_line = single_line.clone();
                let multiline = multiline.clone();
                DOC_ALLOCATOR
                    .nesting(move |nesting| {
                        let nesting = isize::try_from(nesting).unwrap_or(isize::MAX);
                        let positioned =
                            RcDoc::text(" ".repeat(column)).append(probe.clone().nest(nesting));
                        let mut rendered = String::new();
                        positioned.render_fmt(line_width, &mut rendered).unwrap();
                        if rendered.contains('\n') {
                            multiline.clone()
                        } else {
                            single_line.clone()
                        }
                    })
                    .into_doc()
            })
            .into_doc()
    }

    pub(super) fn separated_group_layout(
        &self, items: &[LayoutFragment<'arena>], separator: &'static str,
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

    pub(super) fn select_group_layout(&self, layout: GroupLayout<'arena>) -> RcDoc<'arena> {
        let GroupLayout { compact, retained, expanded, retains_break } = layout;
        let preferred = if retains_break { retained } else { self.single_line(compact.clone()) };
        let selected = preferred.union(expanded);
        if retains_break { selected } else { selected.flat_alt(compact) }
    }

    /// Keep a fitting telescope beside its head. A joined first row stays
    /// beside the head when the telescope becomes multiline, while the
    /// remaining rows hang one level below; the head stands alone only when
    /// the source broke the first row away or the row does not fit.
    /// Retained source breaks partition fitting parameter rows; canonical
    /// expansion gives every parameter its own row.
    pub(super) fn parameter_telescope(
        &self, head: RcDoc<'arena>, after_head: BoundaryIntent,
        parameters: &[LayoutFragment<'arena>],
    ) -> RcDoc<'arena> {
        let GroupLayout {
            compact: compact_parameters,
            retained: retained_parameters,
            expanded: expanded_parameters,
            retains_break: parameters_retain_break,
        } = self.separated_group_layout(parameters, "");
        let retained_after_head = self.retained_placement(after_head);
        let boundary = BoundaryLayout::hanging("", self.indent());
        let compact = head.clone().append(RcDoc::space()).append(compact_parameters);
        let retained = match retained_after_head {
            // A joined first row stays beside the head, and the remaining
            // rows keep their own layout without forcing the telescope into
            // the expanded form when a later row is multiline.
            | BoundaryPlacement::Joined => {
                let row_boundary = BoundaryLayout::aligned("");
                let rows = parameters.windows(2).fold(RcDoc::nil(), |document, pair| {
                    let [left, right] = pair else { unreachable!("windows contain two items") };
                    let intent = BoundaryIntent::between(left.anchors.last, right.anchors.first);
                    document.append(
                        row_boundary.place(self.retained_placement(intent), right.document.clone()),
                    )
                });
                head.clone()
                    .append(RcDoc::space())
                    .append(parameters.first().expect("telescopes are nonempty").document.clone())
                    .append(rows)
                    .nest(self.indent())
            }
            | BoundaryPlacement::Broken | BoundaryPlacement::BlankLine => {
                head.clone().append(boundary.place(retained_after_head, retained_parameters))
            }
        };
        let expanded =
            head.append(boundary.place(self.expanded_placement(after_head), expanded_parameters));
        self.select_group_layout(GroupLayout {
            compact,
            retained,
            expanded,
            retains_break: parameters_retain_break
                || retained_after_head != BoundaryPlacement::Joined,
        })
    }

    pub(super) fn delimited(
        &self, entity: Option<EntityId>, open: &'static str, items: Vec<LayoutFragment<'arena>>,
        separator: &'static str, close: &'static str,
    ) -> RcDoc<'arena> {
        self.delimited_with_spacing(entity, open, items, separator, close, DelimiterSpacing::Tight)
    }

    pub(super) fn delimited_with_spacing(
        &self, entity: Option<EntityId>, open: &'static str, items: Vec<LayoutFragment<'arena>>,
        separator: &'static str, close: &'static str, spacing: DelimiterSpacing,
    ) -> RcDoc<'arena> {
        if items.is_empty() {
            return RcDoc::text(open).append(RcDoc::text(close));
        }
        self.select_group_layout(
            self.delimited_group_layout(entity, open, items, separator, close, spacing),
        )
    }

    pub(super) fn delimited_group_layout(
        &self, entity: Option<EntityId>, open: &'static str, items: Vec<LayoutFragment<'arena>>,
        separator: &'static str, close: &'static str, spacing: DelimiterSpacing,
    ) -> GroupLayout<'arena> {
        debug_assert!(!items.is_empty());
        let anchors = LayoutAnchors {
            first: items.first().expect("delimited items are nonempty").anchors.first,
            last: items.last().expect("delimited items are nonempty").anchors.last,
        };
        let item_layout = self.separated_group_layout(&items, separator);
        let after_open = entity.map_or(BoundaryIntent::Canonical, |entity| {
            // A group whose content anchor is the entity itself spans no
            // source gap, because the open delimiter is grammar-owned: the
            // retained break comes from the delimiter boundary recorded for
            // an existential binder instead.
            if entity == anchors.first
                && let EntityId::Pat(parameter) = entity
            {
                BoundaryIntent::after_existential_open(parameter)
            } else {
                BoundaryIntent::after_start(entity, anchors.first)
            }
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
        GroupLayout {
            compact,
            retained,
            expanded,
            retains_break: items_retain_break
                || retained_after_open != BoundaryPlacement::Joined
                || retained_before_close != BoundaryPlacement::Joined,
        }
    }

    pub(super) fn annotation(
        &self, entity: EntityId, term: LayoutFragment<'arena>, ty: TermId, parenthesized: bool,
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
    pub(super) fn should_elide_parentheses(&self, entity: EntityId, inner: EntityId) -> bool {
        let carries_break = self.forces_break(
            self.presentation.intentions.at(LayoutBoundary::after_start(entity, inner)),
        ) || self.forces_break(
            self.presentation.intentions.at(LayoutBoundary::before_end(inner, entity)),
        );
        self.options.parentheses == Parentheses::Minimal && !carries_break
    }

    /// Applications already own a compact-or-hanging layout boundary, so a
    /// singleton wrapper does not contribute an additional grouping choice.
    pub(super) fn term_layout_subsumes_group(&self, term: TermId) -> bool {
        match &self.arena.terms[&term] {
            | Term::SourceBoundary(SourceBoundary(inner))
            | Term::SignatureBoundary(SignatureBoundary(inner)) => {
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
}
