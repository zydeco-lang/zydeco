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

use crate::metadata::FormatMeta;

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
}

#[derive(Copy, Clone)]
enum ScopedForm {
    Function,
    Pi,
    Forall,
    Sigma,
}

struct ScopeTelescope<Parameter> {
    parameters: Vec<Parameter>,
    body: TermId,
}

impl StagedBoundary {
    fn marker(self) -> &'static str {
        match self {
            | Self::Annotation | Self::BindingType => ":",
        }
    }
}

impl ScopedForm {
    fn keyword(self) -> &'static str {
        match self {
            | Self::Function => "fn",
            | Self::Pi => "pi",
            | Self::Forall => "forall",
            | Self::Sigma => "sigma",
        }
    }

    fn marker(self) -> &'static str {
        match self {
            | Self::Function => "=>",
            | Self::Pi | Self::Forall | Self::Sigma => ".",
        }
    }

    fn body_precedence(self) -> TermPrecedence {
        match self {
            | Self::Function => TermPrecedence::Binder,
            | Self::Pi | Self::Forall | Self::Sigma => TermPrecedence::Quantifier,
        }
    }

    fn split(self, term: &Term) -> Option<(CoPatId, TermId)> {
        match (self, term) {
            | (Self::Function, Term::Abs(Abs(parameter, body)))
            | (Self::Pi, Term::Pi(Pi(parameter, body)))
            | (Self::Forall, Term::Forall(Forall(parameter, body)))
            | (Self::Sigma, Term::Sigma(Sigma(parameter, body))) => Some((*parameter, *body)),
            | _ => None,
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

    /// Render one subtree with different policy over the same arena.
    fn scoped(&self, options: PrettyOptions) -> Self {
        Self {
            arena: self.arena,
            grammar: GrammarContext::new(self.arena),
            punning: Punning::new(self.arena),
            options,
        }
    }

    fn indent(&self) -> isize {
        self.options.indent.nesting()
    }

    /// Render one complete source unit with a trailing newline.
    pub fn render_unit(&self, unit: SourceUnit) -> String {
        let document = self.with_trailing_comments(unit.root.into(), unit.pretty(self));
        self.render_doc(document.append(RcDoc::hardline()))
    }

    /// Render one term without adding a trailing newline.
    pub fn render_term(&self, term: TermId) -> String {
        let document = self.with_trailing_comments(term.into(), term.pretty(self));
        self.render_doc(document)
    }

    /// Render one pattern without adding a trailing newline.
    pub fn render_pattern(&self, pattern: PatId) -> String {
        let document = self.with_trailing_comments(pattern.into(), pattern.pretty(self));
        self.render_doc(document)
    }

    /// Render one copattern spine without adding a trailing newline.
    pub fn render_copattern(&self, pattern: CoPatId) -> String {
        let document = self.with_trailing_comments(pattern.into(), pattern.pretty(self));
        self.render_doc(document)
    }

    fn render_doc(&self, document: RcDoc<'arena>) -> String {
        let mut output = String::new();
        document.render_fmt(self.options.line_width, &mut output).unwrap();
        output
    }

    fn with_leading_comments(&self, entity: EntityId, document: RcDoc<'arena>) -> RcDoc<'arena> {
        self.with_comments(self.arena.trivia.leading_comments(entity), document)
    }

    fn with_before_arm_comments(&self, entity: EntityId, document: RcDoc<'arena>) -> RcDoc<'arena> {
        self.with_comments(self.arena.trivia.before_arm_comments(entity), document)
    }

    fn with_comments(
        &self, comments: &'arena [LeadingComment], document: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        comments
            .iter()
            .fold(RcDoc::nil(), |prefix, comment| {
                prefix
                    .append(if comment.comment().as_text().is_some() {
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

    fn with_trailing_comments(&self, entity: EntityId, document: RcDoc<'arena>) -> RcDoc<'arena> {
        self.arena.trivia.trailing_comments(entity).iter().fold(document, |document, comment| {
            let separation = if comment.comment().as_text().is_some()
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

    fn comment(&self, comment: &'arena SurfaceComment) -> RcDoc<'arena> {
        match comment {
            | SurfaceComment::Text(comment) => self.marked_comment_lines("--|", &comment.text),
            | SurfaceComment::Line(comment) => self.marked_comment_lines("--", &comment.text),
            | SurfaceComment::Block(comment) => self.block_comment(comment),
        }
    }

    fn marked_comment_lines(&self, marker: &'static str, text: &'arena str) -> RcDoc<'arena> {
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

    fn block_comment(&self, comment: &'arena BlockComment) -> RcDoc<'arena> {
        RcDoc::intersperse(comment.text.split('\n').map(RcDoc::text), RcDoc::hardline())
    }

    fn line_separation(&self, separation: LineSeparation) -> RcDoc<'arena> {
        match separation {
            | LineSeparation::SameLine => RcDoc::space(),
            | LineSeparation::NextLine => RcDoc::hardline(),
            | LineSeparation::BlankLine => BoundaryLayout::blank_line(),
        }
    }

    /// Whether an observed blank line must appear at a boundary under the
    /// configured layout-intentions policy.
    fn preserves_blank_line(&self, intention: Option<BreakIntent>) -> bool {
        matches!(
            self.options.layout_intentions,
            LayoutIntentions::Preserve | LayoutIntentions::BlankLinesOnly
        ) && intention == Some(BreakIntent::BlankLine)
    }

    /// Whether an observed separation must appear in the output under the
    /// configured layout-intentions policy. `Preserve` keeps every break,
    /// `BlankLinesOnly` keeps only blank lines, and `Ignore` keeps neither.
    fn forces_break(&self, intention: Option<BreakIntent>) -> bool {
        match (self.options.layout_intentions, intention) {
            | (LayoutIntentions::Ignore, _) => false,
            | (LayoutIntentions::BlankLinesOnly, Some(BreakIntent::BlankLine)) => true,
            | (LayoutIntentions::BlankLinesOnly, _) => false,
            | (LayoutIntentions::Preserve, Some(intention)) => intention.requires_line_break(),
            | (LayoutIntentions::Preserve, None) => false,
        }
    }

    fn mandatory_line_break(&self, intention: Option<BreakIntent>) -> RcDoc<'arena> {
        if self.preserves_blank_line(intention) {
            BoundaryLayout::blank_line()
        } else {
            RcDoc::hardline()
        }
    }

    fn retained_placement(&self, intent: BoundaryIntent) -> BoundaryPlacement {
        let intention = intent.resolve(self.arena);
        if self.preserves_blank_line(intention) {
            return BoundaryPlacement::BlankLine;
        }
        if self.forces_break(intention) {
            return BoundaryPlacement::Broken;
        }
        BoundaryPlacement::Joined
    }

    fn expanded_placement(&self, intent: BoundaryIntent) -> BoundaryPlacement {
        let intention = intent.resolve(self.arena);
        if self.preserves_blank_line(intention) {
            BoundaryPlacement::BlankLine
        } else {
            BoundaryPlacement::Broken
        }
    }

    fn layout_boundary(
        &self, intent: BoundaryIntent, layout: BoundaryLayout<'arena>, continuation: RcDoc<'arena>,
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
        &self, intent: BoundaryIntent, layout: BoundaryLayout<'arena>,
        continuation: LayoutFragment<'arena>,
    ) -> RcDoc<'arena> {
        self.layout_boundary(intent, layout, continuation.document)
    }

    fn prefixed(
        &self, enclosing: impl Into<EntityId>, prefix: &'static str,
        layout: BoundaryLayout<'arena>, child: LayoutFragment<'arena>,
    ) -> RcDoc<'arena> {
        RcDoc::text(prefix).append(self.fragment_boundary(
            BoundaryIntent::after_start(enclosing, child.anchors.first),
            layout,
            child,
        ))
    }

    fn flexible(&self, joined: RcDoc<'arena>, broken: RcDoc<'arena>) -> RcDoc<'arena> {
        // Expose the joined projection to enclosing groups while retaining a
        // complete broken alternative for a boundary whose own group overflows.
        joined.clone().union(broken).flat_alt(joined)
    }

    fn join(
        &self, left: LayoutFragment<'arena>, layout: BoundaryLayout<'arena>,
        right: LayoutFragment<'arena>,
    ) -> LayoutFragment<'arena> {
        let intent = BoundaryIntent::between(left.anchors.last, right.anchors.first);
        let anchors = LayoutAnchors { first: left.anchors.first, last: right.anchors.last };
        let document = left.document.append(self.layout_boundary(intent, layout, right.document));
        LayoutFragment { document, anchors }
    }

    fn separated(
        &self, items: Vec<LayoutFragment<'arena>>, layout: BoundaryLayout<'arena>,
    ) -> Option<LayoutFragment<'arena>> {
        items.into_iter().rev().reduce(|right, left| self.join(left, layout.clone(), right))
    }

    fn grouped_join(
        &self, left: LayoutFragment<'arena>, before_boundary: RcDoc<'arena>,
        right: LayoutFragment<'arena>, continuation_indent: isize,
    ) -> LayoutFragment<'arena> {
        let intent = BoundaryIntent::between(left.anchors.last, right.anchors.first);
        let intention = intent.resolve(self.arena);
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

    /// Separate a multiline binder head from the token that introduces its
    /// scope, while retaining the compact single-line form when it fits.
    fn scoped_join(
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
    ///
    /// A joined boundary keeps the continuation at the separator's level so
    /// its own layout families measure from there; only a broken boundary
    /// hangs the continuation one level below.
    fn staged_join(
        &self, left: LayoutFragment<'arena>, right: LayoutFragment<'arena>,
        boundary: StagedBoundary,
    ) -> LayoutFragment<'arena> {
        let separator = boundary.marker();
        let (inline_layout, aligned_layout) = match boundary {
            | StagedBoundary::Annotation => {
                (BoundaryLayout::aligned(""), BoundaryLayout::nested("", self.indent()))
            }
            | StagedBoundary::BindingType => (
                BoundaryLayout::hanging("", self.indent()),
                BoundaryLayout::nested("", self.indent()),
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
        let aligned =
            left.document.append(RcDoc::hardline().append(RcDoc::text(separator)).append(
                self.layout_boundary(intent.blank_line_only(), aligned_layout, right.document),
            ));
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
    fn definition_join(
        &self, ty: LayoutFragment<'arena>, bindee: LayoutFragment<'arena>, binding_nesting: isize,
    ) -> LayoutFragment<'arena> {
        let intent = BoundaryIntent::between(ty.anchors.last, bindee.anchors.first);
        let anchors = LayoutAnchors { first: ty.anchors.first, last: bindee.anchors.last };
        let inline = self
            .single_line(ty.document.clone().append(RcDoc::space()).append(RcDoc::text("=")))
            .append(self.layout_boundary(
                intent,
                BoundaryLayout::hanging("", self.indent()),
                bindee.document.clone(),
            ));
        // A blank line between the type and the bindee belongs before the
        // separator, so it excludes the attached form.
        let attachable = !matches!(intent.resolve(self.arena), Some(BreakIntent::BlankLine));
        let attached = if attachable {
            ty.document
                .clone()
                .append(Self::at_binding_guard(binding_nesting))
                .append(RcDoc::text(" ="))
                .append(self.layout_boundary(
                    intent,
                    BoundaryLayout::hanging("", self.indent()),
                    bindee.document.clone(),
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
                bindee.document,
            ),
        ));
        let document = self.flexible(inline, self.flexible(attached, aligned));
        LayoutFragment { document, anchors }
    }

    /// A guard that fails unless the current line starts at the given
    /// indentation, used to attach a separator only to lines that return to
    /// the binding level.
    fn at_binding_guard(binding_nesting: isize) -> RcDoc<'arena> {
        DOC_ALLOCATOR
            .nesting(move |nesting| {
                let nesting = isize::try_from(nesting).unwrap_or(isize::MAX);
                if nesting == binding_nesting { RcDoc::nil() } else { RcDoc::fail() }
            })
            .into_doc()
    }

    /// A hardline-separated marker placed at the given indentation.
    fn separator_at_binding(
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

    fn infix_chain(&self, root: TermId, operator: InfixOperator) -> RcDoc<'arena> {
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
            let boundary =
                if self.forces_break(intention) { broken } else { self.flexible(inline, broken) };
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

        let preserves_break =
            boundaries.iter().flatten().any(|intention| self.forces_break(Some(*intention)));
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

    fn select_group_layout(&self, layout: GroupLayout<'arena>) -> RcDoc<'arena> {
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
    fn parameter_telescope(
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

    fn delimited(
        &self, entity: Option<EntityId>, open: &'static str, items: Vec<LayoutFragment<'arena>>,
        separator: &'static str, close: &'static str,
    ) -> RcDoc<'arena> {
        self.delimited_with_spacing(entity, open, items, separator, close, DelimiterSpacing::Tight)
    }

    fn delimited_with_spacing(
        &self, entity: Option<EntityId>, open: &'static str, items: Vec<LayoutFragment<'arena>>,
        separator: &'static str, close: &'static str, spacing: DelimiterSpacing,
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
    fn should_elide_parentheses(&self, entity: EntityId, inner: EntityId) -> bool {
        let carries_break = self
            .forces_break(self.arena.intentions.at(LayoutBoundary::after_start(entity, inner)))
            || self
                .forces_break(self.arena.intentions.at(LayoutBoundary::before_end(inner, entity)));
        self.options.parentheses == Parentheses::Minimal && !carries_break
    }

    /// Applications already own a compact-or-hanging layout boundary, so a
    /// singleton wrapper does not contribute an additional grouping choice.
    fn term_layout_subsumes_group(&self, term: TermId) -> bool {
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

    fn pattern(&self, pattern: PatId) -> RcDoc<'arena> {
        self.pattern_with_requirement(pattern, PatternRequirement::Pattern)
    }

    fn annotated_pattern(&self, pattern: PatId) -> RcDoc<'arena> {
        self.pattern_with_requirement(pattern, PatternRequirement::Annotated)
    }

    fn pattern_with_requirement(
        &self, pattern: PatId, requirement: PatternRequirement,
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

    fn copattern(&self, pattern: CoPatId) -> RcDoc<'arena> {
        match &self.arena.copats[&pattern] {
            | CoPattern::Pat(inner) => {
                self.with_leading_comments(pattern.into(), self.pattern(*inner))
            }
            | CoPattern::Dtor(name) => {
                self.with_leading_comments(pattern.into(), self.destructor(name))
            }
            | CoPattern::App(_) => {
                let parameters = self.copattern_parameters(pattern);
                self.select_group_layout(self.separated_group_layout(&parameters, ""))
                    .nest(self.indent())
            }
        }
    }

    fn copattern_parameters(&self, pattern: CoPatId) -> Vec<LayoutFragment<'arena>> {
        let CoPattern::App(Appli(patterns)) = &self.arena.copats[&pattern] else {
            return vec![LayoutFragment::entity(pattern.into(), self.copattern(pattern))];
        };
        let mut parameters =
            patterns.iter().flat_map(|pattern| self.copattern_parameters(*pattern));
        let first = parameters.next().expect("copattern applications are nonempty");
        let LayoutFragment { document, anchors } = first;
        let first = LayoutFragment {
            document: self.with_leading_comments(pattern.into(), document),
            anchors: LayoutAnchors { first: pattern.into(), last: anchors.last },
        };
        std::iter::once(first).chain(parameters).collect()
    }

    fn term(&self, term: TermId) -> RcDoc<'arena> {
        self.term_with_requirement(term, TermRequirement::Any)
    }

    fn annotated_term(&self, term: TermId) -> RcDoc<'arena> {
        self.term_with_requirement(term, TermRequirement::Annotated)
    }

    fn term_through(&self, term: TermId, precedence: TermPrecedence) -> RcDoc<'arena> {
        self.term_with_requirement(term, TermRequirement::Through(precedence))
    }

    fn term_fragment(&self, term: TermId) -> LayoutFragment<'arena> {
        self.term_fragment_with_requirement(term, TermRequirement::Any)
    }

    fn annotated_term_fragment(&self, term: TermId) -> LayoutFragment<'arena> {
        self.term_fragment_with_requirement(term, TermRequirement::Annotated)
    }

    fn term_through_fragment(
        &self, term: TermId, precedence: TermPrecedence,
    ) -> LayoutFragment<'arena> {
        self.term_fragment_with_requirement(term, TermRequirement::Through(precedence))
    }

    fn term_fragment_with_requirement(
        &self, term: TermId, requirement: TermRequirement,
    ) -> LayoutFragment<'arena> {
        LayoutFragment::entity(term.into(), self.term_with_requirement(term, requirement))
    }

    fn term_with_requirement(&self, term: TermId, requirement: TermRequirement) -> RcDoc<'arena> {
        if let Term::SourceBoundary(SourceBoundary(inner))
        | Term::SignatureBoundary(SignatureBoundary(inner)) = &self.arena.terms[&term]
        {
            let document = self.term_with_requirement(*inner, requirement);
            return self.with_leading_comments(term.into(), document);
        }
        let document = match &self.arena.terms[&term] {
            | Term::Meta(MetaT(meta, inner)) => match meta.specialize::<FormatMeta>() {
                | Ok(Some(directive)) => self.format_annotated(term, meta, *inner, directive),
                | Ok(None) | Err(_) => match &self.arena.terms[inner] {
                    // A commentless hole payload collapses into the parenthesized sugar.
                    // A commented hole keeps the bracket form so its comments survive.
                    | Term::Hole(_)
                        if self.arena.trivia.leading_comments((*inner).into()).is_empty()
                            && self.arena.trivia.trailing_comments((*inner).into()).is_empty() =>
                    {
                        RcDoc::text("@(")
                            .append(RcDoc::text(meta.to_string()))
                            .append(RcDoc::text(")"))
                    }
                    | _ => RcDoc::text("@[").append(RcDoc::text(meta.to_string())).append(
                        self.fragment_boundary(
                            BoundaryIntent::after_start(term, *inner),
                            BoundaryLayout::aligned("]"),
                            self.term_through_fragment(*inner, TermPrecedence::Binder),
                        ),
                    ),
                },
            },
            | Term::SourceBoundary(_) | Term::SignatureBoundary(_) => {
                unreachable!("source boundaries return before rendering")
            }
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
            | Term::Abs(_) => self.scoped_form(term, ScopedForm::Function),
            | Term::App(Appli(terms)) => self.application(terms),
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
            | Term::Pi(_) => self.scoped_form(term, ScopedForm::Pi),
            | Term::Forall(_) => self.scoped_form(term, ScopedForm::Forall),
            | Term::Arrow(_) => self.infix_chain(term, InfixOperator::Arrow),
            | Term::Sigma(_) => self.scoped_form(term, ScopedForm::Sigma),
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
            | Term::Do(Bind { binder, bindee, tail }) => self.sequence_block(
                self.prefixed(
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
            ),
            | Term::Let(GenLet { binding, tail }) => self.sequence_block(
                RcDoc::text("let")
                    .append(self.placed_binding(term, binding, Placement::In))
                    .append(self.sequence_tail(binding.bindee.into(), *tail)),
            ),
            | Term::Param(Param { binder, placement, tail }) => self.sequence_block(
                self.prefixed(
                    term,
                    "param",
                    BoundaryLayout::aligned(""),
                    LayoutFragment::entity((*binder).into(), self.annotated_pattern(*binder)),
                )
                .append(RcDoc::text(" "))
                .append(self.placement(*placement))
                .append(self.sequence_tail((*binder).into(), *tail)),
            ),
            | Term::ContextBind(ContextBind { mode, binding, placement, tail }) => {
                let keyword = match mode {
                    | DefinitionMode::Transparent => "let",
                    | DefinitionMode::Nominal => "def",
                };
                self.sequence_block(
                    RcDoc::text(keyword)
                        .append(self.placed_binding(term, binding, *placement))
                        .append(self.sequence_tail(binding.bindee.into(), *tail)),
                )
            }
            | Term::Block(Block(body)) => self.block(term, "begin", *body, "end"),
            | Term::Data(Data { arms }) => self.data(term, arms),
            | Term::CoData(CoData { arms }) => self.codata(term, arms),
            | Term::Ctor(Ctor(name, body)) => {
                self.constructor(name).append(self.term_constructor_argument(*body))
            }
            | Term::Match(Match { scrut, arms }) => self.matcher(term, *scrut, arms),
            | Term::CoMatch(CoMatchParam { arms }) => self.comatcher(term, arms),
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

    /// Render a `@[format(...)]` annotation, applying its directive to the
    /// payload and everything inside it. The annotation-to-payload boundary
    /// follows the directive's policy as well.
    ///
    /// Indentation, layout intentions, and parenthesis treatment shape the
    /// payload document directly. A width change instead pre-renders the
    /// payload at its own width and embeds the result, because the document
    /// renderer applies one width to the whole document.
    fn format_annotated(
        &self, term: TermId, meta: &'arena Meta, inner: TermId, directive: FormatMeta,
    ) -> RcDoc<'arena> {
        let scoped = self.scoped(self.options.with_format_meta(&directive));
        let prefix = self.annotation_prefix(meta);
        let payload = scoped.term_through_fragment(inner, TermPrecedence::Binder);
        if scoped.options.line_width == self.options.line_width {
            return prefix.append(scoped.fragment_boundary(
                BoundaryIntent::after_start(term, inner),
                BoundaryLayout::aligned(""),
                payload,
            ));
        }
        let rendered = scoped.render_doc(payload.document);
        if rendered.contains('\n') {
            return prefix.append(self.embedded_block(&rendered));
        }
        prefix.append(scoped.fragment_boundary(
            BoundaryIntent::after_start(term, inner),
            BoundaryLayout::aligned(""),
            LayoutFragment::entity(inner.into(), RcDoc::text(rendered)),
        ))
    }

    /// The complete `@[...]` text of one annotation.
    fn annotation_prefix(&self, meta: &Meta) -> RcDoc<'arena> {
        RcDoc::text("@[").append(RcDoc::text(meta.to_string())).append(RcDoc::text("]"))
    }

    /// Place a pre-rendered multiline payload below its annotation, keeping
    /// its relative indentation and its empty lines free of trailing
    /// whitespace.
    fn embedded_block(&self, rendered: &str) -> RcDoc<'arena> {
        rendered.split('\n').enumerate().fold(RcDoc::nil(), |document, (index, line)| {
            if index == 0 {
                document.append(RcDoc::hardline()).append(RcDoc::text(line.to_owned()))
            } else if line.is_empty() {
                document.append(Self::empty_embedded_line())
            } else {
                document.append(RcDoc::hardline()).append(RcDoc::text(line.to_owned()))
            }
        })
    }

    /// One hardline followed by an empty line, leaving the empty line free of
    /// trailing whitespace regardless of the ambient nesting.
    fn empty_embedded_line() -> RcDoc<'arena> {
        RcDoc::nesting(|nesting| {
            let nesting = isize::try_from(nesting).unwrap_or(isize::MAX);
            RcDoc::hardline().append(RcDoc::text("").nest(-nesting))
        })
    }

    fn application(&self, terms: &[TermId]) -> RcDoc<'arena> {
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

    fn named_term(&self, term: TermId, field: &FieldName, inner: TermId) -> RcDoc<'arena> {
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

    fn term_constructor_argument(&self, body: TermId) -> RcDoc<'arena> {
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

    fn pattern_constructor_argument(&self, body: PatId) -> RcDoc<'arena> {
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

    fn named_pattern(&self, pattern: PatId, field: &FieldName, inner: PatId) -> RcDoc<'arena> {
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

    fn projection_pattern(&self, pattern: PatId, field: &FieldName, inner: PatId) -> RcDoc<'arena> {
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

    fn scope_boundary_allows_merging(
        &self, parameter: impl Into<EntityId>, nested: TermId,
    ) -> bool {
        self.arena.trivia.leading_comments(nested.into()).is_empty()
            && !self.forces_break(BoundaryIntent::between(parameter, nested).resolve(self.arena))
    }

    fn scoped_telescope(&self, root: TermId, form: ScopedForm) -> ScopeTelescope<CoPatId> {
        let layers = std::iter::successors(Some(root), |current| {
            let (parameter, nested) = form.split(&self.arena.terms[current])?;
            form.split(&self.arena.terms[&nested])?;
            self.scope_boundary_allows_merging(parameter, nested).then_some(nested)
        })
        .map(|scope| {
            form.split(&self.arena.terms[&scope])
                .expect("scope telescopes contain only their selected form")
        })
        .collect::<Vec<_>>();
        let body = layers.last().expect("scope telescopes are nonempty").1;
        let parameters = layers.into_iter().map(|(parameter, _)| parameter).collect();
        ScopeTelescope { parameters, body }
    }

    fn scoped_form(&self, term: TermId, form: ScopedForm) -> RcDoc<'arena> {
        let ScopeTelescope { parameters, body } = self.scoped_telescope(term, form);
        let first = *parameters.first().expect("scoped forms contain at least one parameter");
        let last = *parameters.last().expect("scoped forms contain at least one parameter");
        let parameters = parameters
            .into_iter()
            .flat_map(|parameter| self.copattern_parameters(parameter))
            .collect::<Vec<_>>();
        let head = LayoutFragment {
            document: self.parameter_telescope(
                RcDoc::text(form.keyword()),
                BoundaryIntent::after_start(term, first),
                &parameters,
            ),
            anchors: LayoutAnchors { first: first.into(), last: last.into() },
        };
        self.scoped_join(
            head,
            form.marker(),
            self.term_through_fragment(body, form.body_precedence()),
            self.indent(),
        )
        .document
    }

    fn existential_telescope(
        &self, first: &'arena Exists,
    ) -> ScopeTelescope<&'arena ExistentialParameter> {
        let layers = std::iter::successors(Some(first), |current| {
            let parameter = current.parameters.last()?;
            let Term::Exists(nested) = &self.arena.terms[&current.body] else {
                return None;
            };
            self.scope_boundary_allows_merging(parameter.binder, current.body).then_some(nested)
        })
        .collect::<Vec<_>>();
        let body = layers.last().expect("existential telescopes are nonempty").body;
        let parameters = layers.into_iter().flat_map(|exists| exists.parameters.iter()).collect();
        ScopeTelescope { parameters, body }
    }

    fn exists(&self, term: TermId, exists: &'arena Exists) -> RcDoc<'arena> {
        let ScopeTelescope { parameters, body } = self.existential_telescope(exists);
        let Some((first, rest)) = parameters.split_first() else {
            unreachable!("the parser requires at least one existential parameter")
        };
        let last = parameters.last().expect("existentials contain at least one parameter");
        let parameters = std::iter::once(self.existential_parameter(first))
            .chain(rest.iter().map(|parameter| self.existential_parameter(parameter)))
            .collect::<Vec<_>>();
        let head = LayoutFragment {
            document: self.parameter_telescope(
                RcDoc::text("exists"),
                BoundaryIntent::before_existential_parameter(term, first.binder),
                &parameters,
            ),
            anchors: LayoutAnchors { first: first.binder.into(), last: last.binder.into() },
        };
        self.scoped_join(head, ".", self.term_fragment(body), self.indent()).document
    }

    fn existential_parameter(&self, parameter: &ExistentialParameter) -> LayoutFragment<'arena> {
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

    fn manifest_parameter_view(&self, pattern: PatId) -> Option<ManifestParameterView<'arena>> {
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
        &self, view: ManifestParameterView<'arena>, entity: PatId,
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

    /// Render one binding while capturing its indentation, so the
    /// stage-closing placement can return to it regardless of how deeply the
    /// bindee nests its own layout.
    fn placed_binding(
        &self, enclosing: TermId, binding: &'arena GenBind<TermId>, placement: Placement,
    ) -> RcDoc<'arena> {
        let arena = self.arena;
        let options = self.options;
        DOC_ALLOCATOR
            .nesting(move |binding_nesting| {
                let binding_nesting = isize::try_from(binding_nesting).unwrap_or(isize::MAX);
                let formatter = PrettyFormatter {
                    arena,
                    grammar: GrammarContext::new(arena),
                    punning: Punning::new(arena),
                    options,
                };
                formatter.placed_binding_at(enclosing, binding, placement, binding_nesting)
            })
            .into_doc()
    }

    fn placed_binding_at(
        &self, enclosing: TermId, binding: &GenBind<TermId>, placement: Placement,
        binding_nesting: isize,
    ) -> RcDoc<'arena> {
        let GenBind { fix, comp, binder, params, ty, bindee } = binding;
        let modifiers = [(*comp).then_some("!"), (*fix).then_some("fix")]
            .into_iter()
            .flatten()
            .map(RcDoc::text);
        let head = RcDoc::intersperse(
            modifiers.chain(std::iter::once(self.pattern(*binder))),
            RcDoc::space(),
        );
        let head = match params {
            | Some(params) => {
                let parameters = self.copattern_parameters(*params);
                LayoutFragment {
                    document: self.parameter_telescope(
                        head,
                        BoundaryIntent::between(*binder, *params),
                        &parameters,
                    ),
                    anchors: LayoutAnchors { first: (*binder).into(), last: (*params).into() },
                }
            }
            | None => LayoutFragment::entity((*binder).into(), head),
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
        let bindee =
            self.bindee_with_placement(self.term_fragment(*bindee), placement, binding_nesting);
        match ty {
            | Some(ty) => {
                let assignment =
                    self.definition_join(self.term_fragment(*ty), bindee, binding_nesting);
                self.staged_join(head, assignment, StagedBoundary::BindingType).document
            }
            | None => {
                self.join(head, BoundaryLayout::hanging(" =", self.indent()), bindee).document
            }
        }
    }

    fn sequence_tail(&self, before: EntityId, tail: TermId) -> RcDoc<'arena> {
        self.mandatory_line_break(BoundaryIntent::between(before, tail).resolve(self.arena))
            .append(self.term_through(tail, TermPrecedence::Binder))
    }

    /// Sequence bindings are block-like: their tail marker always breaks
    /// onto a new line, so the whole binding must start on its own line.
    /// The guard fails mid-line and lets an enclosing boundary fall back to
    /// its broken form instead of anchoring the binding to a running line.
    fn sequence_block(&self, document: RcDoc<'arena>) -> RcDoc<'arena> {
        RcDoc::column(|column| {
            RcDoc::nesting(
                move |nesting| {
                    if column == nesting { RcDoc::nil() } else { RcDoc::fail() }
                },
            )
        })
        .append(document)
    }

    /// Attach the stage-closing placement marker to a bindee. Mirroring the
    /// `=` stage separator, the marker has three tiers: it stays on a
    /// single-line bindee,
    /// follows the bindee's final line when that line returns to the binding
    /// indentation (a delimited closer), and otherwise breaks onto its own
    /// line at the binding indentation, wherever the bindee's own layout
    /// happens to nest.
    fn bindee_with_placement(
        &self, bindee: LayoutFragment<'arena>, placement: RcDoc<'arena>, binding_nesting: isize,
    ) -> LayoutFragment<'arena> {
        let marker = DOC_ALLOCATOR
            .nesting({
                let placement = placement.clone();
                move |bindee_nesting| {
                    let placement = placement.clone();
                    let bindee_nesting = isize::try_from(bindee_nesting).unwrap_or(isize::MAX);
                    RcDoc::hardline()
                        .append(placement)
                        .nest(binding_nesting.saturating_sub(bindee_nesting))
                }
            })
            .into_doc();
        bindee.map_document(|document| {
            let inline = self.after_line_start(
                document.clone().append(RcDoc::space()).append(placement.clone()),
            );
            let attached = document
                .clone()
                .append(Self::at_binding_guard(binding_nesting))
                .append(RcDoc::space())
                .append(placement);
            self.single_line(inline.clone())
                .union(attached.union(document.append(marker)))
                .flat_alt(inline)
        })
    }

    fn placement(&self, placement: Placement) -> RcDoc<'arena> {
        RcDoc::text(match placement {
            | Placement::In => "in",
            | Placement::That => "that",
        })
    }

    fn block(
        &self, enclosing: TermId, keyword: &'static str, body: TermId, end: &'static str,
    ) -> RcDoc<'arena> {
        RcDoc::text(keyword)
            .append(
                self.mandatory_line_break(
                    BoundaryIntent::after_start(enclosing, body).resolve(self.arena),
                )
                .append(self.annotated_term(body))
                .nest(self.indent()),
            )
            .append(self.mandatory_line_break(
                BoundaryIntent::before_end(body, enclosing).resolve(self.arena),
            ))
            .append(RcDoc::text(end))
    }

    fn arm_block(
        &self, enclosing: EntityId, head: RcDoc<'arena>,
        arms: impl IntoIterator<Item = LayoutFragment<'arena>>,
    ) -> RcDoc<'arena> {
        let mut document = head;
        let mut previous = None;
        for arm in arms {
            let gap = match previous {
                | None => BoundaryIntent::after_start(enclosing, arm.anchors.first),
                | Some(previous) => BoundaryIntent::between(previous, arm.anchors.first),
            };
            document = document
                .append(self.mandatory_line_break(gap.resolve(self.arena)))
                .append(self.with_before_arm_comments(arm.anchors.first, arm.document));
            previous = Some(arm.anchors.last);
        }
        let before_end = previous.map_or_else(RcDoc::hardline, |last| {
            self.mandatory_line_break(
                BoundaryIntent::before_end(last, enclosing).resolve(self.arena),
            )
        });
        document.append(before_end).append(RcDoc::text("end"))
    }

    fn data(&self, enclosing: TermId, arms: &[DataArm]) -> RcDoc<'arena> {
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
        self.arm_block(enclosing.into(), RcDoc::text("data"), arms)
    }

    fn codata(&self, enclosing: TermId, arms: &[CoDataArm]) -> RcDoc<'arena> {
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
        self.arm_block(enclosing.into(), RcDoc::text("codata"), arms)
    }

    fn matcher(
        &self, term: TermId, scrutinee: TermId, arms: &[Matcher<PatId, TermId>],
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
        self.arm_block(term.into(), head, arms)
    }

    fn comatcher(&self, enclosing: TermId, arms: &[CoMatcherParam]) -> RcDoc<'arena> {
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
        self.arm_block(enclosing.into(), RcDoc::text("comatch"), arms)
    }

    fn definition(&self, definition: DefId) -> RcDoc<'arena> {
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
            | Literal::Integer(value) => format!("{value:?}"),
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
                    matches!(token.kind, LexicalTokenKind::Comment | LexicalTokenKind::TextBlock)
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
                "forall (A : Type) (B : Type) . A -> B\n",
            ),
            ("fn x => fn y => x", "fn x y => x\n"),
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
    fn aligns_sequential_binders_at_the_current_indentation() {
        let cases = [
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
    fn placement_tiers_mirror_the_definition_stage() {
        let cases = [
            // A single-line bindee keeps the marker on its own line.
            (
                concat!("begin\n", "  def value = item that\n", "  value\n", "end\n",),
                concat!("begin\n", "  def value = item that\n", "  value\n", "end\n",),
            ),
            // A multiline delimited bindee whose closer returns to the
            // binding level takes the attached tier.
            (
                concat!(
                    "begin\n",
                    "  let value = begin\n",
                    "  item\n",
                    "  end in value\n",
                    "end\n",
                ),
                concat!(
                    "begin\n",
                    "  let value = begin\n",
                    "    item\n",
                    "  end in\n",
                    "  value\n",
                    "end\n",
                ),
            ),
            // A broken bindee hangs one level below the binding, so the
            // marker breaks onto its own line at the binding indentation.
            (
                concat!("begin\n", "  let value =\n", "    item\n", "  in value\n", "end\n",),
                concat!("begin\n", "  let value =\n", "    item\n", "  in\n", "  value\n", "end\n",),
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
        });
    }

    #[test]
    fn placement_closes_at_the_binding_indentation() {
        let cases = [
            // A joined multiline bindee sits at the binding level; the
            // placement must not outdent below it.
            (
                concat!(
                    "begin\n",
                    "  let value = begin\n",
                    "  item\n",
                    "  end in value\n",
                    "end\n",
                ),
                concat!(
                    "begin\n",
                    "  let value = begin\n",
                    "    item\n",
                    "  end in\n",
                    "  value\n",
                    "end\n",
                ),
            ),
            // A broken bindee nests one level below the binding; the
            // placement returns to the binding level.
            (
                concat!("begin\n", "  let value =\n", "    item\n", "  in value\n", "end\n",),
                concat!("begin\n", "  let value =\n", "    item\n", "  in\n", "  value\n", "end\n",),
            ),
            // A multiline type breaks the definition stage, and its outdent
            // already returns the bindee to the binding level.
            (
                concat!(
                    "begin\n",
                    "  def branch : Thk (\n",
                    "    forall (B : CType) .\n",
                    "      Bool -> Thk B -> Thk B -> B\n",
                    "  ) = {\n",
                    "    fn (B : CType)\n",
                    "       condition\n",
                    "       when_true\n",
                    "       when_false =>\n",
                    "      match condition\n",
                    "      | +True() => ! when_true\n",
                    "      | +False() => ! when_false\n",
                    "      end\n",
                    "  } that\n",
                    "  branch\n",
                    "end\n",
                ),
                concat!(
                    "begin\n",
                    "  def branch : Thk (\n",
                    "    forall (B : CType) .\n",
                    "      Bool -> Thk B -> Thk B -> B\n",
                    "  ) = {\n",
                    "    fn (B : CType)\n",
                    "      condition\n",
                    "      when_true\n",
                    "      when_false\n",
                    "    =>\n",
                    "      match condition\n",
                    "      | +True() => ! when_true\n",
                    "      | +False() => ! when_false\n",
                    "      end\n",
                    "  } that\n",
                    "  branch\n",
                    "end\n",
                ),
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
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
                concat!("let value = begin\n", "  item\n", "end in\n", "value\n"),
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
    fn definition_separator_tiers_share_the_binding_indentation() {
        let cases = [
            // A delimited type whose closer returns to the binding level
            // takes the attached tier: `)` and `=` share the closer's line.
            (
                concat!(
                    "begin\n",
                    "  def f : (\n",
                    "    Result\n",
                    "  ) = value that\n",
                    "  f\n",
                    "end\n",
                ),
                concat!(
                    "begin\n",
                    "  def f : (\n",
                    "    Result\n",
                    "  ) = value that\n",
                    "  f\n",
                    "end\n",
                ),
            ),
            // A blank line between the type and the separator excludes the
            // attached tier, so the blank survives in the aligned form.
            (
                concat!(
                    "begin\n",
                    "  def f : (\n",
                    "    Result\n",
                    "  )\n",
                    "\n",
                    "  = value that\n",
                    "  f\n",
                    "end\n",
                ),
                concat!(
                    "begin\n",
                    "  def f : (\n",
                    "    Result\n",
                    "  )\n",
                    "  =\n",
                    "\n",
                    "    value\n",
                    "  that\n",
                    "  f\n",
                    "end\n",
                ),
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");
            assert!(formatted.lines().all(|line| line.trim_end() == line));

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
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
                    "    Result\n",
                    "  ) = value that\n",
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
                    "let f\n",
                    "  (first : FirstClassifier)\n",
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
        let parsed = ParsedSource::new("exists (Counter = ((Counter as Int64) : VType)) . Counter");

        assert_eq!(
            parsed.render(LayoutIntentions::Ignore),
            "exists (= Counter as Int64 : VType) . Counter\n"
        );
        let reparsed = ParsedSource::new(&parsed.render(LayoutIntentions::Ignore));
        assert_eq!(
            reparsed.render(LayoutIntentions::Ignore),
            "exists (= Counter as Int64 : VType) . Counter\n"
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
            "    (/VType; /CType; /Thk; /Ret; /Unit; /Int64; /String; /OS; /int64; /string; /stdio; /process) :\n",
            "    @(import(\"package.zy\"))\n",
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
            "    (/VType; /CType; /Thk; /Ret; /Unit; /Int64; /Float64; /Char; /String; /Bytes; /Reader; /Writer; /OS) :\n",
            "    @(import(\"package.zy\"))\n",
            "  ) in\n",
            "  _\n",
            "end\n",
        );
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render_with_options(PrettyOptions::default());

        assert!(formatted.lines().all(|line| line.len() <= 100), "{formatted}");
        assert!(formatted.contains("/OS\n    )\n    : @(import"), "{formatted}");
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        assert_eq!(formatted, reparsed.render_with_options(PrettyOptions::default()));
    }

    #[test]
    fn merges_joined_consecutive_scopes_into_one_telescope() {
        let cases = [
            ("fn a => fn b => body", "fn a b => body\n"),
            ("fn a b => fn c d => body", "fn a b c d => body\n"),
            ("pi (A : VType) . pi (B : VType) . Body", "pi (A : VType) (B : VType) . Body\n"),
            (
                "forall (A : VType) . forall (B : VType) . Body",
                "forall (A : VType) (B : VType) . Body\n",
            ),
            (
                "sigma (A : VType) . sigma (B : VType) . Body",
                "sigma (A : VType) (B : VType) . Body\n",
            ),
            (
                "exists (A : VType) . exists (B : VType) . Body",
                "exists (A : VType) (B : VType) . Body\n",
            ),
            (
                concat!(
                    "exists (A : VType) (B : VType) . ",
                    "exists (C : VType) (D : VType) . Body",
                ),
                "exists (A : VType) (B : VType) (C : VType) (D : VType) . Body\n",
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        });
    }

    #[test]
    fn preserves_a_nested_scope_when_its_introducer_starts_on_a_new_line() {
        let cases = [
            (
                concat!("fn a =>\n", "  fn b =>\n", "    body"),
                concat!("fn a =>\n", "  fn b =>\n", "    body\n"),
            ),
            (
                concat!("forall (A : VType) .\n", "  forall (B : VType) .\n", "    Body"),
                concat!("forall (A : VType) .\n", "  forall (B : VType) .\n", "    Body\n"),
            ),
            (
                concat!("exists (A : VType) .\n", "  exists (B : VType) .\n", "    Body"),
                concat!("exists (A : VType) .\n", "  exists (B : VType) .\n", "    Body\n"),
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        });
    }

    #[test]
    fn keeps_comments_and_distinct_scope_forms_at_telescope_boundaries() {
        let cases = [
            (
                "fn a => /- Keep the nested scope. -/ fn b => body",
                "fn a => /- Keep the nested scope. -/ fn b => body\n",
            ),
            (
                "pi (A : VType) . forall (B : VType) . Body",
                "pi (A : VType) . forall (B : VType) . Body\n",
            ),
            (
                concat!(
                    "exists (A : VType) . /- Keep the nested scope. -/ ",
                    "exists (B : VType) . Body",
                ),
                concat!(
                    "exists (A : VType) . /- Keep the nested scope. -/ ",
                    "exists (B : VType) . Body\n",
                ),
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");
            assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        });
    }

    #[test]
    fn sequence_bindings_start_on_their_own_lines() {
        let cases = [
            // A sequence binding anchored mid-line moves to its own line
            // because its tail marker always breaks.
            (
                "let x = do a <- b; ret a in x",
                concat!("let x =\n", "  do a <- b;\n", "  ret a\n", "in\n", "x\n"),
            ),
            (
                "do x <- do y <- b; ret y;\nret x",
                concat!("do x <-\n", "  do y <- b;\n", "  ret y;\n", "ret x\n"),
            ),
            ("fn x => do y <- b; ret y", concat!("fn x =>\n", "  do y <- b;\n", "  ret y\n")),
            // A sequence binding already at a line start stays there.
            ("begin\n  do x <- f;\n  ret x\nend", "begin\n  do x <- f;\n  ret x\nend\n"),
            // A sequence binding nested in a delimited region starts on a
            // new line without moving the surrounding delimiters.
            (
                "let x = { let y = c in y } in x",
                concat!("let x = {\n", "  let y = c in\n", "  y\n", "} in\n", "x\n"),
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
        });
    }

    #[test]
    fn scope_separator_breaks_only_after_a_multiline_head() {
        let cases = [
            // A single-line head keeps its marker on the same line.
            ("pi (A : VType) . Result", "pi (A : VType) . Result\n"),
            // A multiline head ends with the marker alone on its own line.
            (
                concat!("pi\n", "  (A : VType)\n", "  (B : CType -> CType)\n", ". Result",),
                concat!("pi\n", "  (A : VType)\n", "  (B : CType -> CType)\n", ".\n", "  Result\n",),
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
        });
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
                    "fn\n",
                    "  (first : FirstClassifier)\n",
                    "  (second : SecondClassifier)\n",
                    "=>\n",
                    "  result\n",
                ),
            ),
            (
                "pi (first : FirstClassifier) (second : SecondClassifier) . Result",
                concat!(
                    "pi\n",
                    "  (first : FirstClassifier)\n",
                    "  (second : SecondClassifier)\n",
                    ".\n",
                    "  Result\n",
                ),
            ),
            (
                "forall (first : FirstClassifier) (second : SecondClassifier) . Result",
                concat!(
                    "forall\n",
                    "  (first : FirstClassifier)\n",
                    "  (second : SecondClassifier)\n",
                    ".\n",
                    "  Result\n",
                ),
            ),
            (
                "sigma (first : FirstClassifier) (second : SecondClassifier) . Result",
                concat!(
                    "sigma\n",
                    "  (first : FirstClassifier)\n",
                    "  (second : SecondClassifier)\n",
                    ".\n",
                    "  Result\n",
                ),
            ),
            (
                "exists (first : FirstClassifier) (second : SecondClassifier) . Result",
                concat!(
                    "exists\n",
                    "  (first : FirstClassifier)\n",
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
    fn preserves_fitting_parameter_rows_inside_multiline_telescopes() {
        let cases = [
            (
                concat!(
                    "def ! unwrap_or (A : VType)\n",
                    "  (E : VType)\n",
                    "  (result : Result A E)\n",
                    "  (default : A) : Ret A = value that\n",
                    "unwrap_or",
                ),
                concat!(
                    "def ! unwrap_or (A : VType)\n",
                    "  (E : VType)\n",
                    "  (result : Result A E)\n",
                    "  (default : A)\n",
                    ": Ret A = value that\n",
                    "unwrap_or\n",
                ),
            ),
            (
                concat!(
                    "let f (A : VType) (B : VType)\n",
                    "  (left : A) (right : B) : Result = value in\n",
                    "f",
                ),
                concat!(
                    "let f (A : VType) (B : VType)\n",
                    "  (left : A) (right : B)\n",
                    ": Result = value in\n",
                    "f\n",
                ),
            ),
            (
                concat!("forall (A : VType) (B : VType)\n", "  (C : VType) (D : VType) . Body",),
                concat!(
                    "forall (A : VType) (B : VType)\n",
                    "  (C : VType) (D : VType)\n",
                    ".\n",
                    "  Body\n",
                ),
            ),
            (
                concat!("exists (A : VType) (B : VType)\n", "  (C : VType) (D : VType) . Body",),
                concat!(
                    "exists (A : VType) (B : VType)\n",
                    "  (C : VType) (D : VType)\n",
                    ".\n",
                    "  Body\n",
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
    fn preserves_comments_inside_multiline_parameter_telescopes() {
        let source = concat!(
            "fn\n",
            "  -- Keep the first parameter.\n",
            "  (A : VType)\n",
            "  -- Keep the second parameter.\n",
            "  (B : VType) => Body",
        );
        let expected = concat!(
            "fn\n",
            "  -- Keep the first parameter.\n",
            "  (A : VType)\n",
            "  -- Keep the second parameter.\n",
            "  (B : VType)\n",
            "=>\n",
            "  Body\n",
        );
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);

        assert_eq!(formatted, expected);
        assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
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
    fn preserves_text_blocks_around_formatted_syntax() {
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
    fn preserves_blank_lines_at_arm_block_boundaries() {
        let cases = [
            concat!(
                "match tree\n",
                "| +Leaf() => ret 0\n",
                "\n",
                "| +Node(/tree_height) => ret tree_height\n",
                "end\n",
            ),
            concat!("data\n", "\n", "| +A : Unit\n", "\n", "| +B : Unit\n", "\n", "end\n",),
            concat!(
                "comatch\n",
                "| .a => ret 0\n",
                "\n",
                "-- The second arm.\n",
                "| .b => ret 1\n",
                "end\n",
            ),
        ];

        cases.into_iter().for_each(|source| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, source, "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn preserves_blank_lines_at_block_boundaries() {
        let cases = [
            concat!("begin\n", "\n", "  let x = f in\n", "  ret x\n", "end\n",),
            concat!("begin\n", "  let x = f in\n", "  ret x\n", "\n", "end\n",),
            concat!("begin\n", "\n", "  let x = f in\n", "  ret x\n", "\n", "end\n",),
        ];

        cases.into_iter().for_each(|source| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, source, "source: {source}");
            assert!(formatted.lines().all(|line| line.trim_end() == line));

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn ignores_blank_lines_at_arm_boundaries_when_intentions_are_off() {
        let source = concat!(
            "match tree\n",
            "| +Leaf() => ret 0\n",
            "\n",
            "| +Node(/tree_height) => ret tree_height\n",
            "end\n",
        );
        let expected = concat!(
            "match tree\n",
            "| +Leaf() => ret 0\n",
            "| +Node(/tree_height) => ret tree_height\n",
            "end\n",
        );
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Ignore);

        assert_eq!(formatted, expected);
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Ignore));
    }

    #[test]
    fn blank_lines_only_mode_rejoins_single_breaks_and_keeps_blank_lines() {
        let options =
            PrettyOptions::default().with_layout_intentions(LayoutIntentions::BlankLinesOnly);
        let cases = [
            // A hand-wrapped application rejoins when its compact form fits.
            (
                "! (bool/if)\n  (Ret Int64)\n  greater\n  { ret left }\n  { ret right }",
                "! (bool/if) (Ret Int64) greater { ret left } { ret right }\n",
            ),
            // A blank line still partitions the argument rows and survives.
            (
                "! (bool/if)\n  (Ret Int64)\n\n  greater\n  { ret left }\n  { ret right }",
                "! (bool/if) (Ret Int64)\n\n  greater { ret left } { ret right }\n",
            ),
            // A single infix break rejoins; a blank line keeps the hanging form.
            ("A *\nB", "A * B\n"),
            ("A *\n\nB", "  A\n\n* B\n"),
            // A single break before a nested scope no longer stops the fold.
            ("fn a =>\nfn b => body", "fn a b => body\n"),
            // A blank line before the nested introducer still stops it,
            // while the inner scope remains compact when it fits.
            ("fn a =>\n\nfn b => body", "fn a =>\n\n  fn b => body\n"),
            // Blank lines between sequence stages survive.
            (
                "do x <- ! f a;\n\ndo y <- ! g b;\nret x",
                "do x <- ! f a;\n\ndo y <- ! g b;\nret x\n",
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render_with_options(options);
            assert_eq!(formatted, expected, "source: {source}");
            assert!(formatted.lines().all(|line| line.trim_end() == line));

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render_with_options(options), "source: {source}");
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
        });
    }

    #[test]
    fn format_annotations_scope_layout_intentions() {
        let cases = [
            (
                concat!(
                    "@[format(layout(ignore))] ! (bool/if)\n",
                    "  (Ret Int64)\n",
                    "  greater\n",
                    "  { ret left }\n",
                    "  { ret right }\n",
                ),
                "@[format(layout(ignore))] ! (bool/if) (Ret Int64) greater { ret left } { ret right }\n",
            ),
            (
                concat!(
                    "@[format(layout(blank_lines))] ! (bool/if)\n",
                    "  (Ret Int64)\n",
                    "\n",
                    "  greater\n",
                    "  { ret left }\n",
                    "  { ret right }\n",
                ),
                concat!(
                    "@[format(layout(blank_lines))] ! (bool/if) (Ret Int64)\n",
                    "\n",
                    "  greater { ret left } { ret right }\n",
                ),
            ),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");
            assert!(formatted.lines().all(|line| line.trim_end() == line));

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
        });
    }

    #[test]
    fn format_annotations_scope_indentation() {
        let source = concat!(
            "begin\n",
            "  @[format(indent(4))] begin\n",
            "  let x = f\n",
            "  in ret x\n",
            "  end\n",
            "end\n",
        );
        let expected = concat!(
            "begin\n",
            "  @[format(indent(4))] begin\n",
            "      let x = f in\n",
            "      ret x\n",
            "  end\n",
            "end\n",
        );
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    }

    #[test]
    fn format_annotations_scope_line_width() {
        let cases = [
            (
                "@[format(width(24))] ! (bool/if) (Ret Int64) greater { ret left } { ret right }\n",
                concat!(
                    "@[format(width(24))]\n",
                    "! (bool/if) (Ret Int64)\n",
                    "  greater { ret left } {\n",
                    "  ret right\n",
                    "}\n",
                ),
            ),
            (
                concat!(
                    "@[format(width(20))] ! (bool/if)\n",
                    "\n",
                    "  (Ret Int64)\n",
                    "  greater\n",
                    "  { ret left }\n",
                    "  { ret right }\n",
                ),
                concat!(
                    "@[format(width(20))]\n",
                    "! (bool/if)\n",
                    "\n",
                    "  (Ret Int64)\n",
                    "  greater\n",
                    "  { ret left }\n",
                    "  { ret right }\n",
                ),
            ),
            ("@[format(width(24))] A * B\n", "@[format(width(24))] A * B\n"),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(formatted, expected, "source: {source}");
            assert!(formatted.lines().all(|line| line.trim_end() == line), "source: {source}");

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
        });
    }

    #[test]
    fn format_annotations_compose_innermost_first() {
        let source = concat!(
            "@[format(width(24))] @[format(layout(ignore))] ! (bool/if)\n",
            "  (Ret Int64)\n",
            "  greater\n",
            "  { ret left }\n",
            "  { ret right }\n",
        );
        let expected = concat!(
            "@[format(width(24))]\n",
            "@[format(layout(ignore))]\n",
            "! (bool/if) (Ret Int64)\n",
            "  greater { ret left } {\n",
            "  ret right\n",
            "}\n",
        );
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    }

    #[test]
    fn format_annotations_scope_whole_annotated_terms() {
        let source = concat!(
            "@[format(layout(ignore))] ! (bool/if)\n",
            "  (Ret Int64)\n",
            "  greater\n",
            "  { ret left }\n",
            "  { ret right }\n",
            "* ! (other/if)\n",
            "  (Ret Int64)\n",
            "  other\n",
            "  { ret a }\n",
            "  { ret b }\n",
        );
        let expected = concat!(
            "@[format(layout(ignore))] ! (bool/if) (Ret Int64) greater { ret left } { ret right } * ! (other/if)\n",
            "  (Ret Int64) other { ret a } { ret b }\n",
        );
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render_with_options(PrettyOptions::default());
        assert_eq!(formatted, expected, "source: {source}");
        assert!(formatted.lines().all(|line| line.len() <= 100), "{formatted}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render_with_options(PrettyOptions::default()));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    }

    #[test]
    fn malformed_format_annotations_remain_inert() {
        let cases = [
            "@[format(nope(1))] (field = field, ((x)))\n",
            "@[format(width(0))] (field = field, ((x)))\n",
            "@[format(100)] (field = field, ((x)))\n",
        ];

        cases.into_iter().for_each(|source| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render(LayoutIntentions::Preserve);
            assert_eq!(
                formatted,
                source.replace("(field = field, ((x)))", "(= field, x)"),
                "source: {source}"
            );

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn blank_lines_only_mode_still_collapses_redundant_blank_runs() {
        let options =
            PrettyOptions::default().with_layout_intentions(LayoutIntentions::BlankLinesOnly);
        let cases =
            [("A *\n\n\n\nB", "  A\n\n* B\n"), ("(first,\n\n\nsecond)", "(first,\n\n  second)\n")];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            let formatted = parsed.render_with_options(options);
            assert_eq!(formatted, expected, "source: {source}");
            assert!(formatted.lines().all(|line| line.trim_end() == line));

            let reparsed = ParsedSource::new(&formatted);
            assert_eq!(formatted, reparsed.render_with_options(options));
            assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        });
    }

    #[test]
    fn blank_lines_only_mode_elides_parentheses_with_single_breaks_only() {
        let options =
            PrettyOptions::default().with_layout_intentions(LayoutIntentions::BlankLinesOnly);
        let cases = [
            // A single break inside a singleton group disappears with the group.
            ("(\nvalue\n)", "value\n"),
            // A blank line inside the group keeps the delimiters and the blank.
            ("(\n\nvalue\n)", "(\n\n  value)\n"),
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
            ("--| Detached\n\n@[doc] _", "--| Detached\n\n@(doc)\n"),
            ("--| Detached\n-- barrier\n@[doc] _", "--| Detached\n-- barrier\n@(doc)\n"),
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
    fn parenthesized_metadata_defaults_its_payload_through_pretty_printing() {
        let source = "--| Attached text.\n@(doc)";
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Ignore);
        assert_eq!(formatted, "--| Attached text.\n@(doc)\n");

        let reparsed = ParsedSource::new(&formatted);
        let documentation =
            reparsed.unit.documentation(&reparsed.parser.arena, &reparsed.parser.spans);
        let [site] = documentation.as_slice() else {
            panic!("expected one documentation annotation")
        };
        assert_eq!(
            site.directive.comment.as_ref().map(|comment| comment.text.as_ref()),
            Some("Attached text."),
        );
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Ignore));
    }

    #[test]
    fn metadata_with_a_commented_hole_payload_keeps_the_bracket_form() {
        let source = "@[doc]\n-- Keep the hole.\n_";
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Ignore);

        assert!(formatted.contains("@[doc]"), "{formatted}");
        assert!(formatted.contains("-- Keep the hole."), "{formatted}");
        assert!(!formatted.contains("@(doc)"), "{formatted}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
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
    fn standard_library_has_no_named_term_punning_backlog() {
        let sources = [
            ("bool.zy", include_str!("../../../../lib/std/data/bool.zy")),
            ("io-types.zy", include_str!("../../../../lib/std/system/types.zy")),
            ("list.zy", include_str!("../../../../lib/std/data/list.zy")),
            ("monad.zy", include_str!("../../../../lib/std/control/monad.zy")),
            ("option.zy", include_str!("../../../../lib/std/data/option.zy")),
            ("result.zy", include_str!("../../../../lib/std/data/result.zy")),
            ("std.type.zy", include_str!("../../../../lib/std/std.type.zy")),
            ("std.zy", include_str!("../../../../lib/std/std.zy")),
            ("std.zyi", include_str!("../../../../lib/std/std.zyi")),
        ];

        let remaining = sources
            .into_iter()
            .filter_map(|(name, source)| {
                let parsed = ParsedSource::new(source);
                let candidates =
                    NamedTermPunningAudit::new(source, &parsed.parser.spans, &parsed.parser.arena)
                        .candidates()
                        .len();
                (candidates != 0).then_some((name, candidates))
            })
            .collect::<Vec<_>>();

        assert!(remaining.is_empty(), "unpunned named terms remain: {remaining:?}");
    }

    #[test]
    fn standard_library_pretty_printing_reparses_idempotently() {
        let sources = [
            ("bool.zy", include_str!("../../../../lib/std/data/bool.zy")),
            ("builtin.zy", include_str!("../../../../lib/std/builtin.zy")),
            ("io-types.zy", include_str!("../../../../lib/std/system/types.zy")),
            ("list.zy", include_str!("../../../../lib/std/data/list.zy")),
            ("monad.zy", include_str!("../../../../lib/std/control/monad.zy")),
            ("option.zy", include_str!("../../../../lib/std/data/option.zy")),
            ("result.zy", include_str!("../../../../lib/std/data/result.zy")),
            ("std.type.zy", include_str!("../../../../lib/std/std.type.zy")),
            ("std.zy", include_str!("../../../../lib/std/std.zy")),
            ("std.zyi", include_str!("../../../../lib/std/std.zyi")),
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
