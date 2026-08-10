//! Configurable pretty printing for canonical textual syntax.

mod config;
mod context;
mod punning;

use super::syntax::*;
pub use config::{LayoutIntentions, Parentheses, PrettyOptions};
use context::{
    PatternRequirement, RenderedPatternClass, RenderedTermClass, TermPrecedence, TermRequirement,
};
use pretty::{DocAllocator, RcAllocator, RcDoc};
pub use punning::NamedTermPunningAudit;
use punning::{PunnedPatternPayload, PunnedTermPayload, Punning};
use zydeco_syntax::Pretty;

static DOC_ALLOCATOR: RcAllocator = RcAllocator;

/// A pretty printer over one parsed textual arena.
pub struct PrettyFormatter<'arena> {
    arena: &'arena TextArena,
    options: PrettyOptions,
}

struct ManifestParameterView<'arena> {
    fields: Vec<&'arena FieldName>,
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
    boundary_mode: BoundaryMode,
}

impl<'arena> LayoutFragment<'arena> {
    fn entity(entity: EntityId, document: RcDoc<'arena>) -> Self {
        Self {
            document,
            anchors: LayoutAnchors { first: entity, last: entity },
            boundary_mode: BoundaryMode::Regular,
        }
    }

    fn with_boundary_mode(mut self, boundary_mode: BoundaryMode) -> Self {
        self.boundary_mode = boundary_mode;
        self
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum BoundaryMode {
    Regular,
    Block,
}

#[derive(Copy, Clone)]
enum DelimiterSpacing {
    Tight,
    Spaced,
}

struct BoundaryLayout<'arena> {
    prefix: RcDoc<'arena>,
    joined: RcDoc<'arena>,
    broken: RcDoc<'arena>,
    blank: RcDoc<'arena>,
    joined_indent: isize,
    broken_indent: isize,
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

    fn blank_line() -> RcDoc<'arena> {
        RcDoc::nesting(|nesting| {
            let nesting = isize::try_from(nesting).unwrap_or(isize::MAX);
            RcDoc::hardline().append(RcDoc::hardline()).nest(-nesting)
        })
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
        Self { arena, options }
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
        self.arena
            .trivia
            .leading_comments(entity)
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

    fn require_line_start(&self) -> RcDoc<'arena> {
        RcDoc::column(|column| {
            RcDoc::nesting(
                move |nesting| {
                    if column > nesting { RcDoc::fail() } else { RcDoc::nil() }
                },
            )
        })
    }

    fn with_trailing_comments(
        &'arena self, entity: EntityId, document: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        self.arena.trivia.trailing_comments(entity).iter().fold(document, |document, comment| {
            document
                .append(self.line_separation(comment.separation_before()))
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

    fn boundary(
        &'arena self, intention: Option<BreakIntent>, layout: BoundaryLayout<'arena>,
        continuation: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        let joined = layout.joined.append(continuation.clone()).nest(layout.joined_indent);
        let broken = layout.broken.append(continuation.clone()).nest(layout.broken_indent);
        let blank = layout.blank.append(continuation).nest(layout.broken_indent);
        let choice = match (self.options.layout_intentions, intention) {
            | (LayoutIntentions::Preserve, Some(BreakIntent::Broken)) => broken,
            | (LayoutIntentions::Preserve, Some(BreakIntent::BlankLine)) => blank,
            | _ => self.flexible(joined, broken),
        };
        layout.prefix.append(choice)
    }

    fn block_boundary(
        &'arena self, intention: Option<BreakIntent>, layout: BoundaryLayout<'arena>,
        continuation: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        let joined = layout.joined.append(continuation.clone()).nest(layout.joined_indent);
        let broken = layout.broken.append(continuation.clone()).nest(layout.broken_indent);
        let blank = layout.blank.append(continuation).nest(layout.broken_indent);
        let choice = match (self.options.layout_intentions, intention) {
            | (LayoutIntentions::Preserve, Some(BreakIntent::Broken)) => broken,
            | (LayoutIntentions::Preserve, Some(BreakIntent::BlankLine)) => blank,
            | _ => self.flexible_block(joined, broken),
        };
        layout.prefix.append(choice)
    }

    fn layout_boundary(
        &'arena self, intention: Option<BreakIntent>, layout: BoundaryLayout<'arena>,
        boundary_mode: BoundaryMode, continuation: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        match boundary_mode {
            | BoundaryMode::Regular => self.boundary(intention, layout, continuation),
            | BoundaryMode::Block => self.block_boundary(intention, layout, continuation),
        }
    }

    fn fragment_boundary(
        &'arena self, intention: Option<BreakIntent>, layout: BoundaryLayout<'arena>,
        continuation: LayoutFragment<'arena>,
    ) -> RcDoc<'arena> {
        self.layout_boundary(intention, layout, continuation.boundary_mode, continuation.document)
    }

    fn flexible(&'arena self, joined: RcDoc<'arena>, broken: RcDoc<'arena>) -> RcDoc<'arena> {
        // Expose the joined projection to enclosing groups while retaining a
        // complete broken alternative for a boundary whose own group overflows.
        joined.clone().union(broken).flat_alt(joined)
    }

    fn flexible_block(&'arena self, joined: RcDoc<'arena>, broken: RcDoc<'arena>) -> RcDoc<'arena> {
        joined.union(broken)
    }

    fn join(
        &'arena self, left: LayoutFragment<'arena>, layout: BoundaryLayout<'arena>,
        right: LayoutFragment<'arena>,
    ) -> LayoutFragment<'arena> {
        let intention = self.arena.intentions.between(left.anchors.last, right.anchors.first);
        let anchors = LayoutAnchors { first: left.anchors.first, last: right.anchors.last };
        let boundary_mode = left.boundary_mode;
        let document = left.document.append(self.layout_boundary(
            intention,
            layout,
            right.boundary_mode,
            right.document,
        ));
        LayoutFragment { document, anchors, boundary_mode }
    }

    fn separated(
        &'arena self, items: Vec<LayoutFragment<'arena>>, separator: &'static str,
        joined_indent: isize, broken_indent: isize,
    ) -> Option<LayoutFragment<'arena>> {
        items.into_iter().rev().reduce(|right, left| {
            self.join(left, BoundaryLayout::after(separator, joined_indent, broken_indent), right)
        })
    }

    fn grouped_join(
        &'arena self, left: LayoutFragment<'arena>, before_boundary: RcDoc<'arena>,
        right: LayoutFragment<'arena>, continuation_indent: isize,
    ) -> LayoutFragment<'arena> {
        let intention = self.arena.intentions.between(left.anchors.last, right.anchors.first);
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
            boundary_mode: left.boundary_mode,
        }
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
            boundaries.push(self.arena.intentions.between(left.into(), right.into()));

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
        let hanging_indent = self.options.indent.max(symbol_width.saturating_add(1));
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
            self.require_line_start()
                .append(RcDoc::text(" ".repeat(hanging_indent_usize)))
                .append(table)
        };

        let preserves_break = self.options.layout_intentions == LayoutIntentions::Preserve
            && boundaries.iter().flatten().any(|intention| intention.requires_line_break());
        if preserves_break {
            return hanging;
        }

        let line_width = self.options.line_width;
        DOC_ALLOCATOR
            .column(move |column| {
                let joined = joined.clone();
                let hanging = hanging.clone();
                DOC_ALLOCATOR
                    .nesting(move |nesting| {
                        if Self::document_fits(&joined, column, nesting, line_width) {
                            joined.clone()
                        } else {
                            hanging.clone()
                        }
                    })
                    .into_doc()
            })
            .into_doc()
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

    fn document_fits(
        document: &RcDoc<'arena>, column: usize, nesting: usize, line_width: usize,
    ) -> bool {
        Self::render_document(document, column, nesting, line_width)
            .is_some_and(|rendered| rendered.lines().all(|line| line.len() <= line_width))
    }

    fn document_fits_on_one_line(
        document: &RcDoc<'arena>, column: usize, nesting: usize, line_width: usize,
    ) -> bool {
        Self::render_document(document, column, nesting, line_width)
            .is_some_and(|rendered| !rendered.contains('\n') && rendered.len() <= line_width)
    }

    fn render_document(
        document: &RcDoc<'arena>, column: usize, nesting: usize, line_width: usize,
    ) -> Option<String> {
        let nesting = isize::try_from(nesting).unwrap_or(isize::MAX);
        let mut rendered = String::new();
        if RcDoc::text(" ".repeat(column))
            .append(document.clone().nest(nesting))
            .render_fmt(line_width, &mut rendered)
            .is_err()
        {
            return None;
        }
        Some(rendered)
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
        let items = self.separated(items, separator, 0, 0).expect("delimited items are nonempty");
        let after_open = entity
            .and_then(|entity| self.arena.intentions.after_start(entity, items.anchors.first));
        let before_close =
            entity.and_then(|entity| self.arena.intentions.before_end(items.anchors.last, entity));
        let contents_layout = spacing.boundary(self.options.indent, self.options.indent);
        let contents =
            self.layout_boundary(after_open, contents_layout, items.boundary_mode, items.document);
        let close = self.boundary(before_close, spacing.boundary(0, 0), RcDoc::text(close));
        RcDoc::text(open).append(contents).append(close)
    }

    fn annotation(
        &'arena self, entity: EntityId, term: LayoutFragment<'arena>, ty: TermId,
        parenthesized: bool,
    ) -> RcDoc<'arena> {
        let ty = self.term_fragment(ty);
        let annotation = self.join(term, BoundaryLayout::after(" :", 0, 0), ty);
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
            .after_start(entity, inner)
            .is_some_and(BreakIntent::requires_line_break)
            || self
                .arena
                .intentions
                .before_end(inner, entity)
                .is_some_and(BreakIntent::requires_line_break);
        self.options.parentheses == Parentheses::Minimal
            && !(self.options.layout_intentions == LayoutIntentions::Preserve && carries_break)
    }

    /// Whether a transparent singleton group surrounds an application.
    /// Applications used to print their own delimiters, so recognizing this
    /// shape also removes wrappers left by earlier formatter output.
    fn transparently_groups_application(&self, term: TermId) -> bool {
        match &self.arena.terms[&term] {
            | Term::SourceBoundary(SourceBoundary(inner)) => {
                self.transparently_groups_application(*inner)
            }
            | Term::Paren(Paren(terms)) => match terms.as_slice() {
                | [inner] => self.transparently_groups_application(*inner),
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
                LayoutFragment::entity((*tm).into(), self.annotated_pattern(*tm)),
                *ty,
                requirement == PatternRequirement::Pattern,
            ),
            | Pattern::Manifest(ManifestPattern { binder, definition }) => {
                let manifest = self.join(
                    LayoutFragment::entity((*binder).into(), self.annotated_pattern(*binder)),
                    BoundaryLayout::after(" as", 0, 0),
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
                        && self.pattern_requirement_accepts(requirement, *inner) =>
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
        let document = if requirement.accepts(self.rendered_pattern_class(pattern)) {
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
            | CoPattern::App(_) => self.copattern_application(pattern).nest(self.options.indent),
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
            .with_boundary_mode(self.term_boundary_mode(term, requirement))
    }

    fn term_boundary_mode(&self, term: TermId, requirement: TermRequirement) -> BoundaryMode {
        if !self.term_requirement_accepts(requirement, term) {
            return BoundaryMode::Regular;
        }
        match &self.arena.terms[&term] {
            | Term::SourceBoundary(SourceBoundary(inner)) => {
                self.term_boundary_mode(*inner, requirement)
            }
            | Term::Ann(Ann { tm, .. }) if requirement == TermRequirement::Annotated => {
                self.term_boundary_mode(*tm, TermRequirement::Annotated)
            }
            | Term::Paren(Paren(terms)) => match terms.as_slice() {
                | [inner]
                    if (self.should_elide_parentheses(term.into(), (*inner).into())
                        || self.transparently_groups_application(*inner))
                        && self.term_requirement_accepts(requirement, *inner) =>
                {
                    self.term_boundary_mode(*inner, requirement)
                }
                | _ => BoundaryMode::Regular,
            },
            | Term::Prod(_) | Term::Arrow(_) => BoundaryMode::Block,
            | _ => BoundaryMode::Regular,
        }
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
                    self.arena.intentions.after_start(term.into(), (*inner).into()),
                    BoundaryLayout::after("]", 0, 0),
                    self.term_through_fragment(*inner, TermPrecedence::Binder),
                )),
            | Term::SourceBoundary(_) => unreachable!("source boundaries return before rendering"),
            | Term::Ann(Ann { tm, ty }) => self.annotation(
                term.into(),
                self.annotated_term_fragment(*tm),
                *ty,
                requirement != TermRequirement::Annotated,
            ),
            | Term::Hole(_) => RcDoc::text("_"),
            | Term::Var(name) => self.variable(name),
            | Term::Named(Named(field, inner)) => self.named_term(term, field, *inner),
            | Term::Label(Label(field, inner)) => self.field(field).append(self.fragment_boundary(
                self.arena.intentions.after_start(term.into(), (*inner).into()),
                BoundaryLayout::after(" ::", 0, 0),
                self.annotated_term_fragment(*inner),
            )),
            | Term::Paren(Paren(terms)) => match terms.as_slice() {
                | [inner]
                    if (self.should_elide_parentheses(term.into(), (*inner).into())
                        || self.transparently_groups_application(*inner))
                        && self.term_requirement_accepts(requirement, *inner) =>
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
                self.grouped_join(
                    LayoutFragment::entity(
                        (*pattern).into(),
                        RcDoc::text("fn ").append(self.copattern(*pattern)),
                    ),
                    RcDoc::text(" =>"),
                    self.term_through_fragment(*body, TermPrecedence::Binder),
                    self.options.indent,
                )
                .document
            }
            | Term::App(Appli(terms)) => self.application(terms),
            | Term::KontCall(KontCall { body, tail }) => RcDoc::text("do~")
                .append(self.fragment_boundary(
                    self.arena.intentions.after_start(term.into(), (*body).into()),
                    BoundaryLayout::after("", 0, self.options.indent),
                    self.term_through_fragment(*body, TermPrecedence::Binder),
                ))
                .append(RcDoc::text(";"))
                .append(self.sequence_tail((*body).into(), *tail)),
            | Term::Fix(Fix(pattern, body)) => {
                self.grouped_join(
                    LayoutFragment::entity(
                        (*pattern).into(),
                        RcDoc::text("fix ").append(self.pattern(*pattern)),
                    ),
                    RcDoc::text(" =>"),
                    self.term_through_fragment(*body, TermPrecedence::Binder),
                    self.options.indent,
                )
                .document
            }
            | Term::Pi(Pi(pattern, body)) => self.quantifier("pi", *pattern, *body),
            | Term::Forall(Forall(pattern, body)) => self.quantifier("forall", *pattern, *body),
            | Term::Arrow(_) => self.infix_chain(term, InfixOperator::Arrow),
            | Term::Sigma(Sigma(pattern, body)) => self.quantifier("sigma", *pattern, *body),
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
            | Term::Force(Force(body)) => {
                RcDoc::text("! ").append(self.term_through(*body, TermPrecedence::Atom))
            }
            | Term::Ret(Return(body)) => {
                RcDoc::text("ret ").append(self.term_through(*body, TermPrecedence::Atom))
            }
            | Term::Do(Bind { binder, bindee, tail }) => RcDoc::text("do ")
                .append(self.pattern(*binder))
                .append(self.fragment_boundary(
                    self.arena.intentions.between((*binder).into(), (*bindee).into()),
                    BoundaryLayout::after(" <-", 0, self.options.indent),
                    self.term_through_fragment(*bindee, TermPrecedence::Binder),
                ))
                .append(RcDoc::text(";"))
                .append(self.sequence_tail((*bindee).into(), *tail)),
            | Term::Let(GenLet { binding, tail }) => RcDoc::text("let ")
                .append(self.placed_binding(binding, Placement::In))
                .append(self.sequence_tail(binding.bindee.into(), *tail)),
            | Term::Param(Param { binder, placement, tail }) => RcDoc::text("param ")
                .append(self.annotated_pattern(*binder))
                .append(RcDoc::text(" "))
                .append(self.placement(*placement))
                .append(self.sequence_tail((*binder).into(), *tail)),
            | Term::ContextBind(ContextBind { mode, binding, placement, tail }) => {
                let keyword = match mode {
                    | DefinitionMode::Transparent => "let ",
                    | DefinitionMode::Nominal => "def ",
                };
                RcDoc::text(keyword)
                    .append(self.placed_binding(binding, *placement))
                    .append(self.sequence_tail(binding.bindee.into(), *tail))
            }
            | Term::Block(Block(body)) => self.block("begin", *body, "end"),
            | Term::MoBlock(MoBlock(body)) => self.block("monadic", *body, "end"),
            | Term::Data(Data { arms }) => self.data(arms),
            | Term::CoData(CoData { arms }) => self.codata(arms),
            | Term::Ctor(Ctor(name, body)) => {
                self.constructor(name).append(self.term_constructor_argument(*body))
            }
            | Term::Match(Match { scrut, arms }) => self.matcher(*scrut, arms),
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
        let document = if requirement.accepts(self.rendered_term_class(term)) {
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
        )
        .with_boundary_mode(
            self.term_boundary_mode(*head, TermRequirement::Through(TermPrecedence::Application)),
        );
        let arguments = self
            .separated(
                std::iter::once(first_argument)
                    .chain(arguments)
                    .map(|argument| {
                        self.term_through_fragment(*argument, TermPrecedence::Projection)
                    })
                    .collect(),
                "",
                0,
                self.options.indent,
            )
            .expect("applications contain at least one argument");
        self.join(head, BoundaryLayout::after("", 0, self.options.indent), arguments).document
    }

    fn named_term(&'arena self, term: TermId, field: &FieldName, inner: TermId) -> RcDoc<'arena> {
        let payload = Punning::new(self.arena).term_payload(field, inner);
        match payload {
            | Some(PunnedTermPayload::Variable) => RcDoc::text("= ").append(self.field(field)),
            | Some(PunnedTermPayload::Annotated(ty)) => {
                RcDoc::text("= ").append(self.field(field)).append(self.fragment_boundary(
                    self.arena.intentions.after_start(term.into(), ty.into()),
                    BoundaryLayout::after(" :", 0, 0),
                    self.term_fragment(ty),
                ))
            }
            | _ => self.field(field).append(self.fragment_boundary(
                self.arena.intentions.after_start(term.into(), inner.into()),
                BoundaryLayout::after(" =", 0, 0),
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
        match Punning::new(self.arena).pattern_payload(field, inner) {
            | Some(PunnedPatternPayload::Variable) => RcDoc::text("= ").append(self.field(field)),
            | Some(PunnedPatternPayload::Annotated(ty)) => {
                RcDoc::text("= ").append(self.field(field)).append(self.fragment_boundary(
                    self.arena.intentions.after_start(pattern.into(), ty.into()),
                    BoundaryLayout::after(" :", 0, 0),
                    self.term_fragment(ty),
                ))
            }
            | None => {
                self.field(field).append(RcDoc::text(" = ")).append(self.annotated_pattern(inner))
            }
        }
    }

    fn projection_pattern(
        &'arena self, pattern: PatId, field: &FieldName, inner: PatId,
    ) -> RcDoc<'arena> {
        match Punning::new(self.arena).pattern_payload(field, inner) {
            | Some(PunnedPatternPayload::Variable) => RcDoc::text("/").append(self.field(field)),
            | Some(PunnedPatternPayload::Annotated(ty)) => {
                RcDoc::text("/").append(self.field(field)).append(self.fragment_boundary(
                    self.arena.intentions.after_start(pattern.into(), ty.into()),
                    BoundaryLayout::after(" :", 0, 0),
                    self.term_fragment(ty),
                ))
            }
            | None => RcDoc::text("/")
                .append(self.field(field))
                .append(RcDoc::text(" = "))
                .append(self.annotated_pattern(inner)),
        }
    }

    fn term_requirement_accepts(&self, requirement: TermRequirement, term: TermId) -> bool {
        requirement.accepts(self.rendered_term_class(term))
    }

    fn rendered_term_class(&self, term: TermId) -> RenderedTermClass {
        match &self.arena.terms[&term] {
            | Term::SourceBoundary(SourceBoundary(inner)) => self.rendered_term_class(*inner),
            | Term::Named(_) | Term::Label(_) => RenderedTermClass::AnnotatedOnly,
            // These constructors either are atoms in the grammar or are
            // deliberately rendered with their own delimiters.
            | Term::Ann(_)
            | Term::Hole(_)
            | Term::Var(_)
            | Term::Paren(_)
            | Term::Thunk(_)
            | Term::Force(_)
            | Term::Ret(_)
            | Term::Block(_)
            | Term::MoBlock(_)
            | Term::Data(_)
            | Term::CoData(_)
            | Term::Ctor(_)
            | Term::Match(_)
            | Term::CoMatch(_)
            | Term::Lit(_) => RenderedTermClass::Term(TermPrecedence::Atom),
            | Term::Proj(_) => RenderedTermClass::Term(TermPrecedence::Projection),
            | Term::App(_) | Term::Dtor(_) => RenderedTermClass::Term(TermPrecedence::Application),
            | Term::Prod(_) => RenderedTermClass::Term(TermPrecedence::Product),
            | Term::Arrow(_) => RenderedTermClass::Term(TermPrecedence::Arrow),
            | Term::Pi(_) | Term::Forall(_) | Term::Sigma(_) | Term::Exists(_) => {
                RenderedTermClass::Term(TermPrecedence::Quantifier)
            }
            | Term::Meta(_)
            | Term::Abs(_)
            | Term::KontCall(_)
            | Term::Fix(_)
            | Term::Do(_)
            | Term::Let(_)
            | Term::Param(_)
            | Term::ContextBind(_) => RenderedTermClass::Term(TermPrecedence::Binder),
        }
    }

    fn pattern_requirement_accepts(&self, requirement: PatternRequirement, pattern: PatId) -> bool {
        requirement.accepts(self.rendered_pattern_class(pattern))
    }

    fn rendered_pattern_class(&self, pattern: PatId) -> RenderedPatternClass {
        match self.arena.pats[&pattern] {
            | Pattern::Named(_) | Pattern::Project(_) => RenderedPatternClass::AnnotatedOnly,
            // Annotations and manifest patterns include their own parentheses
            // in canonical output, so their rendered form is an ordinary
            // `Pattern` even though their payload grammar is `PatternAnn`.
            | Pattern::Ann(_)
            | Pattern::Manifest(_)
            | Pattern::Hole(_)
            | Pattern::Var(_)
            | Pattern::Ctor(_)
            | Pattern::Alias(_)
            | Pattern::Paren(_) => RenderedPatternClass::Pattern,
        }
    }

    fn quantifier(
        &'arena self, keyword: &'static str, pattern: CoPatId, body: TermId,
    ) -> RcDoc<'arena> {
        self.grouped_join(
            LayoutFragment::entity(
                pattern.into(),
                RcDoc::text(keyword).append(RcDoc::space()).append(self.copattern(pattern)),
            ),
            RcDoc::text(" ."),
            self.term_through_fragment(body, TermPrecedence::Quantifier),
            self.options.indent,
        )
        .document
    }

    fn exists(&'arena self, term: TermId, exists: &Exists) -> RcDoc<'arena> {
        let Exists { parameters, body } = exists;
        let Some((first, rest)) = parameters.split_first() else {
            unreachable!("the parser requires at least one existential parameter")
        };
        let begins_with_documentation = self
            .arena
            .trivia
            .leading_comments(first.binder().into())
            .iter()
            .any(|comment| comment.comment().as_documentation().is_some());
        let parameters = self
            .grouped_separated(
                std::iter::once(self.existential_parameter(first))
                    .chain(rest.iter().map(|parameter| self.existential_parameter(parameter)))
                    .collect(),
                "",
            )
            .expect("existentials contain at least one parameter");
        let after_exists = self.arena.intentions.after_start(term.into(), parameters.anchors.first);
        let parameter_document = if begins_with_documentation {
            RcDoc::hardline().append(parameters.document.clone()).nest(self.options.indent)
        } else {
            self.layout_boundary(
                after_exists,
                BoundaryLayout::after("", self.options.indent, self.options.indent),
                parameters.boundary_mode,
                parameters.document.clone(),
            )
        };
        let head = LayoutFragment {
            document: RcDoc::text("exists").append(parameter_document),
            anchors: parameters.anchors,
            boundary_mode: BoundaryMode::Regular,
        };
        self.grouped_join(head, RcDoc::text(" ."), self.term_fragment(*body), self.options.indent)
            .document
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
            match &self.arena.pats[&current] {
                | Pattern::Named(Named(field, inner)) => {
                    fields.push(field);
                    current = *inner;
                }
                | Pattern::Ann(Ann { tm, ty }) => {
                    let manifest = self.transparent_pattern_group(*tm);
                    let Pattern::Manifest(ManifestPattern { binder, definition }) =
                        self.arena.pats[&manifest]
                    else {
                        return None;
                    };
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
        let binder_entity = binder;
        let binder_document = fields.iter().rev().enumerate().fold(
            self.annotated_pattern(binder),
            |inner, (depth, field)| {
                if depth == 0 {
                    match Punning::new(self.arena).pattern_payload(field, binder) {
                        | Some(PunnedPatternPayload::Variable) => {
                            RcDoc::text("= ").append(self.field(field))
                        }
                        | Some(PunnedPatternPayload::Annotated(ty)) => RcDoc::text("= ")
                            .append(self.field(field))
                            .append(self.fragment_boundary(
                                None,
                                BoundaryLayout::after(" :", 0, 0),
                                self.term_fragment(ty),
                            )),
                        | None => self.field(field).append(RcDoc::text(" = ")).append(inner),
                    }
                } else {
                    self.field(field).append(RcDoc::text(" = ")).append(inner)
                }
            },
        );
        let binder = self.join(
            LayoutFragment::entity(binder_entity.into(), binder_document),
            BoundaryLayout::after(" as", 0, 0),
            self.term_fragment(definition),
        );
        let binder = match classifier {
            | Some(classifier) => {
                self.join(binder, BoundaryLayout::after(" :", 0, 0), self.term_fragment(classifier))
            }
            | None => binder,
        };
        let document = self.delimited(Some(entity.into()), "(", vec![binder], ",", ")");
        self.with_leading_comments(entity.into(), document)
    }

    fn placed_binding(
        &'arena self, binding: &GenBind<TermId>, placement: Placement,
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
        let head = RcDoc::intersperse(head, RcDoc::space());
        let head = match ty {
            | Some(ty) => head.append(self.fragment_boundary(
                self.arena.intentions.between(head_last, (*ty).into()),
                BoundaryLayout::after(" :", 0, 0),
                self.term_fragment(*ty),
            )),
            | None => head,
        };
        let placement = self.placement(placement);
        let bindee_fragment = self.term_fragment(*bindee);
        let bindee_document = self.bindee_with_placement(bindee_fragment.document, placement);
        head.append(self.layout_boundary(
            self.arena.intentions.between(head_last, (*bindee).into()),
            BoundaryLayout::after(" =", 0, self.options.indent),
            bindee_fragment.boundary_mode,
            bindee_document,
        ))
    }

    fn sequence_tail(&'arena self, before: EntityId, tail: TermId) -> RcDoc<'arena> {
        self.mandatory_line_break(self.arena.intentions.between(before, tail.into()))
            .append(self.term_through(tail, TermPrecedence::Binder))
    }

    fn bindee_with_placement(
        &'arena self, bindee: RcDoc<'arena>, placement: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        let joined = bindee.clone().append(RcDoc::space()).append(placement.clone());
        let broken = bindee.append(RcDoc::hardline().append(placement).nest(-self.options.indent));
        let line_width = self.options.line_width;
        DOC_ALLOCATOR
            .column(move |column| {
                let joined = joined.clone();
                let broken = broken.clone();
                DOC_ALLOCATOR
                    .nesting(move |nesting| {
                        let bindee_starts_own_line = column == nesting;
                        if !bindee_starts_own_line
                            && Self::document_fits_on_one_line(&joined, column, nesting, line_width)
                        {
                            joined.clone()
                        } else {
                            broken.clone()
                        }
                    })
                    .into_doc()
            })
            .into_doc()
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
            .append(RcDoc::hardline().append(self.annotated_term(body)).nest(self.options.indent))
            .append(RcDoc::hardline())
            .append(RcDoc::text(end))
    }

    fn data(&'arena self, arms: &[DataArm]) -> RcDoc<'arena> {
        arms.iter()
            .fold(RcDoc::text("data"), |document, arm| {
                document
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("| "))
                    .append(self.constructor(&arm.name))
                    .append(self.fragment_boundary(
                        None,
                        BoundaryLayout::after(" :", 0, self.options.indent),
                        self.term_fragment(arm.param),
                    ))
            })
            .append(RcDoc::hardline())
            .append(RcDoc::text("end"))
    }

    fn codata(&'arena self, arms: &[CoDataArm]) -> RcDoc<'arena> {
        arms.iter()
            .fold(RcDoc::text("codata"), |document, item| {
                let arm = RcDoc::text("| ").append(self.destructor(&item.name));
                let arm = match item.params {
                    | Some(params) => arm.append(RcDoc::text(" ")).append(self.copattern(params)),
                    | None => arm,
                };
                document.append(RcDoc::hardline()).append(arm).append(self.fragment_boundary(
                    None,
                    BoundaryLayout::after(" :", 0, self.options.indent),
                    self.term_fragment(item.out),
                ))
            })
            .append(RcDoc::hardline())
            .append(RcDoc::text("end"))
    }

    fn matcher(&'arena self, scrutinee: TermId, arms: &[Matcher<PatId, TermId>]) -> RcDoc<'arena> {
        let head = RcDoc::text("match").append(self.fragment_boundary(
            None,
            BoundaryLayout::after("", 0, self.options.indent),
            self.term_fragment(scrutinee),
        ));
        arms.iter()
            .fold(head, |document, arm| {
                document
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("| "))
                    .append(self.pattern(arm.binder))
                    .append(self.fragment_boundary(
                        self.arena.intentions.between(arm.binder.into(), arm.tail.into()),
                        BoundaryLayout::after(" =>", 0, self.options.indent),
                        self.term_fragment(arm.tail),
                    ))
            })
            .append(RcDoc::hardline())
            .append(RcDoc::text("end"))
    }

    fn comatcher(&'arena self, arms: &[CoMatcherParam]) -> RcDoc<'arena> {
        arms.iter()
            .fold(RcDoc::text("comatch"), |document, arm| {
                document
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("| "))
                    .append(self.copattern(arm.params))
                    .append(self.fragment_boundary(
                        self.arena.intentions.between(arm.params.into(), arm.tail.into()),
                        BoundaryLayout::after(" =>", 0, self.options.indent),
                        self.term_fragment(arm.tail),
                    ))
            })
            .append(RcDoc::hardline())
            .append(RcDoc::text("end"))
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
            ("(field :: A *\nB *\nC)", concat!("(field ::\n", "    A\n", "  * B\n", "  * C)\n")),
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
    fn preserves_observed_line_boundaries() {
        let parsed = ParsedSource::new("(first = first,\nsecond = second)");

        assert_eq!(parsed.render(LayoutIntentions::Preserve), "(= first,\n  = second)\n");
        assert_eq!(parsed.render(LayoutIntentions::Ignore), "(= first, = second)\n");
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
    fn indents_parameters_that_wrap() {
        let options = PrettyOptions::default()
            .with_line_width(38)
            .with_layout_intentions(LayoutIntentions::Ignore);
        let cases = [
            (
                "fn (first : FirstClassifier) (second : SecondClassifier) => result",
                concat!(
                    "fn (first : FirstClassifier)\n",
                    "  (second : SecondClassifier) =>\n",
                    "  result\n",
                ),
            ),
            (
                "exists (first : FirstClassifier) (second : SecondClassifier) . Result",
                concat!(
                    "exists (first : FirstClassifier)\n",
                    "  (second : SecondClassifier) .\n",
                    "  Result\n",
                ),
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
                "  (Value : Type) .\n",
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
                "  (Value : Type) .\n",
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
    fn preserves_separation_of_detached_documentation() {
        [
            ("--| Detached\n\n@[doc] _", "--| Detached\n\n@[doc] _\n"),
            ("--| Detached\n-- barrier\n@[doc] _", "--| Detached\n-- barrier\n@[doc] _\n"),
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
