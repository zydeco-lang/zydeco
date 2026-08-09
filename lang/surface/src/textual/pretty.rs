//! Configurable pretty printing for canonical textual syntax.

mod config;
mod context;
mod punning;

use super::syntax::*;
pub use config::{LayoutIntentions, Parentheses, PrettyOptions};
use context::{
    PatternRequirement, RenderedPatternClass, RenderedTermClass, TermPrecedence, TermRequirement,
};
use pretty::RcDoc;
pub use punning::NamedTermPunningAudit;
use punning::{PunnedPatternPayload, PunnedTermPayload, Punning};
use zydeco_syntax::Pretty;

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
            | LineSeparation::BlankLine => RcDoc::nesting(|nesting| {
                let nesting = isize::try_from(nesting).unwrap_or(isize::MAX);
                RcDoc::hardline().append(RcDoc::hardline()).nest(-nesting)
            }),
        }
    }

    fn delimited(
        &'arena self, entity: Option<EntityId>, open: &'static str, items: Vec<RcDoc<'arena>>,
        separator: &'static str, close: &'static str,
    ) -> RcDoc<'arena> {
        if items.is_empty() {
            return RcDoc::text(open).append(RcDoc::text(close));
        }
        let items = RcDoc::intersperse(items, RcDoc::text(separator).append(RcDoc::line()));
        let document = RcDoc::text(open)
            .append(RcDoc::line_().append(items).nest(self.options.indent))
            .append(RcDoc::line_())
            .append(RcDoc::text(close));
        self.group(entity, document)
    }

    fn group(&self, entity: Option<EntityId>, document: RcDoc<'arena>) -> RcDoc<'arena> {
        let preserve_break = self.options.layout_intentions == LayoutIntentions::Preserve
            && entity.is_some_and(|entity| {
                self.arena.intentions.line_layout(entity) == Some(LineLayout::Multiline)
            });
        if preserve_break { document } else { document.group() }
    }

    fn annotation(
        &'arena self, entity: EntityId, term: RcDoc<'arena>, ty: TermId, parenthesized: bool,
    ) -> RcDoc<'arena> {
        let annotation = term.append(RcDoc::text(" :")).append(RcDoc::line().append(self.term(ty)));
        if parenthesized {
            self.delimited(Some(entity), "(", vec![annotation], ",", ")")
        } else {
            self.group(Some(entity), annotation)
        }
    }

    /// Elide a transparent group unless its delimiters carry a preserved
    /// multiline layout boundary.
    fn should_elide_parentheses(&self, entity: EntityId) -> bool {
        self.options.parentheses == Parentheses::Minimal
            && !(self.options.layout_intentions == LayoutIntentions::Preserve
                && self.arena.intentions.line_layout(entity) == Some(LineLayout::Multiline))
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
                self.annotated_pattern(*tm),
                *ty,
                requirement == PatternRequirement::Pattern,
            ),
            | Pattern::Manifest(ManifestPattern { binder, definition }) => self.delimited(
                Some(pattern.into()),
                "(",
                vec![
                    self.annotated_pattern(*binder)
                        .append(RcDoc::text(" as "))
                        .append(self.term(*definition)),
                ],
                ",",
                ")",
            ),
            | Pattern::Hole(_) => RcDoc::text("_"),
            | Pattern::Var(definition) => self.definition(*definition),
            | Pattern::Named(Named(field, inner)) => self.named_pattern(field, *inner),
            | Pattern::Ctor(Ctor(name, inner)) => {
                self.constructor(name).append(self.pattern_constructor_argument(*inner))
            }
            | Pattern::Project(ProjectionPattern(field, inner)) => {
                self.projection_pattern(field, *inner)
            }
            | Pattern::Alias(Alias(patterns)) => self.delimited(
                Some(pattern.into()),
                "(",
                patterns.iter().map(|pattern| self.annotated_pattern(*pattern)).collect(),
                ";",
                ")",
            ),
            | Pattern::Paren(Paren(patterns)) => match patterns.as_slice() {
                | [inner]
                    if self.should_elide_parentheses(pattern.into())
                        && self.pattern_requirement_accepts(requirement, *inner) =>
                {
                    self.pattern_with_requirement(*inner, requirement)
                }
                | _ => self.delimited(
                    Some(pattern.into()),
                    "(",
                    patterns.iter().map(|pattern| self.annotated_pattern(*pattern)).collect(),
                    ",",
                    ")",
                ),
            },
        };
        let document = if requirement.accepts(self.rendered_pattern_class(pattern)) {
            document
        } else {
            self.delimited(Some(pattern.into()), "(", vec![document], ",", ")")
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
        self.group(
            Some(pattern.into()),
            RcDoc::intersperse(
                patterns.iter().map(|pattern| match self.arena.copats[pattern] {
                    | CoPattern::App(_) => self.with_leading_comments(
                        (*pattern).into(),
                        self.copattern_application(*pattern),
                    ),
                    | _ => self.copattern(*pattern),
                }),
                RcDoc::line(),
            ),
        )
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
                .append(RcDoc::text("] "))
                .append(self.term_through(*inner, TermPrecedence::Binder)),
            | Term::SourceBoundary(_) => unreachable!("source boundaries return before rendering"),
            | Term::Ann(Ann { tm, ty }) => self.annotation(
                term.into(),
                self.annotated_term(*tm),
                *ty,
                requirement != TermRequirement::Annotated,
            ),
            | Term::Hole(_) => RcDoc::text("_"),
            | Term::Var(name) => self.variable(name),
            | Term::Named(Named(field, inner)) => self.named_term(term, field, *inner),
            | Term::Label(Label(field, inner)) => {
                self.field(field).append(RcDoc::text(" :: ")).append(self.annotated_term(*inner))
            }
            | Term::Paren(Paren(terms)) => match terms.as_slice() {
                | [inner]
                    if (self.should_elide_parentheses(term.into())
                        || self.transparently_groups_application(*inner))
                        && self.term_requirement_accepts(requirement, *inner) =>
                {
                    self.term_with_requirement(*inner, requirement)
                }
                | _ => self.delimited(
                    Some(term.into()),
                    "(",
                    terms.iter().map(|term| self.annotated_term(*term)).collect(),
                    ",",
                    ")",
                ),
            },
            | Term::Abs(Abs(pattern, body)) => self.group(
                Some(term.into()),
                RcDoc::text("fn ")
                    .append(self.copattern(*pattern))
                    .append(RcDoc::text(" =>"))
                    .append(
                        RcDoc::line()
                            .append(self.term_through(*body, TermPrecedence::Binder))
                            .nest(self.options.indent),
                    ),
            ),
            | Term::App(Appli(terms)) => self.application(term, terms),
            | Term::KontCall(KontCall { body, tail }) => RcDoc::text("do~ ")
                .append(self.term_through(*body, TermPrecedence::Binder))
                .append(RcDoc::text(";"))
                .append(
                    RcDoc::line()
                        .append(self.term_through(*tail, TermPrecedence::Binder))
                        .nest(self.options.indent),
                ),
            | Term::Fix(Fix(pattern, body)) => self.group(
                Some(term.into()),
                RcDoc::text("fix ")
                    .append(self.pattern(*pattern))
                    .append(RcDoc::text(" =>"))
                    .append(
                        RcDoc::line()
                            .append(self.term_through(*body, TermPrecedence::Binder))
                            .nest(self.options.indent),
                    ),
            ),
            | Term::Pi(Pi(pattern, body)) => self.quantifier(term, "pi", *pattern, *body),
            | Term::Forall(Forall(pattern, body)) => {
                self.quantifier(term, "forall", *pattern, *body)
            }
            | Term::Arrow(Arrow(input, output)) => self.group(
                Some(term.into()),
                self.term_through(*input, TermPrecedence::Product)
                    .append(RcDoc::text(" ->"))
                    .append(
                        RcDoc::line()
                            .append(self.term_through(*output, TermPrecedence::Arrow))
                            .nest(self.options.indent),
                    ),
            ),
            | Term::Sigma(Sigma(pattern, body)) => self.quantifier(term, "sigma", *pattern, *body),
            | Term::Exists(exists) => self.exists(term, exists),
            | Term::Prod(Prod(left, right)) => self.group(
                Some(term.into()),
                self.term_through(*left, TermPrecedence::Application)
                    .append(RcDoc::text(" *"))
                    .append(
                        RcDoc::line()
                            .append(self.term_through(*right, TermPrecedence::Product))
                            .nest(self.options.indent),
                    ),
            ),
            | Term::Thunk(Thunk(body)) => {
                self.delimited(Some(term.into()), "{", vec![self.term(*body)], ",", "}")
            }
            | Term::Force(Force(body)) => {
                RcDoc::text("! ").append(self.term_through(*body, TermPrecedence::Atom))
            }
            | Term::Ret(Return(body)) => {
                RcDoc::text("ret ").append(self.term_through(*body, TermPrecedence::Atom))
            }
            | Term::Do(Bind { binder, bindee, tail }) => RcDoc::text("do ")
                .append(self.pattern(*binder))
                .append(RcDoc::text(" <- "))
                .append(self.term_through(*bindee, TermPrecedence::Binder))
                .append(RcDoc::text(";"))
                .append(
                    RcDoc::line()
                        .append(self.term_through(*tail, TermPrecedence::Binder))
                        .nest(self.options.indent),
                ),
            | Term::Let(GenLet { binding, tail }) => {
                RcDoc::text("let ").append(self.placed_binding(binding, Placement::In)).append(
                    RcDoc::line()
                        .append(self.term_through(*tail, TermPrecedence::Binder))
                        .nest(self.options.indent),
                )
            }
            | Term::Param(Param { binder, placement, tail }) => RcDoc::text("param ")
                .append(self.annotated_pattern(*binder))
                .append(RcDoc::text(" "))
                .append(self.placement(*placement))
                .append(RcDoc::hardline())
                .append(self.term_through(*tail, TermPrecedence::Binder)),
            | Term::ContextBind(ContextBind { mode, binding, placement, tail }) => {
                let keyword = match mode {
                    | DefinitionMode::Transparent => "let ",
                    | DefinitionMode::Nominal => "def ",
                };
                RcDoc::text(keyword)
                    .append(self.placed_binding(binding, *placement))
                    .append(RcDoc::hardline())
                    .append(self.term_through(*tail, TermPrecedence::Binder))
            }
            | Term::Block(Block(body)) => self.block("begin", *body, "end"),
            | Term::MoBlock(MoBlock(body)) => self.block("monadic", *body, "end"),
            | Term::Data(Data { arms }) => self.data(arms),
            | Term::CoData(CoData { arms }) => self.codata(arms),
            | Term::Ctor(Ctor(name, body)) => {
                self.constructor(name).append(self.term_constructor_argument(term, *body))
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
            self.delimited(Some(term.into()), "(", vec![document], ",", ")")
        };
        self.with_leading_comments(term.into(), document)
    }

    fn application(&'arena self, term: TermId, terms: &[TermId]) -> RcDoc<'arena> {
        let [head, first_argument, arguments @ ..] = terms else {
            unreachable!("application terms always contain a function and an argument")
        };
        let head = self.term_through(*head, TermPrecedence::Application);
        let arguments = RcDoc::intersperse(
            std::iter::once(first_argument)
                .chain(arguments)
                .map(|argument| self.term_through(*argument, TermPrecedence::Projection)),
            RcDoc::line(),
        );
        self.group(
            Some(term.into()),
            head.append(RcDoc::line().append(arguments).nest(self.options.indent)),
        )
    }

    fn named_term(&'arena self, _term: TermId, field: &FieldName, inner: TermId) -> RcDoc<'arena> {
        let payload = Punning::new(self.arena).term_payload(field, inner);
        match payload {
            | Some(PunnedTermPayload::Variable) => RcDoc::text("= ").append(self.field(field)),
            | Some(PunnedTermPayload::Annotated(ty)) => RcDoc::text("= ")
                .append(self.field(field))
                .append(RcDoc::text(" : "))
                .append(self.term(ty)),
            | _ => self.field(field).append(RcDoc::text(" = ")).append(self.annotated_term(inner)),
        }
    }

    fn term_constructor_argument(&'arena self, constructor: TermId, body: TermId) -> RcDoc<'arena> {
        match &self.arena.terms[&body] {
            | Term::Paren(Paren(terms)) => self.with_leading_comments(
                body.into(),
                self.delimited(
                    Some(body.into()),
                    "(",
                    terms.iter().map(|term| self.annotated_term(*term)).collect(),
                    ",",
                    ")",
                ),
            ),
            | _ => self.delimited(
                Some(constructor.into()),
                "(",
                vec![self.annotated_term(body)],
                ",",
                ")",
            ),
        }
    }

    fn pattern_constructor_argument(&'arena self, body: PatId) -> RcDoc<'arena> {
        match &self.arena.pats[&body] {
            | Pattern::Paren(Paren(patterns)) => self.with_leading_comments(
                body.into(),
                self.delimited(
                    Some(body.into()),
                    "(",
                    patterns.iter().map(|pattern| self.annotated_pattern(*pattern)).collect(),
                    ",",
                    ")",
                ),
            ),
            | _ => {
                self.delimited(Some(body.into()), "(", vec![self.annotated_pattern(body)], ",", ")")
            }
        }
    }

    fn named_pattern(&'arena self, field: &FieldName, inner: PatId) -> RcDoc<'arena> {
        match Punning::new(self.arena).pattern_payload(field, inner) {
            | Some(PunnedPatternPayload::Variable) => RcDoc::text("= ").append(self.field(field)),
            | Some(PunnedPatternPayload::Annotated(ty)) => RcDoc::text("= ")
                .append(self.field(field))
                .append(RcDoc::text(" : "))
                .append(self.term(ty)),
            | None => {
                self.field(field).append(RcDoc::text(" = ")).append(self.annotated_pattern(inner))
            }
        }
    }

    fn projection_pattern(&'arena self, field: &FieldName, inner: PatId) -> RcDoc<'arena> {
        match Punning::new(self.arena).pattern_payload(field, inner) {
            | Some(PunnedPatternPayload::Variable) => RcDoc::text("/").append(self.field(field)),
            | Some(PunnedPatternPayload::Annotated(ty)) => RcDoc::text("/")
                .append(self.field(field))
                .append(RcDoc::text(" : "))
                .append(self.term(ty)),
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
        &'arena self, term: TermId, keyword: &'static str, pattern: CoPatId, body: TermId,
    ) -> RcDoc<'arena> {
        self.group(
            Some(term.into()),
            RcDoc::text(keyword)
                .append(RcDoc::text(" "))
                .append(self.copattern(pattern))
                .append(RcDoc::text(" ."))
                .append(
                    RcDoc::line()
                        .append(self.term_through(body, TermPrecedence::Quantifier))
                        .nest(self.options.indent),
                ),
        )
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
        let separator = if begins_with_documentation { RcDoc::hardline() } else { RcDoc::space() };
        let parameters = RcDoc::intersperse(
            std::iter::once(self.existential_parameter(first))
                .chain(rest.iter().map(|parameter| self.existential_parameter(parameter))),
            RcDoc::line(),
        );
        self.group(
            Some(term.into()),
            RcDoc::text("exists")
                .append(separator.append(parameters).nest(self.options.indent))
                .append(RcDoc::text(" ."))
                .append(RcDoc::line().append(self.term(*body)).nest(self.options.indent)),
        )
    }

    fn existential_parameter(&'arena self, parameter: &ExistentialParameter) -> RcDoc<'arena> {
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
                    Some(parameter.binder.into()),
                    "(",
                    vec![self.annotated_pattern(parameter.binder)],
                    ",",
                    ")",
                ),
            });
        RcDoc::intersperse(annotations.chain(std::iter::once(binder)), RcDoc::line()).group()
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
        if self.should_elide_parentheses(pattern.into())
            && let Pattern::Paren(Paren(patterns)) = &self.arena.pats[&pattern]
            && let [inner] = patterns.as_slice()
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
            self.annotated_pattern(binder),
            |inner, (depth, field)| {
                if depth == 0 {
                    match Punning::new(self.arena).pattern_payload(field, binder) {
                        | Some(PunnedPatternPayload::Variable) => {
                            RcDoc::text("= ").append(self.field(field))
                        }
                        | Some(PunnedPatternPayload::Annotated(ty)) => RcDoc::text("= ")
                            .append(self.field(field))
                            .append(RcDoc::text(" : "))
                            .append(self.term(ty)),
                        | None => self.field(field).append(RcDoc::text(" = ")).append(inner),
                    }
                } else {
                    self.field(field).append(RcDoc::text(" = ")).append(inner)
                }
            },
        );
        let binder = binder.append(RcDoc::text(" as ")).append(self.term(definition));
        let binder = match classifier {
            | Some(classifier) => binder.append(RcDoc::text(" : ")).append(self.term(classifier)),
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
        let head = modifiers
            .chain(std::iter::once(self.pattern(*binder)))
            .chain(params.iter().map(|params| self.copattern(*params)));
        let head = RcDoc::intersperse(head, RcDoc::space());
        let head = match ty {
            | Some(ty) => head.append(RcDoc::text(" : ")).append(self.term(*ty)),
            | None => head,
        };
        let bindee = self
            .term(*bindee)
            .append(RcDoc::line().append(self.placement(placement)).nest(-self.options.indent))
            .group();
        head.append(RcDoc::text(" ="))
            .append(RcDoc::line().append(bindee).nest(self.options.indent))
            .group()
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
                    .append(RcDoc::text(" : "))
                    .append(self.term(arm.param))
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
                document
                    .append(RcDoc::hardline())
                    .append(arm)
                    .append(RcDoc::text(" : "))
                    .append(self.term(item.out))
            })
            .append(RcDoc::hardline())
            .append(RcDoc::text("end"))
    }

    fn matcher(&'arena self, scrutinee: TermId, arms: &[Matcher<PatId, TermId>]) -> RcDoc<'arena> {
        arms.iter()
            .fold(RcDoc::text("match ").append(self.term(scrutinee)), |document, arm| {
                document
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("| "))
                    .append(self.pattern(arm.binder))
                    .append(RcDoc::text(" => "))
                    .append(self.term(arm.tail))
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
                    .append(RcDoc::text(" => "))
                    .append(self.term(arm.tail))
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
            parsed.parser.arena.intentions.line_layout(parsed.unit.root.into()),
            Some(LineLayout::Multiline)
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
                concat!("do x <- first;\n", "  do y <- second;\n", "    ret y\n"),
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
    fn optionally_preserves_multiline_layout() {
        let parsed = ParsedSource::new("(first = first,\nsecond = second)");

        assert_eq!(parsed.render(LayoutIntentions::Preserve), "(\n  = first,\n  = second\n)\n");
        assert_eq!(parsed.render(LayoutIntentions::Ignore), "(= first, = second)\n");
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
        assert!(narrow.contains("\n      /VType;\n"));
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
