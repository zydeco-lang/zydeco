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
        self.render_doc(unit.pretty(self).append(RcDoc::hardline()))
    }

    /// Render one term without adding a trailing newline.
    pub fn render_term(&'arena self, term: TermId) -> String {
        self.render_doc(term.pretty(self))
    }

    /// Render one pattern without adding a trailing newline.
    pub fn render_pattern(&'arena self, pattern: PatId) -> String {
        self.render_doc(pattern.pretty(self))
    }

    /// Render one copattern spine without adding a trailing newline.
    pub fn render_copattern(&'arena self, pattern: CoPatId) -> String {
        self.render_doc(pattern.pretty(self))
    }

    fn render_doc(&self, document: RcDoc<'arena>) -> String {
        let mut output = String::new();
        document.render_fmt(self.options.line_width, &mut output).unwrap();
        output
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
        let annotation = term.append(RcDoc::text(" : ")).append(self.term(ty));
        if parenthesized {
            self.delimited(Some(entity), "(", vec![annotation], ",", ")")
        } else {
            self.group(Some(entity), annotation)
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
                    if self.options.parentheses == Parentheses::Minimal
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
        if requirement.accepts(self.rendered_pattern_class(pattern)) {
            document
        } else {
            self.delimited(Some(pattern.into()), "(", vec![document], ",", ")")
        }
    }

    fn copattern(&'arena self, pattern: CoPatId) -> RcDoc<'arena> {
        match &self.arena.copats[&pattern] {
            | CoPattern::Pat(pattern) => self.pattern(*pattern),
            | CoPattern::Dtor(name) => self.destructor(name),
            | CoPattern::App(Appli(patterns)) => self.group(
                Some(pattern.into()),
                RcDoc::intersperse(
                    patterns.iter().map(|pattern| self.copattern(*pattern)),
                    RcDoc::line(),
                ),
            ),
        }
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
            return self.term_with_requirement(*inner, requirement);
        }
        let document = match &self.arena.terms[&term] {
            | Term::Meta(MetaT(meta, inner)) => RcDoc::text("@[")
                .append(RcDoc::text(meta.to_string()))
                .append(RcDoc::text("] "))
                .append(self.term_through(*inner, TermPrecedence::Quantifier)),
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
                    if self.options.parentheses == Parentheses::Minimal
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
                            .append(self.term_through(*body, TermPrecedence::Quantifier))
                            .nest(self.options.indent),
                    ),
            ),
            | Term::App(Appli(terms)) => self.delimited(
                Some(term.into()),
                "(",
                terms
                    .iter()
                    .enumerate()
                    .map(|(index, term)| {
                        self.term_through(
                            *term,
                            if index == 0 {
                                TermPrecedence::Application
                            } else {
                                TermPrecedence::Projection
                            },
                        )
                    })
                    .collect(),
                "",
                ")",
            ),
            | Term::KontCall(KontCall { body, tail }) => RcDoc::text("do~ ")
                .append(self.term_through(*body, TermPrecedence::Quantifier))
                .append(RcDoc::text(";"))
                .append(
                    RcDoc::line()
                        .append(self.term_through(*tail, TermPrecedence::Quantifier))
                        .nest(self.options.indent),
                ),
            | Term::Fix(Fix(pattern, body)) => self.group(
                Some(term.into()),
                RcDoc::text("fix ")
                    .append(self.pattern(*pattern))
                    .append(RcDoc::text(" =>"))
                    .append(
                        RcDoc::line()
                            .append(self.term_through(*body, TermPrecedence::Quantifier))
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
                .append(self.term_through(*bindee, TermPrecedence::Quantifier))
                .append(RcDoc::text(";"))
                .append(
                    RcDoc::line()
                        .append(self.term_through(*tail, TermPrecedence::Quantifier))
                        .nest(self.options.indent),
                ),
            | Term::Let(GenLet { binding, tail }) => {
                RcDoc::text("let ").append(self.binding(binding)).append(RcDoc::text(" in")).append(
                    RcDoc::line()
                        .append(self.term_through(*tail, TermPrecedence::Quantifier))
                        .nest(self.options.indent),
                )
            }
            | Term::Param(Param { binder, placement, tail }) => RcDoc::text("param ")
                .append(self.annotated_pattern(*binder))
                .append(RcDoc::text(" "))
                .append(self.placement(*placement))
                .append(RcDoc::hardline())
                .append(self.term_through(*tail, TermPrecedence::Quantifier)),
            | Term::ContextBind(ContextBind { mode, binding, placement, tail }) => {
                let keyword = match mode {
                    | DefinitionMode::Transparent => "let ",
                    | DefinitionMode::Nominal => "def ",
                };
                RcDoc::text(keyword)
                    .append(self.binding(binding))
                    .append(RcDoc::hardline())
                    .append(self.placement(*placement))
                    .append(RcDoc::hardline())
                    .append(self.term_through(*tail, TermPrecedence::Quantifier))
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
        if requirement.accepts(self.rendered_term_class(term)) {
            document
        } else {
            self.delimited(Some(term.into()), "(", vec![document], ",", ")")
        }
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
            | Term::Paren(Paren(terms)) => self.delimited(
                Some(body.into()),
                "(",
                terms.iter().map(|term| self.annotated_term(*term)).collect(),
                ",",
                ")",
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
            | Pattern::Paren(Paren(patterns)) => self.delimited(
                Some(body.into()),
                "(",
                patterns.iter().map(|pattern| self.annotated_pattern(*pattern)).collect(),
                ",",
                ")",
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
            | Term::App(_)
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
            | Term::Dtor(_) => RenderedTermClass::Term(TermPrecedence::Application),
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
                        .append(self.term_through(body, TermPrecedence::Arrow))
                        .nest(self.options.indent),
                ),
        )
    }

    fn exists(&'arena self, term: TermId, exists: &Exists) -> RcDoc<'arena> {
        let Exists { parameters, body } = exists;
        let parameters = parameters.iter().map(|parameter| self.existential_parameter(parameter));
        self.group(
            Some(term.into()),
            RcDoc::text("exists ")
                .append(RcDoc::intersperse(parameters, RcDoc::line()))
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
        if self.options.parentheses == Parentheses::Minimal
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
        self.delimited(Some(entity.into()), "(", vec![binder], ",", ")")
    }

    fn binding(&'arena self, binding: &GenBind<TermId>) -> RcDoc<'arena> {
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
        head.append(RcDoc::text(" ="))
            .append(RcDoc::line().append(self.term(*bindee)).nest(self.options.indent))
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
            .append(RcDoc::hardline())
            .append(self.annotated_term(body).nest(self.options.indent))
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

    fn definition(&self, definition: DefId) -> RcDoc<'arena> {
        self.variable(&self.arena.defs[&definition])
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
    use crate::textual::{Lexer, SourceUnitParser};
    use zydeco_syntax::Ugly;
    use zydeco_utils::pass::CompilerPass;
    use zydeco_utils::span::LocationCtx;

    struct ParsedSource {
        unit: SourceUnit,
        parser: Parser,
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
                "let (= field, /projected) = input\n",
                "in\n",
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
            ("f (value/field)", "(f value/field)\n"),
            ("((field = field))", "(= field)\n"),
            ("comatch x => x end", "fn x => x\n"),
            ("! comatch x => x end", "! (fn x => x)\n"),
        ];

        cases.into_iter().for_each(|(source, expected)| {
            let parsed = ParsedSource::new(source);
            assert_eq!(parsed.render(LayoutIntentions::Ignore), expected, "source: {source}");
            ParsedSource::new(expected);
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
        });
    }
}
