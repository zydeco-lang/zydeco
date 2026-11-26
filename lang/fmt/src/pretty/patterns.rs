//! Pattern and copattern formatting.

use super::*;

impl<'arena> PrettyFormatter<'arena> {
    pub(super) fn pattern(&self, pattern: PatId) -> RcDoc<'arena> {
        self.pattern_with_requirement(pattern, PatternRequirement::Pattern)
    }

    pub(super) fn view_pattern_head(&self, view: TermId) -> RcDoc<'arena> {
        let (head, arguments) = ViewSpine::parts(view, &|view| {
            let Term::App(Appli(terms)) = &self.arena.terms[&view] else {
                return None;
            };
            terms.split_first().map(|(function, arguments)| (*function, arguments.to_vec()))
        });
        let head = self.term_through(head, TermPrecedence::Application);
        if arguments.is_empty() {
            head
        } else {
            head.append(self.delimited(
                Some(view.into()),
                "[",
                arguments.into_iter().map(|argument| self.term_fragment(argument)).collect(),
                ",",
                "]",
            ))
        }
    }

    pub(super) fn annotated_pattern(&self, pattern: PatId) -> RcDoc<'arena> {
        self.pattern_with_requirement(pattern, PatternRequirement::Annotated)
    }

    /// A parameter keyword already marks the complete binder extent, so a
    /// singleton parenthesized pattern contributes no structure. Elide it
    /// even when its source layout was multiline; structural tuple and alias
    /// delimiters remain because they contain multiple components.
    pub(super) fn parameter_pattern(&self, pattern: PatId) -> RcDoc<'arena> {
        if self.options.parentheses == Parentheses::Minimal
            && let Pattern::Paren(Paren(patterns)) = &self.arena.pats[&pattern]
            && let [inner] = patterns.as_slice()
            && self.grammar.accepts_pattern(PatternRequirement::Annotated, *inner)
        {
            self.with_leading_comments(pattern.into(), self.parameter_pattern(*inner))
        } else {
            self.annotated_pattern(pattern)
        }
    }

    pub(super) fn pattern_with_requirement(
        &self, pattern: PatId, requirement: PatternRequirement,
    ) -> RcDoc<'arena> {
        let key = (pattern, requirement, self.options);
        if let Some(document) = self.documents.patterns.borrow().get(&key) {
            return document.clone();
        }
        let document =
            zydeco_foundation::stack::maybe_grow(|| self.pattern_document(pattern, requirement));
        self.documents.patterns.borrow_mut().insert(key, document.clone());
        document
    }

    pub(super) fn pattern_document(
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
            | Pattern::Lit(literal) => self.literal(literal),
            | Pattern::Project(ProjectionPattern(field, inner)) => {
                self.projection_pattern(pattern, field, *inner)
            }
            | Pattern::View(ViewPattern { function, pattern: inner }) => self
                .view_pattern_head(*function)
                .append(RcDoc::text(" ~> "))
                .append(self.pattern(*inner)),
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
                // A single-line group elides; a multiline group keeps its
                // delimiters hugging the enclosing line with the contents
                // nested inside.
                | [inner]
                    if self.should_elide_parentheses(pattern.into(), (*inner).into())
                        && self.grammar.accepts_pattern(requirement, *inner) =>
                {
                    let elided = self.pattern_with_requirement(*inner, requirement);
                    let grouped = self.delimited(
                        Some(pattern.into()),
                        "(",
                        vec![LayoutFragment::entity(
                            (*inner).into(),
                            self.annotated_pattern(*inner),
                        )],
                        ",",
                        ")",
                    );
                    self.single_line(elided.clone()).union(grouped).flat_alt(elided)
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

    pub(super) fn copattern(&self, pattern: CoPatId) -> RcDoc<'arena> {
        zydeco_foundation::stack::maybe_grow(|| match &self.arena.copats[&pattern] {
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
        })
    }

    pub(super) fn copattern_parameters(&self, pattern: CoPatId) -> Vec<LayoutFragment<'arena>> {
        let mut parameters = Vec::new();
        self.collect_copattern_parameters(pattern, &mut parameters);
        parameters
    }

    pub(super) fn collect_copattern_parameters(
        &self, pattern: CoPatId, out: &mut Vec<LayoutFragment<'arena>>,
    ) {
        zydeco_foundation::stack::maybe_grow(|| {
            let mut chain = Vec::new();
            let mut right_reversed = Vec::new();
            let mut current = pattern;
            let leaf = loop {
                match &self.arena.copats[&current] {
                    | CoPattern::App(Appli(patterns)) => {
                        let Some((&first, rest)) = patterns.split_first() else {
                            unreachable!("copattern applications are nonempty");
                        };
                        chain.push(current);
                        right_reversed.extend(rest.iter().copied().rev());
                        current = first;
                    }
                    | _ => break current,
                }
            };
            let first_index = out.len();
            out.push(LayoutFragment::entity(leaf.into(), self.copattern(leaf)));
            for app in chain.into_iter().rev() {
                let first = &mut out[first_index];
                let document = std::mem::replace(&mut first.document, RcDoc::nil());
                first.document = self.with_leading_comments(app.into(), document);
                first.anchors.first = app.into();
            }
            for item in right_reversed.into_iter().rev() {
                self.collect_copattern_parameters(item, out);
            }
        })
    }

    pub(super) fn pattern_constructor_argument(&self, body: PatId) -> RcDoc<'arena> {
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

    pub(super) fn named_pattern(
        &self, pattern: PatId, field: &FieldName, inner: PatId,
    ) -> RcDoc<'arena> {
        match self.punning.pattern_payload(field, inner) {
            | Some(PunnedPatternPayload::Variable) => RcDoc::text("= ").append(self.field(field)),
            | Some(PunnedPatternPayload::Annotated { variable, classifier }) => {
                RcDoc::text("= ").append(self.field(field)).append(self.layout_boundary(
                    BoundaryIntent::between(variable, classifier),
                    BoundaryLayout::aligned(" :"),
                    self.term_fragment(classifier),
                ))
            }
            | None => self.marked_field(field).append(self.layout_boundary(
                BoundaryIntent::after_start(pattern, inner),
                BoundaryLayout::aligned(" ="),
                LayoutFragment::entity(inner.into(), self.annotated_pattern(inner)),
            )),
        }
    }

    pub(super) fn projection_pattern(
        &self, pattern: PatId, field: &FieldName, inner: PatId,
    ) -> RcDoc<'arena> {
        match self.punning.pattern_payload(field, inner) {
            | Some(PunnedPatternPayload::Variable) => RcDoc::text("/").append(self.field(field)),
            | Some(PunnedPatternPayload::Annotated { variable, classifier }) => {
                RcDoc::text("/").append(self.field(field)).append(self.layout_boundary(
                    BoundaryIntent::between(variable, classifier),
                    BoundaryLayout::aligned(" :"),
                    self.term_fragment(classifier),
                ))
            }
            | None => RcDoc::text("/").append(self.field(field)).append(self.layout_boundary(
                BoundaryIntent::after_start(pattern, inner),
                BoundaryLayout::aligned(" ="),
                LayoutFragment::entity(inner.into(), self.annotated_pattern(inner)),
            )),
        }
    }
}
