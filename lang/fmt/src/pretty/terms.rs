//! Term, metadata, application, and leaf pretty-printing.

use super::*;

impl<'arena> PrettyFormatter<'arena> {
    pub(super) fn term(&self, term: TermId) -> RcDoc<'arena> {
        self.term_with_requirement(term, TermRequirement::Any)
    }

    pub(super) fn annotated_term(&self, term: TermId) -> RcDoc<'arena> {
        self.term_with_requirement(term, TermRequirement::Annotated)
    }

    pub(super) fn term_through(&self, term: TermId, precedence: TermPrecedence) -> RcDoc<'arena> {
        self.term_with_requirement(term, TermRequirement::Through(precedence))
    }

    pub(super) fn term_fragment(&self, term: TermId) -> LayoutFragment<'arena> {
        self.term_fragment_with_requirement(term, TermRequirement::Any)
    }

    pub(super) fn annotated_term_fragment(&self, term: TermId) -> LayoutFragment<'arena> {
        self.term_fragment_with_requirement(term, TermRequirement::Annotated)
    }

    pub(super) fn term_through_fragment(
        &self, term: TermId, precedence: TermPrecedence,
    ) -> LayoutFragment<'arena> {
        self.term_fragment_with_requirement(term, TermRequirement::Through(precedence))
    }

    pub(super) fn term_fragment_with_requirement(
        &self, term: TermId, requirement: TermRequirement,
    ) -> LayoutFragment<'arena> {
        LayoutFragment::entity(term.into(), self.term_with_requirement(term, requirement))
    }

    pub(super) fn term_with_requirement(
        &self, term: TermId, requirement: TermRequirement,
    ) -> RcDoc<'arena> {
        let key = (term, requirement, self.options);
        if let Some(document) = self.documents.terms.borrow().get(&key) {
            return document.clone();
        }
        let document =
            zydeco_foundation::stack::maybe_grow(|| self.term_document(term, requirement));
        self.documents.terms.borrow_mut().insert(key, document.clone());
        document
    }

    // One arm per term form is the clearest shape for this printer.
    #[allow(clippy::too_many_lines)]
    pub(super) fn term_document(
        &self, term: TermId, requirement: TermRequirement,
    ) -> RcDoc<'arena> {
        if let Term::SourceBoundary(SourceBoundary(inner))
        | Term::SignatureBoundary(SignatureBoundary(inner)) = &self.arena.terms[&term]
        {
            let document = self.term_with_requirement(*inner, requirement);
            return self.with_term_comments(term, requirement, document);
        }
        let document = match &self.arena.terms[&term] {
            | Term::Meta(MetaTerm(meta, inner)) => {
                let semantic = self.arena.semantic_meta(*meta);
                match semantic.specialize::<FormatMeta>() {
                    | Ok(Some(directive)) => self.format_annotated(term, *meta, *inner, directive),
                    | Ok(None) | Err(_) => match &self.arena.terms[inner] {
                        // A commentless hole payload collapses into the parenthesized sugar.
                        // A commented hole keeps the bracket form so its comments survive.
                        | Term::Hole(_)
                            if self
                                .presentation
                                .trivia
                                .leading_comments((*inner).into())
                                .is_empty()
                                && self
                                    .presentation
                                    .trivia
                                    .trailing_comments((*inner).into())
                                    .is_empty() =>
                        {
                            self.metadata_wrapper("@(", ")", *meta)
                        }
                        | _ => self.annotation_prefix(*meta).append(self.layout_boundary(
                            BoundaryIntent::after_start(term, *inner),
                            BoundaryLayout::aligned(""),
                            self.term_through_fragment(*inner, TermPrecedence::Binder),
                        )),
                    },
                }
            }
            | Term::SourceBoundary(_) | Term::SignatureBoundary(_) => {
                unreachable!("source boundaries return before rendering")
            }
            | Term::Ann(Ann { tm, ty }) => self.annotation(
                term.into(),
                self.term_fragment(*tm),
                *ty,
                requirement != TermRequirement::Annotated,
            ),
            | Term::Seal(Seal { tm, ty }) => {
                let value = self.term_through_fragment(*tm, TermPrecedence::Binder);
                let anchors = value.anchors;
                let head = LayoutFragment {
                    document: self.prefixed(
                        term,
                        "seal",
                        BoundaryLayout::hanging("", self.indent()),
                        value,
                    ),
                    anchors,
                };
                self.staged_join(
                    head,
                    self.term_through_fragment(*ty, TermPrecedence::Binder),
                    StagedBoundary::Annotation,
                )
                .document
            }
            | Term::Hole(_) => RcDoc::text("_"),
            | Term::Var(name) => self.variable(name),
            | Term::Named(Named(field, inner)) => self.named_term(term, field, *inner),
            | Term::Label(Label(field, inner)) => {
                self.marked_field(field).append(self.layout_boundary(
                    BoundaryIntent::after_start(term, *inner),
                    BoundaryLayout::aligned(" ::"),
                    self.annotated_term_fragment(*inner),
                ))
            }
            | Term::Paren(Paren(terms)) => match terms.as_slice() {
                // An application owns the grouping layout, so its singleton
                // wrapper is always redundant.
                | [inner]
                    if self.grammar.accepts_term(requirement, *inner)
                        && self.term_layout_subsumes_group(*inner) =>
                {
                    self.term_with_requirement(*inner, requirement)
                }
                // A single-line group elides; a multiline group keeps its
                // delimiters hugging the enclosing line with the contents
                // nested inside.
                | [inner]
                    if self.grammar.accepts_term(requirement, *inner)
                        && self.should_elide_parentheses(term.into(), (*inner).into()) =>
                {
                    let elided = self.term_with_requirement(*inner, requirement);
                    let grouped = self.delimited(
                        Some(term.into()),
                        "(",
                        vec![self.annotated_term_fragment(*inner)],
                        ",",
                        ")",
                    );
                    self.single_line(elided.clone()).union(grouped).flat_alt(elided)
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
            | Term::ValAbs(_) => self.scoped_form(term, ScopedForm::ValueFunction),
            | Term::App(Appli(terms)) => self.application(terms),
            | Term::Fix(Fix(pattern, body)) => {
                self.separator_join(
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
            | Term::ValPi(_) => self.scoped_form(term, ScopedForm::ValPi),
            | Term::Forall(_) => self.scoped_form(term, ScopedForm::Forall),
            | Term::Arrow(_) => self.infix_chain(term, InfixOperator::Arrow),
            | Term::Sigma(_) => self.scoped_form(term, ScopedForm::Sigma),
            | Term::Exists(exists) => self.exists(term, exists),
            | Term::Pack(pack) => self.pack(term, pack),
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
                self.term_through_fragment(*body, TermPrecedence::Prefix),
            ),
            | Term::Ret(Return(body)) => self.prefixed(
                term,
                "ret",
                BoundaryLayout::hanging("", self.indent()),
                self.term_through_fragment(*body, TermPrecedence::Prefix),
            ),
            | Term::Do(Bind { binder, bindee, tail }) => self
                .prefixed(
                    term,
                    "do",
                    BoundaryLayout::aligned(""),
                    LayoutFragment::entity((*binder).into(), self.pattern(*binder)),
                )
                .append(self.layout_boundary(
                    BoundaryIntent::between(*binder, *bindee),
                    BoundaryLayout::hanging(" <-", self.indent()),
                    self.term_through_fragment(*bindee, TermPrecedence::Binder),
                ))
                .append(RcDoc::text(";"))
                .append(self.sequence_tail((*bindee).into(), *tail)),
            | Term::Let(GenLet { binding, tail }) => RcDoc::text("let")
                .append(self.placed_binding(term, binding, Placement::In))
                .append(self.sequence_tail(binding.bindee.into(), *tail)),
            | Term::Param(Param { flavor, binder, placement, tail }) => self
                .prefixed(
                    term,
                    match flavor {
                        | ParameterFlavor::Plain => "param",
                        | ParameterFlavor::Value => "param val",
                    },
                    BoundaryLayout::aligned(""),
                    LayoutFragment::entity((*binder).into(), self.parameter_pattern(*binder)),
                )
                .append(RcDoc::text(" "))
                .append(self.placement(*placement))
                .append(self.sequence_tail((*binder).into(), *tail)),
            | Term::Pipeline(Pipeline { direction, subject, function }) => match direction {
                | PipelineDirection::Forward => self
                    .term_through(*subject, TermPrecedence::ForwardCut)
                    .append(RcDoc::text(" |> "))
                    .append(self.term_through(*function, TermPrecedence::Application)),
                | PipelineDirection::Backward => self
                    .term_through(*function, TermPrecedence::Application)
                    .append(RcDoc::text(" <| "))
                    .append(self.term_with_requirement(*subject, TermRequirement::Any)),
            },
            | Term::ContextBind(ContextBind { mode, binding, placement, tail }) => {
                let keyword = match mode {
                    | DefinitionMode::Transparent => "let",
                    | DefinitionMode::Nominal => "def",
                };
                RcDoc::text(keyword)
                    .append(self.placed_binding(term, binding, *placement))
                    .append(self.sequence_tail(binding.bindee.into(), *tail))
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
        self.with_term_comments(term, requirement, document)
    }

    /// Attach a term's leading comments, anchoring a block construct that
    /// its grammar position leaves unparenthesized.
    pub(super) fn with_term_comments(
        &self, term: TermId, requirement: TermRequirement, document: RcDoc<'arena>,
    ) -> RcDoc<'arena> {
        if self.grammar.accepts_term(requirement, term) && self.is_block_like(term) {
            self.block_like(term, document)
        } else {
            self.with_leading_comments(term.into(), document)
        }
    }

    /// Sequence bindings and arm blocks are block-like: their interior aligns
    /// with their head.
    pub(super) fn is_block_like(&self, term: TermId) -> bool {
        match &self.arena.terms[&term] {
            | Term::SourceBoundary(SourceBoundary(inner))
            | Term::SignatureBoundary(SignatureBoundary(inner)) => self.is_block_like(*inner),
            | Term::Do(_)
            | Term::Let(_)
            | Term::Param(_)
            | Term::ContextBind(_)
            | Term::Data(_)
            | Term::CoData(_)
            | Term::Match(_)
            | Term::CoMatch(_) => true,
            | _ => false,
        }
    }

    /// Render a `@[format(...)]` annotation, applying its directive to the
    /// payload and everything inside it. The annotation-to-payload boundary
    /// follows the directive's policy as well.
    ///
    /// Indentation, layout intentions, and parenthesis treatment shape the
    /// payload document directly. A width change instead pre-renders the
    /// payload at its own width and embeds the result, because the document
    /// renderer applies one width to the whole document.
    pub(super) fn format_annotated(
        &self, term: TermId, meta: MetaId, inner: TermId, directive: FormatMeta,
    ) -> RcDoc<'arena> {
        if directive.verbatim
            && let Some(document) = self.format_verbatim(term, meta, inner)
        {
            return document;
        }
        let scoped = self.scoped(self.options.with_format_meta(&directive));
        let prefix = self.annotation_prefix(meta);
        let payload = scoped.term_through_fragment(inner, TermPrecedence::Binder);
        if scoped.options.line_width == self.options.line_width {
            return prefix.append(scoped.layout_boundary(
                BoundaryIntent::after_start(term, inner),
                BoundaryLayout::aligned(""),
                payload,
            ));
        }
        let rendered = scoped.render_doc(payload.document);
        if rendered.contains('\n') {
            return prefix.append(self.embedded_block(&rendered));
        }
        prefix.append(scoped.layout_boundary(
            BoundaryIntent::after_start(term, inner),
            BoundaryLayout::aligned(""),
            LayoutFragment::entity(inner.into(), RcDoc::text(rendered)),
        ))
    }

    /// Render a `@[format(verbatim)]` annotation by copying the annotated
    /// payload's original source text unchanged.
    ///
    /// The whitespace between the annotation and its payload is copied too,
    /// so comments or line breaks written there survive exactly. If the
    /// original source is unavailable, the caller falls back to normal
    /// formatting.
    pub(super) fn format_verbatim(
        &self, term: TermId, meta: MetaId, inner: TermId,
    ) -> Option<RcDoc<'arena>> {
        let source = self.source?;
        let outer_start = self.spans[&EntityId::Term(term)].lo();
        let inner = self.spans[&EntityId::Term(inner)].range();
        let (inner_start, inner_end) = (inner.start, inner.end);
        let annotation_end = source
            .get(outer_start..inner_start)?
            .rfind(']')?
            .checked_add(outer_start)?
            .checked_add(1)?;
        let boundary = source.get(annotation_end..inner_start)?;
        let payload = source.get(inner_start..inner_end)?;
        Some(
            self.annotation_prefix(meta).append(RcDoc::text(boundary)).append(RcDoc::text(payload)),
        )
    }

    /// The complete `@[...]` text of one annotation.
    pub(super) fn annotation_prefix(&self, meta: MetaId) -> RcDoc<'arena> {
        let document = self.metadata_wrapper("@[", "]", meta);
        self.with_before_metadata_comments(meta.into(), document)
    }

    pub(super) fn metadata_wrapper(
        &self, open: &'static str, close: &'static str, meta: MetaId,
    ) -> RcDoc<'arena> {
        let metadata = self.metadata(meta);
        if self.presentation.trivia.leading_comments(meta.into()).is_empty() {
            RcDoc::text(open).append(metadata).append(RcDoc::text(close))
        } else {
            RcDoc::text(open)
                .append(RcDoc::hardline().append(metadata).nest(self.indent()))
                .append(RcDoc::hardline())
                .append(RcDoc::text(close))
        }
    }

    pub(super) fn metadata(&self, meta: MetaId) -> RcDoc<'arena> {
        let document = match &self.arena.metas[&meta] {
            | MetaNode::Ident(name) => RcDoc::text(name.clone()),
            | MetaNode::String(value) => RcDoc::text(format!("{value:?}")),
            | MetaNode::Integer(value) => RcDoc::as_string(value),
            | MetaNode::Apply { callee, args } => self.metadata_application(meta, callee, args),
        };
        self.with_leading_comments(meta.into(), document)
    }

    pub(super) fn metadata_fragment(&self, meta: MetaId) -> LayoutFragment<'arena> {
        LayoutFragment::entity(meta.into(), self.metadata(meta))
    }

    /// Choose an application's layout from the metadata itself. Without this
    /// local probe, an unrelated long payload following `@[...]` can force a
    /// short metadata call to expand.
    pub(super) fn metadata_application(
        &self, meta: MetaId, callee: &'arena str, args: &'arena [MetaId],
    ) -> RcDoc<'arena> {
        let head = RcDoc::text(callee);
        if args.is_empty() {
            return head.append(RcDoc::text("()"));
        }
        let layout = self.delimited_group_layout(
            Some(meta.into()),
            "(",
            args.iter().map(|argument| self.metadata_fragment(*argument)).collect(),
            ",",
            ")",
            DelimiterSpacing::Tight,
        );
        let selected = self.select_group_layout(layout.clone());
        let probe = head.clone().append(selected.clone());
        self.select_after_multiline_probe(
            probe,
            head.clone().append(layout.compact),
            head.append(selected),
        )
    }

    /// Place a pre-rendered multiline payload below its annotation, keeping
    /// its relative indentation and its empty lines free of trailing
    /// whitespace.
    pub(super) fn embedded_block(&self, rendered: &str) -> RcDoc<'arena> {
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
    pub(super) fn empty_embedded_line() -> RcDoc<'arena> {
        RcDoc::nesting(|nesting| {
            let nesting = isize::try_from(nesting).unwrap_or(isize::MAX);
            RcDoc::hardline().append(RcDoc::text("").nest(-nesting))
        })
    }

    pub(super) fn application(&self, terms: &[TermId]) -> RcDoc<'arena> {
        let [head, first_argument, arguments @ ..] = terms else {
            unreachable!("application terms always contain a function and an argument")
        };
        let head = LayoutFragment::entity(
            (*head).into(),
            self.term_through(*head, self.application_head_precedence(*head)),
        );
        let arguments = self
            .separated(
                std::iter::once(first_argument)
                    .chain(arguments)
                    .map(|argument| self.term_through_fragment(*argument, TermPrecedence::Prefix))
                    .collect(),
                BoundaryLayout::hanging("", self.indent()),
            )
            .expect("applications contain at least one argument");
        self.join(head, BoundaryLayout::hanging("", self.indent()), arguments).document
    }

    /// How loose an application's head may be. A parenthesized application stays
    /// parenthesized: `(f x) y` applies a result, and `f x y` is one application.
    pub(super) fn application_head_precedence(&self, head: TermId) -> TermPrecedence {
        let parenthesized = matches!(self.arena.terms[&head], Term::Paren(_));
        if parenthesized && self.term_layout_subsumes_group(head) {
            TermPrecedence::Prefix
        } else {
            TermPrecedence::Application
        }
    }

    pub(super) fn named_term(
        &self, term: TermId, field: &FieldName, inner: TermId,
    ) -> RcDoc<'arena> {
        let payload = self.punning.term_payload(field, inner);
        match payload {
            | Some(PunnedTermPayload::Variable) => RcDoc::text("= ").append(self.field(field)),
            | Some(PunnedTermPayload::Annotated { variable, classifier }) => {
                RcDoc::text("= ").append(self.field(field)).append(self.layout_boundary(
                    BoundaryIntent::between(variable, classifier),
                    BoundaryLayout::aligned(" :"),
                    self.term_fragment(classifier),
                ))
            }
            | _ => self.marked_field(field).append(self.layout_boundary(
                BoundaryIntent::after_start(term, inner),
                BoundaryLayout::aligned(" ="),
                self.annotated_term_fragment(inner),
            )),
        }
    }

    pub(super) fn term_constructor_argument(&self, body: TermId) -> RcDoc<'arena> {
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

    pub(super) fn definition(&self, definition: DefId) -> RcDoc<'arena> {
        let document = self.variable(&self.arena.defs[&definition]);
        self.with_leading_comments(definition.into(), document)
    }

    pub(super) fn variable(&self, name: &VarName) -> RcDoc<'arena> {
        RcDoc::text(name.as_str())
    }

    pub(super) fn field(&self, name: &FieldName) -> RcDoc<'arena> {
        RcDoc::text(name.as_str())
    }

    /// A field name marked with `#`, used on the left of `=` and `::`.
    pub(super) fn marked_field(&self, name: &FieldName) -> RcDoc<'arena> {
        RcDoc::text("#").append(self.field(name))
    }

    pub(super) fn constructor(&self, name: &CtorName) -> RcDoc<'arena> {
        RcDoc::text(name.as_str())
    }

    pub(super) fn destructor(&self, name: &DtorName) -> RcDoc<'arena> {
        RcDoc::text(name.as_str())
    }

    pub(super) fn literal(&self, literal: &Literal) -> RcDoc<'arena> {
        RcDoc::text(match literal {
            | Literal::Integer(value) => format!("{value:?}"),
            | Literal::Float(value) => format!("{value:?}"),
            | Literal::String(value) => format!("{value:?}"),
            | Literal::Char(value) => format!("{value:?}"),
        })
    }
}
