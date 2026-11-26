//! Scoped telescopes, existential packages, bindings, and arm blocks.

use super::*;

struct ManifestParameterView<'arena> {
    fields: Vec<(PatId, &'arena FieldName)>,
    binder: PatId,
    definition: TermId,
    classifier: Option<TermId>,
}

impl<'arena> PrettyFormatter<'arena> {
    pub(super) fn scope_boundary_allows_merging(
        &self, parameter: impl Into<EntityId>, nested: TermId,
    ) -> bool {
        self.presentation.trivia.leading_comments(nested.into()).is_empty()
            && !self.forces_break(
                BoundaryIntent::between(parameter, nested).resolve(&self.presentation.intentions),
            )
    }

    pub(super) fn scoped_telescope(
        &self, root: TermId, form: ScopedForm,
    ) -> ScopeTelescope<CoPatId> {
        let layers = std::iter::successors(Some(root), |current| {
            if !form.merges_nested_scopes() {
                return None;
            }
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

    pub(super) fn scoped_form(&self, term: TermId, form: ScopedForm) -> RcDoc<'arena> {
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
        self.separator_join(
            head,
            form.marker(),
            self.term_through_fragment(body, form.body_precedence()),
            self.indent(),
        )
        .document
    }

    pub(super) fn existential_telescope(
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

    pub(super) fn exists(&self, term: TermId, exists: &'arena Exists) -> RcDoc<'arena> {
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
        self.separator_join(head, ".", self.term_fragment(body), self.indent()).document
    }

    pub(super) fn pack(&self, term: TermId, pack: &'arena Pack) -> RcDoc<'arena> {
        let Pack { parameters, body } = pack;
        let Some((first, rest)) = parameters.split_first() else {
            unreachable!("the parser requires at least one pack parameter")
        };
        let parameters = std::iter::once(self.pack_parameter(first))
            .chain(rest.iter().map(|parameter| self.pack_parameter(parameter)))
            .collect::<Vec<_>>();
        let head = self.parameter_telescope(
            RcDoc::text("pack"),
            BoundaryIntent::before_existential_parameter(term, first.parameter.binder),
            &parameters,
        );
        let components = match &self.arena.terms[body] {
            | Term::Paren(Paren(terms)) => terms.as_slice(),
            | _ => std::slice::from_ref(body),
        };
        let components =
            components.iter().map(|component| self.annotated_term_fragment(*component)).collect();
        let payload = self.delimited_with_spacing(
            Some((*body).into()),
            "where",
            components,
            ",",
            "end",
            DelimiterSpacing::Spaced,
        );
        head.append(RcDoc::line()).append(self.with_leading_comments((*body).into(), payload))
    }

    pub(super) fn existential_parameter(
        &self, parameter: &ExistentialParameter,
    ) -> LayoutFragment<'arena> {
        let annotations =
            parameter.annotations.iter().map(|annotation| self.annotation_prefix(annotation.inner));
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
        let binder = self.with_before_existential_comments(parameter.binder.into(), binder);
        LayoutFragment::entity(
            parameter.binder.into(),
            RcDoc::intersperse(annotations.chain(std::iter::once(binder)), RcDoc::line()).group(),
        )
    }

    pub(super) fn pack_parameter(&self, parameter: &PackParameter) -> LayoutFragment<'arena> {
        let fragment = self.existential_parameter(&parameter.parameter);
        match parameter.evidence {
            | Some(evidence) => fragment.map_document(|document| {
                document.append(RcDoc::text(" is ")).append(self.term(evidence))
            }),
            | None => fragment,
        }
    }

    fn manifest_parameter_view(&self, pattern: PatId) -> Option<ManifestParameterView<'arena>> {
        let mut fields = Vec::new();
        let mut current = pattern;
        loop {
            current = self.transparent_pattern_group(current);
            if current != pattern
                && !self.presentation.trivia.leading_comments(current.into()).is_empty()
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
                        && !self.presentation.trivia.leading_comments(manifest.into()).is_empty()
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

    pub(super) fn transparent_pattern_group(&self, pattern: PatId) -> PatId {
        if let Pattern::Paren(Paren(patterns)) = &self.arena.pats[&pattern]
            && let [inner] = patterns.as_slice()
            && self.should_elide_parentheses(pattern.into(), (*inner).into())
            && self.presentation.trivia.leading_comments(pattern.into()).is_empty()
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
                                self.layout_boundary(
                                    BoundaryIntent::between(variable, classifier),
                                    BoundaryLayout::aligned(" :"),
                                    self.term_fragment(classifier),
                                ),
                            )
                        }
                        | None => self.marked_field(field).append(self.layout_boundary(
                            BoundaryIntent::after_start(*named, inner.anchors.first),
                            BoundaryLayout::aligned(" ="),
                            inner,
                        )),
                    }
                } else {
                    self.marked_field(field).append(self.layout_boundary(
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
    pub(super) fn placed_binding(
        &self, enclosing: TermId, binding: &'arena GenBind<TermId>, placement: Placement,
    ) -> RcDoc<'arena> {
        let arena = self.arena;
        let spans = self.spans;
        let presentation = self.presentation;
        let source = self.source;
        let options = self.options;
        // Cached documents can contain this deferred layout; avoid an ownership cycle.
        let documents = Rc::downgrade(&self.documents);
        DOC_ALLOCATOR
            .nesting(move |binding_nesting| {
                let binding_nesting = isize::try_from(binding_nesting).unwrap_or(isize::MAX);
                let formatter = PrettyFormatter {
                    arena,
                    spans,
                    presentation,
                    source,
                    grammar: GrammarContext::new(arena),
                    punning: Punning::new(arena, &presentation.trivia),
                    options,
                    documents: documents.upgrade().unwrap_or_default(),
                };
                formatter.placed_binding_at(enclosing, binding, placement, binding_nesting)
            })
            .into_doc()
    }

    pub(super) fn placed_binding_at(
        &self, enclosing: TermId, binding: &GenBind<TermId>, placement: Placement,
        binding_nesting: isize,
    ) -> RcDoc<'arena> {
        let GenBind { flavor, binder, params, ty, bindee } = binding;
        let modifier = match flavor {
            | BindingFlavor::Plain => None,
            | BindingFlavor::Value => Some("val"),
            | BindingFlavor::Computation => Some("!"),
            | BindingFlavor::Recursive => Some("fix"),
        };
        let modifiers = modifier.into_iter().map(RcDoc::text);
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
        let intent = BoundaryIntent::after_start(enclosing, head.anchors.first);
        let document = match modifier {
            // A modifier leads the head, so the binder's comments follow it
            // instead of starting the continuation.
            | Some(_) => self.retained_boundary(intent, BoundaryLayout::aligned(""), head.document),
            | None => self.layout_boundary(intent, BoundaryLayout::aligned(""), head),
        };
        let head = LayoutFragment { document, anchors: head_anchors };
        let placement = self.placement(placement);
        let bindee =
            self.bindee_with_placement(self.term_fragment(*bindee), placement, binding_nesting);
        match ty {
            | Some(ty) => {
                let assignment =
                    self.definition_join(self.term_fragment(*ty), bindee, binding_nesting);
                self.staged_join(head, assignment, StagedBoundary::BindingType).document
            }
            | None => self.untyped_definition_join(head, bindee).document,
        }
    }

    pub(super) fn sequence_tail(&self, before: EntityId, tail: TermId) -> RcDoc<'arena> {
        self.mandatory_line_break(
            BoundaryIntent::between(before, tail).resolve(&self.presentation.intentions),
        )
        .append(self.term_through(tail, TermPrecedence::Binder))
    }

    /// Block constructs — sequence bindings, whose tail marker always
    /// breaks, and arm blocks, whose arms always break — must start on a
    /// line of their own. The guard fails mid-line and lets an enclosing
    /// boundary fall back to its broken form instead of anchoring the
    /// construct to a running line. Leading comments extend the construct's
    /// layout start, so the guard precedes them, and the construct itself
    /// still starts the line after the last one.
    pub(super) fn block_like(&self, term: TermId, document: RcDoc<'arena>) -> RcDoc<'arena> {
        RcDoc::column(|column| {
            RcDoc::nesting(
                move |nesting| {
                    if column == nesting { RcDoc::nil() } else { RcDoc::fail() }
                },
            )
        })
        .append(self.with_separated_comments(
            self.presentation.trivia.leading_comments(term.into()),
            LineSeparation::NextLine,
            document,
        ))
    }

    /// Attach the stage-closing placement marker to a bindee. Mirroring the
    /// `=` stage separator, the marker has three tiers: it stays on a
    /// single-line bindee,
    /// follows the bindee's final line when that line returns to the binding
    /// indentation (a delimited closer), and otherwise breaks onto its own
    /// line at the binding indentation, wherever the bindee's own layout
    /// happens to nest.
    pub(super) fn bindee_with_placement(
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

    pub(super) fn placement(&self, placement: Placement) -> RcDoc<'arena> {
        RcDoc::text(match placement {
            | Placement::In => "in",
            | Placement::That => "that",
        })
    }

    pub(super) fn block(
        &self, enclosing: TermId, keyword: &'static str, body: TermId, end: &'static str,
    ) -> RcDoc<'arena> {
        RcDoc::text(keyword)
            .append(
                self.mandatory_line_break(
                    BoundaryIntent::after_start(enclosing, body)
                        .resolve(&self.presentation.intentions),
                )
                .append(self.annotated_term(body))
                .nest(self.indent()),
            )
            .append(self.mandatory_line_break(
                BoundaryIntent::before_end(body, enclosing).resolve(&self.presentation.intentions),
            ))
            .append(RcDoc::text(end))
    }

    pub(super) fn arm_block(
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
                .append(self.mandatory_line_break(gap.resolve(&self.presentation.intentions)))
                .append(self.with_before_arm_comments(arm.anchors.first, arm.document));
            previous = Some(arm.anchors.last);
        }
        let before_end = previous.map_or_else(RcDoc::hardline, |last| {
            self.mandatory_line_break(
                BoundaryIntent::before_end(last, enclosing).resolve(&self.presentation.intentions),
            )
        });
        document.append(before_end).append(RcDoc::text("end"))
    }

    pub(super) fn data(&self, enclosing: TermId, arms: &[DataArm]) -> RcDoc<'arena> {
        let arms = arms.iter().map(|arm| {
            let document =
                RcDoc::text("| ").append(self.constructor(&arm.name)).append(self.layout_boundary(
                    BoundaryIntent::after_arm_prefix(arm.param),
                    BoundaryLayout::hanging(" :", self.indent()),
                    self.term_fragment(arm.param),
                ));
            LayoutFragment::entity(arm.param.into(), document)
        });
        self.arm_block(enclosing.into(), RcDoc::text("data"), arms)
    }

    pub(super) fn codata(&self, enclosing: TermId, arms: &[CoDataArm]) -> RcDoc<'arena> {
        let arms = arms.iter().map(|arm| {
            let document = RcDoc::text("| ").append(self.destructor(&arm.name));
            let document = match arm.params {
                | Some(params) => document.append(RcDoc::text(" ")).append(self.copattern(params)),
                | None => document,
            };
            let first: EntityId = arm.params.map_or_else(|| arm.out.into(), Into::into);
            let document = document.append(self.layout_boundary(
                BoundaryIntent::after_arm_prefix(arm.out),
                BoundaryLayout::hanging(" :", self.indent()),
                self.term_fragment(arm.out),
            ));
            LayoutFragment { document, anchors: LayoutAnchors { first, last: arm.out.into() } }
        });
        self.arm_block(enclosing.into(), RcDoc::text("codata"), arms)
    }

    pub(super) fn matcher(
        &self, term: TermId, scrutinee: TermId, arms: &[Matcher<PatId, TermId>],
    ) -> RcDoc<'arena> {
        let head = RcDoc::text("match").append(self.layout_boundary(
            BoundaryIntent::after_start(term, scrutinee),
            BoundaryLayout::hanging("", self.indent()),
            self.term_fragment(scrutinee),
        ));
        let arms = arms.iter().map(|arm| {
            let document =
                RcDoc::text("| ").append(self.pattern(arm.binder)).append(self.layout_boundary(
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

    pub(super) fn comatcher(&self, enclosing: TermId, arms: &[CoMatcherParam]) -> RcDoc<'arena> {
        let arms = arms.iter().map(|arm| {
            let document =
                RcDoc::text("| ").append(self.copattern(arm.params)).append(self.layout_boundary(
                    BoundaryIntent::between(arm.params, arm.tail),
                    BoundaryLayout::hanging(" =>", self.indent()),
                    self.term_fragment(arm.tail),
                ));
            LayoutFragment {
                document,
                anchors: LayoutAnchors { first: arm.params.into(), last: arm.tail.into() },
            }
        });
        self.arm_block(enclosing.into(), RcDoc::text("comatch"), arms)
    }
}
