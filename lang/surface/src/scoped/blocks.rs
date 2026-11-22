//! Block candidate collection, block-wide resolution, and context elaboration.

use super::alloc::ScopedBuilder;
use super::binders::collect_textual_binders;
use super::err::ResolveError;
use super::observers::ResolutionObserver;
use super::resolver::{FoldResult, LoweredBinding, ResolveFolder};
use super::scope::BindingSite;
use super::syntax::*;
use crate::metadata::PreludeMeta;
use crate::textual::syntax as t;
use rustc_hash::FxHashMap;
use zydeco_foundation::err::{CollectReported, Report, ReportedError};
use zydeco_foundation::prelude::IdAllocator;
use zydeco_syntax::SpanView;

#[derive(Clone, Debug)]
pub(super) enum TextualCandidateKind {
    Param { flavor: ParameterFlavor, binder: t::PatId },
    ContextBind { mode: t::DefinitionMode, binding: t::GenBind<t::TermId> },
}

#[derive(Clone, Debug)]
pub(super) struct TextualCandidate {
    pub source: t::TermId,
    pub kind: TextualCandidateKind,
    pub prelude: bool,
}

impl TextualCandidate {
    fn outer_pattern(&self) -> t::PatId {
        match &self.kind {
            | TextualCandidateKind::Param { binder, .. } => *binder,
            | TextualCandidateKind::ContextBind { binding, .. } => binding.binder,
        }
    }
}

/// Scans parsed textual syntax directly for `that` candidates belonging to one block boundary.
pub(super) struct TextualBlockCollector<'a> {
    textual: &'a t::TextArena,
}

impl<'a> TextualBlockCollector<'a> {
    pub(super) fn new(textual: &'a t::TextArena) -> Self {
        Self { textual }
    }

    pub(super) fn collect(&self, body: t::TermId) -> Vec<TextualCandidate> {
        let mut out = Vec::new();
        self.scan_term(body, true, &mut out);
        out
    }

    fn scan_pat(&self, pat: t::PatId, prelude: bool, out: &mut Vec<TextualCandidate>) {
        zydeco_foundation::stack::maybe_grow(|| match &self.textual.pats[&pat] {
            | t::Pattern::Ann(t::Ann { tm, ty }) => {
                self.scan_pat(*tm, prelude, out);
                self.scan_term(*ty, prelude, out);
            }
            | t::Pattern::Manifest(t::ManifestPattern { binder, definition }) => {
                self.scan_pat(*binder, prelude, out);
                self.scan_term(*definition, prelude, out);
            }
            | t::Pattern::Named(t::Named(_, inner))
            | t::Pattern::Ctor(t::Ctor(_, inner))
            | t::Pattern::Project(t::ProjectionPattern(_, inner)) => {
                self.scan_pat(*inner, prelude, out);
            }
            | t::Pattern::View(t::ViewPattern { function, pattern }) => {
                self.scan_term(*function, prelude, out);
                self.scan_pat(*pattern, prelude, out);
            }
            | t::Pattern::Alias(t::Alias(patterns)) => {
                for item in patterns.iter() {
                    self.scan_pat(*item, prelude, out);
                }
            }
            | t::Pattern::Paren(t::Paren(patterns)) => {
                for item in patterns {
                    self.scan_pat(*item, prelude, out);
                }
            }
            | t::Pattern::Hole(_) | t::Pattern::Var(_) | t::Pattern::Lit(_) => {}
        });
    }

    fn scan_copat(&self, copat: t::CoPatId, prelude: bool, out: &mut Vec<TextualCandidate>) {
        zydeco_foundation::stack::maybe_grow(|| match &self.textual.copats[&copat] {
            | t::CoPattern::Pat(pat) => self.scan_pat(*pat, prelude, out),
            | t::CoPattern::Dtor(_) => {}
            | t::CoPattern::App(t::Appli(items)) => {
                for item in items {
                    self.scan_copat(*item, prelude, out);
                }
            }
        });
    }

    fn scan_binding(
        &self, binding: &t::GenBind<t::TermId>, prelude: bool, out: &mut Vec<TextualCandidate>,
    ) {
        self.scan_pat(binding.binder, prelude, out);
        if let Some(params) = binding.params {
            self.scan_copat(params, prelude, out);
        }
        if let Some(ty) = binding.ty {
            self.scan_term(ty, prelude, out);
        }
        self.scan_term(binding.bindee, prelude, out);
    }

    fn scan_term(&self, term: t::TermId, prelude: bool, out: &mut Vec<TextualCandidate>) {
        zydeco_foundation::stack::maybe_grow(|| match &self.textual.terms[&term] {
            | t::Term::Block(_) | t::Term::SourceBoundary(_) | t::Term::SignatureBoundary(_) => {}
            | t::Term::Meta(t::MetaTerm(meta, payload)) => {
                let node = &self.textual.metas[meta];
                if node.is("cbv")
                    || node.is("intrinsic")
                    || node.is("ffi")
                    || node.is("cbn")
                    || node.is("import")
                    || node.is("literal")
                {
                    return;
                }
                let next_prelude = if node.is("prelude")
                    && matches!(
                        self.textual.semantic_meta(*meta).specialize::<PreludeMeta>(),
                        Ok(Some(PreludeMeta::None))
                    ) {
                    false
                } else {
                    prelude
                };
                self.scan_term(*payload, next_prelude, out);
            }
            | t::Term::Param(t::Param { flavor, binder, placement, tail }) => {
                if matches!(placement, t::Placement::That) {
                    out.push(TextualCandidate {
                        source: term,
                        kind: TextualCandidateKind::Param { flavor: *flavor, binder: *binder },
                        prelude,
                    });
                }
                self.scan_pat(*binder, prelude, out);
                self.scan_term(*tail, prelude, out);
            }
            | t::Term::ContextBind(t::ContextBind { mode, binding, placement, tail }) => {
                if matches!(placement, t::Placement::That) {
                    out.push(TextualCandidate {
                        source: term,
                        kind: TextualCandidateKind::ContextBind {
                            mode: *mode,
                            binding: binding.clone(),
                        },
                        prelude,
                    });
                }
                self.scan_binding(binding, prelude, out);
                self.scan_term(*tail, prelude, out);
            }
            | t::Term::Let(t::GenLet { binding, tail }) => {
                self.scan_binding(binding, prelude, out);
                self.scan_term(*tail, prelude, out);
            }
            | t::Term::Named(t::Named(_, inner))
            | t::Term::Label(t::Label(_, inner))
            | t::Term::Thunk(t::Thunk(inner))
            | t::Term::Force(t::Force(inner))
            | t::Term::Ret(t::Return(inner))
            | t::Term::Ctor(t::Ctor(_, inner))
            | t::Term::Dtor(t::Dtor(inner, _))
            | t::Term::Proj(t::Proj(inner, _)) => self.scan_term(*inner, prelude, out),
            | t::Term::Ann(t::Ann { tm, ty }) => {
                self.scan_term(*tm, prelude, out);
                self.scan_term(*ty, prelude, out);
            }
            | t::Term::Arrow(t::Arrow(left, right)) => {
                self.scan_term(*left, prelude, out);
                self.scan_term(*right, prelude, out);
            }
            | t::Term::Paren(t::Paren(terms))
            | t::Term::App(t::Appli(terms))
            | t::Term::Prod(t::Prod(terms)) => {
                for item in terms {
                    self.scan_term(*item, prelude, out);
                }
            }
            | t::Term::Abs(t::Abs(params, body))
            | t::Term::ValAbs(t::Abs(params, body))
            | t::Term::Pi(t::Pi(params, body))
            | t::Term::ValPi(t::ValPi(params, body))
            | t::Term::Forall(t::Forall(params, body))
            | t::Term::Sigma(t::Sigma(params, body)) => {
                self.scan_copat(*params, prelude, out);
                self.scan_term(*body, prelude, out);
            }
            | t::Term::Fix(t::Fix(binder, body)) => {
                self.scan_pat(*binder, prelude, out);
                self.scan_term(*body, prelude, out);
            }
            | t::Term::Exists(t::Exists { parameters, body }) => {
                for param in parameters {
                    self.scan_pat(param.binder, prelude, out);
                }
                self.scan_term(*body, prelude, out);
            }
            | t::Term::Pack(t::Pack { parameters, body }) => {
                for param in parameters {
                    self.scan_pat(param.parameter.binder, prelude, out);
                    if let Some(evidence) = param.evidence {
                        self.scan_term(evidence, prelude, out);
                    }
                }
                self.scan_term(*body, prelude, out);
            }
            | t::Term::Pipeline(t::Pipeline { direction: _, subject, function }) => {
                self.scan_term(*subject, prelude, out);
                self.scan_term(*function, prelude, out);
            }
            | t::Term::Do(t::Bind { binder, bindee, tail }) => {
                self.scan_pat(*binder, prelude, out);
                self.scan_term(*bindee, prelude, out);
                self.scan_term(*tail, prelude, out);
            }
            | t::Term::Data(t::Data { arms }) => {
                for arm in arms {
                    self.scan_term(arm.param, prelude, out);
                }
            }
            | t::Term::CoData(t::CoData { arms }) => {
                for arm in arms {
                    if let Some(params) = arm.params {
                        self.scan_copat(params, prelude, out);
                    }
                    self.scan_term(arm.out, prelude, out);
                }
            }
            | t::Term::Match(t::Match { scrut, arms }) => {
                self.scan_term(*scrut, prelude, out);
                for arm in arms {
                    self.scan_pat(arm.binder, prelude, out);
                    self.scan_term(arm.tail, prelude, out);
                }
            }
            | t::Term::CoMatch(t::CoMatchParam { arms }) => {
                for arm in arms {
                    self.scan_copat(arm.params, prelude, out);
                    self.scan_term(arm.tail, prelude, out);
                }
            }
            | t::Term::Hole(_) | t::Term::Var(_) | t::Term::Lit(_) => {}
        });
    }
}

/// Elaborates a finished block `BindingContext` back into resolved terms around `residual`.
struct ContextElaboration<'a> {
    context: &'a BindingContext,
}

impl ContextElaboration<'_> {
    fn build<O: ResolutionObserver>(
        &self, resolver: &mut ResolveFolder<'_, O>, residual: TermId, block: TermId,
    ) -> FoldResult<TermId> {
        let mut rejected = false;
        for (_, node) in self.context.nodes.iter() {
            if let ContextNode::Recursive(bindings) = node
                && bindings.iter().any(|binding| matches!(binding.inner, BindingForm::Parameter(_)))
            {
                let source = bindings.first().map_or(block, |binding| binding.id);
                resolver.report(ResolveError::RecursiveParameter(*source.span(resolver)));
                rejected = true;
            }
        }
        if rejected {
            return Err(ReportedError);
        }
        Ok(self.context.topological_order().into_iter().rev().fold(
            residual,
            |tail, node| match self.context.nodes[&node].clone() {
                | ContextNode::Acyclic(binding) => {
                    let term: Term = match binding.inner {
                        | BindingForm::Parameter(Parameter { flavor, binder }) => match flavor {
                            | ParameterFlavor::Plain => Abs(vec![binder], tail).into(),
                            | ParameterFlavor::Value => Term::ValAbs(Abs(binder, tail)),
                        },
                        | BindingForm::Definition(Definition { binder, bindee }) => {
                            Let { binder, bindee, tail }.into()
                        }
                    };
                    resolver.builder.alloc_term_from_origin_of(binding.id, term)
                }
                | ContextNode::Recursive(bindings) => {
                    let source = bindings.first().map_or(block, |binding| binding.id);
                    let definitions = bindings
                        .into_iter()
                        .map(|binding| match binding.inner {
                            | BindingForm::Definition(Definition { binder, bindee }) => {
                                RecursiveDefinition { binder, bindee }
                            }
                            | BindingForm::Parameter(_) => unreachable!(
                                "recursive parameters were rejected before elaboration"
                            ),
                        })
                        .collect();
                    resolver
                        .builder
                        .alloc_term_from_origin_of(source, RecGroup { definitions, tail })
                }
            },
        ))
    }
}

struct PreparedCandidate {
    source: t::TermId,
    binding_id: TermId,
    kind: TextualCandidateKind,
    prelude: bool,
    preallocated: FxHashMap<t::DefId, DefId>,
}

impl<O: ResolutionObserver> ResolveFolder<'_, O> {
    pub(super) fn resolve_block(
        &mut self, block_origin: t::TermId, body: t::TermId,
    ) -> FoldResult<TermId> {
        let candidates = TextualBlockCollector::new(self.textual).collect(body);
        self.resolve_block_with_candidates(block_origin, body, candidates)
    }

    pub(super) fn resolve_block_with_candidates(
        &mut self, block_origin: t::TermId, body: t::TermId, candidates: Vec<TextualCandidate>,
    ) -> FoldResult<TermId> {
        let block_id = self.builder.reserve_term(block_origin.into());
        let block_mark = self.scope.mark();
        let saved_candidates = std::mem::take(&mut self.active_block_candidates);

        let prepared = self.prepare_block_candidates(block_id, candidates);
        let prepared = match prepared {
            | Ok(prepared) => prepared,
            | Err(err) => {
                self.scope.rollback(block_mark);
                self.active_block_candidates = saved_candidates;
                return Err(err);
            }
        };

        self.scope.boundary = Some(block_id);
        for item in &prepared {
            self.active_block_candidates.insert(item.source, Some(item.binding_id));
        }
        self.dependencies.begin_block(block_id, prepared.iter().map(|item| item.binding_id));

        let bindings = prepared
            .iter()
            .enumerate()
            .map(|(order, candidate)| self.lower_block_candidate(block_id, candidate, order))
            .collect_reported::<Vec<_>>();
        let residual = self.lower_term(body);

        self.scope.rollback(block_mark);
        self.active_block_candidates = saved_candidates;

        let (bindings, residual) = match (bindings, residual) {
            | (Ok(bindings), Ok(residual)) => {
                (bindings.into_iter().map(|binding| (binding.id, binding)).collect(), residual)
            }
            | _ => {
                self.dependencies.abort_block(block_id);
                return Err(ReportedError);
            }
        };

        let dependencies = self.dependencies.finish_block(block_id);
        let context = BindingContext::from_bindings(IdAllocator::new(), bindings, dependencies);
        let elaborated =
            ContextElaboration { context: &context }.build(self, residual, block_id)?;
        self.builder.blocks.insert_new(
            block_id,
            ContextualTerm { context, body: BlockBody { residual, elaborated } },
        );
        self.builder.fill_term(block_id, Block(elaborated).into());
        Ok(block_id)
    }

    fn prepare_block_candidates(
        &mut self, block_id: TermId, candidates: Vec<TextualCandidate>,
    ) -> FoldResult<Vec<PreparedCandidate>> {
        let mut prepared = Vec::with_capacity(candidates.len());
        let mut raw_binders = Vec::new();
        let mut seen = FxHashMap::<VarName, DefId>::default();
        let mut block_binders = Vec::new();
        let mut rejected = false;

        for candidate in candidates {
            let binding_id = self.builder.reserve_term(candidate.source.into());
            raw_binders.clear();
            collect_textual_binders(self.textual, candidate.outer_pattern(), &mut raw_binders);
            let mut preallocated = FxHashMap::default();
            for &(name, t_def) in &raw_binders {
                let def = self.alloc_def(t_def);
                if let Some(&previous) = seen.get(&name) {
                    self.report(ResolveError::DuplicateDefinition(
                        previous.span(self).make(name),
                        def.span(self).make(name),
                    ));
                    rejected = true;
                } else {
                    seen.insert(name, def);
                    block_binders.push((name, def));
                }
                self.scope.under_map.insert(def, BindingSite { owner: block_id, id: binding_id });
                preallocated.insert(t_def, def);
            }
            prepared.push(PreparedCandidate {
                source: candidate.source,
                binding_id,
                kind: candidate.kind,
                prelude: candidate.prelude,
                preallocated,
            });
        }

        if rejected {
            return Err(ReportedError);
        }
        self.scope.bind_group(block_binders);
        Ok(prepared)
    }

    fn lower_block_candidate(
        &mut self, block_id: TermId, candidate: &PreparedCandidate, source_order: usize,
    ) -> FoldResult<Binding> {
        let prev_prelude = self.scope.prelude_enabled;
        self.scope.prelude_enabled &= candidate.prelude;
        self.scope.under.push(BindingSite { owner: block_id, id: candidate.binding_id });

        let inner = match &candidate.kind {
            | TextualCandidateKind::Param { flavor, binder } => self
                .lower_pattern(*binder, Some(&candidate.preallocated))
                .map(|binder| BindingForm::Parameter(Parameter { flavor: *flavor, binder })),
            | TextualCandidateKind::ContextBind { mode, binding } => self
                .lower_binding(binding.clone(), *mode, Some(&candidate.preallocated))
                .map(|LoweredBinding { binder, bindee }| {
                    let bindee = match mode {
                        | t::DefinitionMode::Transparent => bindee,
                        | t::DefinitionMode::Nominal => {
                            self.builder.alloc_term(Sealed(bindee), candidate.source.into())
                        }
                    };
                    BindingForm::Definition(Definition { binder, bindee })
                }),
        };

        self.scope.under.pop();
        self.scope.prelude_enabled = prev_prelude;
        Ok(Binding::from_term(candidate.binding_id, inner?, source_order))
    }
}

pub(super) fn take_or_alloc_residual(
    builder: &mut ScopedBuilder, active_candidates: &mut FxHashMap<t::TermId, Option<TermId>>,
    source: t::TermId, tail: TermId,
) -> TermId {
    let term: Term = Residual(tail).into();
    match active_candidates.get_mut(&source).and_then(Option::take) {
        | Some(reserved) => {
            builder.fill_term(reserved, term);
            reserved
        }
        | None => builder.alloc_term(term, source.into()),
    }
}
