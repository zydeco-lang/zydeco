//! Fused surface lowering and lexical name resolution with passive observers.

use super::{
    alloc::ScopedBuilder, blocks::TextualBlockCollector, dependencies::DependencyAnalyzer,
    scope::ScopeState, syntax::*, *,
};
use crate::metadata::CorePrelude;
use crate::{
    diagnostic::Diagnostics,
    textual::{LiteralSite, syntax as t},
};
use rustc_hash::FxHashMap;
use std::sync::Arc;
use zydeco_foundation::{
    err::{CollectReported, Report, ReportedError},
    prelude::FrozenArena,
};
use zydeco_syntax::SpanView;

mod bindings;
mod meta;
mod telescopes;
#[cfg(test)]
mod tests;

pub(super) use bindings::LoweredBinding;

pub(super) type FoldResult<T> = std::result::Result<T, ReportedError>;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum UnboundPolicy {
    Reject,
    RecoverForCompletion,
    BindFree,
}

pub struct UnitResolveConfig<'a> {
    pub literals: &'a [LiteralSite],
    pub has_prelude: bool,
    pub is_signature: bool,
}

pub struct ResolvedUnitOut {
    pub arena: FrozenArena<ScopedArena>,
    pub root: TermId,
    /// `(textual_import_term, scoped_source_boundary_term)` in encounter order.
    pub imports: Vec<(t::TermId, TermId)>,
    /// Scoped `SourceBoundary` term for the default prelude when `has_prelude` is true.
    pub prelude_boundary: Option<TermId>,
    /// True if this unit lowered any `@[monadic]` block.
    pub has_monadic: bool,
}

pub(super) struct UnitResolveState {
    pub literals: FxHashMap<t::TermId, Arc<str>>,
    pub imports: Vec<(t::TermId, TermId)>,
    pub has_monadic: bool,
}

impl UnitResolveState {
    fn from_literals(literals: &[LiteralSite]) -> Self {
        let literals =
            literals.iter().map(|site| (site.term, site.directive.text.text.clone())).collect();
        Self { literals, imports: Vec::new(), has_monadic: false }
    }
}

pub struct ResolveFolder<'a, O = ()> {
    pub spans: &'a t::SpanArena,
    pub textual: &'a t::TextArena,
    pub(super) builder: ScopedBuilder,
    pub(super) scope: ScopeState,
    pub(super) dependencies: DependencyAnalyzer,
    pub(super) active_block_candidates: FxHashMap<t::TermId, Option<TermId>>,
    providers: FxHashMap<t::TermId, FoldResult<TermId>>,
    free_vars: FxHashMap<VarName, DefId>,
    unbound_policy: UnboundPolicy,
    pub(super) secondary_copy: bool,
    pub(super) unit_mode: Option<UnitResolveState>,
    observers: (ReferenceIndex, O),
    diagnostics: Vec<ResolveError>,
}

pub struct ResolveSourceOut {
    pub arena: FrozenArena<ScopedArena>,
    pub root: TermId,
}

/// Observations remain available after rejection; only accepted syntax is published.
pub struct ResolutionOutput<T> {
    pub program: Result<ResolveSourceOut>,
    pub observations: T,
    pub diagnostics: Vec<ResolveError>,
}

impl<'a> ResolveFolder<'a> {
    pub fn new(spans: &'a t::SpanArena, textual: &'a t::TextArena) -> Self {
        Self {
            spans,
            textual,
            builder: ScopedBuilder::new(),
            scope: ScopeState::new(),
            dependencies: DependencyAnalyzer::default(),
            active_block_candidates: FxHashMap::default(),
            providers: FxHashMap::default(),
            free_vars: FxHashMap::default(),
            unbound_policy: UnboundPolicy::Reject,
            secondary_copy: false,
            unit_mode: None,
            observers: (ReferenceIndex::default(), ()),
            diagnostics: Vec::new(),
        }
    }

    pub fn with_observer<O: ResolutionObserver>(self, observer: O) -> ResolveFolder<'a, O> {
        ResolveFolder {
            spans: self.spans,
            textual: self.textual,
            builder: self.builder,
            scope: self.scope,
            dependencies: self.dependencies,
            active_block_candidates: self.active_block_candidates,
            providers: self.providers,
            free_vars: self.free_vars,
            unbound_policy: self.unbound_policy,
            secondary_copy: self.secondary_copy,
            unit_mode: self.unit_mode,
            observers: (self.observers.0, observer),
            diagnostics: self.diagnostics,
        }
    }

    pub fn run_completion(mut self, root: t::TermId, target: t::TermId) -> CompletionResolution {
        self.unbound_policy = UnboundPolicy::RecoverForCompletion;
        let output = self.with_observer(CompletionObserver::new(target)).run_inner(root);
        CompletionResolution {
            site: output.observations,
            diagnostics: output.diagnostics,
            program: output.program,
        }
    }

    /// Lower and resolve one closed compilation unit directly from its frozen template.
    pub fn run_unit(
        self, root: t::TermId, config: UnitResolveConfig<'_>,
    ) -> Result<ResolvedUnitOut> {
        self.run_unit_inner(root, config).0
    }

    /// Lower and resolve one unit for completion recovery without reading dependency files.
    pub fn run_unit_completion(
        mut self, root: t::TermId, target: t::TermId, config: UnitResolveConfig<'_>,
    ) -> UnitCompletionResolution {
        self.unbound_policy = UnboundPolicy::RecoverForCompletion;
        let (unit, site, diagnostics) =
            self.with_observer(CompletionObserver::new(target)).run_unit_inner(root, config);
        UnitCompletionResolution { site, diagnostics, unit }
    }

    /// Lower and resolve an arbitrary snippet with free variables left abstract.
    ///
    /// Used by the formatter equivalence checks and fragment debug tests where
    /// open terms and bare top-level `that` declarations appear without a driver.
    pub fn run_open_fragment(mut self, root: t::TermId) -> Result<ResolveSourceOut> {
        self.unbound_policy = UnboundPolicy::BindFree;
        let candidates = TextualBlockCollector::new(self.textual).collect(root);
        let resolved = if candidates.is_empty() {
            self.lower_term(root)
        } else {
            self.resolve_block_with_candidates(root, root, candidates)
        };
        self.finish_strict(resolved)
    }
}

impl<O: ResolutionObserver> Report<ResolveError> for ResolveFolder<'_, O> {
    fn report(&mut self, error: ResolveError) -> ReportedError {
        self.diagnostics.push(error);
        ReportedError
    }
}

impl<O: ResolutionObserver> ResolveFolder<'_, O> {
    pub fn run_source(self, root: t::TermId) -> Result<ResolveSourceOut> {
        self.run_observed(root).program
    }

    pub fn run_observed(self, root: t::TermId) -> ResolutionOutput<O::Output> {
        self.run_inner(root)
    }

    fn install_default_prelude(&mut self, root: t::TermId) -> (TermId, PatId) {
        let placeholder = self.builder.alloc_synthetic_term(Hole);
        let boundary = self.builder.alloc_synthetic_term(SourceBoundary(placeholder));
        let patterns = CorePrelude::closed_names()
            .map(|name| {
                let var_name = VarName::new(name);
                let definition = self.builder.alloc_default_prelude_def(var_name);
                self.scope.prelude_definitions.insert(var_name, definition);
                let pat_var = self.builder.alloc_pat(Pattern::Var(definition), root.into());
                self.builder
                    .alloc_pat(ProjectionPattern(FieldName::new(name), pat_var).into(), root.into())
            })
            .collect();
        let binder = self.builder.alloc_pat(
            Alias(ConsN::from_vec(patterns).expect("default prelude is nonempty")).into(),
            root.into(),
        );
        (boundary, binder)
    }

    fn run_unit_inner(
        mut self, root: t::TermId, config: UnitResolveConfig<'_>,
    ) -> (Result<ResolvedUnitOut>, O::Output, Vec<ResolveError>) {
        self.unit_mode = Some(UnitResolveState::from_literals(config.literals));
        let prelude = if config.has_prelude {
            Some(self.install_default_prelude(root))
        } else {
            self.scope.prelude_enabled = false;
            None
        };
        let mut resolved = self.lower_term(root);
        if let Some((prelude_boundary, binder)) = prelude {
            resolved = resolved.map(|tail| {
                self.builder.alloc_term(Let { binder, bindee: prelude_boundary, tail }, root.into())
            });
        }
        if config.is_signature {
            resolved = resolved
                .map(|inner| self.builder.alloc_term(SignatureBoundary(inner), root.into()));
        }
        self.dependencies.assert_closed();
        let unit_state = self.unit_mode.take().expect("unit mode remains active");
        let (users, observations) = self.observers.finish();
        let rejected = resolved.is_err()
            || (matches!(self.unbound_policy, UnboundPolicy::Reject)
                && !self.diagnostics.is_empty());
        let program = if rejected {
            Err(Box::new(
                Diagnostics::with_errors(self.diagnostics.clone())
                    .expect("rejection has a recorded diagnostic"),
            ))
        } else {
            Ok(ResolvedUnitOut {
                arena: FrozenArena::new(self.builder.finish(users)),
                root: resolved.expect("accepted unit lowers to a root term"),
                imports: unit_state.imports,
                prelude_boundary: prelude.map(|(boundary, _)| boundary),
                has_monadic: unit_state.has_monadic,
            })
        };
        (program, observations, self.diagnostics)
    }

    fn run_inner(mut self, root: t::TermId) -> ResolutionOutput<O::Output> {
        let resolved = self.lower_term(root);
        self.dependencies.assert_closed();
        let (users, observations) = self.observers.finish();
        let rejected = resolved.is_err()
            || (matches!(self.unbound_policy, UnboundPolicy::Reject)
                && !self.diagnostics.is_empty());
        let program = if rejected {
            Err(Box::new(
                Diagnostics::with_errors(self.diagnostics.clone())
                    .expect("rejection has a recorded diagnostic"),
            ))
        } else {
            Ok(ResolveSourceOut {
                arena: FrozenArena::new(self.builder.finish(users)),
                root: resolved.expect("accepted source lowers to a root term"),
            })
        };
        ResolutionOutput { program, observations, diagnostics: self.diagnostics }
    }

    fn finish_strict(self, resolved: FoldResult<TermId>) -> Result<ResolveSourceOut> {
        self.dependencies.assert_closed();
        let (users, _) = self.observers.finish();
        match resolved {
            | Ok(root) if self.diagnostics.is_empty() => {
                Ok(ResolveSourceOut { arena: FrozenArena::new(self.builder.finish(users)), root })
            }
            | _ => Err(Box::new(
                Diagnostics::with_errors(self.diagnostics)
                    .expect("rejection has a recorded diagnostic"),
            )),
        }
    }

    pub(super) fn report_primary(&mut self, error: ResolveError) -> ReportedError {
        if !self.secondary_copy {
            self.diagnostics.push(error);
        }
        ReportedError
    }

    pub(super) fn alloc_def(&mut self, id: t::DefId) -> DefId {
        let name = self.textual.defs[&id];
        self.builder.alloc_def(name, id.into())
    }

    pub(super) fn scope_event(&mut self, id: TermId) {
        if self.secondary_copy {
            return;
        }
        self.observers.scope(&ScopeEvent {
            occurrence: id,
            origin: self.builder.origins.source(&id.into()),
            scope: self.scope.view(),
        });
    }

    pub(super) fn reference(
        &mut self, occurrence: TermId, name: VarName, origin: t::EntityId,
    ) -> Term {
        let Some(binding) = self.scope.view().lookup(&name) else {
            if self.scope.prelude_enabled && name == "OS" {
                // The checker resolves this against witnesses from explicit package
                // openings. It neither opens another package nor chooses between providers.
                self.dependencies.implicit_provider(&self.scope.under);
                return Term::Internal(Internal::OS);
            }
            if matches!(self.unbound_policy, UnboundPolicy::BindFree) {
                let definition = *self
                    .free_vars
                    .entry(name)
                    .or_insert_with(|| self.builder.alloc_def(name, origin));
                let event = ResolvedReference {
                    occurrence,
                    definition,
                    dependency: None,
                    active_bindings: &self.scope.under,
                };
                self.observers.reference(&event);
                return Term::Var(definition);
            }
            if !self.secondary_copy {
                self.report(ResolveError::UnboundVar(occurrence.span(self).make(name)));
            }
            return Term::Hole(Hole);
        };
        let event = ResolvedReference {
            occurrence,
            definition: binding.definition,
            dependency: binding.dependency,
            active_bindings: &self.scope.under,
        };
        self.dependencies.reference(&event);
        self.observers.reference(&event);
        Term::Var(binding.definition)
    }

    fn provider(&mut self, inner: t::TermId) -> FoldResult<TermId> {
        if let Some(result) = self.providers.get(&inner) {
            return *result;
        }
        let saved_scope = std::mem::take(&mut self.scope);
        let saved_candidates = std::mem::take(&mut self.active_block_candidates);
        let result = self.lower_term(inner);
        self.scope = saved_scope;
        self.active_block_candidates = saved_candidates;
        self.providers.insert(inner, result);
        result
    }

    pub(super) fn lower_pattern(
        &mut self, id: t::PatId, preallocated: Option<&FxHashMap<t::DefId, DefId>>,
    ) -> FoldResult<PatId> {
        zydeco_foundation::stack::maybe_grow(|| {
            let pat = match self.textual.pats[&id].clone() {
                | t::Pattern::Ann(t::Ann { tm, ty }) => {
                    let ty = self.lower_term(ty);
                    let tm = self.lower_pattern(tm, preallocated);
                    Pattern::Ann(Ann { tm: tm?, ty: ty? })
                }
                | t::Pattern::Manifest(_) => {
                    let pattern = id.span(self.spans).clone().make(id);
                    return Err(self
                        .report_primary(ResolveError::ManifestPatternOutsideExistential(pattern)));
                }
                | t::Pattern::Hole(hole) => Pattern::Hole(hole),
                | t::Pattern::Var(t_def) => {
                    let def = if let Some(map) = preallocated {
                        map[&t_def]
                    } else {
                        let def = self.alloc_def(t_def);
                        self.scope.bind_single(self.textual.defs[&t_def], def);
                        def
                    };
                    Pattern::Var(def)
                }
                | t::Pattern::Named(t::Named(name, inner)) => {
                    Pattern::Named(Named(name, self.lower_pattern(inner, preallocated)?))
                }
                | t::Pattern::Ctor(t::Ctor(name, inner)) => {
                    Pattern::Ctor(Ctor(name, self.lower_pattern(inner, preallocated)?))
                }
                | t::Pattern::Lit(literal) => Pattern::Lit(literal),
                | t::Pattern::Project(t::ProjectionPattern(field, inner)) => Pattern::Project(
                    ProjectionPattern(field, self.lower_pattern(inner, preallocated)?),
                ),
                | t::Pattern::View(t::ViewPattern { function, pattern }) => {
                    let function = self.lower_term(function);
                    let pattern = self.lower_pattern(pattern, preallocated);
                    Pattern::View(ViewPattern { function: function?, pattern: pattern? })
                }
                | t::Pattern::Alias(t::Alias(patterns)) => {
                    let patterns = patterns
                        .into_iter()
                        .map(|pat| self.lower_pattern(pat, preallocated))
                        .collect_reported::<Vec<_>>()?;
                    Pattern::Alias(Alias(ConsN::from_vec(patterns).expect("alias is nonempty")))
                }
                | t::Pattern::Paren(t::Paren(patterns)) => {
                    let pats = patterns
                        .into_iter()
                        .map(|pat| self.lower_pattern(pat, preallocated))
                        .collect_reported::<Vec<_>>()?;
                    match pats.len() {
                        | 0 => Pattern::Triv(Triv),
                        | 1 => return Ok(pats.into_iter().next().unwrap()),
                        | _ => Pattern::Cons(pats),
                    }
                }
            };
            Ok(self.builder.alloc_pat(pat, id.into()))
        })
    }

    pub(super) fn lower_term(&mut self, id: t::TermId) -> FoldResult<TermId> {
        zydeco_foundation::stack::maybe_grow(|| {
            let term: Term = match self.textual.terms[&id].clone() {
                | t::Term::Meta(meta) => return self.lower_meta(id, meta),
                | t::Term::SourceBoundary(t::SourceBoundary(inner)) => {
                    SourceBoundary(self.provider(inner)?).into()
                }
                | t::Term::SignatureBoundary(t::SignatureBoundary(inner)) => {
                    SignatureBoundary(self.provider(inner)?).into()
                }
                | t::Term::Hole(hole) => {
                    let occurrence = self.builder.alloc_term(hole, id.into());
                    self.scope_event(occurrence);
                    return Ok(occurrence);
                }
                | t::Term::Var(name) => {
                    let occurrence = self.builder.reserve_term(id.into());
                    let resolved = self.reference(occurrence, name, id.into());
                    self.builder.fill_term(occurrence, resolved);
                    return Ok(occurrence);
                }
                | t::Term::Ann(t::Ann { tm, ty }) => {
                    let ty = self.lower_term(ty);
                    let tm = self.lower_term(tm);
                    Ann { tm: tm?, ty: ty? }.into()
                }
                | t::Term::Seal(t::Seal { tm, ty }) => {
                    let ty = self.lower_term(ty);
                    let tm = self.lower_term(tm);
                    Seal { tm: tm?, ty: ty? }.into()
                }
                | t::Term::Named(t::Named(name, inner)) => {
                    Named(name, self.lower_term(inner)?).into()
                }
                | t::Term::Label(t::Label(name, inner)) => {
                    Label(name, self.lower_term(inner)?).into()
                }
                | t::Term::Paren(t::Paren(terms)) => {
                    let terms = terms
                        .into_iter()
                        .map(|t| self.lower_term(t))
                        .collect_reported::<Vec<_>>()?;
                    match terms.len() {
                        | 0 => Triv.into(),
                        | 1 => return Ok(terms.into_iter().next().unwrap()),
                        | _ => Term::Cons(terms),
                    }
                }
                | t::Term::Prod(t::Prod(components)) => {
                    let components = components
                        .into_iter()
                        .map(|t| self.lower_term(t))
                        .collect_reported::<Vec<_>>()?;
                    Term::Cons(components)
                }
                | t::Term::App(app) => return self.lower_app(id, app),
                | t::Term::Pipeline(t::Pipeline { subject, function, .. }) => {
                    let subject = self.lower_term(subject);
                    let function = self.lower_term(function);
                    App(function?, vec![subject?]).into()
                }
                | t::Term::Abs(abs) => return self.lower_abstraction(abs, id),
                | t::Term::ValAbs(abs) => return self.lower_value_abstraction(abs, id),
                | t::Term::Fix(fix) => return self.lower_fix(fix, id),
                | t::Term::Pi(t::Pi(params, body)) => {
                    return self.lower_quantifier(params, body, telescopes::Quantifier::Pi, id);
                }
                | t::Term::ValPi(t::ValPi(params, body)) => {
                    return self.lower_quantifier(params, body, telescopes::Quantifier::ValPi, id);
                }
                | t::Term::Forall(t::Forall(params, body)) => {
                    return self.lower_quantifier(params, body, telescopes::Quantifier::Pi, id);
                }
                | t::Term::Sigma(t::Sigma(params, body)) => {
                    return self.lower_quantifier(params, body, telescopes::Quantifier::Sigma, id);
                }
                | t::Term::Arrow(t::Arrow(ty_in, ty_out)) => {
                    let ty_in = self.lower_term(ty_in)?;
                    let hole = self.builder.alloc_pat(Pattern::Hole(Hole), id.into());
                    let ann = self
                        .builder
                        .alloc_pat(Pattern::Ann(Ann { tm: hole, ty: ty_in }), id.into());
                    let ty_out = self.lower_term(ty_out)?;
                    Pi(vec![ann], ty_out).into()
                }
                | t::Term::Exists(exists) => return self.lower_exists(exists, id),
                | t::Term::Pack(pack) => return self.lower_pack(pack, id),
                | t::Term::Thunk(t::Thunk(body)) => {
                    let body = self.lower_term(body)?;
                    return Ok(self.builder.introduce(alloc::Introduction::Thunk, body, id.into()));
                }
                | t::Term::Force(t::Force(inner)) => Force(self.lower_term(inner)?).into(),
                | t::Term::Ret(t::Return(body)) => {
                    let body = self.lower_term(body)?;
                    return Ok(self.builder.introduce(
                        alloc::Introduction::Return,
                        body,
                        id.into(),
                    ));
                }
                | t::Term::Do(do_bind) => return self.lower_do(do_bind, id),
                | t::Term::Let(t::GenLet { binding, tail }) => {
                    return self.lower_let_in(binding, t::DefinitionMode::Transparent, tail, id);
                }
                | t::Term::ContextBind(bind) => return self.lower_context_bind(bind, id),
                | t::Term::Param(param) => return self.lower_param(param, id),
                | t::Term::Block(t::Block(body)) => return self.resolve_block(id, body),
                | t::Term::Data(data) => return self.lower_data(data, id.into()),
                | t::Term::CoData(codata) => return self.lower_codata(codata, id.into()),
                | t::Term::Ctor(t::Ctor(name, inner)) => Ctor(name, self.lower_term(inner)?).into(),
                | t::Term::Match(m) => return self.lower_match(m, id),
                | t::Term::CoMatch(cm) => return self.lower_comatch(cm, id),
                | t::Term::Dtor(t::Dtor(inner, name)) => Dtor(self.lower_term(inner)?, name).into(),
                | t::Term::Proj(t::Proj(head, name)) => Proj(self.lower_term(head)?, name).into(),
                | t::Term::Lit(lit) => lit.into(),
            };
            Ok(self.builder.alloc_term(term, id.into()))
        })
    }

    fn lower_app(
        &mut self, id: t::TermId, t::Appli(mut current): t::Appli<t::TermId>,
    ) -> FoldResult<TermId> {
        let mut reversed_tail = Vec::new();
        loop {
            while current.len() > 1 {
                reversed_tail.push(current.pop().unwrap());
            }
            let Some(first) = current.pop() else {
                unreachable!("application spine is nonempty");
            };
            match &self.textual.terms[&first] {
                | t::Term::App(t::Appli(inner)) => current = inner.clone(),
                | _ => {
                    let mut inputs = Vec::with_capacity(1 + reversed_tail.len());
                    inputs.push(first);
                    inputs.extend(reversed_tail.into_iter().rev());
                    let terms = inputs
                        .into_iter()
                        .map(|t| self.lower_term(t))
                        .collect_reported::<Vec<_>>()?;
                    let mut iter = terms.into_iter();
                    let head = iter.next().unwrap();
                    return if iter.len() == 0 {
                        Ok(head)
                    } else {
                        Ok(self.builder.alloc_term(App(head, iter.collect()), id.into()))
                    };
                }
            }
        }
    }

    fn lower_match(
        &mut self, t::Match { scrut, arms }: t::Match<t::TermId, t::PatId, t::TermId>,
        id: t::TermId,
    ) -> FoldResult<TermId> {
        let scrut = self.lower_term(scrut);
        let arms = arms
            .into_iter()
            .map(|t::Matcher { binder, tail }| {
                let mark = self.scope.mark();
                let binder = self.lower_pattern(binder, None);
                let tail = binder.and_then(|binder| Ok((binder, self.lower_term(tail)?)));
                self.scope.rollback(mark);
                let (binder, tail) = tail?;
                Ok(Matcher { binder, tail })
            })
            .collect_reported::<Vec<_>>()?;
        Ok(self.builder.alloc_term(Match { scrut: scrut?, arms }, id.into()))
    }
}
