//! Binding flavors, parameter abstractions, copattern spines, and lexical `in`/`that` emission.

use super::{FoldResult, ResolveFolder};
use crate::scoped::blocks::take_or_alloc_residual;
use crate::scoped::syntax::*;
use crate::textual::syntax as t;
use rustc_hash::FxHashMap;
use zydeco_foundation::err::CollectReported;
use zydeco_syntax::SpanView;

pub(in crate::scoped) struct LoweredBinding {
    pub binder: PatId,
    pub bindee: TermId,
}

#[derive(Clone, Copy)]
pub(super) enum TextualCoPatItem {
    Pat(t::PatId),
    Dtor(DtorName),
}

enum Classifier {
    Attached(TermId),
    Telescope { result: TermId, value_flavor: bool },
}

enum SegmentKind {
    Plain(Vec<t::PatId>),
    Value(t::PatId),
    Dtor(DtorName),
}

pub(super) fn flatten_textual_copattern(
    textual: &t::TextArena, id: t::CoPatId,
) -> Vec<TextualCoPatItem> {
    let mut out = Vec::new();
    collect_textual_copattern(textual, id, &mut out);
    out
}

fn collect_textual_copattern(
    textual: &t::TextArena, id: t::CoPatId, out: &mut Vec<TextualCoPatItem>,
) {
    zydeco_foundation::stack::maybe_grow(|| {
        let mut current = id;
        let mut right_reversed = Vec::new();
        let leaf = loop {
            match &textual.copats[&current] {
                | t::CoPattern::App(t::Appli(items)) => {
                    let mut idx = items.len();
                    while idx > 1 {
                        idx -= 1;
                        right_reversed.push(items[idx]);
                    }
                    if let Some(&first) = items.first() {
                        current = first;
                    } else {
                        break None;
                    }
                }
                | other => break Some(other),
            }
        };
        match leaf {
            | Some(t::CoPattern::Pat(pat)) => out.push(TextualCoPatItem::Pat(*pat)),
            | Some(t::CoPattern::Dtor(dtor)) => out.push(TextualCoPatItem::Dtor(*dtor)),
            | Some(t::CoPattern::App(_)) | None => {}
        }
        for item in right_reversed.into_iter().rev() {
            collect_textual_copattern(textual, item, out);
        }
    });
}

fn split_segments(
    items: &[TextualCoPatItem], flavor: ParameterFlavor,
) -> (Vec<SegmentKind>, Option<DtorName>) {
    let mut segments = Vec::new();
    let mut run = Vec::new();
    let mut first_dtor = None;
    for &item in items {
        match item {
            | TextualCoPatItem::Pat(pat) => match flavor {
                | ParameterFlavor::Plain => run.push(pat),
                | ParameterFlavor::Value => segments.push(SegmentKind::Value(pat)),
            },
            | TextualCoPatItem::Dtor(dtor) => {
                first_dtor.get_or_insert(dtor);
                if !run.is_empty() {
                    segments.push(SegmentKind::Plain(std::mem::take(&mut run)));
                }
                segments.push(SegmentKind::Dtor(dtor));
            }
        }
    }
    if !run.is_empty() {
        segments.push(SegmentKind::Plain(run));
    }
    (segments, first_dtor)
}

impl<O: super::ResolutionObserver> ResolveFolder<'_, O> {
    pub(in crate::scoped) fn lower_binding(
        &mut self, binding: t::GenBind<t::TermId>, _mode: t::DefinitionMode,
        preallocated: Option<&FxHashMap<t::DefId, DefId>>,
    ) -> FoldResult<LoweredBinding> {
        let t::GenBind { flavor, binder, params, ty, bindee } = binding;
        let source = bindee.into();
        let items = params.map(|p| flatten_textual_copattern(self.textual, p)).unwrap_or_default();
        if flavor == t::BindingFlavor::Value
            && items.iter().any(|item| matches!(item, TextualCoPatItem::Dtor(_)))
        {
            let origin = params.expect("dtor came from parameters");
            return Err(self.report_primary(super::ResolveError::ValueParameterNotPattern(
                origin.span(self.spans).clone().make(origin),
            )));
        }

        let param_flavor = if flavor == t::BindingFlavor::Value {
            ParameterFlavor::Value
        } else {
            ParameterFlavor::Plain
        };
        let (segments, has_dtor) = split_segments(&items, param_flavor);
        let follows_parameters = ty.is_some() || flavor != t::BindingFlavor::Plain;

        let body_res = self.lower_binding_body(flavor, binder, &segments, bindee, source);
        let classifier_res = self.lower_binding_classifier(
            flavor,
            &items,
            has_dtor.is_some(),
            follows_parameters,
            ty,
            source,
        );
        let binder_res = self.lower_pattern(binder, preallocated);

        let mut body = body_res?;
        let mut classifier = classifier_res?;
        let binder = binder_res?;

        if matches!(flavor, t::BindingFlavor::Recursive | t::BindingFlavor::Computation) {
            body = self.builder.alloc_term(Thunk(body), source);
            if let Classifier::Telescope { result, value_flavor } = classifier {
                let thunk = self.builder.thunk(source);
                let result = self.builder.alloc_term(App(thunk, vec![result]), source);
                classifier = Classifier::Telescope { result, value_flavor };
            }
        }

        let bindee = match classifier {
            | Classifier::Attached(ty) | Classifier::Telescope { result: ty, .. } => {
                self.builder.alloc_term(Ann { tm: body, ty }, source)
            }
        };
        Ok(LoweredBinding { binder, bindee })
    }

    fn lower_binding_body(
        &mut self, flavor: t::BindingFlavor, binder: t::PatId, segments: &[SegmentKind],
        bindee: t::TermId, source: t::EntityId,
    ) -> FoldResult<TermId> {
        let mark = self.scope.mark();
        let fix_binder = if flavor == t::BindingFlavor::Recursive {
            let prev_copy = self.secondary_copy;
            self.secondary_copy = true;
            let res = self.lower_pattern(binder, None);
            self.secondary_copy = prev_copy;
            Some(res)
        } else {
            None
        };
        let segs_res = self.lower_segments_and_body(segments, bindee, source);
        self.scope.rollback(mark);
        let fix_binder = fix_binder.transpose()?;
        let mut body = segs_res?;
        if let Some(fix_binder) = fix_binder {
            body = self.builder.alloc_term(Fix(fix_binder, body), source);
        }
        Ok(body)
    }

    fn lower_binding_classifier(
        &mut self, flavor: t::BindingFlavor, items: &[TextualCoPatItem], has_dtor: bool,
        follows_parameters: bool, ty: Option<t::TermId>, source: t::EntityId,
    ) -> FoldResult<Classifier> {
        if has_dtor {
            if let Some(ty) = ty {
                let _ = self.lower_term(ty)?;
            }
            let leading_pats = items
                .iter()
                .take_while(|item| matches!(item, TextualCoPatItem::Pat(_)))
                .filter_map(|item| match item {
                    | TextualCoPatItem::Pat(p) => Some(*p),
                    | TextualCoPatItem::Dtor(_) => None,
                })
                .collect::<Vec<_>>();
            let mut cls = self.builder.alloc_term(Hole, source);
            if follows_parameters && !leading_pats.is_empty() {
                let mark = self.scope.mark();
                let pats = self.lower_classifier_pats(&leading_pats)?;
                self.scope.rollback(mark);
                cls = self.builder.alloc_term(Pi(pats, cls), source);
            }
            return Ok(if follows_parameters {
                Classifier::Telescope { result: cls, value_flavor: false }
            } else {
                Classifier::Attached(cls)
            });
        }

        let leading_pats = items
            .iter()
            .filter_map(|item| match item {
                | TextualCoPatItem::Pat(p) => Some(*p),
                | TextualCoPatItem::Dtor(_) => None,
            })
            .collect::<Vec<_>>();
        let mark = self.scope.mark();
        let lowered_pats = if follows_parameters && !leading_pats.is_empty() {
            self.lower_classifier_pats(&leading_pats)
        } else {
            Ok(Vec::new())
        };
        let lowered_ty = match (ty, &lowered_pats) {
            | (Some(ty), Ok(_)) | (Some(ty), Err(_)) => self.lower_term(ty),
            | (None, _) => Ok(self.builder.alloc_term(Hole, source)),
        };
        self.scope.rollback(mark);
        let pats = lowered_pats?;
        let mut cls = lowered_ty?;
        if follows_parameters && !pats.is_empty() {
            cls = if flavor == t::BindingFlavor::Value {
                pats.into_iter()
                    .rev()
                    .fold(cls, |body, pat| self.builder.alloc_term(ValPi(pat, body), source))
            } else {
                self.builder.alloc_term(Pi(pats, cls), source)
            };
        }
        Ok(if follows_parameters {
            Classifier::Telescope { result: cls, value_flavor: flavor == t::BindingFlavor::Value }
        } else {
            Classifier::Attached(cls)
        })
    }

    fn lower_classifier_pats(&mut self, pats: &[t::PatId]) -> FoldResult<Vec<PatId>> {
        let prev = self.secondary_copy;
        self.secondary_copy = true;
        let res = pats.iter().map(|p| self.lower_pattern(*p, None)).collect_reported();
        self.secondary_copy = prev;
        res
    }

    fn lower_segments_and_body(
        &mut self, segments: &[SegmentKind], body_term: t::TermId, source: t::EntityId,
    ) -> FoldResult<TermId> {
        let mut lowered = Vec::with_capacity(segments.len());
        let mut failed = None;
        for segment in segments {
            match segment {
                | SegmentKind::Plain(pats) => {
                    match pats.iter().map(|p| self.lower_pattern(*p, None)).collect_reported() {
                        | Ok(pats) => lowered.push(LoweredSegment::Plain(pats)),
                        | Err(err) => failed = Some(err),
                    }
                }
                | SegmentKind::Value(pat) => match self.lower_pattern(*pat, None) {
                    | Ok(pat) => lowered.push(LoweredSegment::Value(pat)),
                    | Err(err) => failed = Some(err),
                },
                | SegmentKind::Dtor(dtor) => lowered.push(LoweredSegment::Dtor(*dtor)),
            }
        }
        let body = self.lower_term(body_term);
        if let Some(err) = failed {
            let _ = body?;
            return Err(err);
        }
        let acc = body?;
        Ok(lowered.into_iter().rev().fold(acc, |acc, seg| match seg {
            | LoweredSegment::Plain(pats) => self.builder.alloc_term(Abs(pats, acc), source),
            | LoweredSegment::Value(pat) => {
                self.builder.alloc_term(Term::ValAbs(Abs(pat, acc)), source)
            }
            | LoweredSegment::Dtor(dtor) => self
                .builder
                .alloc_term(CoMatch { arms: vec![CoMatcher { dtor, tail: acc }] }, source),
        }))
    }

    pub(super) fn lower_abstraction(
        &mut self, t::Abs(params, tail): t::Abs<t::CoPatId, t::TermId>, id: t::TermId,
    ) -> FoldResult<TermId> {
        let items = flatten_textual_copattern(self.textual, params);
        let (segments, first_dtor) = split_segments(&items, ParameterFlavor::Plain);
        let ann = match self.textual.terms[&tail] {
            | t::Term::Ann(t::Ann { tm, ty }) => Some((tm, ty)),
            | _ => None,
        };
        let body_term = ann.map_or(tail, |(tm, _)| tm);
        let mark = self.scope.mark();
        let body = self.lower_segments_and_body(&segments, body_term, id.into());
        self.scope.rollback(mark);

        if first_dtor.is_none()
            && let Some((_, ty)) = ann
        {
            let cls_mark = self.scope.mark();
            let leading = items
                .iter()
                .filter_map(|item| match item {
                    | TextualCoPatItem::Pat(p) => Some(*p),
                    | TextualCoPatItem::Dtor(_) => None,
                })
                .collect::<Vec<_>>();
            let pats_res = if leading.is_empty() {
                Ok(Vec::new())
            } else {
                self.lower_classifier_pats(&leading)
            };
            let ty_res = self.lower_term(ty);
            self.scope.rollback(cls_mark);
            let pats = pats_res?;
            let ty = ty_res?;
            let cls =
                if pats.is_empty() { ty } else { self.builder.alloc_term(Pi(pats, ty), id.into()) };
            return Ok(self.builder.alloc_term(Ann { tm: body?, ty: cls }, id.into()));
        }

        if let Some((_, ty)) = ann {
            let cls_mark = self.scope.mark();
            let _ = self.lower_term(ty)?;
            self.scope.rollback(cls_mark);
        }
        body
    }

    pub(super) fn lower_value_abstraction(
        &mut self, t::Abs(params, tail): t::Abs<t::CoPatId, t::TermId>, id: t::TermId,
    ) -> FoldResult<TermId> {
        let items = flatten_textual_copattern(self.textual, params);
        if items.iter().any(|item| matches!(item, TextualCoPatItem::Dtor(_))) {
            return Err(self.report_primary(super::ResolveError::ValueParameterNotPattern(
                params.span(self.spans).clone().make(params),
            )));
        }
        let (segments, _) = split_segments(&items, ParameterFlavor::Value);
        let mark = self.scope.mark();
        let body = self.lower_segments_and_body(&segments, tail, id.into());
        self.scope.rollback(mark);
        body
    }

    pub(super) fn lower_fix(
        &mut self, t::Fix(binder, body): t::Fix<t::PatId, t::TermId>, id: t::TermId,
    ) -> FoldResult<TermId> {
        let mark = self.scope.mark();
        let binder = self.lower_pattern(binder, None);
        let body = binder.and_then(|binder| Ok((binder, self.lower_term(body)?)));
        self.scope.rollback(mark);
        let (binder, body) = body?;
        Ok(self.builder.alloc_term(Fix(binder, body), id.into()))
    }

    pub(super) fn lower_do(
        &mut self, t::Bind { binder, bindee, tail }: t::Bind<t::PatId, t::TermId, t::TermId>,
        id: t::TermId,
    ) -> FoldResult<TermId> {
        let bindee = self.lower_term(bindee);
        let mark = self.scope.mark();
        let binder = self.lower_pattern(binder, None);
        let res = match (bindee, binder) {
            | (Ok(bindee), Ok(binder)) => {
                self.lower_term(tail).map(|tail| Bind { binder, bindee, tail })
            }
            | (Err(err), _) | (_, Err(err)) => Err(err),
        };
        self.scope.rollback(mark);
        Ok(self.builder.alloc_term(res?, id.into()))
    }

    pub(super) fn lower_let_in(
        &mut self, binding: t::GenBind<t::TermId>, mode: t::DefinitionMode, tail: t::TermId,
        id: t::TermId,
    ) -> FoldResult<TermId> {
        let source_binder = binding.binder;
        let mark = self.scope.mark();
        let lowered = self.lower_binding(binding, mode, None);
        let lowered = match lowered {
            | Ok(mut lowered) => {
                if matches!(mode, t::DefinitionMode::Nominal) {
                    lowered.bindee = self.builder.alloc_term(Sealed(lowered.bindee), id.into());
                }
                Ok(lowered)
            }
            | Err(err) => Err(err),
        };
        let res = match lowered {
            | Ok(LoweredBinding { binder, bindee }) => {
                let saved_prelude = if self.builder.default_prelude_binders.contains(&source_binder)
                {
                    let defs = self
                        .scope
                        .var_to_def
                        .iter()
                        .filter(|(_, binding)| binding.depth > mark.depth())
                        .map(|(name, binding)| (*name, binding.definition))
                        .collect();
                    self.scope.rollback(mark);
                    Some(std::mem::replace(&mut self.scope.prelude_definitions, defs))
                } else {
                    None
                };
                let tail_res = self.lower_term(tail);
                if let Some(prev_prelude) = saved_prelude {
                    self.scope.prelude_definitions = prev_prelude;
                }
                self.scope.rollback(mark);
                tail_res.map(|tail| Let { binder, bindee, tail })
            }
            | Err(err) => {
                self.scope.rollback(mark);
                Err(err)
            }
        };
        Ok(self.builder.alloc_term(res?, id.into()))
    }

    pub(super) fn lower_context_bind(
        &mut self, t::ContextBind { mode, binding, placement, tail }: t::ContextBind, id: t::TermId,
    ) -> FoldResult<TermId> {
        match placement {
            | t::Placement::In => self.lower_let_in(binding, mode, tail, id),
            | t::Placement::That => {
                if self.scope.boundary.is_none() || !self.active_block_candidates.contains_key(&id)
                {
                    return Err(self.report_primary(super::ResolveError::UnenclosedThat(
                        *id.span(self.spans),
                    )));
                }
                let tail = self.lower_term(tail)?;
                Ok(take_or_alloc_residual(
                    &mut self.builder,
                    &mut self.active_block_candidates,
                    id,
                    tail,
                ))
            }
        }
    }

    pub(super) fn lower_param(
        &mut self, t::Param { flavor, binder, placement, tail }: t::Param, id: t::TermId,
    ) -> FoldResult<TermId> {
        match placement {
            | t::Placement::In => {
                let mark = self.scope.mark();
                let binder = self.lower_pattern(binder, None);
                let tail = binder.and_then(|binder| Ok((binder, self.lower_term(tail)?)));
                self.scope.rollback(mark);
                let (binder, tail) = tail?;
                let term: Term = match flavor {
                    | ParameterFlavor::Plain => Abs(vec![binder], tail).into(),
                    | ParameterFlavor::Value => Term::ValAbs(Abs(binder, tail)),
                };
                Ok(self.builder.alloc_term(term, id.into()))
            }
            | t::Placement::That => {
                if self.scope.boundary.is_none() || !self.active_block_candidates.contains_key(&id)
                {
                    return Err(self.report_primary(super::ResolveError::UnenclosedThat(
                        *id.span(self.spans),
                    )));
                }
                let tail = self.lower_term(tail)?;
                Ok(take_or_alloc_residual(
                    &mut self.builder,
                    &mut self.active_block_candidates,
                    id,
                    tail,
                ))
            }
        }
    }

    pub(super) fn lower_comatch(
        &mut self, t::CoMatchParam { arms }: t::CoMatchParam, id: t::TermId,
    ) -> FoldResult<TermId> {
        let clauses = arms
            .into_iter()
            .map(|t::CoMatcherParam { params, tail }| {
                let items = flatten_textual_copattern(self.textual, params);
                let mark = self.scope.mark();
                let mut lowered = Vec::with_capacity(items.len());
                let mut failed = None;
                for item in items {
                    match item {
                        | TextualCoPatItem::Pat(pat) => match self.lower_pattern(pat, None) {
                            | Ok(pat) => lowered.push(CoPatternItem::Pat(pat)),
                            | Err(err) => failed = Some(err),
                        },
                        | TextualCoPatItem::Dtor(dtor) => lowered.push(CoPatternItem::Dtor(dtor)),
                    }
                }
                let tail_res = self.lower_term(tail);
                self.scope.rollback(mark);
                if let Some(err) = failed {
                    let _ = tail_res?;
                    return Err(err);
                }
                let spine = CoPatternSpine::from_items(lowered)
                    .expect("parsed comatch clauses have a nonempty copattern spine");
                Ok(CoPatternClause { spine, tail: tail_res? })
            })
            .collect_reported::<Vec<_>>()?;
        Ok(self.builder.alloc_term(CoMatchClauses { clauses }, id.into()))
    }
}

enum LoweredSegment {
    Plain(Vec<PatId>),
    Value(PatId),
    Dtor(DtorName),
}
