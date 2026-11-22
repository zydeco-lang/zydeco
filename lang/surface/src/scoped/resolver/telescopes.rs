//! Quantifiers, existential telescopes, packages, and nominal type declarations.

use super::{
    FoldResult, ResolveFolder,
    bindings::{TextualCoPatItem, flatten_textual_copattern},
};
use crate::scoped::syntax::*;
use crate::textual::syntax as t;
use zydeco_foundation::err::{CollectReported, ReportedError};
use zydeco_syntax::SpanView;

#[derive(Copy, Clone)]
pub(super) enum Quantifier {
    Pi,
    ValPi,
    Sigma,
}

enum ExistentialForm {
    Abstract { binder: PatId, span_pat: t::PatId },
    Manifest { binder: PatId, span_pat: t::PatId, definition: TermId },
}

impl ExistentialForm {
    fn binder(&self) -> PatId {
        match self {
            | Self::Abstract { binder, .. } | Self::Manifest { binder, .. } => *binder,
        }
    }

    fn with_binder(self, binder: PatId) -> Self {
        match self {
            | Self::Abstract { span_pat, .. } => Self::Abstract { binder, span_pat },
            | Self::Manifest { span_pat, definition, .. } => {
                Self::Manifest { binder, span_pat, definition }
            }
        }
    }
}

struct LoweredExistentialParam {
    annotations: Vec<ResolvedMeta>,
    form: ExistentialForm,
    source: t::EntityId,
}

impl<O: super::ResolutionObserver> ResolveFolder<'_, O> {
    pub(super) fn lower_quantifier(
        &mut self, params: t::CoPatId, body: t::TermId, quantifier: Quantifier, id: t::TermId,
    ) -> FoldResult<TermId> {
        let span = params.span(self.spans).clone().make(params);
        let items = flatten_textual_copattern(self.textual, params);
        let mark = self.scope.mark();
        let pats = items
            .into_iter()
            .map(|item| match item {
                | TextualCoPatItem::Pat(pat) => self.lower_pattern(pat, None),
                | TextualCoPatItem::Dtor(_) => Err(self.report_primary(
                    super::ResolveError::QuantifierParameterNotPattern(span.clone()),
                )),
            })
            .collect_reported::<Vec<_>>();
        let body_res = self.lower_term(body);
        self.scope.rollback(mark);
        let (pats, body) = (pats?, body_res?);
        Ok(self.quantify_parameters(pats, quantifier, body, id.into()))
    }

    fn quantify_parameters(
        &mut self, pats: Vec<PatId>, quantifier: Quantifier, body: TermId, source: t::EntityId,
    ) -> TermId {
        if let Quantifier::Pi = quantifier {
            return self.builder.alloc_term(Pi(pats, body), source);
        }
        pats.into_iter().rev().fold(body, |body, pat| {
            let term: Term = match quantifier {
                | Quantifier::Pi => unreachable!("a `pi` telescope is not folded"),
                | Quantifier::ValPi => ValPi(pat, body).into(),
                | Quantifier::Sigma => Sigma(pat, body).into(),
            };
            self.builder.alloc_term(term, source)
        })
    }

    fn lower_existential_form(&mut self, pattern: t::PatId) -> FoldResult<ExistentialForm> {
        match self.textual.pats[&pattern].clone() {
            | t::Pattern::Ann(t::Ann { tm, ty }) => {
                let ty = self.lower_term(ty)?;
                let form = self.lower_existential_form(tm)?;
                let binder =
                    self.builder.alloc_pat(Ann { tm: form.binder(), ty }.into(), pattern.into());
                Ok(form.with_binder(binder))
            }
            | t::Pattern::Named(t::Named(field, inner)) => {
                let form = self.lower_existential_form(inner)?;
                let binder =
                    self.builder.alloc_pat(Named(field, form.binder()).into(), pattern.into());
                Ok(form.with_binder(binder))
            }
            | t::Pattern::Manifest(t::ManifestPattern { binder: inner, definition }) => {
                let definition = self.lower_term(definition)?;
                let binder = self.lower_pattern(inner, None)?;
                Ok(ExistentialForm::Manifest { binder, span_pat: inner, definition })
            }
            | t::Pattern::Paren(t::Paren(patterns)) if patterns.len() == 1 => {
                self.lower_existential_form(patterns[0])
            }
            | _ => Ok(ExistentialForm::Abstract {
                binder: self.lower_pattern(pattern, None)?,
                span_pat: pattern,
            }),
        }
    }

    fn lower_existential_param(
        &mut self,
        t::ExistentialParameter { annotations, binder: pattern }: t::ExistentialParameter,
    ) -> FoldResult<LoweredExistentialParam> {
        let source = pattern.into();
        let form = self.lower_existential_form(pattern);
        let annotations = self.lower_existential_annotations(pattern, annotations);
        Ok(LoweredExistentialParam { annotations: annotations?, form: form?, source })
    }

    pub(super) fn lower_exists(&mut self, first: t::Exists, id: t::TermId) -> FoldResult<TermId> {
        let layers = std::iter::successors(Some(first), |current| {
            match &self.textual.terms[&current.body] {
                | t::Term::Exists(nested) => Some(nested.clone()),
                | _ => None,
            }
        })
        .collect::<Vec<_>>();
        let body_term = layers.last().expect("existential telescopes are nonempty").body;
        let mark = self.scope.mark();
        let params = layers
            .into_iter()
            .flat_map(|exists| exists.parameters)
            .map(|param| self.lower_existential_param(param))
            .collect_reported::<Vec<_>>();
        let body = self.lower_term(body_term);
        self.scope.rollback(mark);
        let (params, body) = (params?, body?);

        let exists = params.into_iter().rev().fold(body, |body, param| {
            let LoweredExistentialParam { annotations, form, source } = param;
            let term: Term = match form {
                | ExistentialForm::Abstract { binder, .. } => Sigma(binder, body).into(),
                | ExistentialForm::Manifest { binder, definition, .. } => {
                    ManifestExists { binder, definition, body }.into()
                }
            };
            let term = self.builder.alloc_term(term, id.into());
            annotations
                .into_iter()
                .rev()
                .fold(term, |body, meta| self.builder.alloc_term(MetaTerm { meta, body }, source))
        });
        let vtype = self.builder.vtype(id.into());
        Ok(self.builder.alloc_term(Ann { tm: exists, ty: vtype }, id.into()))
    }

    pub(super) fn lower_pack(
        &mut self, t::Pack { parameters, body: body_term }: t::Pack, id: t::TermId,
    ) -> FoldResult<TermId> {
        let mark = self.scope.mark();
        let mut lowered = Vec::with_capacity(parameters.len());
        let mut failed: Option<ReportedError> = None;
        for t::PackParameter { parameter, evidence } in parameters {
            let ev_res = evidence.map(|ev| self.lower_term(ev)).transpose();
            let param_res = self.lower_existential_param(parameter);
            match (ev_res, param_res) {
                | (Ok(ev), Ok(param)) => lowered.push((ev, param)),
                | (Err(err), _) | (_, Err(err)) => failed = Some(err),
            }
        }
        let body = self.lower_term(body_term);
        self.scope.rollback(mark);
        if let Some(err) = failed {
            let _ = body?;
            return Err(err);
        }
        let body = body?;
        lowered.into_iter().rev().try_fold(body, |body, (evidence, param)| {
            let LoweredExistentialParam { annotations, form, source } = param;
            let term: Term = match (form, evidence) {
                | (ExistentialForm::Manifest { binder, definition, .. }, None) => {
                    Pack { mode: PackMode::Disclosed, binder, definition, body }.into()
                }
                | (ExistentialForm::Abstract { binder, .. }, Some(definition)) => {
                    Pack { mode: PackMode::Sealed, binder, definition, body }.into()
                }
                | (ExistentialForm::Manifest { span_pat, .. }, Some(_)) => {
                    let span = span_pat.span(self.spans).clone().make(span_pat);
                    return Err(self.report_primary(
                        super::ResolveError::PackParameterRedundantEvidence(span),
                    ));
                }
                | (ExistentialForm::Abstract { span_pat, .. }, None) => {
                    let span = span_pat.span(self.spans).clone().make(span_pat);
                    return Err(
                        self.report_primary(super::ResolveError::PackParameterNeedsEvidence(span))
                    );
                }
            };
            let term = self.builder.alloc_term(term, id.into());
            Ok(annotations
                .into_iter()
                .rev()
                .fold(term, |body, meta| self.builder.alloc_term(MetaTerm { meta, body }, source)))
        })
    }

    pub(super) fn lower_data(
        &mut self, t::Data { arms }: t::Data, source: t::EntityId,
    ) -> FoldResult<TermId> {
        let arms = arms
            .into_iter()
            .map(|t::DataArm { name, param }| Ok(DataArm { name, param: self.lower_term(param)? }))
            .collect_reported::<Vec<_>>()?;
        let data = self.builder.alloc_term(Data { arms }, source);
        let vtype = self.builder.vtype(source);
        Ok(self.builder.alloc_term(Ann { tm: data, ty: vtype }, source))
    }

    pub(super) fn lower_codata(
        &mut self, t::CoData { arms }: t::CoData, source: t::EntityId,
    ) -> FoldResult<TermId> {
        let arms = arms
            .into_iter()
            .map(|t::CoDataArm { name, params, out }| {
                if let Some(params) = params {
                    let span = params.span(self.spans).clone().make(params);
                    let items = flatten_textual_copattern(self.textual, params);
                    let mark = self.scope.mark();
                    let pats = items
                        .into_iter()
                        .map(|item| match item {
                            | TextualCoPatItem::Pat(pat) => self.lower_pattern(pat, None),
                            | TextualCoPatItem::Dtor(_) => Err(self.report_primary(
                                super::ResolveError::QuantifierParameterNotPattern(span.clone()),
                            )),
                        })
                        .collect_reported::<Vec<_>>();
                    let out_res = self.lower_term(out);
                    self.scope.rollback(mark);
                    let out = self.quantify_parameters(pats?, Quantifier::Pi, out_res?, source);
                    Ok(CoDataArm { name, out })
                } else {
                    Ok(CoDataArm { name, out: self.lower_term(out)? })
                }
            })
            .collect_reported::<Vec<_>>()?;
        let codata = self.builder.alloc_term(CoData { arms }, source);
        let ctype = self.builder.ctype(source);
        Ok(self.builder.alloc_term(Ann { tm: codata, ty: ctype }, source))
    }
}
