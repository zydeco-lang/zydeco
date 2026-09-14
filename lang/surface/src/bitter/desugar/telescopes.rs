//! Parameter lowering and quantifier/package construction.

use super::*;

#[derive(Copy, Clone)]
pub(super) enum Quantifier {
    Pi,
    ValPi,
    Sigma,
}

pub(super) struct ParameterTelescope {
    parameters: Vec<b::PatId>,
    source: t::EntityId,
}

impl ParameterTelescope {
    pub(super) fn desugar(
        params: t::CoPatId, source: t::EntityId, desugarer: &mut DesugarFolder,
    ) -> Result<Self> {
        let span = params.span(desugarer.spans).clone().make(params);
        let b::Appli(parameters) = desugarer.copattern(params)?;
        let parameters = parameters
            .into_iter()
            .map(|parameter| match parameter {
                | b::CoPatternItem::Pat(pattern) => Ok(pattern),
                | b::CoPatternItem::Dtor(_) => {
                    Err(desugarer.report(DesugarError::QuantifierParameterNotPattern(span.clone())))
                }
            })
            .collect_reported()?;
        Ok(Self { parameters, source })
    }

    pub(super) fn quantify(
        self, quantifier: Quantifier, body: b::TermId, builder: &mut BitterBuilder,
    ) -> b::TermId {
        self.parameters.into_iter().rev().fold(body, |body, parameter| {
            let term = match quantifier {
                | Quantifier::Pi => b::Pi(parameter, body).into(),
                | Quantifier::ValPi => b::ValPi(parameter, body).into(),
                | Quantifier::Sigma => b::Sigma(parameter, body).into(),
            };
            Alloc::alloc(builder, term, self.source)
        })
    }
}

pub(super) enum ExistentialParameterForm {
    Abstract(b::PatId),
    Manifest { binder: b::PatId, definition: b::TermId },
}

impl ExistentialParameterForm {
    pub(super) fn binder(&self) -> b::PatId {
        match self {
            | Self::Abstract(binder) | Self::Manifest { binder, .. } => *binder,
        }
    }

    pub(super) fn with_binder(self, binder: b::PatId) -> Self {
        match self {
            | Self::Abstract(_) => Self::Abstract(binder),
            | Self::Manifest { definition, .. } => Self::Manifest { binder, definition },
        }
    }

    pub(super) fn desugar(pattern: t::PatId, desugarer: &mut DesugarFolder) -> Result<Self> {
        match desugarer.lookup_pat(pattern) {
            | t::Pattern::Ann(t::Ann { tm, ty }) => {
                let form = Self::desugar(tm, desugarer)?;
                let ty = desugarer.term(ty)?;
                let binder = Alloc::alloc(
                    &mut desugarer.builder,
                    b::Ann { tm: form.binder(), ty }.into(),
                    pattern.into(),
                );
                Ok(form.with_binder(binder))
            }
            | t::Pattern::Named(t::Named(field, inner)) => {
                let form = Self::desugar(inner, desugarer)?;
                let binder = Alloc::alloc(
                    &mut desugarer.builder,
                    b::Named(field, form.binder()).into(),
                    pattern.into(),
                );
                Ok(form.with_binder(binder))
            }
            | t::Pattern::Manifest(t::ManifestPattern { binder, definition }) => {
                let binder = desugarer.pattern(binder)?;
                let definition = desugarer.term(definition)?;
                Ok(Self::Manifest { binder, definition })
            }
            | t::Pattern::Paren(t::Paren(patterns)) if patterns.len() == 1 => {
                Self::desugar(patterns[0], desugarer)
            }
            | _ => Ok(Self::Abstract(desugarer.pattern(pattern)?)),
        }
    }
}

pub(super) struct ExistentialParameter {
    annotations: Vec<t::Meta>,
    form: ExistentialParameterForm,
    source: t::EntityId,
}

impl ExistentialParameter {
    pub(super) fn desugar(
        parameter: t::ExistentialParameter, desugarer: &mut DesugarFolder,
    ) -> Result<Self> {
        let t::ExistentialParameter { annotations, binder: pattern } = parameter;
        let source = pattern.into();
        let form = ExistentialParameterForm::desugar(pattern, desugarer)?;
        let annotations = annotations
            .into_iter()
            .map(|annotation| {
                let annotation_site = annotation.inner.span(desugarer.spans).clone().make(pattern);
                let meta = desugarer.textual.semantic_meta(annotation.inner);
                match meta.specialize::<BuiltinMeta>() {
                    | Ok(Some(BuiltinMeta { role: BuiltinRole::Type(_) })) => Ok(meta),
                    | Ok(Some(BuiltinMeta { role: BuiltinRole::Value(role) })) => Err(desugarer
                        .report(DesugarError::BuiltinValueRoleOnExistentialPattern {
                            pattern: annotation_site.clone(),
                            role,
                        })),
                    | Ok(None) => Err(desugarer.report(
                        DesugarError::UnsupportedExistentialPatternMeta(annotation_site.clone()),
                    )),
                    | Err(source) => {
                        Err(desugarer.report(DesugarError::InvalidBuiltinPatternMeta {
                            pattern: annotation_site.clone(),
                            source,
                        }))
                    }
                }
            })
            .collect_reported()?;
        Ok(Self { annotations, form, source })
    }
}

pub(super) struct ExistentialTelescope {
    parameters: Vec<ExistentialParameter>,
    source: t::EntityId,
}

/// Consecutive textual `exists` nodes denote one existential telescope.
/// Keeping this normalization in desugaring makes the repeated and merged
/// surface spellings elaborate identically.
pub(super) struct TextualExistentialTelescope {
    pub(super) parameters: Vec<t::ExistentialParameter>,
    pub(super) body: t::TermId,
}

impl TextualExistentialTelescope {
    pub(super) fn new(first: t::Exists, desugarer: &DesugarFolder) -> Self {
        let layers = std::iter::successors(Some(first), |current| {
            match desugarer.lookup_term(current.body) {
                | t::Term::Exists(nested) => Some(nested),
                | _ => None,
            }
        })
        .collect::<Vec<_>>();
        let body = layers.last().expect("existential telescopes are nonempty").body;
        let parameters = layers.into_iter().flat_map(|exists| exists.parameters).collect();
        Self { parameters, body }
    }
}

impl ExistentialTelescope {
    pub(super) fn desugar(
        parameters: Vec<t::ExistentialParameter>, source: t::EntityId,
        desugarer: &mut DesugarFolder,
    ) -> Result<Self> {
        let parameters = parameters
            .into_iter()
            .map(|parameter| ExistentialParameter::desugar(parameter, desugarer))
            .collect_reported()?;
        Ok(Self { parameters, source })
    }

    pub(super) fn quantify(self, body: b::TermId, builder: &mut BitterBuilder) -> b::TermId {
        self.parameters.into_iter().rev().fold(body, |body, parameter| {
            let ExistentialParameter { annotations, form, source } = parameter;
            let term = match form {
                | ExistentialParameterForm::Abstract(binder) => b::Sigma(binder, body).into(),
                | ExistentialParameterForm::Manifest { binder, definition } => {
                    b::ManifestExists { binder, definition, body }.into()
                }
            };
            let term = Alloc::alloc(builder, term, self.source);
            annotations
                .into_iter()
                .rev()
                .fold(term, |term, meta| Alloc::alloc(builder, b::MetaT(meta, term).into(), source))
        })
    }

    /// Nest package layers over the payload. Manifest parameters disclose
    /// their witness in `as`; abstract parameters take sealed evidence
    /// after `is`; anything else is rejected with a pointed error.
    pub(super) fn quantify_pack(
        parameters: Vec<t::PackParameter>, body: b::TermId, source: t::EntityId,
        desugarer: &mut DesugarFolder,
    ) -> Result<b::TermId> {
        parameters.into_iter().rev().try_fold(body, |body, parameter| {
            let t::PackParameter { parameter, evidence } = parameter;
            let evidence = evidence.map(|evidence| desugarer.term(evidence)).transpose()?;
            let ExistentialParameter { annotations, form, source: parameter_source } =
                ExistentialParameter::desugar(parameter, desugarer)?;
            let term = match (form, evidence) {
                | (ExistentialParameterForm::Manifest { binder, definition }, None) => {
                    b::Pack { mode: b::PackMode::Disclosed, binder, definition, body }.into()
                }
                | (ExistentialParameterForm::Abstract(binder), Some(definition)) => {
                    b::Pack { mode: b::PackMode::Sealed, binder, definition, body }.into()
                }
                | (ExistentialParameterForm::Manifest { binder, .. }, Some(_)) => {
                    let span = binder.span(desugarer).clone().make(binder);
                    return Err(
                        desugarer.report(DesugarError::PackParameterRedundantEvidence(span))
                    );
                }
                | (ExistentialParameterForm::Abstract(binder), None) => {
                    let span = binder.span(desugarer).clone().make(binder);
                    return Err(desugarer.report(DesugarError::PackParameterNeedsEvidence(span)));
                }
            };
            let term = Alloc::alloc(&mut desugarer.builder, term, source);
            let term = annotations.into_iter().rev().fold(term, |term, meta| {
                Alloc::alloc(&mut desugarer.builder, b::MetaT(meta, term).into(), parameter_source)
            });
            Ok(term)
        })
    }
}
